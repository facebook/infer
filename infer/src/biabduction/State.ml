(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** State of symbolic execution *)

(** failure statistics for symbolic execution on a given node *)
type failure_stats =
  { mutable instr_fail: int
  ; (* number of instruction failures since the current node started *)
    mutable instr_ok: int
  ; (* number of instruction successes since the current node started *)
    mutable node_fail: int
  ; (* number of node failures (i.e. at least one instruction failure) *)
    mutable node_ok: int
  ; (* number of node successes (i.e. no instruction failures) *)
    mutable first_failure: (Location.t * Procdesc.Node.t * int * Errlog.loc_trace * exn) option
        (* exception at the first failure *) }

module NodeHash = Procdesc.NodeHash

type t =
  { mutable diverging_states_node: Paths.PathSet.t
        (** Diverging states since the last reset for the node *)
  ; mutable diverging_states_proc: Paths.PathSet.t
        (** Diverging states since the last reset for the procedure *)
  ; mutable last_path: (Paths.Path.t * PredSymb.path_pos option) option  (** Last path seen *)
  ; mutable last_prop_tenv_pdesc: (Prop.normal Prop.t * Tenv.t * Procdesc.t) option
        (** Last prop,tenv,pdesc seen *)
  ; failure_map: failure_stats NodeHash.t  (** Map visited nodes to failure statistics *) }

let initial () =
  { diverging_states_node= Paths.PathSet.empty
  ; diverging_states_proc= Paths.PathSet.empty
  ; last_path= None
  ; last_prop_tenv_pdesc= None
  ; failure_map= NodeHash.create 1 }


(** Global state *)
let gs = ref (initial ())

let () = AnalysisGlobalState.register_ref gs ~init:initial

let reset_diverging_states_node () = !gs.diverging_states_node <- Paths.PathSet.empty

let reset () = gs := initial ()

let get_failure_stats node =
  try NodeHash.find !gs.failure_map node
  with Caml.Not_found ->
    let fs = {instr_fail= 0; instr_ok= 0; node_fail= 0; node_ok= 0; first_failure= None} in
    NodeHash.add !gs.failure_map node fs ;
    fs


let add_diverging_states pset =
  !gs.diverging_states_proc <- Paths.PathSet.union pset !gs.diverging_states_proc ;
  !gs.diverging_states_node <- Paths.PathSet.union pset !gs.diverging_states_node


let get_diverging_states_node () = !gs.diverging_states_node

let get_diverging_states_proc () = !gs.diverging_states_proc

let get_inst_update pos =
  let loc = AnalysisState.get_loc_exn () in
  Predicates.inst_update loc pos


let get_path () =
  match !gs.last_path with
  | None ->
      (Paths.Path.start (AnalysisState.get_node_exn ()), None)
  | Some (path, pos_opt) ->
      (path, pos_opt)


let get_loc_trace () : Errlog.loc_trace =
  let path, pos_opt = get_path () in
  Paths.Path.create_loc_trace path pos_opt


let get_prop_tenv_pdesc () = !gs.last_prop_tenv_pdesc

(** extract the footprint of the prop, and turn it into a normalized precondition using spec
    variables *)
let extract_pre p tenv pdesc abstract_fun =
  let sub =
    let idlist = Prop.free_vars p |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys in
    let count = ref 0 in
    Predicates.subst_of_list
      (List.map
         ~f:(fun id ->
           incr count ;
           (id, Exp.Var (Ident.create_normal Ident.name_spec !count)) )
         idlist )
  in
  let _, p' = PropUtil.remove_locals_formals tenv pdesc p in
  let pre, _ = Prop.extract_spec p' in
  let pre' = try abstract_fun tenv pre with exn when Exception.exn_not_failure exn -> pre in
  Prop.normalize tenv (Prop.prop_sub sub pre')


(** return the normalized precondition extracted form the last prop seen, if any the abstraction
    function is a parameter to get around module dependencies *)
let get_normalized_pre (abstract_fun : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t) :
    Prop.normal Prop.t option =
  match get_prop_tenv_pdesc () with
  | None ->
      None
  | Some (prop, tenv, pdesc) ->
      Some (extract_pre prop tenv pdesc abstract_fun)


let get_path_pos () =
  let pname =
    match get_prop_tenv_pdesc () with
    | Some (_, _, pdesc) ->
        Procdesc.get_proc_name pdesc
    | None ->
        Procname.from_string_c_fun "unknown_procedure"
  in
  let nid = Procdesc.Node.get_id (AnalysisState.get_node_exn ()) in
  (pname, (nid :> int))


let mark_execution_start node =
  let fs = get_failure_stats node in
  fs.instr_ok <- 0 ;
  fs.instr_fail <- 0


let mark_execution_end node =
  let fs = get_failure_stats node in
  let success = Int.equal fs.instr_fail 0 in
  fs.instr_ok <- 0 ;
  fs.instr_fail <- 0 ;
  if success then fs.node_ok <- fs.node_ok + 1 else fs.node_fail <- fs.node_fail + 1


let mark_instr_ok () =
  let fs = get_failure_stats (AnalysisState.get_node_exn ()) in
  fs.instr_ok <- fs.instr_ok + 1


let mark_instr_fail exn =
  let loc = AnalysisState.get_loc_exn () in
  let node = AnalysisState.get_node_exn () in
  let session = AnalysisState.get_session () in
  let loc_trace = get_loc_trace () in
  let fs = get_failure_stats node in
  if is_none fs.first_failure then
    fs.first_failure <- Some (loc, node, (session :> int), loc_trace, exn) ;
  fs.instr_fail <- fs.instr_fail + 1


type log_issue = ?node:Procdesc.Node.t -> ?loc:Location.t -> ?ltr:Errlog.loc_trace -> exn -> unit

let process_execution_failures (log_issue : log_issue) =
  let do_failure _ fs =
    (* L.out "Node:%a node_ok:%d node_fail:%d@." Procdesc.Node.pp node fs.node_ok fs.node_fail; *)
    match (fs.node_ok, fs.first_failure) with
    | 0, Some (loc, node, _, loc_trace, exn) when not Config.debug_exceptions ->
        let error = Exceptions.recognize_exception exn in
        let desc' = Localise.verbatim_desc ("exception: " ^ error.issue_type.unique_id) in
        let exn' = Exceptions.Analysis_stops (desc', error.ocaml_pos) in
        log_issue ~loc ~node ~ltr:loc_trace exn'
    | _ ->
        ()
  in
  NodeHash.iter do_failure !gs.failure_map


let set_path path pos_opt = !gs.last_path <- Some (path, pos_opt)

let set_prop_tenv_pdesc prop tenv pdesc = !gs.last_prop_tenv_pdesc <- Some (prop, tenv, pdesc)
