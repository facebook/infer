(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** State of symbolic execution *)

module L = Logging
module F = Format

type const_map = Cfg.Node.t -> Exp.t -> Const.t option

(** failure statistics for symbolic execution on a given node *)
type failure_stats = {
  mutable instr_fail: int; (* number of instruction failures since the current node started *)
  mutable instr_ok: int; (* number of instruction successes since the current node started *)
  mutable node_fail: int; (* number of node failures (i.e. at least one instruction failure) *)
  mutable node_ok: int; (* number of node successes (i.e. no instruction failures) *)
  mutable first_failure :
    (Location.t * (int * int) * int * Errlog.loc_trace * exn) option
    (* exception at the first failure *)
}

module NodeHash = Cfg.NodeHash

type t = {
  mutable const_map : const_map;
  (** Constant map for the procedure *)

  mutable diverging_states_node : Paths.PathSet.t;
  (** Diverging states since the last reset for the node *)

  mutable diverging_states_proc : Paths.PathSet.t;
  (** Diverging states since the last reset for the procedure *)

  mutable last_instr : Sil.instr option;
  (** Last instruction seen *)

  mutable last_node : Cfg.Node.t;
  (** Last node seen *)

  mutable last_path : (Paths.Path.t * (PredSymb.path_pos option)) option;
  (** Last path seen *)

  mutable last_prop_tenv_pdesc : (Prop.normal Prop.t * Tenv.t * Cfg.Procdesc.t) option;
  (** Last prop,tenv,pdesc seen *)

  mutable last_session : int;
  (** Last session seen *)

  failure_map : failure_stats NodeHash.t;
  (** Map visited nodes to failure statistics *)
}

let initial () = {
  const_map = (fun _ _ -> None);
  diverging_states_node = Paths.PathSet.empty;
  diverging_states_proc = Paths.PathSet.empty;
  last_instr = None;
  last_node = Cfg.Node.dummy ();
  last_path = None;
  last_prop_tenv_pdesc = None;
  last_session = 0;
  failure_map = NodeHash.create 1;
}

(** Global state *)
let gs = ref (initial ())

(** Return the old state, and revert the current state to the initial one. *)
let save_state () =
  let old = !gs in
  gs := initial ();
  old

(** Restore the old state. *)
let restore_state st =
  gs := st

let reset_diverging_states_node () =
  !gs.diverging_states_node <- Paths.PathSet.empty

let reset () =
  gs := initial ()

let get_failure_stats node =
  try NodeHash.find !gs.failure_map node
  with Not_found ->
    let fs = { instr_fail = 0; instr_ok = 0; node_fail = 0; node_ok = 0; first_failure = None } in
    NodeHash.add !gs.failure_map node fs;
    fs

let add_diverging_states pset =
  !gs.diverging_states_proc <- Paths.PathSet.union pset !gs.diverging_states_proc;
  !gs.diverging_states_node <- Paths.PathSet.union pset !gs.diverging_states_node

let get_diverging_states_node () =
  !gs.diverging_states_node

let get_diverging_states_proc () =
  !gs.diverging_states_proc

let get_instr () =
  !gs.last_instr

let get_loc () = match !gs.last_instr with
  | Some instr -> Sil.instr_get_loc instr
  | None -> Cfg.Node.get_loc !gs.last_node

let get_node () =
  !gs.last_node

(** simple key for a node: just look at the instructions *)
let node_simple_key node =
  let key = ref [] in
  let add_key k = key := k :: !key in
  let do_instr instr =
    if Sil.instr_is_auxiliary instr then ()
    else
      match instr with
      | Sil.Load _ -> add_key 1
      | Sil.Store _ -> add_key 2
      | Sil.Prune _ -> add_key 3
      | Sil.Call _ -> add_key 4
      | Sil.Nullify _ -> add_key 5
      | Sil.Abstract _ -> add_key 6
      | Sil.Remove_temps _ -> add_key 7
      | Sil.Declare_locals _ -> add_key 8 in
  IList.iter do_instr (Cfg.Node.get_instrs node);
  Hashtbl.hash !key

(** key for a node: look at the current node, successors and predecessors *)
let node_key node =
  let succs = Cfg.Node.get_succs node in
  let preds = Cfg.Node.get_preds node in
  let v = (node_simple_key node, IList.map node_simple_key succs, IList.map node_simple_key preds) in
  Hashtbl.hash v

(** normalize the list of instructions by renaming let-bound ids *)
let instrs_normalize instrs =
  let bound_ids =
    let do_instr ids = function
      | Sil.Load (id, _, _, _) -> id :: ids
      | _ -> ids in
    IList.fold_left do_instr [] instrs in
  let subst =
    let count = ref min_int in
    let gensym id =
      incr count;
      Ident.set_stamp id !count in
    Sil.sub_of_list (IList.map (fun id -> (id, Exp.Var (gensym id))) bound_ids) in
  IList.map (Sil.instr_sub subst) instrs

(** Create a function to find duplicate nodes.
    A node is a duplicate of another one if they have the same kind and location
    and normalized (w.r.t. renaming of let - bound ids) list of instructions. *)
let mk_find_duplicate_nodes proc_desc : (Cfg.Node.t -> Cfg.NodeSet.t) =
  let module M = (* map from (loc,kind) *)
    Map.Make(struct
      type t = Location.t * Cfg.Node.nodekind
      let compare (loc1, k1) (loc2, k2) =
        let n = Location.compare loc1 loc2 in
        if n <> 0 then n else Cfg.Node.kind_compare k1 k2
    end) in

  let module S = (* set of nodes with normalized insructions *)
    Set.Make(struct
      type t = Cfg.Node.t * Sil.instr list
      let compare (n1, _) (n2, _) =
        Cfg.Node.compare n1 n2
    end) in

  let get_key node = (* map key *)
    let loc = Cfg.Node.get_loc node in
    let kind = Cfg.Node.get_kind node in
    (loc, kind) in

  let map =
    let m = ref M.empty in (* map from (loc, kind) to (instructions, node) set *)

    let module E = struct
      (** Threshold: do not build the map if too many nodes are duplicates. *)
      let threshold = 100
      exception Threshold
    end in

    let do_node node =
      let normalized_instrs = instrs_normalize (Cfg.Node.get_instrs node) in
      let key = get_key node in
      let s = try M.find key !m with Not_found -> S.empty in
      if S.cardinal s > E.threshold then raise E.Threshold;
      let s' = S.add (node, normalized_instrs) s in
      m := M.add key s' !m in

    let nodes = Cfg.Procdesc.get_nodes proc_desc in
    try
      IList.iter do_node nodes;
      !m
    with E.Threshold ->
      M.empty in

  let find_duplicate_nodes node =
    try
      let s = M.find (get_key node) map in
      let elements = S.elements s in
      let (_, node_normalized_instrs), _ =
        let filter (node', _) = Cfg.Node.equal node node' in
        match IList.partition filter elements with
        | [this], others -> this, others
        | _ -> raise Not_found in
      let duplicates =
        let equal_normalized_instrs (_, normalized_instrs') =
          IList.compare Sil.instr_compare node_normalized_instrs normalized_instrs' = 0 in
        IList.filter equal_normalized_instrs elements in
      IList.fold_left
        (fun nset (node', _) -> Cfg.NodeSet.add node' nset)
        Cfg.NodeSet.empty duplicates
    with Not_found -> Cfg.NodeSet.singleton node in

  find_duplicate_nodes

let get_node_id () =
  Cfg.Node.get_id !gs.last_node

let get_node_id_key () =
  (Cfg.Node.get_id !gs.last_node, node_key !gs.last_node)

let get_inst_update pos =
  let loc = get_loc () in
  Sil.inst_update loc pos

let get_path () = match !gs.last_path with
  | None -> Paths.Path.start !gs.last_node, None
  | Some (path, pos_opt) -> path, pos_opt

let get_loc_trace () : Errlog.loc_trace =
  let path, pos_opt = get_path () in
  Paths.Path.create_loc_trace path pos_opt

let get_prop_tenv_pdesc () =
  !gs.last_prop_tenv_pdesc

(** extract the footprint of the prop, and turn it into a normalized precondition using spec variables *)
let extract_pre p tenv pdesc abstract_fun =
  let sub =
    let fav = Prop.prop_fav p in
    let idlist = Sil.fav_to_list fav in
    let count = ref 0 in
    Sil.sub_of_list (IList.map (fun id ->
        incr count; (id, Exp.Var (Ident.create_normal Ident.name_spec !count))) idlist) in
  let _, p' = PropUtil.remove_locals_formals tenv pdesc p in
  let pre, _ = Prop.extract_spec p' in
  let pre' = try abstract_fun tenv pre with exn when SymOp.exn_not_failure exn -> pre in
  Prop.normalize tenv (Prop.prop_sub sub pre')

(** return the normalized precondition extracted form the last prop seen, if any
    the abstraction function is a parameter to get around module dependencies *)
let get_normalized_pre (abstract_fun : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t)
  : Prop.normal Prop.t option =
  match get_prop_tenv_pdesc () with
  | None -> None
  | Some (prop, tenv, pdesc) ->
      Some (extract_pre prop tenv pdesc abstract_fun)

let get_session () =
  !gs.last_session

let get_path_pos () =
  let pname = match get_prop_tenv_pdesc () with
    | Some (_, _, pdesc) -> Cfg.Procdesc.get_proc_name pdesc
    | None -> Procname.from_string_c_fun "unknown_procedure" in
  let nid = get_node_id () in
  (pname, (nid :> int))

let mark_execution_start node =
  let fs = get_failure_stats node in
  fs.instr_ok <- 0;
  fs.instr_fail <- 0

let mark_execution_end node =
  let fs = get_failure_stats node in
  let success = fs.instr_fail = 0 in
  fs.instr_ok <- 0;
  fs.instr_fail <- 0;
  if success then fs.node_ok <- fs.node_ok + 1
  else fs.node_fail <- fs.node_fail + 1

let mark_instr_ok () =
  let fs = get_failure_stats (get_node ()) in
  fs.instr_ok <- fs.instr_ok + 1

let mark_instr_fail exn =
  let loc = get_loc () in
  let key = (get_node_id_key () :> int * int) in
  let session = get_session () in
  let loc_trace = get_loc_trace () in
  let fs = get_failure_stats (get_node ()) in
  if fs.first_failure = None then
    fs.first_failure <- Some (loc, key, (session :> int), loc_trace, exn);
  fs.instr_fail <- fs.instr_fail + 1

type log_issue =
  Procname.t ->
  ?loc: Location.t ->
  ?node_id: (int * int) ->
  ?session: int ->
  ?ltr: Errlog.loc_trace ->
  exn ->
  unit

let process_execution_failures (log_issue : log_issue) pname =
  let do_failure _ fs =
    (* L.err "Node:%a node_ok:%d node_fail:%d@." Cfg.Node.pp node fs.node_ok fs.node_fail; *)
    match fs.node_ok, fs.first_failure with
    | 0, Some (loc, key, _, loc_trace, exn) ->
        let ex_name, _, ml_loc_opt, _, _, _, _ = Exceptions.recognize_exception exn in
        let desc' = Localise.verbatim_desc ("exception: " ^ Localise.to_string ex_name) in
        let exn' = Exceptions.Analysis_stops (desc', ml_loc_opt) in
        log_issue pname ~loc ~node_id:key ~ltr:loc_trace exn'
    | _ -> () in
  NodeHash.iter do_failure !gs.failure_map

let set_instr (instr: Sil.instr) =
  !gs.last_instr <- Some instr

let set_path path pos_opt =
  !gs.last_path <- Some (path, pos_opt)

let set_prop_tenv_pdesc prop tenv pdesc =
  !gs.last_prop_tenv_pdesc <- Some (prop, tenv, pdesc)

let set_node (node: Cfg.node) =
  !gs.last_instr <- None;
  !gs.last_node <- node

let set_session (session: int) =
  !gs.last_session <- session

let get_const_map () =
  !gs.const_map

let set_const_map const_map' =
  !gs.const_map <- const_map'
