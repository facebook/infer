(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Interprocedural Analysis *)

module L = Logging
module F = Format

(** A node with a number of visits *)
type visitednode = {node: Procdesc.Node.t; visits: int}

(** Set of nodes with number of visits *)
module NodeVisitSet = Caml.Set.Make (struct
  type t = visitednode

  let compare_ids n1 n2 =
    (* higher id is better *)
    Procdesc.Node.compare n2 n1


  let compare_distance_to_exit {node= n1} {node= n2} =
    (* smaller means higher priority *)
    let n =
      match (Procdesc.Node.get_distance_to_exit n1, Procdesc.Node.get_distance_to_exit n2) with
      | None, None ->
          0
      | None, Some _ ->
          1
      | Some _, None ->
          -1
      | Some d1, Some d2 ->
          (* shorter distance to exit is better *)
          Int.compare d1 d2
    in
    if n <> 0 then n else compare_ids n1 n2


  let compare_number_of_visits x1 x2 =
    let n = Int.compare x1.visits x2.visits in
    (* visited fewer times is better *)
    if n <> 0 then n else compare_distance_to_exit x1 x2


  let compare x1 x2 =
    if !BiabductionConfig.footprint then
      match Config.biabduction_worklist_mode with
      | 0 ->
          compare_ids x1.node x2.node
      | 1 ->
          compare_distance_to_exit x1 x2
      | _ ->
          compare_number_of_visits x1 x2
    else compare_ids x1.node x2.node
end)

(** Table for the results of the join operation on nodes. *)
module Join_table : sig
  type t

  val add : t -> Procdesc.Node.id -> Paths.PathSet.t -> unit

  val create : unit -> t

  val find : t -> Procdesc.Node.id -> Paths.PathSet.t
end = struct
  type t = (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t

  let create () : t = Hashtbl.create 11

  let find table i = try Hashtbl.find table i with Caml.Not_found -> Paths.PathSet.empty

  let add table i dset = Hashtbl.replace table i dset
end

(* =============== START of module Worklist =============== *)
module Worklist = struct
  type t =
    { join_table: Join_table.t  (** Table of join results *)
    ; path_set_todo: (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t  (** Pathset todo *)
    ; path_set_visited: (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t  (** Pathset visited *)
    ; mutable todo_set: NodeVisitSet.t  (** Set of nodes still to do, with visit count *)
    ; mutable visit_map: int Procdesc.NodeMap.t  (** Map from nodes done to visit count *) }

  let create () =
    { join_table= Join_table.create ()
    ; path_set_todo= Hashtbl.create 11
    ; path_set_visited= Hashtbl.create 11
    ; todo_set= NodeVisitSet.empty
    ; visit_map= Procdesc.NodeMap.empty }


  let is_empty (wl : t) : bool = NodeVisitSet.is_empty wl.todo_set

  let add (wl : t) (node : Procdesc.Node.t) : unit =
    let visits =
      (* recover visit count if it was visited before *)
      try Procdesc.NodeMap.find node wl.visit_map with Caml.Not_found -> 0
    in
    wl.todo_set <- NodeVisitSet.add {node; visits} wl.todo_set


  (** remove the minimum element from the worklist, and increase its number of visits *)
  let remove (wl : t) : Procdesc.Node.t =
    try
      let min = NodeVisitSet.min_elt wl.todo_set in
      wl.todo_set <- NodeVisitSet.remove min wl.todo_set ;
      wl.visit_map <- Procdesc.NodeMap.add min.node (min.visits + 1) wl.visit_map ;
      (* increase the visits *)
      min.node
    with Caml.Not_found ->
      L.internal_error "@\n...Work list is empty! Impossible to remove edge...@\n" ;
      assert false
end

(* =============== END of module Worklist =============== *)

let path_set_create_worklist proc_cfg =
  (* TODO: reimplement compute_distance_to_exit_node in ProcCfg, and use that instead *)
  State.reset () ;
  Procdesc.compute_distance_to_exit_node (ProcCfg.Exceptional.proc_desc proc_cfg) ;
  Worklist.create ()


let htable_retrieve (htable : (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t) (key : Procdesc.Node.id)
    : Paths.PathSet.t =
  try Hashtbl.find htable key
  with Caml.Not_found ->
    Hashtbl.replace htable key Paths.PathSet.empty ;
    Paths.PathSet.empty


(** Add [d] to the pathset todo at [node] returning true if changed *)
let path_set_put_todo (wl : Worklist.t) (node : Procdesc.Node.t) (d : Paths.PathSet.t) : bool =
  if Paths.PathSet.is_empty d then false
  else
    let node_id = Procdesc.Node.get_id node in
    let old_todo = htable_retrieve wl.Worklist.path_set_todo node_id in
    let old_visited = htable_retrieve wl.Worklist.path_set_visited node_id in
    let d' = Paths.PathSet.diff d old_visited in
    (* differential fixpoint *)
    let todo_new = Paths.PathSet.union old_todo d' in
    Hashtbl.replace wl.Worklist.path_set_todo node_id todo_new ;
    not (Paths.PathSet.equal old_todo todo_new)


let path_set_checkout_todo (wl : Worklist.t) (node : Procdesc.Node.t) : Paths.PathSet.t =
  try
    let node_id = Procdesc.Node.get_id node in
    let todo = Hashtbl.find wl.Worklist.path_set_todo node_id in
    Hashtbl.replace wl.Worklist.path_set_todo node_id Paths.PathSet.empty ;
    let visited = Hashtbl.find wl.Worklist.path_set_visited node_id in
    let new_visited = Paths.PathSet.union visited todo in
    Hashtbl.replace wl.Worklist.path_set_visited node_id new_visited ;
    todo
  with Caml.Not_found ->
    L.die InternalError "could not find todo for node %a" Procdesc.Node.pp node


(* =============== END of the edge_set object =============== *)

let collect_do_abstract_pre analysis_data (pset : Propset.t) : Propset.t =
  if !BiabductionConfig.footprint then
    BiabductionConfig.run_in_re_execution_mode (Abs.lifted_abstract analysis_data) pset
  else Abs.lifted_abstract analysis_data pset


let collect_do_abstract_post ({InterproceduralAnalysis.tenv; _} as analysis_data)
    (pathset : Paths.PathSet.t) : Paths.PathSet.t =
  if !BiabductionConfig.footprint then
    L.die InternalError
      "Interproc.collect_do_abstract_post ignores the _fp part of propositions, so it should only \
       be used during re-execution." ;
  let abstract_o p =
    if Prover.check_inconsistency tenv p then None else Some (Abs.abstract analysis_data p)
  in
  Paths.PathSet.map_option abstract_o pathset


let do_join_pre plist = Dom.proplist_collapse_pre plist

let do_meet_pre tenv pset =
  if Config.meet_level > 0 then Dom.propset_meet_generate_pre tenv pset
  else Propset.to_proplist pset


(** Find the preconditions in the current spec table, apply meet then join, and return the joined
    preconditions *)
let collect_preconditions ({InterproceduralAnalysis.tenv; _} as analysis_data) summary :
    Prop.normal BiabductionSummary.Jprop.t list =
  let collect_do_abstract_one tenv prop =
    if !BiabductionConfig.footprint then
      BiabductionConfig.run_in_re_execution_mode (Abs.abstract_no_symop tenv) prop
    else Abs.abstract_no_symop tenv prop
  in
  let pres =
    List.map
      ~f:(fun spec -> BiabductionSummary.Jprop.to_prop spec.BiabductionSummary.pre)
      (BiabductionSummary.get_specs summary)
  in
  let pset = Propset.from_proplist tenv pres in
  let pset' =
    let f p = Prop.prop_normal_vars_to_primed_vars tenv p in
    Propset.map tenv f pset
  in
  L.d_printfln "#### Extracted footprint  ####" ;
  L.d_increase_indent () ;
  Propset.d Prop.prop_emp pset' ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  L.d_ln () ;
  let pset'' = collect_do_abstract_pre analysis_data pset' in
  let plist_meet = do_meet_pre tenv pset'' in
  L.d_printfln "#### Footprint after Meet  ####" ;
  L.d_increase_indent () ;
  Propgraph.d_proplist Prop.prop_emp plist_meet ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  L.d_ln () ;
  L.d_increase_indent () ;
  (* Indent for the join output *)
  let jplist = do_join_pre tenv plist_meet in
  L.d_decrease_indent () ;
  L.d_ln () ;
  L.d_printfln "#### Footprint after Join  ####" ;
  L.d_increase_indent () ;
  BiabductionSummary.Jprop.d_list ~shallow:false jplist ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  let jplist' =
    List.map ~f:(BiabductionSummary.Jprop.map (Prop.prop_rename_primed_footprint_vars tenv)) jplist
  in
  L.d_printfln "#### Renamed footprint ####" ;
  L.d_increase_indent () ;
  BiabductionSummary.Jprop.d_list ~shallow:false jplist' ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  let jplist'' =
    let f p = Prop.prop_primed_vars_to_normal_vars tenv (collect_do_abstract_one analysis_data p) in
    List.map ~f:(BiabductionSummary.Jprop.map f) jplist'
  in
  L.d_printfln "#### Abstracted footprint ####" ;
  L.d_increase_indent () ;
  BiabductionSummary.Jprop.d_list ~shallow:false jplist'' ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  jplist''


(* =============== START of symbolic execution =============== *)

(** propagate a set of results to the given node *)
let propagate (wl : Worklist.t) pname ~is_exception (pset : Paths.PathSet.t)
    (curr_node : Procdesc.Node.t) =
  let edgeset_todo =
    (* prop must be a renamed prop by the invariant preserved by PropSet *)
    let f prop path edgeset_curr =
      let exn_opt = if is_exception then Tabulation.prop_get_exn_name pname prop else None in
      Paths.PathSet.add_renamed_prop prop
        (Paths.Path.extend curr_node exn_opt (AnalysisState.get_session ()) path)
        edgeset_curr
    in
    Paths.PathSet.fold f pset Paths.PathSet.empty
  in
  let changed = path_set_put_todo wl curr_node edgeset_todo in
  if changed then Worklist.add wl curr_node


(** propagate a set of results, including exceptions and divergence *)
let propagate_nodes_divergence ({InterproceduralAnalysis.tenv; _} as analysis_data)
    (proc_cfg : ProcCfg.Exceptional.t) (pset : Paths.PathSet.t) curr_node (wl : Worklist.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pset_exn, pset_ok = Paths.PathSet.partition (Tabulation.prop_is_exn pname) pset in
  if
    !BiabductionConfig.footprint
    && not (Paths.PathSet.is_empty (State.get_diverging_states_node ()))
  then (
    Errdesc.warning_err (AnalysisState.get_loc_exn ()) "Propagating Divergence@." ;
    let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
    let diverging_states = State.get_diverging_states_node () in
    let prop_incons =
      let mk_incons prop =
        let p_abs = Abs.abstract analysis_data prop in
        let p_zero = Prop.set p_abs ~sub:Predicates.sub_empty ~sigma:[] in
        Prop.normalize tenv (Prop.set p_zero ~pi:[Predicates.Aneq (Exp.zero, Exp.zero)])
      in
      Paths.PathSet.map mk_incons diverging_states
    in
    L.d_strln ~color:Orange "Propagating Divergence -- diverging states:" ;
    Propgraph.d_proplist Prop.prop_emp (Paths.PathSet.to_proplist prop_incons) ;
    L.d_ln () ;
    propagate wl pname ~is_exception:false prop_incons exit_node ) ;
  Container.iter curr_node
    ~fold:(ProcCfg.Exceptional.fold_normal_succs proc_cfg)
    ~f:(propagate wl pname ~is_exception:false pset_ok) ;
  Container.iter curr_node
    ~fold:(ProcCfg.Exceptional.fold_exceptional_succs proc_cfg)
    ~f:(propagate wl pname ~is_exception:true pset_exn)


(* ===================== END of symbolic execution ===================== *)
(* =============== START of forward_tabulate =============== *)

(** Symbolic execution for a Join node *)
let do_symexec_join analysis_data proc_cfg wl curr_node (edgeset_todo : Paths.PathSet.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let curr_node_id = ProcCfg.Exceptional.Node.id curr_node in
  let new_dset = edgeset_todo in
  let old_dset = Join_table.find wl.Worklist.join_table curr_node_id in
  let old_dset', new_dset' = Dom.pathset_join analysis_data old_dset new_dset in
  Join_table.add wl.Worklist.join_table curr_node_id (Paths.PathSet.union old_dset' new_dset') ;
  Container.iter curr_node ~fold:(ProcCfg.Exceptional.fold_normal_succs proc_cfg) ~f:(fun node ->
      Paths.PathSet.iter
        (fun prop path ->
          State.set_path path None ;
          propagate wl pname ~is_exception:false
            (Paths.PathSet.from_renamed_list [(prop, path)])
            node )
        new_dset' )


let prop_max_size = ref (0, Prop.prop_emp)

let prop_max_chain_size = ref (0, Prop.prop_emp)

(* Check if the prop exceeds the current max *)
let check_prop_size_ p _ =
  let size = Prop.Metrics.prop_size p in
  if size > fst !prop_max_size then (
    prop_max_size := (size, p) ;
    L.d_printfln "Prop with new max size %d:" size ;
    Prop.d_prop p ;
    L.d_ln () )


(* Check prop size and filter out possible unabstracted lists *)
let check_prop_size edgeset_todo =
  if Config.biabduction_monitor_prop_size then Paths.PathSet.iter check_prop_size_ edgeset_todo


let reset_prop_metrics () =
  prop_max_size := (0, Prop.prop_emp) ;
  prop_max_chain_size := (0, Prop.prop_emp)


exception RE_EXE_ERROR

let pp_name fmt = F.pp_print_string fmt "biabduction"

(** Perform symbolic execution for a node starting from an initial prop *)
let do_symbolic_execution ({InterproceduralAnalysis.tenv; _} as analysis_data) proc_cfg handle_exn
    (node : ProcCfg.Exceptional.Node.t) (prop : Prop.normal Prop.t) (path : Paths.Path.t) =
  State.mark_execution_start node ;
  let instrs = ProcCfg.Exceptional.instrs node in
  (* fresh normal vars must be fresh w.r.t. instructions *)
  Ident.update_name_generator (Instrs.instrs_get_normal_vars instrs) ;
  let pset =
    SymExec.node handle_exn analysis_data proc_cfg node
      (Paths.PathSet.from_renamed_list [(prop, path)])
  in
  L.d_strln ".... After Symbolic Execution ...." ;
  Propset.d prop (Paths.PathSet.to_propset tenv pset) ;
  L.d_ln () ;
  L.d_ln () ;
  State.mark_execution_end node ;
  pset


let forward_tabulate ({InterproceduralAnalysis.proc_desc; err_log; tenv; _} as analysis_data)
    proc_cfg summary wl =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let handle_exn_node curr_node exn =
    Exceptions.print_exception_html "Failure of symbolic execution: " exn ;
    let pre_opt =
      (* precondition leading to error, if any *)
      State.get_normalized_pre (fun _tenv -> Abs.abstract_no_symop analysis_data)
    in
    ( match pre_opt with
    | Some pre ->
        L.d_strln "Precondition:" ;
        Prop.d_prop pre ;
        L.d_ln ()
    | None ->
        () ) ;
    L.d_strln "SIL INSTR:" ;
    Procdesc.Node.d_instrs ~highlight:(AnalysisState.get_instr ()) curr_node ;
    L.d_ln () ;
    BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn ;
    State.mark_instr_fail exn
  in
  let exe_iter f pathset =
    let ps_size = Paths.PathSet.size pathset in
    let cnt = ref 0 in
    let exe prop path =
      State.set_path path None ;
      incr cnt ;
      f prop path !cnt ps_size
    in
    Paths.PathSet.iter exe pathset
  in
  let print_node_preamble curr_node pathset_todo =
    let log_string proc_name =
      let phase_string =
        let open BiabductionSummary in
        opt_get_phase summary |> string_of_phase_short
      in
      F.sprintf "[%s:Pending] %s" phase_string (Procname.to_string proc_name)
    in
    L.d_printfln "**** %s Node: %a, Procedure: %a, Todo: %d ****" (log_string pname)
      Procdesc.Node.pp curr_node Procname.pp pname (Paths.PathSet.size pathset_todo) ;
    L.d_increase_indent () ;
    Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset_todo) ;
    L.d_strln ".... Instructions: ...." ;
    Procdesc.Node.d_instrs ~highlight:(AnalysisState.get_instr ()) curr_node ;
    L.d_ln () ;
    L.d_ln ()
  in
  let do_prop (curr_node : ProcCfg.Exceptional.Node.t) handle_exn prop path cnt num_paths =
    L.d_printfln "Processing prop %d/%d" cnt num_paths ;
    L.d_increase_indent () ;
    try
      State.reset_diverging_states_node () ;
      let pset = do_symbolic_execution analysis_data proc_cfg handle_exn curr_node prop path in
      propagate_nodes_divergence analysis_data proc_cfg pset curr_node wl ;
      L.d_decrease_indent () ;
      L.d_ln ()
    with exn ->
      IExn.reraise_if exn ~f:(fun () ->
          (not !BiabductionConfig.footprint) || not (Exceptions.handle_exception exn) ) ;
      handle_exn exn ;
      L.d_decrease_indent () ;
      L.d_ln ()
  in
  let do_node curr_node pathset_todo handle_exn =
    check_prop_size pathset_todo ;
    print_node_preamble curr_node pathset_todo ;
    match Procdesc.Node.get_kind curr_node with
    | Join_node ->
        do_symexec_join analysis_data proc_cfg wl curr_node pathset_todo
    | Stmt_node _ | Prune_node _ | Exit_node | Skip_node _ | Start_node ->
        exe_iter (do_prop curr_node handle_exn) pathset_todo
  in
  let do_node_and_handle curr_node =
    let pathset_todo = path_set_checkout_todo wl curr_node in
    try
      let handle_exn_called = ref false in
      let handle_exn exn =
        handle_exn_called := true ;
        handle_exn_node curr_node exn
      in
      do_node curr_node pathset_todo handle_exn
    with exn ->
      IExn.reraise_if exn ~f:(fun () -> not (Exceptions.handle_exception exn)) ;
      handle_exn_node curr_node exn ;
      if not !BiabductionConfig.footprint then raise RE_EXE_ERROR
  in
  while not (Worklist.is_empty wl) do
    let curr_node = Worklist.remove wl in
    AnalysisCallbacks.html_debug_new_node_session ~pp_name curr_node ~f:(fun () ->
        do_node_and_handle curr_node )
  done ;
  L.d_strln ".... Work list empty. Stop ...." ;
  L.d_ln ()


(** Remove locals and formals, and check if the address of a stack variable is left in the result *)
let remove_locals_formals_and_check {InterproceduralAnalysis.tenv; _} proc_cfg p =
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let _pvars, p' = PropUtil.remove_locals_formals tenv pdesc p in
  p'


(** Collect the analysis results for the exit node. *)
let collect_analysis_result analysis_data wl proc_cfg : Paths.PathSet.t =
  let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
  let exit_node_id = ProcCfg.Exceptional.Node.id exit_node in
  let pathset = htable_retrieve wl.Worklist.path_set_visited exit_node_id in
  Paths.PathSet.map (remove_locals_formals_and_check analysis_data proc_cfg) pathset


let vset_add_path vset path =
  Paths.Path.fold_all_nodes_nocalls path ~init:vset ~f:(fun vset n -> Procdesc.NodeSet.add n vset)


let vset_add_pathset vset pathset =
  Paths.PathSet.fold (fun _ path vset -> vset_add_path vset path) pathset vset


let compute_visited vset =
  let res = ref BiabductionSummary.Visitedset.empty in
  let node_get_all_lines n =
    let node_loc = Procdesc.Node.get_loc n in
    let lines =
      node_loc.Location.line
      :: IContainer.rev_map_to_list ~fold:Instrs.fold
           ~f:(fun instr -> (Sil.location_of_instr instr).Location.line)
           (ProcCfg.Exceptional.instrs n)
    in
    List.remove_consecutive_duplicates ~equal:Int.equal (List.sort ~compare:Int.compare lines)
  in
  let do_node n =
    res := BiabductionSummary.Visitedset.add (Procdesc.Node.get_id n, node_get_all_lines n) !res
  in
  Procdesc.NodeSet.iter do_node vset ;
  !res


(* Extract specs from a pathset, after the footprint phase. The postconditions will be thrown away
   by the re-execution phase, but they are first used to detect custom errors. *)
let extract_specs ({InterproceduralAnalysis.tenv; _} as analysis_data) pdesc pathset :
    Prop.normal BiabductionSummary.spec list =
  if not !BiabductionConfig.footprint then
    L.die InternalError
      "Interproc.extract_specs should not be used for footprint but not for re-execution, because \
       it does not optimize postconditions." ;
  let sub =
    let fav =
      Paths.PathSet.fold
        (fun prop _ res -> Prop.free_vars prop |> Ident.hashqueue_of_sequence ~init:res)
        pathset (Ident.HashQueue.create ())
      |> Ident.HashQueue.keys
    in
    let sub_list = List.map ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.knormal))) fav in
    Predicates.subst_of_list sub_list
  in
  let pre_post_list =
    let f (prop, path) =
      let _remaining, prop = PropUtil.remove_locals_formals tenv pdesc prop in
      let prop = Abs.abstract analysis_data prop in
      let pre, post = Prop.extract_spec prop in
      let pre = Prop.normalize tenv (Prop.prop_sub sub pre) in
      let post = PropUtil.remove_seed_vars tenv (Prop.prop_sub sub post) in
      (pre, (post, path))
    in
    let compare (prop1, _) (prop2, _) = Prop.compare_prop prop1 prop2 in
    let break a b = not (Int.equal 0 (compare a b)) in
    let separate ps =
      let pre =
        match ps with
        | (pre, _) :: _ ->
            pre
        | [] ->
            L.die InternalError "Interproc.extract_specs: List.group outputs empty list??"
      in
      let pposts = List.map ~f:snd ps in
      (pre, pposts)
    in
    pathset |> Paths.PathSet.elements |> List.map ~f |> List.sort ~compare |> List.group ~break
    |> List.map ~f:separate
  in
  let mk_spec (pre, posts) =
    BiabductionSummary.{pre= Jprop.Prop (1, pre); posts; visited= Visitedset.empty}
  in
  List.map ~f:mk_spec pre_post_list


let collect_postconditions analysis_data wl tenv proc_cfg :
    Paths.PathSet.t * BiabductionSummary.Visitedset.t =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pathset = collect_analysis_result analysis_data wl proc_cfg in
  (* Assuming C++ developers use RAII, remove resources from the constructor posts *)
  let pathset =
    match pname with
    | Procname.ObjC_Cpp _ ->
        if Procname.is_constructor pname then
          Paths.PathSet.map
            (fun prop ->
              Attribute.remove_resource tenv Racquire (Rmemory Mobjc)
                (Attribute.remove_resource tenv Racquire (Rmemory Mmalloc)
                   (Attribute.remove_resource tenv Racquire Rfile prop) ) )
            pathset
        else pathset
    | _ ->
        pathset
  in
  L.d_printfln "#### [FUNCTION %a] Analysis result ####" Procname.pp pname ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset) ;
  L.d_ln () ;
  let res =
    try
      let pathset = collect_do_abstract_post analysis_data pathset in
      let pathset_diverging = State.get_diverging_states_proc () in
      let visited =
        let vset = vset_add_pathset Procdesc.NodeSet.empty pathset in
        (* nodes from diverging states were also visited *)
        let vset = vset_add_pathset vset pathset_diverging in
        compute_visited vset
      in
      (pathset, visited)
    with Exceptions.Leak _ ->
      L.d_strln "Leak in post collection" ;
      assert false
  in
  L.d_printfln "#### [FUNCTION %a] Postconditions after join ####" Procname.pp pname ;
  L.d_increase_indent () ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv (fst res)) ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  res


let create_seed_vars sigma =
  let hpred_add_seed sigma = function
    | Predicates.Hpointsto (Exp.Lvar pv, se, typ) when not (Pvar.is_abduced pv) ->
        Predicates.Hpointsto (Exp.Lvar (Pvar.to_seed pv), se, typ) :: sigma
    | _ ->
        sigma
  in
  List.fold ~f:hpred_add_seed ~init:[] sigma


(** Initialize proposition for execution given formal and global parameters. The footprint is
    initialized according to the execution mode. The prop is not necessarily emp, so it should be
    incorporated when the footprint is constructed. *)
let prop_init_formals_seed tenv new_formals (prop : 'a Prop.t) : Prop.exposed Prop.t =
  let sigma_new_formals =
    let do_formal (pv, typ) =
      let texp =
        match !Language.curr_language with
        | Clang ->
            Exp.Sizeof
              {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= true}
        | Java | CIL ->
            Exp.Sizeof
              {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes; nullable= true}
        | Erlang ->
            L.die InternalError "Erlang not supported"
        | Hack ->
            L.die InternalError "Hack not supported"
        | Python ->
            L.die InternalError "Python not supported"
      in
      Prop.mk_ptsto_lvar tenv Prop.Fld_init Predicates.inst_formal (pv, texp, None)
    in
    List.map ~f:do_formal new_formals
  in
  let sigma_seed =
    create_seed_vars ((* formals already there plus new ones *)
                      prop.Prop.sigma @ sigma_new_formals)
  in
  let sigma = sigma_seed @ sigma_new_formals in
  let new_pi = prop.Prop.pi in
  let prop' = Prop.set (Prop.prop_sigma_star prop sigma) ~pi:new_pi in
  Prop.set prop' ~sigma_fp:(prop'.Prop.sigma_fp @ sigma_new_formals)


(** Construct an initial prop by extending [prop] with locals, and formals if [add_formals] is true
    as well as seed variables *)
let initial_prop tenv (curr_f : Procdesc.t) (prop : 'a Prop.t) ~add_formals : Prop.normal Prop.t =
  let construct_decl (x, typ, _) = (Pvar.mk x (Procdesc.get_proc_name curr_f), typ) in
  let new_formals =
    if add_formals then List.map ~f:construct_decl (Procdesc.get_formals curr_f) else []
    (* no new formals added *)
  in
  let prop1 =
    Prop.prop_reset_inst
      (fun inst_old -> Predicates.update_inst inst_old Predicates.inst_formal)
      prop
  in
  let prop2 = prop_init_formals_seed tenv new_formals prop1 in
  Prop.prop_rename_primed_footprint_vars tenv (Prop.normalize tenv prop2)
  |> SymExec.declare_locals_and_ret tenv curr_f


(** Construct an initial prop from the empty prop *)
let initial_prop_from_emp tenv curr_f = initial_prop tenv curr_f Prop.prop_emp ~add_formals:true

(** Construct an initial prop from an existing pre with formals *)
let initial_prop_from_pre tenv curr_f pre =
  if !BiabductionConfig.footprint then
    let vars = Prop.free_vars pre |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys in
    let sub_list =
      List.map ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.kfootprint))) vars
    in
    let sub = Predicates.subst_of_list sub_list in
    let pre2 = Prop.prop_sub sub pre in
    let pre3 = Prop.set pre2 ~pi_fp:(Prop.get_pure pre2) ~sigma_fp:pre2.Prop.sigma in
    initial_prop tenv curr_f pre3 ~add_formals:false
  else initial_prop tenv curr_f pre ~add_formals:false


(** Re-execute one precondition and return some spec if there was no re-execution error. *)
let execute_filter_prop ({InterproceduralAnalysis.tenv; _} as analysis_data) proc_cfg summary
    (precondition : Prop.normal BiabductionSummary.Jprop.t) :
    Prop.normal BiabductionSummary.spec option =
  let init_node = ProcCfg.Exceptional.start_node proc_cfg in
  let wl = path_set_create_worklist proc_cfg in
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let pname = Procdesc.get_proc_name pdesc in
  let init_edgeset =
    AnalysisCallbacks.html_debug_new_node_session ~pp_name init_node ~f:(fun () ->
        L.d_printfln "#### Start: RE-execution for %a ####" Procname.pp pname ;
        L.d_indent 1 ;
        L.d_strln "Precond:" ;
        BiabductionSummary.Jprop.d_shallow precondition ;
        L.d_ln () ;
        L.d_ln () ;
        let init_prop =
          initial_prop_from_pre tenv pdesc (BiabductionSummary.Jprop.to_prop precondition)
        in
        Paths.PathSet.add_renamed_prop init_prop (Paths.Path.start init_node) Paths.PathSet.empty )
  in
  try
    Worklist.add wl init_node ;
    ignore (path_set_put_todo wl init_node init_edgeset) ;
    forward_tabulate analysis_data proc_cfg summary wl ;
    AnalysisCallbacks.html_debug_new_node_session ~pp_name init_node ~f:(fun () ->
        L.d_printfln ~color:Green "#### Finished: RE-execution for %a ####" Procname.pp pname ;
        L.d_increase_indent () ;
        L.d_strln "Precond:" ;
        Prop.d_prop (BiabductionSummary.Jprop.to_prop precondition) ;
        L.d_ln () ;
        let posts, visited =
          let pset, visited = collect_postconditions analysis_data wl tenv proc_cfg in
          let plist =
            List.map
              ~f:(fun (p, path) -> (PropUtil.remove_seed_vars tenv p, path))
              (Paths.PathSet.elements pset)
          in
          (plist, visited)
        in
        let pre =
          BiabductionSummary.Jprop.shallow_map
            ~f:(PropUtil.remove_locals_ret tenv pdesc)
            precondition
        in
        let spec = BiabductionSummary.{pre; posts; visited} in
        L.d_decrease_indent () ;
        Some spec )
  with RE_EXE_ERROR ->
    AnalysisCallbacks.html_debug_new_node_session ~pp_name init_node ~f:(fun () ->
        L.d_printfln ~color:Red "#### [FUNCTION %a] ...ERROR" Procname.pp pname ;
        L.d_increase_indent () ;
        L.d_strln "when starting from pre:" ;
        Prop.d_prop (BiabductionSummary.Jprop.to_prop precondition) ;
        L.d_strln "This precondition is filtered out." ;
        L.d_decrease_indent () ;
        None )


type exe_phase =
  (unit -> unit) * (unit -> Prop.normal BiabductionSummary.spec list * BiabductionSummary.phase)

(** Return functions to perform one phase of the analysis for a procedure. Given [proc_name], return
    [do, get_results] where [go ()] performs the analysis phase and [get_results ()] returns the
    results computed. This function is architected so that [get_results ()] can be called even after
    [go ()] was interrupted by and exception. *)
let perform_analysis_phase ({InterproceduralAnalysis.proc_desc; err_log; tenv} as analysis_data)
    (proc_cfg : ProcCfg.Exceptional.t) summary_opt : exe_phase =
  let pname = Procdesc.get_proc_name proc_desc in
  let start_node = ProcCfg.Exceptional.start_node proc_cfg in
  let compute_footprint () : exe_phase =
    let go (wl : Worklist.t) () =
      let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
      let init_prop = initial_prop_from_emp tenv pdesc in
      (* use existing pre's (in recursion some might exist) as starting points *)
      let init_props_from_pres =
        (* rename spec vars to footprint vars, and copy current to footprint *)
        let mk_init precondition =
          initial_prop_from_pre tenv pdesc (BiabductionSummary.Jprop.to_prop precondition)
        in
        List.map
          ~f:(fun spec -> mk_init spec.BiabductionSummary.pre)
          (Option.value_map summary_opt ~default:[] ~f:BiabductionSummary.get_specs)
      in
      let init_props = Propset.from_proplist tenv (init_prop :: init_props_from_pres) in
      let init_edgeset =
        let add pset prop =
          Paths.PathSet.add_renamed_prop prop (Paths.Path.start start_node) pset
        in
        Propset.fold add Paths.PathSet.empty init_props
      in
      L.d_increase_indent () ;
      L.d_strln "initial props =" ;
      Propset.d Prop.prop_emp init_props ;
      L.d_ln () ;
      L.d_ln () ;
      L.d_decrease_indent () ;
      Worklist.add wl start_node ;
      ignore (path_set_put_todo wl start_node init_edgeset) ;
      forward_tabulate analysis_data proc_cfg summary_opt wl
    in
    let get_results (wl : Worklist.t) () =
      State.process_execution_failures
        (BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log) ;
      let results = collect_analysis_result analysis_data wl proc_cfg in
      let specs =
        try extract_specs analysis_data (ProcCfg.Exceptional.proc_desc proc_cfg) results
        with Exceptions.Leak _ ->
          let exn =
            Exceptions.Internal_error
              (Localise.verbatim_desc "Leak_while_collecting_specs_after_footprint")
          in
          BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn ;
          (* returning no specs *) []
      in
      (specs, BiabductionSummary.FOOTPRINT)
    in
    let wl = path_set_create_worklist proc_cfg in
    (go wl, get_results wl)
  in
  let re_execution () : exe_phase =
    let candidate_preconditions =
      Option.value_map summary_opt ~default:[] ~f:BiabductionSummary.get_specs
      |> List.map ~f:(fun spec -> spec.BiabductionSummary.pre)
    in
    let valid_specs_rev = ref [] in
    let go () =
      let filter p =
        let speco = execute_filter_prop analysis_data proc_cfg summary_opt p in
        (match speco with None -> () | Some spec -> valid_specs_rev := spec :: !valid_specs_rev) ;
        speco
      in
      ignore (BiabductionSummary.Jprop.filter filter candidate_preconditions)
    in
    let get_results () =
      let specs = List.rev !valid_specs_rev in
      let source = (Procdesc.get_loc (ProcCfg.Exceptional.proc_desc proc_cfg)).file in
      let filename =
        DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source)
          [Procname.to_filename pname]
      in
      if Config.biabduction_write_dotty then DotBiabduction.emit_specs_to_file filename specs ;
      (specs, BiabductionSummary.RE_EXECUTION)
    in
    (go, get_results)
  in
  match BiabductionSummary.opt_get_phase summary_opt with
  | FOOTPRINT ->
      compute_footprint ()
  | RE_EXECUTION ->
      re_execution ()


let set_current_language proc_desc =
  let language = Procname.get_language (Procdesc.get_proc_name proc_desc) in
  Language.curr_language := language


(** reset global values before analysing a procedure *)
let reset_global_values proc_desc =
  Ident.NameGenerator.reset () ;
  SymOp.reset_total () ;
  reset_prop_metrics () ;
  set_current_language proc_desc


(* Collect all pairs of the kind (precondition, custom error) from a summary *)
let custom_error_preconditions summary =
  let collect_errors pre (errors, all_post_error) (prop, _) =
    match Tabulation.lookup_custom_errors prop with
    | None ->
        (errors, false)
    | Some e ->
        ((pre, e) :: errors, all_post_error)
  in
  let collect_spec errors spec =
    List.fold
      ~f:(collect_errors spec.BiabductionSummary.pre)
      ~init:errors spec.BiabductionSummary.posts
  in
  List.fold ~f:collect_spec ~init:([], true) (BiabductionSummary.get_specs summary)


(* Remove the constrain of the form this != null which is true for all Java virtual calls *)
let remove_this_not_null tenv prop =
  let collect_hpred (var_option, hpreds) = function
    | Predicates.Hpointsto (Exp.Lvar pvar, Eexp (Exp.Var var, _), _)
      when (Language.curr_language_is Java || Language.curr_language_is CIL) && Pvar.is_this pvar ->
        (Some var, hpreds)
    | hpred ->
        (var_option, hpred :: hpreds)
  in
  let collect_atom var atoms = function
    | Predicates.Aneq (Exp.Var v, e) when Ident.equal v var && Exp.equal e Exp.null ->
        atoms
    | a ->
        a :: atoms
  in
  match List.fold ~f:collect_hpred ~init:(None, []) prop.Prop.sigma with
  | None, _ ->
      prop
  | Some var, filtered_hpreds ->
      let filtered_atoms = List.fold ~f:(collect_atom var) ~init:[] prop.Prop.pi in
      let prop' = Prop.set Prop.prop_emp ~pi:filtered_atoms ~sigma:filtered_hpreds in
      Prop.normalize tenv prop'


(** Is true when the precondition does not contain constrains that can be false at call site. This
    means that the post-conditions associated with this precondition cannot be prevented by the
    calling context. *)
let is_unavoidable tenv pre =
  let prop = remove_this_not_null tenv (BiabductionSummary.Jprop.to_prop pre) in
  match Prop.CategorizePreconditions.categorize [prop] with
  | Prop.CategorizePreconditions.NoPres | Prop.CategorizePreconditions.Empty ->
      true
  | _ ->
      false


let report_custom_errors {InterproceduralAnalysis.proc_desc; err_log; tenv} summary =
  let error_preconditions, all_post_error = custom_error_preconditions summary in
  let report (pre, custom_error) =
    if all_post_error || is_unavoidable tenv pre then
      let loc = Procdesc.get_loc proc_desc in
      let err_desc = Localise.desc_custom_error loc in
      let exn = Exceptions.Custom_error (custom_error, Error, err_desc) in
      BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn
  in
  List.iter ~f:report error_preconditions


module SpecMap = Caml.Map.Make (struct
  type t = Prop.normal BiabductionSummary.Jprop.t

  let compare = BiabductionSummary.Jprop.compare
end)

(** Update the specs of the current proc after the execution of one phase *)
let update_specs analysis_data prev_summary_opt phase
    (new_specs : BiabductionSummary.NormSpec.t list) : BiabductionSummary.NormSpec.t list * bool =
  let new_specs = BiabductionSummary.normalized_specs_to_specs new_specs in
  let old_specs = Option.value_map ~default:[] ~f:BiabductionSummary.get_specs prev_summary_opt in
  let changed = ref false in
  let current_specs =
    ref
      (List.fold
         ~f:(fun map spec ->
           SpecMap.add spec.BiabductionSummary.pre
             ( Paths.PathSet.from_renamed_list spec.BiabductionSummary.posts
             , spec.BiabductionSummary.visited )
             map )
         ~init:SpecMap.empty old_specs )
  in
  let re_exe_filter old_spec =
    (* filter out pres which failed re-exe *)
    if
      BiabductionSummary.equal_phase phase RE_EXECUTION
      && not
           (List.exists
              ~f:(fun new_spec ->
                BiabductionSummary.Jprop.equal new_spec.BiabductionSummary.pre
                  old_spec.BiabductionSummary.pre )
              new_specs )
    then (
      changed := true ;
      current_specs := SpecMap.remove old_spec.BiabductionSummary.pre !current_specs )
  in
  let add_spec spec =
    (* add a new spec by doing union of the posts *)
    try
      let old_post, old_visited = SpecMap.find spec.BiabductionSummary.pre !current_specs in
      let new_post, new_visited =
        ( Paths.PathSet.union old_post
            (Paths.PathSet.from_renamed_list spec.BiabductionSummary.posts)
        , BiabductionSummary.Visitedset.union old_visited spec.BiabductionSummary.visited )
      in
      if not (Paths.PathSet.equal old_post new_post) then (
        changed := true ;
        current_specs :=
          SpecMap.add spec.BiabductionSummary.pre (new_post, new_visited)
            (SpecMap.remove spec.BiabductionSummary.pre !current_specs) )
    with Caml.Not_found ->
      changed := true ;
      current_specs :=
        SpecMap.add spec.BiabductionSummary.pre
          ( Paths.PathSet.from_renamed_list spec.BiabductionSummary.posts
          , spec.BiabductionSummary.visited )
          !current_specs
  in
  let res = ref [] in
  let convert pre (post_set, visited) =
    res :=
      Abs.abstract_spec analysis_data
        {BiabductionSummary.pre; posts= Paths.PathSet.elements post_set; visited}
      :: !res
  in
  List.iter ~f:re_exe_filter old_specs ;
  (* filter out pre's which failed re-exe *)
  List.iter ~f:add_spec new_specs ;
  (* add new specs *)
  SpecMap.iter convert !current_specs ;
  (!res, !changed)


(** update a summary after analysing a procedure *)
let update_summary ({InterproceduralAnalysis.tenv; update_stats} as analysis_data) prev_summary
    specs phase res =
  let normal_specs = List.map ~f:(BiabductionSummary.spec_normalize tenv) specs in
  let new_specs, _ = update_specs analysis_data prev_summary phase normal_specs in
  update_stats ~add_symops:(SymOp.get_total ()) ?failure_kind:res () ;
  let preposts =
    match phase with
    | BiabductionSummary.FOOTPRINT ->
        new_specs
    | BiabductionSummary.RE_EXECUTION ->
        List.map ~f:(BiabductionSummary.NormSpec.erase_join_info_pre tenv) new_specs
  in
  {BiabductionSummary.preposts; phase}


(** Analyze the procedure and return the resulting summary. *)
let analyze_proc analysis_data summary_opt proc_cfg : BiabductionSummary.t =
  let proc_desc = ProcCfg.Exceptional.proc_desc proc_cfg in
  reset_global_values proc_desc ;
  let go, get_results = perform_analysis_phase analysis_data proc_cfg summary_opt in
  let res = Timeout.exe_timeout go () in
  let specs, phase = get_results () in
  let updated_summary = update_summary analysis_data summary_opt specs phase res in
  if Language.curr_language_is Clang && Config.report_custom_error then
    report_custom_errors analysis_data updated_summary ;
  updated_summary


(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe summary tenv joined_pres : BiabductionSummary.t =
  if Config.biabduction_only_footprint then
    if BiabductionSummary.equal_phase summary.BiabductionSummary.phase FOOTPRINT then
      {summary with BiabductionSummary.phase= RE_EXECUTION}
    else summary
  else
    let preposts =
      List.map
        ~f:(fun jp ->
          BiabductionSummary.spec_normalize tenv
            {BiabductionSummary.pre= jp; posts= []; visited= BiabductionSummary.Visitedset.empty} )
        joined_pres
    in
    {BiabductionSummary.preposts; phase= RE_EXECUTION}


(** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for the procedures enabled after the
    analysis of [proc_name] *)
let perform_transition ({InterproceduralAnalysis.tenv; _} as analysis_data) proc_cfg proc_name
    summary =
  let transition summary =
    (* disable exceptions for leaks and protect against any other errors *)
    let joined_pres =
      let allow_leak = !BiabductionConfig.allow_leak in
      (* apply the start node to f, and do nothing in case of exception *)
      let with_start_node_session ~f =
        match ProcCfg.Exceptional.start_node proc_cfg with
        | start_node ->
            AnalysisCallbacks.html_debug_new_node_session ~pp_name start_node ~f
        | exception exn when Exception.exn_not_failure exn ->
            f ()
      in
      with_start_node_session ~f:(fun () ->
          try
            BiabductionConfig.allow_leak := true ;
            let res = collect_preconditions analysis_data summary in
            BiabductionConfig.allow_leak := allow_leak ;
            res
          with exn when Exception.exn_not_failure exn ->
            BiabductionConfig.allow_leak := allow_leak ;
            L.debug Analysis Medium "Error in collect_preconditions for %a@." Procname.pp proc_name ;
            let error = Exceptions.recognize_exception exn in
            let err_str = "exception raised " ^ error.issue_type.unique_id in
            L.(debug Analysis Medium) "Error: %s %a@." err_str L.pp_ocaml_pos_opt error.ocaml_pos ;
            [] )
    in
    transition_footprint_re_exe summary tenv joined_pres
  in
  if BiabductionSummary.equal_phase summary.BiabductionSummary.phase FOOTPRINT then
    transition summary
  else summary


let analyze_procedure_aux ({InterproceduralAnalysis.proc_desc; _} as analysis_data) :
    BiabductionSummary.t =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
  let summaryfp =
    BiabductionConfig.run_in_footprint_mode (analyze_proc analysis_data None) proc_cfg
    |> perform_transition analysis_data proc_cfg proc_name
  in
  let summaryre =
    BiabductionConfig.run_in_re_execution_mode
      (analyze_proc analysis_data (Some summaryfp))
      proc_cfg
  in
  let summary_compact =
    if Config.save_compact_summaries then
      let sharing_env = Predicates.create_sharing_env () in
      let compact_preposts =
        List.map
          ~f:(BiabductionSummary.NormSpec.compact sharing_env)
          summaryre.BiabductionSummary.preposts
      in
      {summaryre with BiabductionSummary.preposts= compact_preposts}
    else summaryre
  in
  summary_compact


let analyze_procedure ({InterproceduralAnalysis.proc_desc; err_log} as analysis_data) :
    BiabductionSummary.t option =
  (* make sure models have been registered *)
  BuiltinDefn.init () ;
  try Some (analyze_procedure_aux analysis_data)
  with exn ->
    IExn.reraise_if exn ~f:(fun () -> not (Exceptions.handle_exception exn)) ;
    BiabductionReporting.log_issue_using_state proc_desc err_log exn ;
    None
