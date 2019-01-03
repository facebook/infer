(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
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
      match Config.worklist_mode with
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


let htable_retrieve (htable : (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t)
    (key : Procdesc.Node.id) : Paths.PathSet.t =
  try Hashtbl.find htable key with Caml.Not_found ->
    Hashtbl.replace htable key Paths.PathSet.empty ;
    Paths.PathSet.empty


(** Add [d] to the pathset todo at [node] returning true if changed *)
let path_set_put_todo (wl : Worklist.t) (node : Procdesc.Node.t) (d : Paths.PathSet.t) : bool =
  let changed =
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
  in
  changed


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

let collect_do_abstract_pre pname tenv (pset : Propset.t) : Propset.t =
  if !BiabductionConfig.footprint then
    BiabductionConfig.run_in_re_execution_mode (Abs.lifted_abstract pname tenv) pset
  else Abs.lifted_abstract pname tenv pset


let collect_do_abstract_post pname tenv (pathset : Paths.PathSet.t) : Paths.PathSet.t =
  let abs_option p =
    if Prover.check_inconsistency tenv p then None else Some (Abs.abstract pname tenv p)
  in
  if !BiabductionConfig.footprint then
    BiabductionConfig.run_in_re_execution_mode (Paths.PathSet.map_option abs_option) pathset
  else Paths.PathSet.map_option abs_option pathset


let do_join_pre plist = Dom.proplist_collapse_pre plist

let do_join_post pname tenv (pset : Paths.PathSet.t) =
  if Config.spec_abs_level <= 0 then Dom.pathset_collapse tenv pset
  else Dom.pathset_collapse tenv (Dom.pathset_collapse_impl pname tenv pset)


let do_meet_pre tenv pset =
  if Config.meet_level > 0 then Dom.propset_meet_generate_pre tenv pset
  else Propset.to_proplist pset


(** Find the preconditions in the current spec table,
    apply meet then join, and return the joined preconditions *)
let collect_preconditions tenv summary : Prop.normal BiabductionSummary.Jprop.t list =
  let proc_name = Summary.get_proc_name summary in
  let collect_do_abstract_one tenv prop =
    if !BiabductionConfig.footprint then
      BiabductionConfig.run_in_re_execution_mode (Abs.abstract_no_symop tenv) prop
    else Abs.abstract_no_symop tenv prop
  in
  let pres =
    List.map
      ~f:(fun spec -> BiabductionSummary.Jprop.to_prop spec.BiabductionSummary.pre)
      (Tabulation.get_specs_from_payload summary)
  in
  let pset = Propset.from_proplist tenv pres in
  let pset' =
    let f p = Prop.prop_normal_vars_to_primed_vars tenv p in
    Propset.map tenv f pset
  in
  L.d_printfln "#### Extracted footprint of %a:  ####" Typ.Procname.pp proc_name ;
  L.d_increase_indent () ;
  Propset.d Prop.prop_emp pset' ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  L.d_ln () ;
  let pset'' = collect_do_abstract_pre proc_name tenv pset' in
  let plist_meet = do_meet_pre tenv pset'' in
  L.d_printfln "#### Footprint of %a after Meet  ####" Typ.Procname.pp proc_name ;
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
  L.d_printfln "#### Footprint of %a after Join  ####" Typ.Procname.pp proc_name ;
  L.d_increase_indent () ;
  BiabductionSummary.Jprop.d_list ~shallow:false jplist ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  let jplist' =
    List.map ~f:(BiabductionSummary.Jprop.map (Prop.prop_rename_primed_footprint_vars tenv)) jplist
  in
  L.d_printfln "#### Renamed footprint of %a:  ####" Typ.Procname.pp proc_name ;
  L.d_increase_indent () ;
  BiabductionSummary.Jprop.d_list ~shallow:false jplist' ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  let jplist'' =
    let f p =
      Prop.prop_primed_vars_to_normal_vars tenv (collect_do_abstract_one proc_name tenv p)
    in
    List.map ~f:(BiabductionSummary.Jprop.map f) jplist'
  in
  L.d_printfln "#### Abstracted footprint of %a:  ####" Typ.Procname.pp proc_name ;
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
        (Paths.Path.extend curr_node exn_opt (State.get_session ()) path)
        edgeset_curr
    in
    Paths.PathSet.fold f pset Paths.PathSet.empty
  in
  let changed = path_set_put_todo wl curr_node edgeset_todo in
  if changed then Worklist.add wl curr_node


(** propagate a set of results, including exceptions and divergence *)
let propagate_nodes_divergence tenv (proc_cfg : ProcCfg.Exceptional.t) (pset : Paths.PathSet.t)
    curr_node (wl : Worklist.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pset_exn, pset_ok = Paths.PathSet.partition (Tabulation.prop_is_exn pname) pset in
  if
    !BiabductionConfig.footprint
    && not (Paths.PathSet.is_empty (State.get_diverging_states_node ()))
  then (
    Errdesc.warning_err (State.get_loc_exn ()) "Propagating Divergence@." ;
    let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
    let diverging_states = State.get_diverging_states_node () in
    let prop_incons =
      let mk_incons prop =
        let p_abs = Abs.abstract pname tenv prop in
        let p_zero = Prop.set p_abs ~sub:Sil.sub_empty ~sigma:[] in
        Prop.normalize tenv (Prop.set p_zero ~pi:[Sil.Aneq (Exp.zero, Exp.zero)])
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
let do_symexec_join proc_cfg tenv wl curr_node (edgeset_todo : Paths.PathSet.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let curr_node_id = ProcCfg.Exceptional.Node.id curr_node in
  let new_dset = edgeset_todo in
  let old_dset = Join_table.find wl.Worklist.join_table curr_node_id in
  let old_dset', new_dset' = Dom.pathset_join pname tenv old_dset new_dset in
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
  if Config.monitor_prop_size then Paths.PathSet.iter check_prop_size_ edgeset_todo


let reset_prop_metrics () =
  prop_max_size := (0, Prop.prop_emp) ;
  prop_max_chain_size := (0, Prop.prop_emp)


exception RE_EXE_ERROR

let pp_name fmt = F.pp_print_string fmt "interproc"

let do_before_node session node =
  State.set_node node ;
  State.set_session session ;
  L.reset_delayed_prints () ;
  Printer.node_start_session ~pp_name node (session :> int)


let do_after_node node = Printer.node_finish_session node

(** Return the list of normal ids occurring in the instructions *)
let instrs_get_normal_vars instrs =
  let do_instr res instr =
    Sil.instr_get_exps instr
    |> List.fold_left ~init:res ~f:(fun res e ->
           Exp.free_vars e
           |> Sequence.filter ~f:Ident.is_normal
           |> Ident.hashqueue_of_sequence ~init:res )
  in
  Instrs.fold ~init:(Ident.HashQueue.create ()) ~f:do_instr instrs |> Ident.HashQueue.keys


(** Perform symbolic execution for a node starting from an initial prop *)
let do_symbolic_execution exe_env summary proc_cfg handle_exn tenv
    (node : ProcCfg.Exceptional.Node.t) (prop : Prop.normal Prop.t) (path : Paths.Path.t) =
  State.mark_execution_start node ;
  let instrs = ProcCfg.Exceptional.instrs node in
  (* fresh normal vars must be fresh w.r.t. instructions *)
  Ident.update_name_generator (instrs_get_normal_vars instrs) ;
  let pset =
    SymExec.node handle_exn exe_env tenv summary proc_cfg node
      (Paths.PathSet.from_renamed_list [(prop, path)])
  in
  L.d_strln ".... After Symbolic Execution ...." ;
  Propset.d prop (Paths.PathSet.to_propset tenv pset) ;
  L.d_ln () ;
  L.d_ln () ;
  State.mark_execution_end node ;
  pset


let mark_visited summary node =
  if not !BiabductionConfig.footprint then
    let node_id = (Procdesc.Node.get_id node :> int) in
    let stats = summary.Summary.stats in
    Summary.Stats.add_visited stats node_id


let forward_tabulate summary exe_env tenv proc_cfg wl =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let handle_exn_node curr_node exn =
    Exceptions.print_exception_html "Failure of symbolic execution: " exn ;
    let pre_opt =
      (* precondition leading to error, if any *)
      State.get_normalized_pre (Abs.abstract_no_symop pname)
    in
    ( match pre_opt with
    | Some pre ->
        L.d_strln "Precondition:" ; Prop.d_prop pre ; L.d_ln ()
    | None ->
        () ) ;
    L.d_strln "SIL INSTR:" ;
    Procdesc.Node.d_instrs ~highlight:(State.get_instr ()) curr_node ;
    L.d_ln () ;
    Reporting.log_issue_deprecated_using_state Exceptions.Error pname exn ;
    State.mark_instr_fail exn
  in
  let exe_iter f pathset =
    let ps_size = Paths.PathSet.size pathset in
    let cnt = ref 0 in
    let exe prop path = State.set_path path None ; incr cnt ; f prop path !cnt ps_size in
    Paths.PathSet.iter exe pathset
  in
  let print_node_preamble curr_node session pathset_todo =
    let log_string proc_name =
      let phase_string =
        let open BiabductionSummary in
        summary.Summary.payloads.biabduction |> opt_get_phase |> string_of_phase_short
      in
      let status = Summary.get_status summary in
      F.sprintf "[%s:%s] %s" phase_string (Summary.Status.to_string status)
        (Typ.Procname.to_string proc_name)
    in
    L.d_printfln "**** %s Node: %a, Procedure: %a, Session: %d, Todo: %d ****" (log_string pname)
      Procdesc.Node.pp curr_node Typ.Procname.pp pname session (Paths.PathSet.size pathset_todo) ;
    L.d_increase_indent () ;
    Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset_todo) ;
    L.d_strln ".... Instructions: ...." ;
    Procdesc.Node.d_instrs ~highlight:(State.get_instr ()) curr_node ;
    L.d_ln () ;
    L.d_ln ()
  in
  let do_prop (curr_node : ProcCfg.Exceptional.Node.t) handle_exn prop path cnt num_paths =
    L.d_printfln "Processing prop %d/%d" cnt num_paths ;
    L.d_increase_indent () ;
    try
      State.reset_diverging_states_node () ;
      let pset =
        do_symbolic_execution exe_env summary proc_cfg handle_exn tenv curr_node prop path
      in
      propagate_nodes_divergence tenv proc_cfg pset curr_node wl ;
      L.d_decrease_indent () ;
      L.d_ln ()
    with exn ->
      IExn.reraise_if exn ~f:(fun () ->
          (not !BiabductionConfig.footprint) || not (Exceptions.handle_exception exn) ) ;
      handle_exn exn ;
      L.d_decrease_indent () ;
      L.d_ln ()
  in
  let do_node curr_node pathset_todo session handle_exn =
    check_prop_size pathset_todo ;
    print_node_preamble curr_node session pathset_todo ;
    match Procdesc.Node.get_kind curr_node with
    | Procdesc.Node.Join_node ->
        do_symexec_join proc_cfg tenv wl curr_node pathset_todo
    | Procdesc.Node.Stmt_node _
    | Procdesc.Node.Prune_node _
    | Procdesc.Node.Exit_node
    | Procdesc.Node.Skip_node _
    | Procdesc.Node.Start_node ->
        exe_iter (do_prop curr_node handle_exn) pathset_todo
  in
  let do_node_and_handle curr_node session =
    let pathset_todo = path_set_checkout_todo wl curr_node in
    try
      let handle_exn_called = ref false in
      let handle_exn exn =
        handle_exn_called := true ;
        handle_exn_node curr_node exn
      in
      do_node curr_node pathset_todo session handle_exn ;
      if !handle_exn_called then Printer.force_delayed_prints () ;
      do_after_node curr_node
    with exn ->
      IExn.reraise_if exn ~f:(fun () -> not (Exceptions.handle_exception exn)) ;
      handle_exn_node curr_node exn ;
      Printer.force_delayed_prints () ;
      do_after_node curr_node ;
      if not !BiabductionConfig.footprint then raise RE_EXE_ERROR
  in
  while not (Worklist.is_empty wl) do
    let curr_node = Worklist.remove wl in
    mark_visited summary curr_node ;
    (* mark nodes visited in fp and re phases *)
    let session = incr summary.Summary.sessions ; !(summary.Summary.sessions) in
    do_before_node session curr_node ;
    do_node_and_handle curr_node session
  done ;
  L.d_strln ".... Work list empty. Stop ...." ;
  L.d_ln ()


(** Remove locals and formals,
    and check if the address of a stack variable is left in the result *)
let remove_locals_formals_and_check tenv proc_cfg p =
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let pname = Procdesc.get_proc_name pdesc in
  let pvars, p' = PropUtil.remove_locals_formals tenv pdesc p in
  let check_pvar pvar =
    if not (Pvar.is_frontend_tmp pvar) then
      let loc = ProcCfg.Exceptional.Node.loc (ProcCfg.Exceptional.exit_node proc_cfg) in
      let dexp_opt, _ = Errdesc.vpath_find tenv p (Exp.Lvar pvar) in
      let desc = Errdesc.explain_stack_variable_address_escape loc pvar dexp_opt in
      let exn = Exceptions.Stack_variable_address_escape (desc, __POS__) in
      Reporting.log_issue_deprecated_using_state Exceptions.Warning pname exn
  in
  List.iter ~f:check_pvar pvars ; p'


(** Collect the analysis results for the exit node. *)
let collect_analysis_result tenv wl proc_cfg : Paths.PathSet.t =
  let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
  let exit_node_id = ProcCfg.Exceptional.Node.id exit_node in
  let pathset = htable_retrieve wl.Worklist.path_set_visited exit_node_id in
  Paths.PathSet.map (remove_locals_formals_and_check tenv proc_cfg) pathset


module Pmap = Caml.Map.Make (struct
  type t = Prop.normal Prop.t

  let compare = Prop.compare_prop
end)

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
           ~f:(fun instr -> (Sil.instr_get_loc instr).Location.line)
           (ProcCfg.Exceptional.instrs n)
    in
    List.remove_consecutive_duplicates ~equal:Int.equal (List.sort ~compare:Int.compare lines)
  in
  let do_node n =
    res := BiabductionSummary.Visitedset.add (Procdesc.Node.get_id n, node_get_all_lines n) !res
  in
  Procdesc.NodeSet.iter do_node vset ;
  !res


(** Extract specs from a pathset *)
let extract_specs tenv pdesc pathset : Prop.normal BiabductionSummary.spec list =
  let pname = Procdesc.get_proc_name pdesc in
  let sub =
    let fav =
      Paths.PathSet.fold
        (fun prop _ res -> Prop.free_vars prop |> Ident.hashqueue_of_sequence ~init:res)
        pathset (Ident.HashQueue.create ())
      |> Ident.HashQueue.keys
    in
    let sub_list = List.map ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.knormal))) fav in
    Sil.subst_of_list sub_list
  in
  let pre_post_visited_list =
    let pplist = Paths.PathSet.elements pathset in
    let f (prop, path) =
      let _, prop' = PropUtil.remove_locals_formals tenv pdesc prop in
      let prop'' = Abs.abstract pname tenv prop' in
      let pre, post = Prop.extract_spec prop'' in
      let pre' = Prop.normalize tenv (Prop.prop_sub sub pre) in
      let post' =
        if Prover.check_inconsistency_base tenv prop then None
        else Some (Prop.normalize tenv (Prop.prop_sub sub post), path)
      in
      let visited =
        let vset = vset_add_path Procdesc.NodeSet.empty path in
        compute_visited vset
      in
      (pre', post', visited)
    in
    List.map ~f pplist
  in
  let pre_post_map =
    let add map (pre, post, visited) =
      let current_posts, current_visited =
        try Pmap.find pre map with Caml.Not_found ->
          (Paths.PathSet.empty, BiabductionSummary.Visitedset.empty)
      in
      let new_posts =
        match post with
        | None ->
            current_posts
        | Some (post, path) ->
            Paths.PathSet.add_renamed_prop post path current_posts
      in
      let new_visited = BiabductionSummary.Visitedset.union visited current_visited in
      Pmap.add pre (new_posts, new_visited) map
    in
    List.fold ~f:add ~init:Pmap.empty pre_post_visited_list
  in
  let specs = ref [] in
  let add_spec pre ((posts : Paths.PathSet.t), visited) =
    let posts' =
      List.map
        ~f:(fun (p, path) -> (PropUtil.remove_seed_vars tenv p, path))
        (Paths.PathSet.elements (do_join_post pname tenv posts))
    in
    let spec = BiabductionSummary.{pre= Jprop.Prop (1, pre); posts= posts'; visited} in
    specs := spec :: !specs
  in
  Pmap.iter add_spec pre_post_map ; !specs


let collect_postconditions wl tenv proc_cfg : Paths.PathSet.t * BiabductionSummary.Visitedset.t =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pathset = collect_analysis_result tenv wl proc_cfg in
  (* Assuming C++ developers use RAII, remove resources from the constructor posts *)
  let pathset =
    match pname with
    | Typ.Procname.ObjC_Cpp _ ->
        if Typ.Procname.is_constructor pname then
          Paths.PathSet.map
            (fun prop ->
              Attribute.remove_resource tenv Racquire (Rmemory Mobjc)
                (Attribute.remove_resource tenv Racquire (Rmemory Mmalloc)
                   (Attribute.remove_resource tenv Racquire Rfile prop)) )
            pathset
        else pathset
    | _ ->
        pathset
  in
  L.d_printfln "#### [FUNCTION %a] Analysis result ####" Typ.Procname.pp pname ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset) ;
  L.d_ln () ;
  let res =
    try
      let pathset = collect_do_abstract_post pname tenv pathset in
      let pathset_diverging = State.get_diverging_states_proc () in
      let visited =
        let vset = vset_add_pathset Procdesc.NodeSet.empty pathset in
        (* nodes from diverging states were also visited *)
        let vset = vset_add_pathset vset pathset_diverging in
        compute_visited vset
      in
      (do_join_post pname tenv pathset, visited)
    with Exceptions.Leak _ ->
      L.d_strln "Leak in post collection" ;
      assert false
  in
  L.d_printfln "#### [FUNCTION %a] Postconditions after join ####" Typ.Procname.pp pname ;
  L.d_increase_indent () ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv (fst res)) ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  res


let create_seed_vars sigma =
  let hpred_add_seed sigma = function
    | Sil.Hpointsto (Exp.Lvar pv, se, typ) when not (Pvar.is_abduced pv) ->
        Sil.Hpointsto (Exp.Lvar (Pvar.to_seed pv), se, typ) :: sigma
    | _ ->
        sigma
  in
  List.fold ~f:hpred_add_seed ~init:[] sigma


(** Initialize proposition for execution given formal and global
    parameters. The footprint is initialized according to the
    execution mode. The prop is not necessarily emp, so it
    should be incorporated when the footprint is constructed. *)
let prop_init_formals_seed tenv new_formals (prop : 'a Prop.t) : Prop.exposed Prop.t =
  let sigma_new_formals =
    let do_formal (pv, typ) =
      let texp =
        match !Language.curr_language with
        | Clang ->
            Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        | Java ->
            Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes}
      in
      Prop.mk_ptsto_lvar tenv Prop.Fld_init Sil.inst_formal (pv, texp, None)
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
let initial_prop tenv (curr_f : Procdesc.t) (prop : 'a Prop.t) add_formals : Prop.normal Prop.t =
  let construct_decl (x, typ) = (Pvar.mk x (Procdesc.get_proc_name curr_f), typ) in
  let new_formals =
    if add_formals then List.map ~f:construct_decl (Procdesc.get_formals curr_f) else []
    (* no new formals added *)
  in
  let prop1 =
    Prop.prop_reset_inst (fun inst_old -> Sil.update_inst inst_old Sil.inst_formal) prop
  in
  let prop2 = prop_init_formals_seed tenv new_formals prop1 in
  Prop.prop_rename_primed_footprint_vars tenv (Prop.normalize tenv prop2)
  |> SymExec.declare_locals_and_ret tenv curr_f


(** Construct an initial prop from the empty prop *)
let initial_prop_from_emp tenv curr_f = initial_prop tenv curr_f Prop.prop_emp true

(** Construct an initial prop from an existing pre with formals *)
let initial_prop_from_pre tenv curr_f pre =
  if !BiabductionConfig.footprint then
    let vars = Prop.free_vars pre |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys in
    let sub_list =
      List.map ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.kfootprint))) vars
    in
    let sub = Sil.subst_of_list sub_list in
    let pre2 = Prop.prop_sub sub pre in
    let pre3 = Prop.set pre2 ~pi_fp:(Prop.get_pure pre2) ~sigma_fp:pre2.Prop.sigma in
    initial_prop tenv curr_f pre3 false
  else initial_prop tenv curr_f pre false


(** Re-execute one precondition and return some spec if there was no re-execution error. *)
let execute_filter_prop summary exe_env tenv proc_cfg
    (precondition : Prop.normal BiabductionSummary.Jprop.t) :
    Prop.normal BiabductionSummary.spec option =
  let init_node = ProcCfg.Exceptional.start_node proc_cfg in
  let wl = path_set_create_worklist proc_cfg in
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let pname = Procdesc.get_proc_name pdesc in
  do_before_node 0 init_node ;
  L.d_printfln "#### Start: RE-execution for %a ####" Typ.Procname.pp pname ;
  L.d_indent 1 ;
  L.d_strln "Precond:" ;
  BiabductionSummary.Jprop.d_shallow precondition ;
  L.d_ln () ;
  L.d_ln () ;
  let init_prop =
    initial_prop_from_pre tenv pdesc (BiabductionSummary.Jprop.to_prop precondition)
  in
  let init_edgeset =
    Paths.PathSet.add_renamed_prop init_prop (Paths.Path.start init_node) Paths.PathSet.empty
  in
  do_after_node init_node ;
  try
    Worklist.add wl init_node ;
    ignore (path_set_put_todo wl init_node init_edgeset) ;
    forward_tabulate summary exe_env tenv proc_cfg wl ;
    do_before_node 0 init_node ;
    L.d_printfln ~color:Green "#### Finished: RE-execution for %a ####" Typ.Procname.pp pname ;
    L.d_increase_indent () ;
    L.d_strln "Precond:" ;
    Prop.d_prop (BiabductionSummary.Jprop.to_prop precondition) ;
    L.d_ln () ;
    let posts, visited =
      let pset, visited = collect_postconditions wl tenv proc_cfg in
      let plist =
        List.map
          ~f:(fun (p, path) -> (PropUtil.remove_seed_vars tenv p, path))
          (Paths.PathSet.elements pset)
      in
      (plist, visited)
    in
    let pre =
      let p =
        PropUtil.remove_locals_ret tenv pdesc (BiabductionSummary.Jprop.to_prop precondition)
      in
      match precondition with
      | BiabductionSummary.Jprop.Prop (n, _) ->
          BiabductionSummary.Jprop.Prop (n, p)
      | BiabductionSummary.Jprop.Joined (n, _, jp1, jp2) ->
          BiabductionSummary.Jprop.Joined (n, p, jp1, jp2)
    in
    let spec = BiabductionSummary.{pre; posts; visited} in
    L.d_decrease_indent () ; do_after_node init_node ; Some spec
  with RE_EXE_ERROR ->
    do_before_node 0 init_node ;
    Printer.force_delayed_prints () ;
    L.d_printfln ~color:Red "#### [FUNCTION %a] ...ERROR" Typ.Procname.pp pname ;
    L.d_increase_indent () ;
    L.d_strln "when starting from pre:" ;
    Prop.d_prop (BiabductionSummary.Jprop.to_prop precondition) ;
    L.d_strln "This precondition is filtered out." ;
    L.d_decrease_indent () ;
    do_after_node init_node ;
    None


type exe_phase =
  (unit -> unit) * (unit -> Prop.normal BiabductionSummary.spec list * BiabductionSummary.phase)

(** Return functions to perform one phase of the analysis for a procedure.
    Given [proc_name], return [do, get_results] where [go ()] performs the analysis phase
    and [get_results ()] returns the results computed.
    This function is architected so that [get_results ()] can be called even after
    [go ()] was interrupted by and exception. *)
let perform_analysis_phase exe_env tenv (summary : Summary.t) (proc_cfg : ProcCfg.Exceptional.t) :
    exe_phase =
  let pname = Summary.get_proc_name summary in
  let start_node = ProcCfg.Exceptional.start_node proc_cfg in
  let compute_footprint () : exe_phase =
    let go (wl : Worklist.t) () =
      let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
      let init_prop = initial_prop_from_emp tenv pdesc in
      (* use existing pre's (in recursion some might exist) as starting points *)
      let init_props_from_pres =
        let specs = Tabulation.get_specs_from_payload summary in
        (* rename spec vars to footprint vars, and copy current to footprint *)
        let mk_init precondition =
          initial_prop_from_pre tenv pdesc (BiabductionSummary.Jprop.to_prop precondition)
        in
        List.map ~f:(fun spec -> mk_init spec.BiabductionSummary.pre) specs
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
      forward_tabulate summary exe_env tenv proc_cfg wl
    in
    let get_results (wl : Worklist.t) () =
      State.process_execution_failures
        (Reporting.log_issue_deprecated_using_state Exceptions.Warning)
        pname ;
      let results = collect_analysis_result tenv wl proc_cfg in
      let specs =
        try extract_specs tenv (ProcCfg.Exceptional.proc_desc proc_cfg) results
        with Exceptions.Leak _ ->
          let exn =
            Exceptions.Internal_error
              (Localise.verbatim_desc "Leak_while_collecting_specs_after_footprint")
          in
          Reporting.log_issue_deprecated_using_state Exceptions.Error pname exn ;
          (* retuning no specs *) []
      in
      (specs, BiabductionSummary.FOOTPRINT)
    in
    let wl = path_set_create_worklist proc_cfg in
    (go wl, get_results wl)
  in
  let re_execution () : exe_phase =
    let candidate_preconditions =
      List.map
        ~f:(fun spec -> spec.BiabductionSummary.pre)
        (Tabulation.get_specs_from_payload summary)
    in
    let valid_specs = ref [] in
    let go () =
      let filter p =
        let speco = execute_filter_prop summary exe_env tenv proc_cfg p in
        (match speco with None -> () | Some spec -> valid_specs := !valid_specs @ [spec]) ;
        speco
      in
      if Config.undo_join then
        ignore (BiabductionSummary.Jprop.filter filter candidate_preconditions)
      else ignore (List.map ~f:filter candidate_preconditions)
    in
    let get_results () =
      let specs = !valid_specs in
      let source = (Procdesc.get_loc (ProcCfg.Exceptional.proc_desc proc_cfg)).file in
      let filename =
        DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source)
          [Typ.Procname.to_filename pname]
      in
      if Config.write_dotty then Dotty.pp_speclist_dotty_file filename specs ;
      (specs, BiabductionSummary.RE_EXECUTION)
    in
    (go, get_results)
  in
  match BiabductionSummary.opt_get_phase summary.payloads.biabduction with
  | FOOTPRINT ->
      compute_footprint ()
  | RE_EXECUTION ->
      re_execution ()


let set_current_language proc_desc =
  let language = Typ.Procname.get_language (Procdesc.get_proc_name proc_desc) in
  Language.curr_language := language


(** reset global values before analysing a procedure *)
let reset_global_values proc_desc =
  BiabductionConfig.reset_abs_val () ;
  Ident.NameGenerator.reset () ;
  SymOp.reset_total () ;
  reset_prop_metrics () ;
  Abs.reset_current_rules () ;
  set_current_language proc_desc


(* Collect all pairs of the kind (precondition, runtime exception) from a summary *)
let exception_preconditions tenv pname summary =
  let collect_exceptions pre (exns, all_post_exn) (prop, _) =
    match Tabulation.prop_get_exn_name pname prop with
    | Some exn_name when PatternMatch.is_runtime_exception tenv exn_name ->
        ((pre, exn_name) :: exns, all_post_exn)
    | _ ->
        (exns, false)
  in
  let collect_spec errors spec =
    List.fold
      ~f:(collect_exceptions spec.BiabductionSummary.pre)
      ~init:errors spec.BiabductionSummary.posts
  in
  List.fold ~f:collect_spec ~init:([], true) (Tabulation.get_specs_from_payload summary)


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
  List.fold ~f:collect_spec ~init:([], true) (Tabulation.get_specs_from_payload summary)


(* Remove the constrain of the form this != null which is true for all Java virtual calls *)
let remove_this_not_null tenv prop =
  let collect_hpred (var_option, hpreds) = function
    | Sil.Hpointsto (Exp.Lvar pvar, Sil.Eexp (Exp.Var var, _), _)
      when Language.curr_language_is Java && Pvar.is_this pvar ->
        (Some var, hpreds)
    | hpred ->
        (var_option, hpred :: hpreds)
  in
  let collect_atom var atoms = function
    | Sil.Aneq (Exp.Var v, e) when Ident.equal v var && Exp.equal e Exp.null ->
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


(** Is true when the precondition does not contain constrains that can be false at call site.
    This means that the post-conditions associated with this precondition cannot be prevented
    by the calling context. *)
let is_unavoidable tenv pre =
  let prop = remove_this_not_null tenv (BiabductionSummary.Jprop.to_prop pre) in
  match Prop.CategorizePreconditions.categorize [prop] with
  | Prop.CategorizePreconditions.NoPres | Prop.CategorizePreconditions.Empty ->
      true
  | _ ->
      false


(** Detects if there are specs of the form {precondition} proc {runtime exception} and report
    an error in that case, generating the trace that lead to the runtime exception if the method is
    called in the context { precondition } *)
let report_runtime_exceptions tenv pdesc summary =
  let pname = Summary.get_proc_name summary in
  let is_public_method =
    PredSymb.equal_access (Summary.get_attributes summary).access PredSymb.Public
  in
  let is_main =
    is_public_method
    &&
    match pname with
    | Typ.Procname.Java pname_java ->
        Typ.Procname.Java.is_static pname_java
        && String.equal (Typ.Procname.Java.get_method pname_java) "main"
    | _ ->
        false
  in
  let is_annotated pdesc = Annotations.pdesc_has_return_annot pdesc Annotations.ia_is_verify in
  let exn_preconditions, all_post_exn = exception_preconditions tenv pname summary in
  let should_report pre =
    all_post_exn || is_main || is_annotated pdesc || is_unavoidable tenv pre
  in
  let report (pre, runtime_exception) =
    if should_report pre then
      let pre_str =
        F.asprintf "%a" (Prop.pp_prop Pp.text) (BiabductionSummary.Jprop.to_prop pre)
      in
      let exn_desc = Localise.java_unchecked_exn_desc pname runtime_exception pre_str in
      let exn = Exceptions.Java_runtime_exception (runtime_exception, pre_str, exn_desc) in
      Reporting.log_issue_deprecated_using_state Exceptions.Error pname exn
  in
  List.iter ~f:report exn_preconditions


let report_custom_errors tenv summary =
  let pname = Summary.get_proc_name summary in
  let error_preconditions, all_post_error = custom_error_preconditions summary in
  let report (pre, custom_error) =
    if all_post_error || is_unavoidable tenv pre then
      let loc = Summary.get_loc summary in
      let err_desc = Localise.desc_custom_error loc in
      let exn = Exceptions.Custom_error (custom_error, err_desc) in
      Reporting.log_issue_deprecated_using_state Exceptions.Error pname exn
  in
  List.iter ~f:report error_preconditions


module SpecMap = Caml.Map.Make (struct
  type t = Prop.normal BiabductionSummary.Jprop.t

  let compare = BiabductionSummary.Jprop.compare
end)

(** Update the specs of the current proc after the execution of one phase *)
let update_specs tenv prev_summary phase (new_specs : BiabductionSummary.NormSpec.t list) :
    BiabductionSummary.NormSpec.t list * bool =
  let new_specs = BiabductionSummary.normalized_specs_to_specs new_specs in
  let old_specs = Tabulation.get_specs_from_payload prev_summary in
  let changed = ref false in
  let current_specs =
    ref
      (List.fold
         ~f:(fun map spec ->
           SpecMap.add spec.BiabductionSummary.pre
             ( Paths.PathSet.from_renamed_list spec.BiabductionSummary.posts
             , spec.BiabductionSummary.visited )
             map )
         ~init:SpecMap.empty old_specs)
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
              new_specs)
    then (
      changed := true ;
      current_specs := SpecMap.remove old_spec.BiabductionSummary.pre !current_specs )
    else ()
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
      BiabductionSummary.spec_normalize tenv
        BiabductionSummary.{pre; posts= Paths.PathSet.elements post_set; visited}
      :: !res
  in
  List.iter ~f:re_exe_filter old_specs ;
  (* filter out pre's which failed re-exe *)
  List.iter ~f:add_spec new_specs ;
  (* add new specs *)
  SpecMap.iter convert !current_specs ;
  (!res, !changed)


(** update a summary after analysing a procedure *)
let update_summary tenv prev_summary specs phase res =
  let normal_specs = List.map ~f:(BiabductionSummary.spec_normalize tenv) specs in
  let new_specs, _ = update_specs tenv prev_summary phase normal_specs in
  let stats =
    Summary.Stats.update prev_summary.Summary.stats ~add_symops:(SymOp.get_total ())
      ?failure_kind:res
  in
  let preposts =
    match phase with
    | BiabductionSummary.FOOTPRINT ->
        new_specs
    | BiabductionSummary.RE_EXECUTION ->
        List.map ~f:(BiabductionSummary.NormSpec.erase_join_info_pre tenv) new_specs
  in
  let payloads =
    { prev_summary.Summary.payloads with
      Payloads.biabduction= Some BiabductionSummary.{preposts; phase} }
  in
  {prev_summary with Summary.stats; payloads}


(** Analyze the procedure and return the resulting summary. *)
let analyze_proc summary exe_env tenv proc_cfg : Summary.t =
  let proc_desc = ProcCfg.Exceptional.proc_desc proc_cfg in
  reset_global_values proc_desc ;
  let go, get_results = perform_analysis_phase exe_env tenv summary proc_cfg in
  let res = Timeout.exe_timeout go () in
  let specs, phase = get_results () in
  let updated_summary = update_summary tenv summary specs phase res in
  if Language.curr_language_is Clang && Config.report_custom_error then
    report_custom_errors tenv updated_summary ;
  if Language.curr_language_is Java && Config.tracing then
    report_runtime_exceptions tenv proc_desc updated_summary ;
  updated_summary


(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe summary tenv joined_pres : Summary.t =
  let summary' =
    if Config.only_footprint then
      match summary.Summary.payloads.biabduction with
      | Some ({phase= FOOTPRINT} as biabduction) ->
          { summary with
            Summary.payloads=
              { summary.Summary.payloads with
                Payloads.biabduction= Some {biabduction with BiabductionSummary.phase= RE_EXECUTION}
              } }
      | _ ->
          summary
    else
      let preposts =
        List.map
          ~f:(fun jp ->
            BiabductionSummary.spec_normalize tenv
              {BiabductionSummary.pre= jp; posts= []; visited= BiabductionSummary.Visitedset.empty}
            )
          joined_pres
      in
      let payloads =
        { summary.Summary.payloads with
          biabduction= Some BiabductionSummary.{preposts; phase= RE_EXECUTION} }
      in
      {summary with Summary.payloads}
  in
  summary'


(** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for
    the procedures enabled after the analysis of [proc_name] *)
let perform_transition proc_cfg tenv proc_name summary =
  let transition summary =
    (* disable exceptions for leaks and protect against any other errors *)
    let joined_pres =
      let allow_leak = !BiabductionConfig.allow_leak in
      (* apply the start node to f, and do nothing in case of exception *)
      let apply_start_node f =
        try f (ProcCfg.Exceptional.start_node proc_cfg)
        with exn when SymOp.exn_not_failure exn -> ()
      in
      apply_start_node (do_before_node 0) ;
      try
        BiabductionConfig.allow_leak := true ;
        let res = collect_preconditions tenv summary in
        BiabductionConfig.allow_leak := allow_leak ;
        apply_start_node do_after_node ;
        res
      with exn when SymOp.exn_not_failure exn ->
        apply_start_node do_after_node ;
        BiabductionConfig.allow_leak := allow_leak ;
        L.(debug Analysis Medium)
          "Error in collect_preconditions for %a@." Typ.Procname.pp proc_name ;
        let error = Exceptions.recognize_exception exn in
        let err_str = "exception raised " ^ error.name.IssueType.unique_id in
        L.(debug Analysis Medium) "Error: %s %a@." err_str L.pp_ocaml_pos_opt error.ocaml_pos ;
        []
    in
    transition_footprint_re_exe summary tenv joined_pres
  in
  if
    let open BiabductionSummary in
    summary.Summary.payloads.biabduction |> opt_get_phase |> equal_phase FOOTPRINT
  then transition summary
  else summary


let analyze_procedure_aux summary exe_env tenv proc_desc : Summary.t =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
  Preanal.do_preanalysis proc_desc tenv ;
  let summaryfp =
    BiabductionConfig.run_in_footprint_mode (analyze_proc summary exe_env tenv) proc_cfg
    |> perform_transition proc_cfg tenv proc_name
  in
  let summaryre =
    BiabductionConfig.run_in_re_execution_mode (analyze_proc summaryfp exe_env tenv) proc_cfg
  in
  let summary_compact =
    match summaryre.Summary.payloads.biabduction with
    | Some BiabductionSummary.({preposts} as biabduction) when Config.save_compact_summaries ->
        let sharing_env = Sil.create_sharing_env () in
        let compact_preposts =
          List.map ~f:(BiabductionSummary.NormSpec.compact sharing_env) preposts
        in
        { summaryre with
          payloads=
            { summaryre.payloads with
              biabduction= Some {biabduction with BiabductionSummary.preposts= compact_preposts} }
        }
    | _ ->
        summaryre
  in
  summary_compact


let analyze_procedure {Callbacks.summary; proc_desc; tenv; exe_env} : Summary.t =
  (* make sure models have been registered *)
  BuiltinDefn.init () ;
  try analyze_procedure_aux summary exe_env tenv proc_desc with exn ->
    IExn.reraise_if exn ~f:(fun () -> not (Exceptions.handle_exception exn)) ;
    Reporting.log_error_using_state summary exn ;
    summary
