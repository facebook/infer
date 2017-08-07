(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant
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
      | None, None
       -> 0
      | None, Some _
       -> 1
      | Some _, None
       -> -1
      | Some d1, Some d2
       -> (* shorter distance to exit is better *)
          Int.compare d1 d2
    in
    if n <> 0 then n else compare_ids n1 n2

  let compare_number_of_visits x1 x2 =
    let n = Int.compare x1.visits x2.visits in
    (* visited fewer times is better *)
    if n <> 0 then n else compare_distance_to_exit x1 x2

  let compare x1 x2 =
    if !Config.footprint then
      match Config.worklist_mode with
      | 0
       -> compare_ids x1.node x2.node
      | 1
       -> compare_distance_to_exit x1 x2
      | _
       -> compare_number_of_visits x1 x2
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

  let find table i =
    try Hashtbl.find table i
    with Not_found -> Paths.PathSet.empty

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

  let is_empty (wl: t) : bool = NodeVisitSet.is_empty wl.todo_set

  let add (wl: t) (node: Procdesc.Node.t) : unit =
    let visits =
      (* recover visit count if it was visited before *)
      try Procdesc.NodeMap.find node wl.visit_map
      with Not_found -> 0
    in
    wl.todo_set <- NodeVisitSet.add {node; visits} wl.todo_set

  (** remove the minimum element from the worklist, and increase its number of visits *)
  let remove (wl: t) : Procdesc.Node.t =
    try
      let min = NodeVisitSet.min_elt wl.todo_set in
      wl.todo_set <- NodeVisitSet.remove min wl.todo_set ;
      wl.visit_map <- Procdesc.NodeMap.add min.node (min.visits + 1) wl.visit_map ;
      (* increase the visits *)
      min.node
    with Not_found ->
      L.internal_error "@\n...Work list is empty! Impossible to remove edge...@\n" ;
      assert false
end

(* =============== END of module Worklist =============== *)

let path_set_create_worklist proc_cfg =
  (* TODO: reimplement compute_distance_to_exit_node in ProcCfg, and use that instead *)
  State.reset () ;
  Procdesc.compute_distance_to_exit_node (ProcCfg.Exceptional.proc_desc proc_cfg) ;
  Worklist.create ()

let htable_retrieve (htable: (Procdesc.Node.id, Paths.PathSet.t) Hashtbl.t) (key: Procdesc.Node.id)
    : Paths.PathSet.t =
  try Hashtbl.find htable key
  with Not_found -> Hashtbl.replace htable key Paths.PathSet.empty ; Paths.PathSet.empty

(** Add [d] to the pathset todo at [node] returning true if changed *)
let path_set_put_todo (wl: Worklist.t) (node: Procdesc.Node.t) (d: Paths.PathSet.t) : bool =
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

let path_set_checkout_todo (wl: Worklist.t) (node: Procdesc.Node.t) : Paths.PathSet.t =
  try
    let node_id = Procdesc.Node.get_id node in
    let todo = Hashtbl.find wl.Worklist.path_set_todo node_id in
    Hashtbl.replace wl.Worklist.path_set_todo node_id Paths.PathSet.empty ;
    let visited = Hashtbl.find wl.Worklist.path_set_visited node_id in
    let new_visited = Paths.PathSet.union visited todo in
    Hashtbl.replace wl.Worklist.path_set_visited node_id new_visited ; todo
  with Not_found ->
    L.internal_error "@\n@\nERROR: could not find todo for node %a@\n@." Procdesc.Node.pp node ;
    assert false

(* =============== END of the edge_set object =============== *)

let collect_do_abstract_pre pname tenv (pset: Propset.t) : Propset.t =
  if !Config.footprint then Config.run_in_re_execution_mode (Abs.lifted_abstract pname tenv) pset
  else Abs.lifted_abstract pname tenv pset

let collect_do_abstract_post pname tenv (pathset: Paths.PathSet.t) : Paths.PathSet.t =
  let abs_option p =
    if Prover.check_inconsistency tenv p then None else Some (Abs.abstract pname tenv p)
  in
  if !Config.footprint then
    Config.run_in_re_execution_mode (Paths.PathSet.map_option abs_option) pathset
  else Paths.PathSet.map_option abs_option pathset

let do_join_pre plist = Dom.proplist_collapse_pre plist

let do_join_post pname tenv (pset: Paths.PathSet.t) =
  if Config.spec_abs_level <= 0 then Dom.pathset_collapse tenv pset
  else Dom.pathset_collapse tenv (Dom.pathset_collapse_impl pname tenv pset)

let do_meet_pre tenv pset =
  if Config.meet_level > 0 then Dom.propset_meet_generate_pre tenv pset
  else Propset.to_proplist pset

(** Find the preconditions in the current spec table,
    apply meet then join, and return the joined preconditions *)
let collect_preconditions tenv summary : Prop.normal Specs.Jprop.t list =
  let proc_name = Specs.get_proc_name summary in
  let collect_do_abstract_one tenv prop =
    if !Config.footprint then Config.run_in_re_execution_mode (Abs.abstract_no_symop tenv) prop
    else Abs.abstract_no_symop tenv prop
  in
  let pres =
    List.map
      ~f:(fun spec -> Specs.Jprop.to_prop spec.Specs.pre)
      (Specs.get_specs_from_payload summary)
  in
  let pset = Propset.from_proplist tenv pres in
  let pset' =
    let f p = Prop.prop_normal_vars_to_primed_vars tenv p in
    Propset.map tenv f pset
  in
  L.d_strln ("#### Extracted footprint of " ^ Typ.Procname.to_string proc_name ^ ":  ####") ;
  L.d_increase_indent 1 ;
  Propset.d Prop.prop_emp pset' ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  L.d_ln () ;
  let pset'' = collect_do_abstract_pre proc_name tenv pset' in
  let plist_meet = do_meet_pre tenv pset'' in
  L.d_strln ("#### Footprint of " ^ Typ.Procname.to_string proc_name ^ " after Meet  ####") ;
  L.d_increase_indent 1 ;
  Propgraph.d_proplist Prop.prop_emp plist_meet ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  L.d_ln () ;
  L.d_increase_indent 2 ;
  (* Indent for the join output *)
  let jplist = do_join_pre tenv plist_meet in
  L.d_decrease_indent 2 ;
  L.d_ln () ;
  L.d_strln ("#### Footprint of " ^ Typ.Procname.to_string proc_name ^ " after Join  ####") ;
  L.d_increase_indent 1 ;
  Specs.Jprop.d_list false jplist ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  let jplist' =
    List.map ~f:(Specs.Jprop.map (Prop.prop_rename_primed_footprint_vars tenv)) jplist
  in
  L.d_strln ("#### Renamed footprint of " ^ Typ.Procname.to_string proc_name ^ ":  ####") ;
  L.d_increase_indent 1 ;
  Specs.Jprop.d_list false jplist' ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  let jplist'' =
    let f p =
      Prop.prop_primed_vars_to_normal_vars tenv (collect_do_abstract_one proc_name tenv p)
    in
    List.map ~f:(Specs.Jprop.map f) jplist'
  in
  L.d_strln ("#### Abstracted footprint of " ^ Typ.Procname.to_string proc_name ^ ":  ####") ;
  L.d_increase_indent 1 ;
  Specs.Jprop.d_list false jplist'' ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  jplist''

(* =============== START of symbolic execution =============== *)

(** propagate a set of results to the given node *)
let propagate (wl: Worklist.t) pname ~is_exception (pset: Paths.PathSet.t)
    (curr_node: Procdesc.Node.t) =
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
let propagate_nodes_divergence tenv (proc_cfg: ProcCfg.Exceptional.t) (pset: Paths.PathSet.t)
    (succ_nodes: Procdesc.Node.t list) (exn_nodes: Procdesc.Node.t list) (wl: Worklist.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pset_exn, pset_ok = Paths.PathSet.partition (Tabulation.prop_is_exn pname) pset in
  if !Config.footprint && not (Paths.PathSet.is_empty (State.get_diverging_states_node ())) then (
    Errdesc.warning_err (State.get_loc ()) "Propagating Divergence@." ;
    let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
    let diverging_states = State.get_diverging_states_node () in
    let prop_incons =
      let mk_incons prop =
        let p_abs = Abs.abstract pname tenv prop in
        let p_zero = Prop.set p_abs ~sub:Sil.exp_sub_empty ~sigma:[] in
        Prop.normalize tenv (Prop.set p_zero ~pi:[Sil.Aneq (Exp.zero, Exp.zero)])
      in
      Paths.PathSet.map mk_incons diverging_states
    in
    L.d_strln_color Orange "Propagating Divergence -- diverging states:" ;
    Propgraph.d_proplist Prop.prop_emp (Paths.PathSet.to_proplist prop_incons) ;
    L.d_ln () ;
    propagate wl pname ~is_exception:false prop_incons exit_node ) ;
  List.iter ~f:(propagate wl pname ~is_exception:false pset_ok) succ_nodes ;
  List.iter ~f:(propagate wl pname ~is_exception:true pset_exn) exn_nodes

(* ===================== END of symbolic execution ===================== *)
(* =============== START of forward_tabulate =============== *)

(** Symbolic execution for a Join node *)
let do_symexec_join proc_cfg tenv wl curr_node (edgeset_todo: Paths.PathSet.t) =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let curr_node_id = ProcCfg.Exceptional.id curr_node in
  let succ_nodes = ProcCfg.Exceptional.normal_succs proc_cfg curr_node in
  let new_dset = edgeset_todo in
  let old_dset = Join_table.find wl.Worklist.join_table curr_node_id in
  let old_dset', new_dset' = Dom.pathset_join pname tenv old_dset new_dset in
  Join_table.add wl.Worklist.join_table curr_node_id (Paths.PathSet.union old_dset' new_dset') ;
  List.iter
    ~f:(fun node ->
      Paths.PathSet.iter
        (fun prop path ->
          State.set_path path None ;
          propagate wl pname ~is_exception:false
            (Paths.PathSet.from_renamed_list [(prop, path)])
            node)
        new_dset')
    succ_nodes

let prop_max_size = ref (0, Prop.prop_emp)

let prop_max_chain_size = ref (0, Prop.prop_emp)

(* Check if the prop exceeds the current max *)
let check_prop_size_ p _ =
  let size = Prop.Metrics.prop_size p in
  if size > fst !prop_max_size then (
    prop_max_size := (size, p) ;
    L.d_strln ("Prop with new max size " ^ string_of_int size ^ ":") ;
    Prop.d_prop p ;
    L.d_ln () )

(* Check prop size and filter out possible unabstracted lists *)
let check_prop_size edgeset_todo =
  if Config.monitor_prop_size then Paths.PathSet.iter check_prop_size_ edgeset_todo

let reset_prop_metrics () =
  prop_max_size := (0, Prop.prop_emp) ;
  prop_max_chain_size := (0, Prop.prop_emp)

exception RE_EXE_ERROR

let do_before_node session node =
  State.set_node node ;
  State.set_session session ;
  L.reset_delayed_prints () ;
  Printer.node_start_session node (session :> int)

let do_after_node node = Printer.node_finish_session node

(** Return the list of normal ids occurring in the instructions *)
let instrs_get_normal_vars instrs =
  let fav = Sil.fav_new () in
  let do_instr instr =
    let do_e e = Sil.exp_fav_add fav e in
    let exps = Sil.instr_get_exps instr in
    List.iter ~f:do_e exps
  in
  List.iter ~f:do_instr instrs ; Sil.fav_filter_ident fav Ident.is_normal ; Sil.fav_to_list fav

(** Perform symbolic execution for a node starting from an initial prop *)
let do_symbolic_execution proc_cfg handle_exn tenv (node: ProcCfg.Exceptional.node)
    (prop: Prop.normal Prop.t) (path: Paths.Path.t) =
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  State.mark_execution_start node ;
  (* build the const map lazily *)
  State.set_const_map (ConstantPropagation.build_const_map tenv pdesc) ;
  let instrs = ProcCfg.Exceptional.instrs node in
  (* fresh normal vars must be fresh w.r.t. instructions *)
  Ident.update_name_generator (instrs_get_normal_vars instrs) ;
  let pset =
    SymExec.node handle_exn tenv proc_cfg node (Paths.PathSet.from_renamed_list [(prop, path)])
  in
  L.d_strln ".... After Symbolic Execution ...." ;
  Propset.d prop (Paths.PathSet.to_propset tenv pset) ;
  L.d_ln () ;
  L.d_ln () ;
  State.mark_execution_end node ;
  pset

let mark_visited summary node =
  let node_id = Procdesc.Node.get_id node in
  let stats = summary.Specs.stats in
  if !Config.footprint then
    stats.Specs.nodes_visited_fp <- IntSet.add (node_id :> int) stats.Specs.nodes_visited_fp
  else stats.Specs.nodes_visited_re <- IntSet.add (node_id :> int) stats.Specs.nodes_visited_re

let forward_tabulate tenv proc_cfg wl =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let handle_exn_node curr_node exn =
    Exceptions.print_exception_html "Failure of symbolic execution: " exn ;
    let pre_opt =
      (* precondition leading to error, if any *)
      State.get_normalized_pre (Abs.abstract_no_symop pname)
    in
    ( match pre_opt with
    | Some pre
     -> L.d_strln "Precondition:" ; Prop.d_prop pre ; L.d_ln ()
    | None
     -> () ) ;
    L.d_strln "SIL INSTR:" ;
    Procdesc.Node.d_instrs ~sub_instrs:true (State.get_instr ()) curr_node ;
    L.d_ln () ;
    Reporting.log_error_deprecated pname exn ;
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
  let print_node_preamble curr_node session pathset_todo =
    let log_string proc_name =
      let summary = Specs.get_summary_unsafe "forward_tabulate" proc_name in
      let phase_string =
        if Specs.equal_phase (Specs.get_phase summary) Specs.FOOTPRINT then "FP" else "RE"
      in
      let status = Specs.get_status summary in
      F.sprintf "[%s:%s] %s" phase_string (Specs.string_of_status status)
        (Typ.Procname.to_string proc_name)
    in
    L.d_strln
      ( "**** " ^ log_string pname ^ " " ^ "Node: "
      ^ string_of_int (Procdesc.Node.get_id curr_node :> int) ^ ", " ^ "Procedure: "
      ^ Typ.Procname.to_string pname ^ ", " ^ "Session: " ^ string_of_int session ^ ", " ^ "Todo: "
      ^ string_of_int (Paths.PathSet.size pathset_todo) ^ " ****" ) ;
    L.d_increase_indent 1 ;
    Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset_todo) ;
    L.d_strln ".... Instructions: .... " ;
    Procdesc.Node.d_instrs ~sub_instrs:true (State.get_instr ()) curr_node ;
    L.d_ln () ;
    L.d_ln ()
  in
  let do_prop (curr_node: ProcCfg.Exceptional.node) handle_exn prop path cnt num_paths =
    L.d_strln ("Processing prop " ^ string_of_int cnt ^ "/" ^ string_of_int num_paths) ;
    L.d_increase_indent 1 ;
    try
      State.reset_diverging_states_node () ;
      let pset = do_symbolic_execution proc_cfg handle_exn tenv curr_node prop path in
      let normal_succ_nodes = ProcCfg.Exceptional.normal_succs proc_cfg curr_node in
      let exn_succ_nodes = ProcCfg.Exceptional.exceptional_succs proc_cfg curr_node in
      propagate_nodes_divergence tenv proc_cfg pset normal_succ_nodes exn_succ_nodes wl ;
      L.d_decrease_indent 1 ;
      L.d_ln ()
    with exn when Exceptions.handle_exception exn && !Config.footprint ->
      handle_exn exn ; L.d_decrease_indent 1 ; L.d_ln ()
  in
  let do_node curr_node pathset_todo session handle_exn =
    check_prop_size pathset_todo ;
    print_node_preamble curr_node session pathset_todo ;
    match Procdesc.Node.get_kind curr_node with
    | Procdesc.Node.Join_node
     -> do_symexec_join proc_cfg tenv wl curr_node pathset_todo
    | Procdesc.Node.Stmt_node _
    | Procdesc.Node.Prune_node _
    | Procdesc.Node.Exit_node _
    | Procdesc.Node.Skip_node _
    | Procdesc.Node.Start_node _
     -> exe_iter (do_prop curr_node handle_exn) pathset_todo
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
    with exn when Exceptions.handle_exception exn ->
      handle_exn_node curr_node exn ;
      Printer.force_delayed_prints () ;
      do_after_node curr_node ;
      if not !Config.footprint then raise RE_EXE_ERROR
  in
  while not (Worklist.is_empty wl) do
    let curr_node = Worklist.remove wl in
    let summary = Specs.get_summary_unsafe "forward_tabulate" pname in
    mark_visited summary curr_node ;
    (* mark nodes visited in fp and re phases *)
    let session = incr summary.Specs.sessions ; !(summary.Specs.sessions) in
    do_before_node session curr_node ; do_node_and_handle curr_node session
  done ;
  L.d_strln ".... Work list empty. Stop ...." ;
  L.d_ln ()

(** if possible, produce a (fieldname, typ) path from one of the [src_exps] to [sink_exp] using
    [reachable_hpreds]. *)
let get_fld_typ_path_opt src_exps sink_exp_ reachable_hpreds_ =
  let strexp_matches target_exp = function
    | Sil.Eexp (e, _)
     -> Exp.equal target_exp e
    | _
     -> false
  in
  let extend_path hpred (sink_exp, path, reachable_hpreds) =
    match hpred with
    | Sil.Hpointsto (lhs, Sil.Estruct (flds, _), Exp.Sizeof {typ})
     -> List.find ~f:(function _, se -> strexp_matches sink_exp se) flds
        |> Option.value_map
             ~f:(function
                 | fld, _
                  -> let reachable_hpreds' = Sil.HpredSet.remove hpred reachable_hpreds in
                     (lhs, (Some fld, typ) :: path, reachable_hpreds'))
             ~default:(sink_exp, path, reachable_hpreds)
    | Sil.Hpointsto (lhs, Sil.Earray (_, elems, _), Exp.Sizeof {typ})
     -> if List.exists ~f:(function _, se -> strexp_matches sink_exp se) elems then
          let reachable_hpreds' = Sil.HpredSet.remove hpred reachable_hpreds in
          (* None means "no field name" ~=~ nameless array index *)
          (lhs, (None, typ) :: path, reachable_hpreds')
        else (sink_exp, path, reachable_hpreds)
    | _
     -> (sink_exp, path, reachable_hpreds)
  in
  (* terminates because [reachable_hpreds] is shrinking on each recursive call *)
  let rec get_fld_typ_path sink_exp path reachable_hpreds =
    let sink_exp', path', reachable_hpreds' =
      Sil.HpredSet.fold extend_path reachable_hpreds (sink_exp, path, reachable_hpreds)
    in
    if Exp.Set.mem sink_exp' src_exps then Some path'
    else if Sil.HpredSet.cardinal reachable_hpreds' >= Sil.HpredSet.cardinal reachable_hpreds then
      None (* can't find a path from [src_exps] to [sink_exp] *)
    else get_fld_typ_path sink_exp' path' reachable_hpreds'
  in
  get_fld_typ_path sink_exp_ [] reachable_hpreds_

(** report an error if any Context is reachable from a static field *)
let report_context_leaks pname sigma tenv =
  (* report an error if an expression in [context_exps] is reachable from [field_strexp] *)
  let check_reachable_context_from_fld (fld_name, fld_strexp) context_exps =
    let fld_exps = Prop.strexp_get_exps fld_strexp in
    let reachable_hpreds, reachable_exps = Prop.compute_reachable_hpreds sigma fld_exps in
    (* raise an error if any Context expression is in [reachable_exps] *)
    List.iter
      ~f:(fun (context_exp, name) ->
        if Exp.Set.mem context_exp reachable_exps then
          let leak_path =
            match get_fld_typ_path_opt fld_exps context_exp reachable_hpreds with
            | Some path
             -> path
            | None
             -> assert false
            (* a path must exist in order for a leak to be reported *)
          in
          let err_desc =
            Errdesc.explain_context_leak pname (Typ.mk (Tstruct name)) fld_name leak_path
          in
          let exn = Exceptions.Context_leak (err_desc, __POS__) in
          Reporting.log_error_deprecated pname exn)
      context_exps
  in
  (* get the set of pointed-to expressions of type T <: Context *)
  let context_exps =
    List.fold
      ~f:(fun exps hpred ->
        match hpred with
        | Sil.Hpointsto (_, Eexp (exp, _), Sizeof {typ= {desc= Tptr ({desc= Tstruct name}, _)}})
          when not (Exp.is_null_literal exp) && AndroidFramework.is_context tenv name
               && not (AndroidFramework.is_application tenv name)
         -> (exp, name) :: exps
        | _
         -> exps)
      ~init:[] sigma
  in
  List.iter
    ~f:(function
        | Sil.Hpointsto (Exp.Lvar pv, Sil.Estruct (static_flds, _), _) when Pvar.is_global pv
         -> List.iter
              ~f:(fun (f_name, f_strexp) ->
                check_reachable_context_from_fld (f_name, f_strexp) context_exps)
              static_flds
        | _
         -> ())
    sigma

(** Remove locals and formals,
    and check if the address of a stack variable is left in the result *)
let remove_locals_formals_and_check tenv proc_cfg p =
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let pname = Procdesc.get_proc_name pdesc in
  let pvars, p' = PropUtil.remove_locals_formals tenv pdesc p in
  let check_pvar pvar =
    let loc = ProcCfg.Exceptional.loc (ProcCfg.Exceptional.exit_node proc_cfg) in
    let dexp_opt, _ = Errdesc.vpath_find tenv p (Exp.Lvar pvar) in
    let desc = Errdesc.explain_stack_variable_address_escape loc pvar dexp_opt in
    let exn = Exceptions.Stack_variable_address_escape (desc, __POS__) in
    Reporting.log_warning_deprecated pname exn
  in
  List.iter ~f:check_pvar pvars ; p'

(** Collect the analysis results for the exit node. *)
let collect_analysis_result tenv wl proc_cfg : Paths.PathSet.t =
  let exit_node = ProcCfg.Exceptional.exit_node proc_cfg in
  let exit_node_id = ProcCfg.Exceptional.id exit_node in
  let pathset = htable_retrieve wl.Worklist.path_set_visited exit_node_id in
  Paths.PathSet.map (remove_locals_formals_and_check tenv proc_cfg) pathset

module Pmap = Caml.Map.Make (struct
  type t = Prop.normal Prop.t

  let compare = Prop.compare_prop
end)

let vset_ref_add_path vset_ref path =
  Paths.Path.iter_all_nodes_nocalls (fun n -> vset_ref := Procdesc.NodeSet.add n !vset_ref) path

let vset_ref_add_pathset vset_ref pathset =
  Paths.PathSet.iter (fun _ path -> vset_ref_add_path vset_ref path) pathset

let compute_visited vset =
  let res = ref Specs.Visitedset.empty in
  let node_get_all_lines n =
    let node_loc = Procdesc.Node.get_loc n in
    let instrs_loc = List.map ~f:Sil.instr_get_loc (ProcCfg.Exceptional.instrs n) in
    let lines = List.map ~f:(fun loc -> loc.Location.line) (node_loc :: instrs_loc) in
    List.remove_consecutive_duplicates ~equal:Int.equal (List.sort ~cmp:Int.compare lines)
  in
  let do_node n =
    res := Specs.Visitedset.add (Procdesc.Node.get_id n, node_get_all_lines n) !res
  in
  Procdesc.NodeSet.iter do_node vset ; !res

(** Extract specs from a pathset *)
let extract_specs tenv pdesc pathset : Prop.normal Specs.spec list =
  let pname = Procdesc.get_proc_name pdesc in
  let sub =
    let fav = Sil.fav_new () in
    Paths.PathSet.iter (fun prop _ -> Prop.prop_fav_add fav prop) pathset ;
    let sub_list =
      List.map
        ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.knormal)))
        (Sil.fav_to_list fav)
    in
    Sil.exp_subst_of_list sub_list
  in
  let pre_post_visited_list =
    let pplist = Paths.PathSet.elements pathset in
    let f (prop, path) =
      let _, prop' = PropUtil.remove_locals_formals tenv pdesc prop in
      let prop'' = Abs.abstract pname tenv prop' in
      let pre, post = Prop.extract_spec prop'' in
      let pre' = Prop.normalize tenv (Prop.prop_sub (`Exp sub) pre) in
      if Config.curr_language_is Config.Java && Procdesc.get_access pdesc <> PredSymb.Private then
        report_context_leaks pname post.Prop.sigma tenv ;
      let post' =
        if Prover.check_inconsistency_base tenv prop then None
        else Some (Prop.normalize tenv (Prop.prop_sub (`Exp sub) post), path)
      in
      let visited =
        let vset_ref = ref Procdesc.NodeSet.empty in
        vset_ref_add_path vset_ref path ;
        compute_visited !vset_ref
      in
      (pre', post', visited)
    in
    List.map ~f pplist
  in
  let pre_post_map =
    let add map (pre, post, visited) =
      let current_posts, current_visited =
        try Pmap.find pre map
        with Not_found -> (Paths.PathSet.empty, Specs.Visitedset.empty)
      in
      let new_posts =
        match post with
        | None
         -> current_posts
        | Some (post, path)
         -> Paths.PathSet.add_renamed_prop post path current_posts
      in
      let new_visited = Specs.Visitedset.union visited current_visited in
      Pmap.add pre (new_posts, new_visited) map
    in
    List.fold ~f:add ~init:Pmap.empty pre_post_visited_list
  in
  let specs = ref [] in
  let add_spec pre ((posts: Paths.PathSet.t), visited) =
    let posts' =
      List.map
        ~f:(fun (p, path) -> (PropUtil.remove_seed_vars tenv p, path))
        (Paths.PathSet.elements (do_join_post pname tenv posts))
    in
    let spec =
      {Specs.pre= Specs.Jprop.Prop (1, pre); Specs.posts= posts'; Specs.visited= visited}
    in
    specs := spec :: !specs
  in
  Pmap.iter add_spec pre_post_map ; !specs

let collect_postconditions wl tenv proc_cfg : Paths.PathSet.t * Specs.Visitedset.t =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let pathset = collect_analysis_result tenv wl proc_cfg in
  (* Assuming C++ developers use RAII, remove resources from the constructor posts *)
  let pathset =
    match pname with
    | Typ.Procname.ObjC_Cpp _
     -> if Typ.Procname.is_constructor pname then
          Paths.PathSet.map
            (fun prop ->
              Attribute.remove_resource tenv Racquire (Rmemory Mobjc)
                (Attribute.remove_resource tenv Racquire (Rmemory Mmalloc)
                   (Attribute.remove_resource tenv Racquire Rfile prop)))
            pathset
        else pathset
    | _
     -> pathset
  in
  L.d_strln ("#### [FUNCTION " ^ Typ.Procname.to_string pname ^ "] Analysis result ####") ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv pathset) ;
  L.d_ln () ;
  let res =
    try
      let pathset = collect_do_abstract_post pname tenv pathset in
      let pathset_diverging = State.get_diverging_states_proc () in
      let visited =
        let vset_ref = ref Procdesc.NodeSet.empty in
        vset_ref_add_pathset vset_ref pathset ;
        (* nodes from diverging states were also visited *)
        vset_ref_add_pathset vset_ref pathset_diverging ;
        compute_visited !vset_ref
      in
      (do_join_post pname tenv pathset, visited)
    with exn when match exn with Exceptions.Leak _ -> true | _ -> false ->
      L.d_strln "Leak in post collection" ;
      assert false
  in
  L.d_strln ("#### [FUNCTION " ^ Typ.Procname.to_string pname ^ "] Postconditions after join ####") ;
  L.d_increase_indent 1 ;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset tenv (fst res)) ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  res

let create_seed_vars sigma =
  let hpred_add_seed sigma = function
    | Sil.Hpointsto (Exp.Lvar pv, se, typ) when not (Pvar.is_abduced pv)
     -> Sil.Hpointsto (Exp.Lvar (Pvar.to_seed pv), se, typ) :: sigma
    | _
     -> sigma
  in
  List.fold ~f:hpred_add_seed ~init:[] sigma

(** Initialize proposition for execution given formal and global
    parameters. The footprint is initialized according to the
    execution mode. The prop is not necessarily emp, so it
    should be incorporated when the footprint is constructed. *)
let prop_init_formals_seed tenv new_formals (prop: 'a Prop.t) : Prop.exposed Prop.t =
  let sigma_new_formals =
    let do_formal (pv, typ) =
      let texp =
        match !Config.curr_language with
        | Config.Clang
         -> Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        | Config.Java
         -> Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes}
      in
      Prop.mk_ptsto_lvar tenv Prop.Fld_init Sil.inst_formal (pv, texp, None)
    in
    List.map ~f:do_formal new_formals
  in
  let sigma_seed =
    create_seed_vars (* formals already there plus new ones *)
                     (prop.Prop.sigma @ sigma_new_formals)
  in
  let sigma = sigma_seed @ sigma_new_formals in
  let new_pi = prop.Prop.pi in
  let prop' = Prop.set (Prop.prop_sigma_star prop sigma) ~pi:new_pi in
  Prop.set prop' ~sigma_fp:(prop'.Prop.sigma_fp @ sigma_new_formals)

(** Construct an initial prop by extending [prop] with locals, and formals if [add_formals] is true
    as well as seed variables *)
let initial_prop tenv (curr_f: Procdesc.t) (prop: 'a Prop.t) add_formals : Prop.normal Prop.t =
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

(** Construct an initial prop from the empty prop *)
let initial_prop_from_emp tenv curr_f = initial_prop tenv curr_f Prop.prop_emp true

(** Construct an initial prop from an existing pre with formals *)
let initial_prop_from_pre tenv curr_f pre =
  if !Config.footprint then
    let vars = Sil.fav_to_list (Prop.prop_fav pre) in
    let sub_list =
      List.map ~f:(fun id -> (id, Exp.Var (Ident.create_fresh Ident.kfootprint))) vars
    in
    let sub = Sil.subst_of_list sub_list in
    let pre2 = Prop.prop_sub sub pre in
    let pre3 = Prop.set pre2 ~pi_fp:(Prop.get_pure pre2) ~sigma_fp:pre2.Prop.sigma in
    initial_prop tenv curr_f pre3 false
  else initial_prop tenv curr_f pre false

(** Re-execute one precondition and return some spec if there was no re-execution error. *)
let execute_filter_prop wl tenv proc_cfg init_node (precondition: Prop.normal Specs.Jprop.t)
    : Prop.normal Specs.spec option =
  let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let pname = Procdesc.get_proc_name pdesc in
  do_before_node 0 init_node ;
  L.d_strln ("#### Start: RE-execution for " ^ Typ.Procname.to_string pname ^ " ####") ;
  L.d_indent 1 ;
  L.d_strln "Precond:" ;
  Specs.Jprop.d_shallow precondition ;
  L.d_ln () ;
  L.d_ln () ;
  let init_prop = initial_prop_from_pre tenv pdesc (Specs.Jprop.to_prop precondition) in
  let init_edgeset =
    Paths.PathSet.add_renamed_prop init_prop (Paths.Path.start init_node) Paths.PathSet.empty
  in
  do_after_node init_node ;
  try
    Worklist.add wl init_node ;
    ignore (path_set_put_todo wl init_node init_edgeset) ;
    forward_tabulate tenv proc_cfg wl ;
    do_before_node 0 init_node ;
    L.d_strln_color Green
      ("#### Finished: RE-execution for " ^ Typ.Procname.to_string pname ^ " ####") ;
    L.d_increase_indent 1 ;
    L.d_strln "Precond:" ;
    Prop.d_prop (Specs.Jprop.to_prop precondition) ;
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
      let p = PropUtil.remove_locals_ret tenv pdesc (Specs.Jprop.to_prop precondition) in
      match precondition with
      | Specs.Jprop.Prop (n, _)
       -> Specs.Jprop.Prop (n, p)
      | Specs.Jprop.Joined (n, _, jp1, jp2)
       -> Specs.Jprop.Joined (n, p, jp1, jp2)
    in
    let spec = {Specs.pre= pre; Specs.posts= posts; Specs.visited= visited} in
    L.d_decrease_indent 1 ; do_after_node init_node ; Some spec
  with RE_EXE_ERROR ->
    do_before_node 0 init_node ;
    Printer.force_delayed_prints () ;
    L.d_strln_color Red ("#### [FUNCTION " ^ Typ.Procname.to_string pname ^ "] ...ERROR") ;
    L.d_increase_indent 1 ;
    L.d_strln "when starting from pre:" ;
    Prop.d_prop (Specs.Jprop.to_prop precondition) ;
    L.d_strln "This precondition is filtered out." ;
    L.d_decrease_indent 1 ;
    do_after_node init_node ;
    None

let pp_intra_stats wl proc_cfg fmt _ =
  let nstates = ref 0 in
  let nodes = ProcCfg.Exceptional.nodes proc_cfg in
  List.iter
    ~f:(fun node ->
      nstates
      := !nstates
         + Paths.PathSet.size
             (htable_retrieve wl.Worklist.path_set_visited (ProcCfg.Exceptional.id node)))
    nodes ;
  F.fprintf fmt "(%d nodes containing %d states)" (List.length nodes) !nstates

type exe_phase = (unit -> unit) * (unit -> Prop.normal Specs.spec list * Specs.phase)

(** Return functions to perform one phase of the analysis for a procedure.
    Given [proc_name], return [do, get_results] where [go ()] performs the analysis phase
    and [get_results ()] returns the results computed.
    This function is architected so that [get_results ()] can be called even after
    [go ()] was interrupted by and exception. *)
let perform_analysis_phase tenv (summary: Specs.summary) (proc_cfg: ProcCfg.Exceptional.t)
    : exe_phase =
  let pname = Specs.get_proc_name summary in
  let start_node = ProcCfg.Exceptional.start_node proc_cfg in
  let compute_footprint () : exe_phase =
    let go (wl: Worklist.t) () =
      let pdesc = ProcCfg.Exceptional.proc_desc proc_cfg in
      let init_prop = initial_prop_from_emp tenv pdesc in
      (* use existing pre's (in recursion some might exist) as starting points *)
      let init_props_from_pres =
        let specs = Specs.get_specs_from_payload summary in
        (* rename spec vars to footrpint vars, and copy current to footprint *)
        let mk_init precondition =
          initial_prop_from_pre tenv pdesc (Specs.Jprop.to_prop precondition)
        in
        List.map ~f:(fun spec -> mk_init spec.Specs.pre) specs
      in
      let init_props = Propset.from_proplist tenv (init_prop :: init_props_from_pres) in
      let init_edgeset =
        let add pset prop =
          Paths.PathSet.add_renamed_prop prop (Paths.Path.start start_node) pset
        in
        Propset.fold add Paths.PathSet.empty init_props
      in
      L.(debug Analysis Medium)
        "@\n#### Start: Footprint Computation for %a ####@." Typ.Procname.pp pname ;
      L.d_increase_indent 1 ;
      L.d_strln "initial props =" ;
      Propset.d Prop.prop_emp init_props ;
      L.d_ln () ;
      L.d_ln () ;
      L.d_decrease_indent 1 ;
      Worklist.add wl start_node ;
      Config.arc_mode := Hashtbl.mem (Procdesc.get_flags pdesc) Mleak_buckets.objc_arc_flag ;
      ignore (path_set_put_todo wl start_node init_edgeset) ;
      forward_tabulate tenv proc_cfg wl
    in
    let get_results (wl: Worklist.t) () =
      State.process_execution_failures Reporting.log_warning_deprecated pname ;
      let results = collect_analysis_result tenv wl proc_cfg in
      L.(debug Analysis Medium) "#### [FUNCTION %a] ... OK #####@\n" Typ.Procname.pp pname ;
      L.(debug Analysis Medium)
        "#### Finished: Footprint Computation for %a %a ####@." Typ.Procname.pp pname
        (pp_intra_stats wl proc_cfg) pname ;
      L.(debug Analysis Medium)
        "#### [FUNCTION %a] Footprint Analysis result ####@\n%a@." Typ.Procname.pp pname
        (Paths.PathSet.pp Pp.text) results ;
      let specs =
        try extract_specs tenv (ProcCfg.Exceptional.proc_desc proc_cfg) results
        with Exceptions.Leak _ ->
          let exn =
            Exceptions.Internal_error
              (Localise.verbatim_desc "Leak_while_collecting_specs_after_footprint")
          in
          Reporting.log_error_deprecated pname exn ; []
        (* retuning no specs *)
      in
      (specs, Specs.FOOTPRINT)
    in
    let wl = path_set_create_worklist proc_cfg in
    (go wl, get_results wl)
  in
  let re_execution () : exe_phase =
    let candidate_preconditions =
      List.map ~f:(fun spec -> spec.Specs.pre) (Specs.get_specs_from_payload summary)
    in
    let valid_specs = ref [] in
    let go () =
      L.(debug Analysis Medium) "@.#### Start: Re-Execution for %a ####@." Typ.Procname.pp pname ;
      let filter p =
        let wl = path_set_create_worklist proc_cfg in
        let speco = execute_filter_prop wl tenv proc_cfg start_node p in
        let is_valid =
          match speco with
          | None
           -> false
          | Some spec
           -> valid_specs := !valid_specs @ [spec] ;
              true
        in
        let outcome = if is_valid then "pass" else "fail" in
        L.(debug Analysis Medium)
          "Finished re-execution for precondition %d %a (%s)@." (Specs.Jprop.to_number p)
          (pp_intra_stats wl proc_cfg) pname outcome ;
        speco
      in
      if Config.undo_join then ignore (Specs.Jprop.filter filter candidate_preconditions)
      else ignore (List.map ~f:filter candidate_preconditions)
    in
    let get_results () =
      let specs = !valid_specs in
      L.(debug Analysis Medium) "#### [FUNCTION %a] ... OK #####@\n" Typ.Procname.pp pname ;
      L.(debug Analysis Medium) "#### Finished: Re-Execution for %a ####@." Typ.Procname.pp pname ;
      let valid_preconditions = List.map ~f:(fun spec -> spec.Specs.pre) specs in
      let source = (Procdesc.get_loc (ProcCfg.Exceptional.proc_desc proc_cfg)).file in
      let filename =
        DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source)
          [Typ.Procname.to_filename pname]
      in
      if Config.write_dotty then Dotty.pp_speclist_dotty_file filename specs ;
      L.(debug Analysis Medium) "@\n@\n================================================" ;
      L.(debug Analysis Medium) "@\n *** CANDIDATE PRECONDITIONS FOR %a: " Typ.Procname.pp pname ;
      L.(debug Analysis Medium) "@\n================================================@\n" ;
      L.(debug Analysis Medium)
        "@\n%a @\n@\n" (Specs.Jprop.pp_list Pp.text false) candidate_preconditions ;
      L.(debug Analysis Medium) "@\n@\n================================================" ;
      L.(debug Analysis Medium) "@\n *** VALID PRECONDITIONS FOR %a: " Typ.Procname.pp pname ;
      L.(debug Analysis Medium) "@\n================================================@\n" ;
      L.(debug Analysis Medium)
        "@\n%a @\n@." (Specs.Jprop.pp_list Pp.text true) valid_preconditions ;
      (specs, Specs.RE_EXECUTION)
    in
    (go, get_results)
  in
  match Specs.get_phase summary with
  | Specs.FOOTPRINT
   -> compute_footprint ()
  | Specs.RE_EXECUTION
   -> re_execution ()

let set_current_language proc_desc =
  let language = (Procdesc.get_attributes proc_desc).ProcAttributes.language in
  Config.curr_language := language

(** reset global values before analysing a procedure *)
let reset_global_values proc_desc =
  Config.reset_abs_val () ;
  Ident.NameGenerator.reset () ;
  SymOp.reset_total () ;
  reset_prop_metrics () ;
  Abs.reset_current_rules () ;
  set_current_language proc_desc

(* Collect all pairs of the kind (precondition, runtime exception) from a summary *)
let exception_preconditions tenv pname summary =
  let collect_exceptions pre (exns, all_post_exn) (prop, _) =
    match Tabulation.prop_get_exn_name pname prop with
    | Some exn_name when PatternMatch.is_runtime_exception tenv exn_name
     -> ((pre, exn_name) :: exns, all_post_exn)
    | _
     -> (exns, false)
  in
  let collect_spec errors spec =
    List.fold ~f:(collect_exceptions spec.Specs.pre) ~init:errors spec.Specs.posts
  in
  List.fold ~f:collect_spec ~init:([], true) (Specs.get_specs_from_payload summary)

(* Collect all pairs of the kind (precondition, custom error) from a summary *)
let custom_error_preconditions summary =
  let collect_errors pre (errors, all_post_error) (prop, _) =
    match Tabulation.lookup_custom_errors prop with
    | None
     -> (errors, false)
    | Some e
     -> ((pre, e) :: errors, all_post_error)
  in
  let collect_spec errors spec =
    List.fold ~f:(collect_errors spec.Specs.pre) ~init:errors spec.Specs.posts
  in
  List.fold ~f:collect_spec ~init:([], true) (Specs.get_specs_from_payload summary)

(* Remove the constrain of the form this != null which is true for all Java virtual calls *)
let remove_this_not_null tenv prop =
  let collect_hpred (var_option, hpreds) = function
    | Sil.Hpointsto (Exp.Lvar pvar, Sil.Eexp (Exp.Var var, _), _)
      when Config.curr_language_is Config.Java && Pvar.is_this pvar
     -> (Some var, hpreds)
    | hpred
     -> (var_option, hpred :: hpreds)
  in
  let collect_atom var atoms = function
    | Sil.Aneq (Exp.Var v, e) when Ident.equal v var && Exp.equal e Exp.null
     -> atoms
    | a
     -> a :: atoms
  in
  match List.fold ~f:collect_hpred ~init:(None, []) prop.Prop.sigma with
  | None, _
   -> prop
  | Some var, filtered_hpreds
   -> let filtered_atoms = List.fold ~f:(collect_atom var) ~init:[] prop.Prop.pi in
      let prop' = Prop.set Prop.prop_emp ~pi:filtered_atoms ~sigma:filtered_hpreds in
      Prop.normalize tenv prop'

(** Is true when the precondition does not contain constrains that can be false at call site.
    This means that the post-conditions associated with this precondition cannot be prevented
    by the calling context. *)
let is_unavoidable tenv pre =
  let prop = remove_this_not_null tenv (Specs.Jprop.to_prop pre) in
  match Prop.CategorizePreconditions.categorize [prop] with
  | Prop.CategorizePreconditions.NoPres | Prop.CategorizePreconditions.Empty
   -> true
  | _
   -> false

(** Detects if there are specs of the form {precondition} proc {runtime exception} and report
    an error in that case, generating the trace that lead to the runtime exception if the method is
    called in the context { precondition } *)
let report_runtime_exceptions tenv pdesc summary =
  let pname = Specs.get_proc_name summary in
  let is_public_method =
    PredSymb.equal_access (Specs.get_attributes summary).access PredSymb.Public
  in
  let is_main =
    is_public_method
    &&
    match pname with
    | Typ.Procname.Java pname_java
     -> Typ.Procname.java_is_static pname
        && String.equal (Typ.Procname.java_get_method pname_java) "main"
    | _
     -> false
  in
  let is_annotated pdesc = Annotations.pdesc_has_return_annot pdesc Annotations.ia_is_verify in
  let exn_preconditions, all_post_exn = exception_preconditions tenv pname summary in
  let should_report pre =
    all_post_exn || is_main || is_annotated pdesc || is_unavoidable tenv pre
  in
  let report (pre, runtime_exception) =
    if should_report pre then
      let pre_str = F.asprintf "%a" (Prop.pp_prop Pp.text) (Specs.Jprop.to_prop pre) in
      let exn_desc = Localise.java_unchecked_exn_desc pname runtime_exception pre_str in
      let exn = Exceptions.Java_runtime_exception (runtime_exception, pre_str, exn_desc) in
      Reporting.log_error_deprecated pname exn
  in
  List.iter ~f:report exn_preconditions

let report_custom_errors tenv summary =
  let pname = Specs.get_proc_name summary in
  let error_preconditions, all_post_error = custom_error_preconditions summary in
  let report (pre, custom_error) =
    if all_post_error || is_unavoidable tenv pre then
      let loc = summary.Specs.attributes.ProcAttributes.loc in
      let err_desc = Localise.desc_custom_error loc in
      let exn = Exceptions.Custom_error (custom_error, err_desc) in
      Reporting.log_error_deprecated pname exn
  in
  List.iter ~f:report error_preconditions

module SpecMap = Caml.Map.Make (struct
  type t = Prop.normal Specs.Jprop.t

  let compare = Specs.Jprop.compare
end)

(** Update the specs of the current proc after the execution of one phase *)
let update_specs tenv prev_summary phase (new_specs: Specs.NormSpec.t list)
    : Specs.NormSpec.t list * bool =
  let new_specs = Specs.normalized_specs_to_specs new_specs in
  let old_specs = Specs.get_specs_from_payload prev_summary in
  let changed = ref false in
  let current_specs =
    ref
      (List.fold
         ~f:(fun map spec ->
           SpecMap.add spec.Specs.pre
             (Paths.PathSet.from_renamed_list spec.Specs.posts, spec.Specs.visited) map)
         ~init:SpecMap.empty old_specs)
  in
  let re_exe_filter old_spec =
    (* filter out pres which failed re-exe *)
    if Specs.equal_phase phase Specs.RE_EXECUTION
       && not
            (List.exists
               ~f:(fun new_spec -> Specs.Jprop.equal new_spec.Specs.pre old_spec.Specs.pre)
               new_specs)
    then (
      changed := true ;
      L.(debug Analysis Medium)
        "Specs changed: removing pre of spec@\n%a@." (Specs.pp_spec Pp.text None) old_spec ;
      current_specs := SpecMap.remove old_spec.Specs.pre !current_specs )
    else ()
  in
  let add_spec spec =
    (* add a new spec by doing union of the posts *)
    try
      let old_post, old_visited = SpecMap.find spec.Specs.pre !current_specs in
      let new_post, new_visited =
        ( Paths.PathSet.union old_post (Paths.PathSet.from_renamed_list spec.Specs.posts)
        , Specs.Visitedset.union old_visited spec.Specs.visited )
      in
      if not (Paths.PathSet.equal old_post new_post) then (
        changed := true ;
        L.(debug Analysis Medium)
          "Specs changed: added new post@\n%a@."
          (Propset.pp Pp.text (Specs.Jprop.to_prop spec.Specs.pre))
          (Paths.PathSet.to_propset tenv new_post) ;
        current_specs
        := SpecMap.add spec.Specs.pre (new_post, new_visited)
             (SpecMap.remove spec.Specs.pre !current_specs) )
    with Not_found ->
      changed := true ;
      L.(debug Analysis Medium)
        "Specs changed: added new pre@\n%a@." (Specs.Jprop.pp_short Pp.text) spec.Specs.pre ;
      current_specs
      := SpecMap.add spec.Specs.pre
           (Paths.PathSet.from_renamed_list spec.Specs.posts, spec.Specs.visited) !current_specs
  in
  let res = ref [] in
  let convert pre (post_set, visited) =
    res
    := Specs.spec_normalize tenv
         {Specs.pre= pre; Specs.posts= Paths.PathSet.elements post_set; Specs.visited= visited}
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
  let normal_specs = List.map ~f:(Specs.spec_normalize tenv) specs in
  let new_specs, _ = update_specs tenv prev_summary phase normal_specs in
  let symops = prev_summary.Specs.stats.Specs.symops + SymOp.get_total () in
  let stats_failure =
    match res with None -> prev_summary.Specs.stats.Specs.stats_failure | Some _ -> res
  in
  let stats = {prev_summary.Specs.stats with symops; stats_failure} in
  let preposts =
    match phase with
    | Specs.FOOTPRINT
     -> Some new_specs
    | Specs.RE_EXECUTION
     -> Some (List.map ~f:(Specs.NormSpec.erase_join_info_pre tenv) new_specs)
  in
  let payload = {prev_summary.Specs.payload with Specs.preposts= preposts} in
  {prev_summary with Specs.phase= phase; stats; payload}

(** Analyze the procedure and return the resulting summary. *)
let analyze_proc tenv proc_cfg : Specs.summary =
  let proc_desc = ProcCfg.Exceptional.proc_desc proc_cfg in
  let proc_name = Procdesc.get_proc_name proc_desc in
  reset_global_values proc_desc ;
  let summary = Specs.get_summary_unsafe "analyze_proc" proc_name in
  let go, get_results = perform_analysis_phase tenv summary proc_cfg in
  let res = Timeout.exe_timeout go () in
  let specs, phase = get_results () in
  let updated_summary = update_summary tenv summary specs phase res in
  if Config.curr_language_is Config.Clang && Config.report_custom_error then
    report_custom_errors tenv updated_summary ;
  if Config.curr_language_is Config.Java && Config.tracing then
    report_runtime_exceptions tenv proc_desc updated_summary ;
  updated_summary

(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe tenv proc_name joined_pres =
  L.(debug Analysis Medium) "Transition %a from footprint to re-exe@." Typ.Procname.pp proc_name ;
  let summary = Specs.get_summary_unsafe "transition_footprint_re_exe" proc_name in
  let summary' =
    if Config.only_footprint then {summary with Specs.phase= Specs.RE_EXECUTION}
    else
      let specs =
        List.map
          ~f:(fun jp ->
            Specs.spec_normalize tenv {Specs.pre= jp; posts= []; visited= Specs.Visitedset.empty})
          joined_pres
      in
      let payload = {summary.Specs.payload with Specs.preposts= Some specs} in
      {summary with Specs.phase= Specs.RE_EXECUTION; payload}
  in
  Specs.add_summary proc_name summary'

(** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for
    the procedures enabled after the analysis of [proc_name] *)
let perform_transition proc_cfg tenv proc_name =
  let transition summary =
    (* disable exceptions for leaks and protect against any other errors *)
    let joined_pres =
      let allow_leak = !Config.allow_leak in
      (* apply the start node to f, and do nothing in case of exception *)
      let apply_start_node f =
        try f (ProcCfg.Exceptional.start_node proc_cfg)
        with exn when SymOp.exn_not_failure exn -> ()
      in
      apply_start_node (do_before_node 0) ;
      try
        Config.allow_leak := true ;
        let res = collect_preconditions tenv summary in
        Config.allow_leak := allow_leak ;
        apply_start_node do_after_node ;
        res
      with exn when SymOp.exn_not_failure exn ->
        apply_start_node do_after_node ;
        Config.allow_leak := allow_leak ;
        L.(debug Analysis Medium)
          "Error in collect_preconditions for %a@." Typ.Procname.pp proc_name ;
        let err_name, _, ml_loc_opt, _, _, _, _ = Exceptions.recognize_exception exn in
        let err_str = "exception raised " ^ Localise.to_issue_id err_name in
        L.(debug Analysis Medium) "Error: %s %a@." err_str L.pp_ml_loc_opt ml_loc_opt ; []
    in
    transition_footprint_re_exe tenv proc_name joined_pres
  in
  match Specs.get_summary proc_name with
  | Some summary when Specs.equal_phase (Specs.get_phase summary) Specs.FOOTPRINT
   -> transition summary
  | _
   -> ()

(* Create closures for the interprocedural algorithm *)
let interprocedural_algorithm_closures ~prepare_proc exe_env : Tasks.closure list =
  let call_graph = Exe_env.get_cg exe_env in
  let process_one_proc proc_name =
    prepare_proc proc_name ;
    let analyze proc_desc = ignore (Ondemand.analyze_proc_desc proc_desc proc_desc) in
    match Exe_env.get_proc_desc exe_env proc_name with
    | Some proc_desc
      when Config.reactive_mode
           (* in reactive mode, only analyze changed procedures *)
           && (Procdesc.get_attributes proc_desc).ProcAttributes.changed
     -> analyze proc_desc
    | Some proc_desc
     -> analyze proc_desc
    | None
     -> ()
  in
  let procs_to_analyze = Cg.get_defined_nodes call_graph in
  let create_closure proc_name () = process_one_proc proc_name in
  List.map ~f:create_closure procs_to_analyze

let analyze_procedure_aux cg_opt tenv proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
  if not (Procdesc.did_preanalysis proc_desc) then (
    Preanal.do_liveness proc_desc tenv ;
    Preanal.do_abstraction proc_desc ;
    Option.iter cg_opt ~f:(fun cg -> Preanal.do_dynamic_dispatch proc_desc cg tenv) ) ;
  let summaryfp = Config.run_in_footprint_mode (analyze_proc tenv) proc_cfg in
  Specs.add_summary proc_name summaryfp ;
  perform_transition proc_cfg tenv proc_name ;
  let summaryre = Config.run_in_re_execution_mode (analyze_proc tenv) proc_cfg in
  Specs.add_summary proc_name summaryre ; summaryre

let analyze_procedure {Callbacks.summary; proc_desc; tenv} : Specs.summary =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Specs.add_summary proc_name summary ;
  ignore (analyze_procedure_aux None tenv proc_desc) ;
  Specs.get_summary_unsafe __FILE__ proc_name

(** Create closures to perform the analysis of an exe_env *)
let do_analysis_closures exe_env : Tasks.closure list =
  let get_calls caller_pdesc =
    let calls = ref [] in
    let f (callee_pname, loc) = calls := (callee_pname, loc) :: !calls in
    Procdesc.iter_calls f caller_pdesc ;
    List.rev !calls
  in
  let init_proc pname =
    let pdesc =
      match Exe_env.get_proc_desc exe_env pname with Some pdesc -> pdesc | None -> assert false
    in
    let nodes = List.map ~f:(fun n -> Procdesc.Node.get_id n) (Procdesc.get_nodes pdesc) in
    let proc_flags = Procdesc.get_flags pdesc in
    let static_err_log = Procdesc.get_err_log pdesc in
    (* err log from translation *)
    let calls = get_calls pdesc in
    let attributes =
      {(Procdesc.get_attributes pdesc) with ProcAttributes.err_log= static_err_log}
    in
    let proc_desc_option = if Config.dynamic_dispatch = `Lazy then Some pdesc else None in
    ignore (Specs.init_summary (nodes, proc_flags, calls, attributes, proc_desc_option))
  in
  let callbacks =
    let get_proc_desc proc_name =
      match Exe_env.get_proc_desc exe_env proc_name with
      | Some pdesc
       -> Some pdesc
      | None when Config.dynamic_dispatch = `Lazy
       -> Option.bind (Specs.get_summary proc_name) ~f:(fun summary ->
              summary.Specs.proc_desc_option )
      | None
       -> None
    in
    let analyze_ondemand _ proc_desc =
      let proc_name = Procdesc.get_proc_name proc_desc in
      let tenv = Exe_env.get_tenv exe_env proc_name in
      let cg = Exe_env.get_cg exe_env in
      analyze_procedure_aux (Some cg) tenv proc_desc
    in
    {Ondemand.analyze_ondemand= analyze_ondemand; get_proc_desc}
  in
  let prepare_proc pn =
    let should_init = Config.models_mode || is_none (Specs.get_summary pn) in
    if should_init then init_proc pn
  in
  let closures =
    List.map
      ~f:(fun closure () ->
        Ondemand.set_callbacks callbacks ; closure () ; Ondemand.unset_callbacks ())
      (interprocedural_algorithm_closures ~prepare_proc exe_env)
  in
  closures

let visited_and_total_nodes ~filter cfg =
  let filter_node pdesc n =
    Procdesc.is_defined pdesc && filter pdesc
    &&
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Stmt_node _
    | Procdesc.Node.Prune_node _
    | Procdesc.Node.Start_node _
    | Procdesc.Node.Exit_node _
     -> true
    | Procdesc.Node.Skip_node _ | Procdesc.Node.Join_node
     -> false
  in
  let counted_nodes, visited_nodes_re =
    let set = ref Procdesc.NodeSet.empty in
    let set_visited_re = ref Procdesc.NodeSet.empty in
    let add pdesc n =
      if filter_node pdesc n then (
        set := Procdesc.NodeSet.add n !set ;
        if snd (Printer.node_is_visited n) then set_visited_re
          := Procdesc.NodeSet.add n !set_visited_re )
    in
    Cfg.iter_all_nodes add cfg ; (!set, !set_visited_re)
  in
  (Procdesc.NodeSet.elements visited_nodes_re, Procdesc.NodeSet.elements counted_nodes)

(** Print the stats for the given cfg.
    Consider every defined proc unless a proc with the same name
    was defined in another module, and was the one which was analyzed *)
let print_stats_cfg proc_shadowed source cfg =
  let err_table = Errlog.create_err_table () in
  let filter pdesc =
    match Specs.get_summary (Procdesc.get_proc_name pdesc) with
    | None
     -> false
    | Some summary
     -> Specs.get_specs_from_payload summary <> []
  in
  let nodes_visited, nodes_total = visited_and_total_nodes ~filter cfg in
  let num_proc = ref 0 in
  let num_nospec_noerror_proc = ref 0 in
  let num_spec_noerror_proc = ref 0 in
  let num_nospec_error_proc = ref 0 in
  let num_spec_error_proc = ref 0 in
  let tot_specs = ref 0 in
  let tot_symops = ref 0 in
  let num_timeout = ref 0 in
  let compute_stats_proc proc_desc =
    let proc_name = Procdesc.get_proc_name proc_desc in
    match Specs.get_summary proc_name with
    | None
     -> ()
    | Some _ when proc_shadowed proc_desc
     -> L.(debug Analysis Medium)
          "print_stats: ignoring function %a which is also defined in another file@."
          Typ.Procname.pp proc_name
    | Some summary
     -> let stats = summary.Specs.stats in
        let err_log = summary.Specs.attributes.ProcAttributes.err_log in
        incr num_proc ;
        let specs = Specs.get_specs_from_payload summary in
        tot_specs := List.length specs + !tot_specs ;
        let () =
          match
            ( specs
            , Errlog.size
                (fun ekind in_footprint ->
                  Exceptions.equal_err_kind ekind Exceptions.Kerror && in_footprint)
                err_log )
          with
          | [], 0
           -> incr num_nospec_noerror_proc
          | _, 0
           -> incr num_spec_noerror_proc
          | [], _
           -> incr num_nospec_error_proc
          | _, _
           -> incr num_spec_error_proc
        in
        tot_symops := !tot_symops + stats.Specs.symops ;
        if Option.is_some stats.Specs.stats_failure then incr num_timeout ;
        Errlog.extend_table err_table err_log
  in
  let print_file_stats fmt () =
    let num_errors = Errlog.err_table_size_footprint Exceptions.Kerror err_table in
    let num_warnings = Errlog.err_table_size_footprint Exceptions.Kwarning err_table in
    let num_infos = Errlog.err_table_size_footprint Exceptions.Kinfo err_table in
    let num_ok_proc = !num_spec_noerror_proc + !num_spec_error_proc in
    (* F.fprintf fmt "VISITED: %a@\n" (pp_seq pp_node) nodes_visited;
       F.fprintf fmt "TOTAL: %a@\n" (pp_seq pp_node) nodes_total; *)
    F.fprintf fmt "@\n++++++++++++++++++++++++++++++++++++++++++++++++++@\n" ;
    F.fprintf fmt "+ FILE: %a  VISITED: %d/%d SYMOPS: %d@\n" SourceFile.pp source
      (List.length nodes_visited) (List.length nodes_total) !tot_symops ;
    F.fprintf fmt "+  num_procs: %d (%d ok, %d timeouts, %d errors, %d warnings, %d infos)@\n"
      !num_proc num_ok_proc !num_timeout num_errors num_warnings num_infos ;
    F.fprintf fmt "+  detail procs:@\n" ;
    F.fprintf fmt "+    - No Errors and No Specs: %d@\n" !num_nospec_noerror_proc ;
    F.fprintf fmt "+    - Some Errors and No Specs: %d@\n" !num_nospec_error_proc ;
    F.fprintf fmt "+    - No Errors and Some Specs: %d@\n" !num_spec_noerror_proc ;
    F.fprintf fmt "+    - Some Errors and Some Specs: %d@\n" !num_spec_error_proc ;
    F.fprintf fmt "+  errors: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kerror) err_table ;
    F.fprintf fmt "+  warnings: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kwarning) err_table ;
    F.fprintf fmt "+  infos: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kinfo) err_table ;
    F.fprintf fmt "+  specs: %d@\n" !tot_specs ;
    F.fprintf fmt "++++++++++++++++++++++++++++++++++++++++++++++++++@\n" ;
    Errlog.print_err_table_details fmt err_table
  in
  let save_file_stats () =
    let source_dir = DB.source_dir_from_source_file source in
    let stats_file = DB.source_dir_get_internal_file source_dir ".stats" in
    try
      let outc = Out_channel.create (DB.filename_to_string stats_file) in
      let fmt = F.formatter_of_out_channel outc in
      print_file_stats fmt () ; Out_channel.close outc
    with Sys_error _ -> ()
  in
  List.iter ~f:compute_stats_proc (Cfg.get_defined_procs cfg) ;
  L.(debug Analysis Medium) "%a" print_file_stats () ;
  save_file_stats ()

(** Print the stats for all the files in the cluster *)
let print_stats cluster =
  let exe_env = Exe_env.from_cluster cluster in
  Exe_env.iter_files
    (fun source cfg ->
      let proc_shadowed proc_desc =
        (* return true if a proc with the same name in another module was analyzed instead *)
        let proc_name = Procdesc.get_proc_name proc_desc in
        Exe_env.get_source exe_env proc_name <> Some source
      in
      print_stats_cfg proc_shadowed source cfg)
    exe_env
