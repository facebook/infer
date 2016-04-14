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

(** Interprocedural Analysis *)

module L = Logging
module F = Format

(** A node with a number of visits *)
type visitednode =
  {
    node: Cfg.Node.t;
    visits: int;
  }

(** Set of nodes with number of visits *)
module NodeVisitSet =
  Set.Make(struct
    type t = visitednode
    let compare_ids n1 n2 =
      (* higher id is better *)
      Cfg.Node.compare n2 n1
    let compare_distance_to_exit { node = n1 } { node = n2 } =
      (* smaller means higher priority *)
      let n =
        match Cfg.Node.get_distance_to_exit n1, Cfg.Node.get_distance_to_exit n2 with
        | None, None ->
            0
        | None, Some _ ->
            1
        | Some _, None ->
            - 1
        | Some d1, Some d2 ->
            (* shorter distance to exit is better *)
            int_compare d1 d2 in
      if n <> 0 then n else compare_ids n1 n2
    let compare_number_of_visits x1 x2 =
      let n = int_compare x1.visits x2.visits in (* visited fewer times is better *)
      if n <> 0 then n else compare_distance_to_exit x1 x2
    let compare x1 x2 =
      if !Config.footprint then
        match !Config.worklist_mode with
        | 0 -> compare_ids x1.node x2.node
        | 1 -> compare_distance_to_exit x1 x2
        | _ -> compare_number_of_visits x1 x2
      else compare_ids x1.node x2.node
  end)

(** Table for the results of the join operation on nodes. *)
module Join_table : sig
  type t

  val add : t -> Cfg.Node.id -> Paths.PathSet.t -> unit
  val create : unit -> t
  val find : t -> Cfg.Node.id -> Paths.PathSet.t
end = struct
  type t = (Cfg.Node.id, Paths.PathSet.t) Hashtbl.t

  let create () : t =
    Hashtbl.create 11

  let find table i =
    try Hashtbl.find table i with
    | Not_found -> Paths.PathSet.empty

  let add table i dset =
    Hashtbl.replace table i dset
end

(* =============== START of module Worklist =============== *)
module Worklist = struct
  module NodeMap = Map.Make(Cfg.Node)

  type t = {
    join_table : Join_table.t; (** Table of join results *)
    path_set_todo : (Cfg.Node.id, Paths.PathSet.t) Hashtbl.t; (** Pathset todo *)
    path_set_visited : (Cfg.Node.id, Paths.PathSet.t) Hashtbl.t; (** Pathset visited *)
    mutable todo_set : NodeVisitSet.t; (** Set of nodes still to do, with visit count *)
    mutable visit_map : int NodeMap.t; (** Map from nodes done to visit count *)
  }

  let create () = {
    join_table = Join_table.create ();
    path_set_todo = Hashtbl.create 11;
    path_set_visited = Hashtbl.create 11;
    todo_set = NodeVisitSet.empty;
    visit_map = NodeMap.empty;
  }

  let is_empty (wl : t) : bool =
    NodeVisitSet.is_empty wl.todo_set

  let add (wl : t) (node : Cfg.node) : unit =
    let visits = (* recover visit count if it was visited before *)
      try NodeMap.find node wl.visit_map with
      | Not_found -> 0 in
    wl.todo_set <- NodeVisitSet.add { node; visits } wl.todo_set

  (** remove the minimum element from the worklist, and increase its number of visits *)
  let remove (wl : t) : Cfg.Node.t =
    try
      let min = NodeVisitSet.min_elt wl.todo_set in
      wl.todo_set <-
        NodeVisitSet.remove min wl.todo_set;
      wl.visit_map <-
        NodeMap.add min.node (min.visits + 1) wl.visit_map; (* increase the visits *)
      min.node
    with Not_found -> begin
        L.out "@\n...Work list is empty! Impossible to remove edge...@\n";
        assert false
      end
end
(* =============== END of module Worklist =============== *)


let path_set_create_worklist pdesc =
  State.reset ();
  Cfg.Procdesc.compute_distance_to_exit_node pdesc;
  Worklist.create ()

let htable_retrieve (htable : (Cfg.Node.id, Paths.PathSet.t) Hashtbl.t) (key : Cfg.Node.id)
  : Paths.PathSet.t =
  try
    Hashtbl.find htable key
  with Not_found ->
    Hashtbl.replace htable key Paths.PathSet.empty;
    Paths.PathSet.empty

(** Add [d] to the pathset todo at [node] returning true if changed *)
let path_set_put_todo (wl : Worklist.t) (node: Cfg.node) (d: Paths.PathSet.t) : bool =
  let changed =
    if Paths.PathSet.is_empty d then false
    else
      let node_id = Cfg.Node.get_id node in
      let old_todo = htable_retrieve wl.Worklist.path_set_todo node_id in
      let old_visited = htable_retrieve wl.Worklist.path_set_visited node_id in
      let d' = Paths.PathSet.diff d old_visited in (* differential fixpoint *)
      let todo_new = Paths.PathSet.union old_todo d' in
      Hashtbl.replace wl.Worklist.path_set_todo node_id todo_new;
      not (Paths.PathSet.equal old_todo todo_new) in
  changed

let path_set_checkout_todo (wl : Worklist.t) (node: Cfg.node) : Paths.PathSet.t =
  try
    let node_id = Cfg.Node.get_id node in
    let todo = Hashtbl.find wl.Worklist.path_set_todo node_id in
    Hashtbl.replace wl.Worklist.path_set_todo node_id Paths.PathSet.empty;
    let visited = Hashtbl.find wl.Worklist.path_set_visited node_id in
    let new_visited = Paths.PathSet.union visited todo in
    Hashtbl.replace wl.Worklist.path_set_visited node_id new_visited;
    todo
  with Not_found ->
    L.out "@.@.ERROR: could not find todo for node %a@.@." Cfg.Node.pp node;
    assert false

(* =============== END of the edge_set object =============== *)

let collect_do_abstract_pre pname tenv (pset : Propset.t) : Propset.t =
  if !Config.footprint
  then
    run_in_re_execution_mode
      (Abs.lifted_abstract pname tenv)
      pset
  else
    Abs.lifted_abstract pname tenv pset

let collect_do_abstract_post pname tenv (pathset : Paths.PathSet.t) : Paths.PathSet.t =
  let abs_option p =
    if Prover.check_inconsistency p then None
    else Some (Abs.abstract pname tenv p) in
  if !Config.footprint
  then
    run_in_re_execution_mode
      (Paths.PathSet.map_option abs_option)
      pathset
  else
    Paths.PathSet.map_option abs_option pathset

let do_join_pre plist =
  Dom.proplist_collapse_pre plist

let do_join_post pname tenv (pset: Paths.PathSet.t) =
  if !Config.spec_abs_level <= 0 then
    Dom.pathset_collapse pset
  else
    Dom.pathset_collapse (Dom.pathset_collapse_impl pname tenv pset)

let do_meet_pre pset =
  if !Config.meet_level > 0 then
    Dom.propset_meet_generate_pre pset
  else
    Propset.to_proplist pset

(** Find the preconditions in the current spec table,
    apply meet then join, and return the joined preconditions *)
let collect_preconditions tenv proc_name : Prop.normal Specs.Jprop.t list =
  let collect_do_abstract_one tenv prop =
    if !Config.footprint
    then
      run_in_re_execution_mode
        (Abs.abstract_no_symop tenv)
        prop
    else
      Abs.abstract_no_symop tenv prop in
  let pres =
    IList.map
      (fun spec -> Specs.Jprop.to_prop spec.Specs.pre)
      (Specs.get_specs proc_name) in
  let pset = Propset.from_proplist pres in
  let pset' =
    let f p = Prop.prop_normal_vars_to_primed_vars p in
    Propset.map f pset in

  L.d_strln ("#### Extracted footprint of " ^ Procname.to_string proc_name ^ ":  ####");
  L.d_increase_indent 1; Propset.d Prop.prop_emp pset'; L.d_decrease_indent 1; L.d_ln ();
  L.d_ln ();
  let pset'' = collect_do_abstract_pre proc_name tenv pset' in
  let plist_meet = do_meet_pre pset'' in
  L.d_strln ("#### Footprint of " ^ Procname.to_string proc_name ^ " after Meet  ####");
  L.d_increase_indent 1; Propgraph.d_proplist Prop.prop_emp plist_meet;
  L.d_decrease_indent 1; L.d_ln ();
  L.d_ln ();
  L.d_increase_indent 2; (* Indent for the join output *)
  let jplist = do_join_pre plist_meet in
  L.d_decrease_indent 2; L.d_ln ();
  L.d_strln ("#### Footprint of " ^ Procname.to_string proc_name ^ " after Join  ####");
  L.d_increase_indent 1; Specs.Jprop.d_list false jplist; L.d_decrease_indent 1; L.d_ln ();
  let jplist' = IList.map (Specs.Jprop.map Prop.prop_rename_primed_footprint_vars) jplist in
  L.d_strln ("#### Renamed footprint of " ^ Procname.to_string proc_name ^ ":  ####");
  L.d_increase_indent 1; Specs.Jprop.d_list false jplist'; L.d_decrease_indent 1; L.d_ln ();
  let jplist'' =
    let f p =
      Prop.prop_primed_vars_to_normal_vars
        (collect_do_abstract_one proc_name tenv p) in
    IList.map (Specs.Jprop.map f) jplist' in
  L.d_strln ("#### Abstracted footprint of " ^ Procname.to_string proc_name ^ ":  ####");
  L.d_increase_indent 1; Specs.Jprop.d_list false jplist''; L.d_decrease_indent 1; L.d_ln();
  jplist''

(* =============== START of symbolic execution =============== *)

(** propagate a set of results to the given node *)
let propagate
    (wl : Worklist.t) pname is_exception (pset: Paths.PathSet.t) (curr_node: Cfg.node) =
  let edgeset_todo =
    (** prop must be a renamed prop by the invariant preserved by PropSet *)
    let f prop path edgeset_curr =
      let exn_opt =
        if is_exception
        then Some (Tabulation.prop_get_exn_name pname prop)
        else None in
      Paths.PathSet.add_renamed_prop
        prop
        (Paths.Path.extend curr_node exn_opt (State.get_session ()) path)
        edgeset_curr in
    Paths.PathSet.fold f pset Paths.PathSet.empty in
  let changed = path_set_put_todo wl curr_node edgeset_todo in
  if changed then
    Worklist.add wl curr_node

(** propagate a set of results, including exceptions and divergence *)
let propagate_nodes_divergence
    tenv (pdesc: Cfg.Procdesc.t) (pset: Paths.PathSet.t)
    (succ_nodes: Cfg.node list) (exn_nodes: Cfg.node list) (wl : Worklist.t) =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let pset_exn, pset_ok = Paths.PathSet.partition (Tabulation.prop_is_exn pname) pset in
  if !Config.footprint && not (Paths.PathSet.is_empty (State.get_diverging_states_node ())) then
    begin
      Errdesc.warning_err (State.get_loc ()) "Propagating Divergence@.";
      let exit_node = Cfg.Procdesc.get_exit_node pdesc in
      let diverging_states = State.get_diverging_states_node () in
      let prop_incons =
        let mk_incons prop =
          let p_abs = Abs.abstract pname tenv prop in
          let p_zero = Prop.replace_sigma [] (Prop.replace_sub Sil.sub_empty p_abs) in
          Prop.normalize (Prop.replace_pi [Sil.Aneq (Sil.exp_zero, Sil.exp_zero)] p_zero) in
        Paths.PathSet.map mk_incons diverging_states in
      (L.d_strln_color Orange) "Propagating Divergence -- diverging states:";
      Propgraph.d_proplist Prop.prop_emp (Paths.PathSet.to_proplist prop_incons); L.d_ln ();
      propagate wl pname false prop_incons exit_node
    end;
  IList.iter (propagate wl pname false pset_ok) succ_nodes;
  IList.iter (propagate wl pname true pset_exn) exn_nodes

(* ===================== END of symbolic execution ===================== *)

(* =============== START of forward_tabulate =============== *)

(** Symbolic execution for a Join node *)
let do_symexec_join pname tenv wl curr_node (edgeset_todo : Paths.PathSet.t) =
  let curr_pdesc = Cfg.Node.get_proc_desc curr_node in
  let curr_pname = Cfg.Procdesc.get_proc_name curr_pdesc in
  let curr_node_id = Cfg.Node.get_id curr_node in
  let succ_nodes = Cfg.Node.get_succs curr_node in
  let new_dset = edgeset_todo in
  let old_dset = Join_table.find wl.Worklist.join_table curr_node_id in
  let old_dset', new_dset' = Dom.pathset_join curr_pname tenv old_dset new_dset in
  Join_table.add wl.Worklist.join_table curr_node_id (Paths.PathSet.union old_dset' new_dset');
  IList.iter (fun node ->
      Paths.PathSet.iter (fun prop path ->
          State.set_path path None;
          propagate wl pname false (Paths.PathSet.from_renamed_list [(prop, path)]) node)
        new_dset') succ_nodes

let prop_max_size = ref (0, Prop.prop_emp)
let prop_max_chain_size = ref (0, Prop.prop_emp)

(* Check if the prop exceeds the current max *)
let check_prop_size p _ =
  let size = Prop.Metrics.prop_size p in
  if size > fst !prop_max_size then
    (prop_max_size := (size, p);
     L.d_strln ("Prop with new max size " ^ string_of_int size ^ ":");
     Prop.d_prop p;
     L.d_ln ())

(* Check prop size and filter out possible unabstracted lists *)
let check_prop_size edgeset_todo =
  if !Config.monitor_prop_size then Paths.PathSet.iter check_prop_size edgeset_todo

let reset_prop_metrics () =
  prop_max_size := (0, Prop.prop_emp);
  prop_max_chain_size := (0, Prop.prop_emp)

exception RE_EXE_ERROR

let do_before_node session node =
  let loc = Cfg.Node.get_loc node in
  let proc_desc = Cfg.Node.get_proc_desc node in
  let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
  State.set_node node;
  State.set_session session;
  L.reset_delayed_prints ();
  Printer.node_start_session node loc proc_name (session :> int)

let do_after_node node =
  Printer.node_finish_session node

(** Return the list of normal ids occurring in the instructions *)
let instrs_get_normal_vars instrs =
  let fav = Sil.fav_new () in
  let do_instr instr =
    let do_e e = Sil.exp_fav_add fav e in
    let exps = Sil.instr_get_exps instr in
    IList.iter do_e exps in
  IList.iter do_instr instrs;
  Sil.fav_filter_ident fav Ident.is_normal;
  Sil.fav_to_list fav

(* checks that boolean conditions on a conditional are assignment *)
(* The check is done as follows: we check that the successors or a node that make an *)
(* set instruction are prune nodes, they are all at the same location and the condition on*)
(* which they prune is the variable (or the negation) which was set in the set instruction *)
(* we exclude function calls: if (g(x,y)) ....*)
(* we check that prune nodes have simple guards: a var or its negation*)
let check_assignement_guard node =
  let pdesc = Cfg.Node.get_proc_desc node in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let verbose = false in
  let node_contains_call n =
    let instrs = Cfg.Node.get_instrs n in
    let is_call = function
      | Sil.Call _ -> true
      | _ -> false in
    IList.exists is_call instrs in
  let is_set_instr i =
    match i with
    | Sil.Set _ -> true
    | _ -> false in
  let is_prune_instr i =
    match i with
    | Sil.Prune _ -> true
    | _ -> false in
  let is_letderef_instr i =
    match i with
    | Sil.Letderef _ -> true
    | _ -> false in
  let is_cil_tmp e =
    match e with
    | Sil.Lvar pv ->
        Errdesc.pvar_is_cil_tmp pv
    | _ -> false in
  let succs = Cfg.Node.get_succs node in
  let l_node = Cfg.Node.get_last_loc node in
  (* e is prune if in all successors prune nodes we have for some temp n$1: *)
  (* n$1=*&e;Prune(n$1) or n$1=*&e;Prune(!n$1) *)
  let is_prune_exp e =
    let prune_var n =
      let ins = Cfg.Node.get_instrs n in
      let pi = IList.filter is_prune_instr ins in
      let leti = IList.filter is_letderef_instr ins in
      match pi, leti with
      | [Sil.Prune (Sil.Var(e1), _, _, _)], [Sil.Letderef(e2, e', _, _)]
      | [Sil.Prune (Sil.UnOp(Sil.LNot, Sil.Var(e1), _), _, _, _)],
        [Sil.Letderef(e2, e', _, _)]
        when (Ident.equal e1 e2) ->
          if verbose
          then
            L.d_strln ("Found " ^ (Sil.exp_to_string e') ^ " as prune var");
          [e']
      | _ -> [] in
    let prune_vars = IList.flatten(IList.map (fun n -> prune_var n) succs) in
    IList.for_all (fun e' -> Sil.exp_equal e' e) prune_vars in
  let succs_loc = IList.map (fun n -> Cfg.Node.get_loc n) succs in
  let succs_are_all_prune_nodes () =
    IList.for_all (fun n -> match Cfg.Node.get_kind n with
        | Cfg.Node.Prune_node(_) -> true
        | _ -> false) succs in
  let succs_same_loc_as_node () =
    if verbose then
      (L.d_str ("LOCATION NODE: line: " ^ (string_of_int l_node.Location.line) ^
                " nLOC: " ^ (string_of_int l_node.Location.nLOC));
       L.d_strln " ");
    IList.for_all (fun l ->
        if verbose then
          (L.d_str ("LOCATION l: line: " ^ (string_of_int l.Location.line) ^
                    " nLOC: " ^ (string_of_int l.Location.nLOC));
           L.d_strln " ");
        Location.equal l l_node) succs_loc in
  (* check that the guards of the succs are a var or its negation *)
  let succs_have_simple_guards () =
    let check_instr = function
      | Sil.Prune (Sil.Var _, _, _, _) -> true
      | Sil.Prune (Sil.UnOp(Sil.LNot, Sil.Var _, _), _, _, _) -> true
      | Sil.Prune _ -> false
      | _ -> true in
    let check_guard n =
      IList.for_all check_instr (Cfg.Node.get_instrs n) in
    IList.for_all check_guard succs in
  if !Config.curr_language = Config.C_CPP &&
     succs_are_all_prune_nodes () &&
     succs_same_loc_as_node () &&
     succs_have_simple_guards () then
    (let instr = Cfg.Node.get_instrs node in
     match succs_loc with
     (* at this point all successors are at the same location, so we can take the first*)
     | loc_succ:: _ ->
         let set_instr_at_succs_loc =
           IList.filter
             (fun i ->
                Location.equal (Sil.instr_get_loc i) loc_succ &&
                is_set_instr i)
             instr in
         (match set_instr_at_succs_loc with
          | [Sil.Set(e, _, _, _)] ->
              (* we now check if e is the same expression used to prune*)
              if (is_prune_exp e) && not ((node_contains_call node) && (is_cil_tmp e)) then (
                let desc = Errdesc.explain_condition_is_assignment l_node in
                let exn = Exceptions.Condition_is_assignment (desc, __POS__) in
                let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
                Reporting.log_warning pname ~loc: (Some l_node) ~pre: pre_opt exn
              )
              else ()
          | _ ->
              ())
     | _ ->
         if verbose then L.d_strln "NOT FOUND loc_succ"
    ) else ()

(** Perform symbolic execution for a node starting from an initial prop *)
let do_symbolic_execution handle_exn tenv
    (node : Cfg.node) (prop: Prop.normal Prop.t) (path : Paths.Path.t) =
  let pdesc = Cfg.Node.get_proc_desc node in
  State.mark_execution_start node;
  (* build the const map lazily *)
  State.set_const_map (ConstantPropagation.build_const_map tenv pdesc);
  check_assignement_guard node;
  let instrs = Cfg.Node.get_instrs node in
  (* fresh normal vars must be fresh w.r.t. instructions *)
  Ident.update_name_generator (instrs_get_normal_vars instrs);
  let pset = SymExec.node handle_exn tenv node (Paths.PathSet.from_renamed_list [(prop, path)]) in
  L.d_strln ".... After Symbolic Execution ....";
  Propset.d prop (Paths.PathSet.to_propset pset);
  L.d_ln (); L.d_ln();
  State.mark_execution_end node;
  pset

let mark_visited summary node =
  let node_id = Cfg.Node.get_id node in
  let stats = summary.Specs.stats in
  if !Config.footprint
  then
    stats.Specs.nodes_visited_fp <- IntSet.add (node_id :> int) stats.Specs.nodes_visited_fp
  else
    stats.Specs.nodes_visited_re <- IntSet.add (node_id :> int) stats.Specs.nodes_visited_re

let forward_tabulate tenv wl =
  let handled_some_exception = ref false in
  let handle_exn curr_node exn =
    let curr_pdesc = Cfg.Node.get_proc_desc curr_node in
    let curr_pname = Cfg.Procdesc.get_proc_name curr_pdesc in
    Exceptions.print_exception_html "Failure of symbolic execution: " exn;
    let pre_opt = (* precondition leading to error, if any *)
      State.get_normalized_pre (Abs.abstract_no_symop curr_pname) in
    (match pre_opt with
     | Some pre ->
         L.d_strln "Precondition:"; Prop.d_prop pre; L.d_ln ()
     | None -> ());
    L.d_strln "SIL INSTR:";
    Cfg.Node.d_instrs ~sub_instrs: true (State.get_instr ()) curr_node; L.d_ln ();
    Reporting.log_error ~pre: pre_opt curr_pname exn;
    State.mark_instr_fail pre_opt exn;
    handled_some_exception := true in

  while not (Worklist.is_empty wl) do
    let curr_node = Worklist.remove wl in
    let curr_node_kind = Cfg.Node.get_kind curr_node in
    let curr_node_id = Cfg.Node.get_id curr_node in
    let proc_desc = Cfg.Node.get_proc_desc curr_node in
    let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
    let formal_params = Cfg.Procdesc.get_formals proc_desc in
    let summary = Specs.get_summary_unsafe "forward_tabulate" proc_name in
    mark_visited summary curr_node; (* mark nodes visited in fp and re phases *)
    let session =
      incr summary.Specs.sessions;
      !(summary.Specs.sessions) in
    do_before_node session curr_node;
    let pathset_todo = path_set_checkout_todo wl curr_node in
    let succ_nodes = Cfg.Node.get_succs curr_node in
    let exn_nodes = Cfg.Node.get_exn curr_node in
    let exe_iter f pathset =
      let ps_size = Paths.PathSet.size pathset in
      let cnt = ref 0 in
      let exe prop path =
        State.set_path path None;
        incr cnt;
        f prop path !cnt ps_size in
      Paths.PathSet.iter exe pathset in
    let log_string proc_name =
      let phase_string =
        if Specs.get_phase proc_name == Specs.FOOTPRINT then "FP" else "RE" in
      let summary = Specs.get_summary_unsafe "forward_tabulate" proc_name in
      let timestamp = Specs.get_timestamp summary in
      F.sprintf "[%s:%d] %s" phase_string timestamp (Procname.to_string proc_name) in
    let doit () =
      handled_some_exception := false;
      check_prop_size pathset_todo;
      L.d_strln ("**** " ^ (log_string proc_name) ^ " " ^
                 "Node: " ^ string_of_int (curr_node_id :> int) ^ ", " ^
                 "Procedure: " ^ Procname.to_string proc_name ^ ", " ^
                 "Session: " ^ string_of_int session ^ ", " ^
                 "Todo: " ^ string_of_int (Paths.PathSet.size pathset_todo) ^ " ****");
      L.d_increase_indent 1;
      Propset.d Prop.prop_emp (Paths.PathSet.to_propset pathset_todo);
      L.d_strln ".... Instructions: .... ";
      Cfg.Node.d_instrs ~sub_instrs: true (State.get_instr ()) curr_node;
      L.d_ln (); L.d_ln ();

      match curr_node_kind with
      | Cfg.Node.Join_node ->
          do_symexec_join proc_name tenv wl curr_node pathset_todo
      | Cfg.Node.Stmt_node _
      | Cfg.Node.Prune_node _
      | Cfg.Node.Exit_node _
      | Cfg.Node.Skip_node _
      | Cfg.Node.Start_node _ ->
          exe_iter
            (fun prop path cnt num_paths ->
               try
                 let prop = if !Config.taint_analysis then
                     let tainted_params = Taint.tainted_params proc_name in
                     Tabulation.add_param_taint proc_name formal_params prop tainted_params
                   else prop in
                 L.d_strln
                   ("Processing prop " ^ string_of_int cnt ^ "/" ^ string_of_int num_paths);
                 L.d_increase_indent 1;
                 State.reset_diverging_states_node ();
                 let pset =
                   do_symbolic_execution (handle_exn curr_node) tenv curr_node prop path in
                 L.d_decrease_indent 1; L.d_ln();
                 propagate_nodes_divergence tenv proc_desc pset succ_nodes exn_nodes wl;
               with
               | exn when Exceptions.handle_exception exn && !Config.footprint ->
                   handle_exn curr_node exn;
                   if !Config.nonstop then
                     propagate_nodes_divergence
                       tenv proc_desc (Paths.PathSet.from_renamed_list [(prop, path)])
                       succ_nodes exn_nodes wl;
                   L.d_decrease_indent 1; L.d_ln ())
            pathset_todo in
    try
      begin
        doit();
        if !handled_some_exception then Printer.force_delayed_prints ();
        do_after_node curr_node
      end
    with
    | exn when Exceptions.handle_exception exn ->
        handle_exn curr_node exn;
        Printer.force_delayed_prints ();
        do_after_node curr_node;
        if not !Config.footprint then raise RE_EXE_ERROR
  done;
  L.d_strln ".... Work list empty. Stop ...."; L.d_ln ()


(** report an error if any Context is reachable from a static field *)
let report_context_leaks pname sigma tenv =
  (* report an error if an expression in [context_exps] is reachable from [field_strexp] *)
  let check_reachable_context_from_fld (fld_name, fld_strexp) context_exps =
    let fld_exps = Prop.strexp_get_exps fld_strexp in
    let reachable_hpreds, reachable_exps =
      Prop.compute_reachable_hpreds sigma fld_exps in
    (* raise an error if any Context expression is in [reachable_exps] *)
    IList.iter
      (fun (context_exp, typ) ->
         if Sil.ExpSet.mem context_exp reachable_exps then
           let leak_path =
             match Prop.get_fld_typ_path_opt fld_exps context_exp reachable_hpreds with
             | Some path -> path
             | None -> assert false in (* a path must exist in order for a leak to be reported *)
           let err_desc = Errdesc.explain_context_leak pname typ fld_name leak_path in
           let exn = Exceptions.Context_leak (err_desc, __POS__) in
           Reporting.log_error pname exn)
      context_exps in
  (* get the set of pointed-to expressions of type T <: Context *)
  let context_exps =
    IList.fold_left
      (fun exps hpred -> match hpred with
         | Sil.Hpointsto (_, Sil.Eexp (exp, _), Sil.Sizeof (Sil.Tptr (typ, _), _))
           when AndroidFramework.is_context typ tenv
             && not (AndroidFramework.is_application typ tenv) ->
             (exp, typ) :: exps
         | _ -> exps)
      []
      sigma in
  IList.iter
    (function
      | Sil.Hpointsto (Sil.Lvar pv, Sil.Estruct (static_flds, _), _)
        when Pvar.is_global pv ->
          IList.iter
            (fun (f_name, f_strexp) ->
               check_reachable_context_from_fld (f_name, f_strexp) context_exps)
            static_flds
      | _ -> ())
    sigma

(** Remove locals and formals,
    and check if the address of a stack variable is left in the result *)
let remove_locals_formals_and_check pdesc p =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let pvars, p' = Cfg.remove_locals_formals pdesc p in
  let check_pvar pvar =
    let loc = Cfg.Node.get_loc (Cfg.Procdesc.get_exit_node pdesc) in
    let dexp_opt, _ = Errdesc.vpath_find p (Sil.Lvar pvar) in
    let desc = Errdesc.explain_stack_variable_address_escape loc pvar dexp_opt in
    let exn = Exceptions.Stack_variable_address_escape (desc, __POS__) in
    Reporting.log_warning pname exn in
  IList.iter check_pvar pvars;
  p'

(** Collect the analysis results for the exit node. *)
let collect_analysis_result wl pdesc : Paths.PathSet.t =
  let exit_node = Cfg.Procdesc.get_exit_node pdesc in
  let exit_node_id = Cfg.Node.get_id exit_node in
  let pathset = htable_retrieve wl.Worklist.path_set_visited exit_node_id in
  Paths.PathSet.map (remove_locals_formals_and_check pdesc) pathset

module Pmap = Map.Make
    (struct
      type t = Prop.normal Prop.t
      let compare = Prop.prop_compare
    end)

let vset_ref_add_path vset_ref path =
  Paths.Path.iter_all_nodes_nocalls
    (fun n -> vset_ref := Cfg.NodeSet.add n !vset_ref)
    path

let vset_ref_add_pathset vset_ref pathset =
  Paths.PathSet.iter (fun _ path -> vset_ref_add_path vset_ref path) pathset

let compute_visited vset =
  let res = ref Specs.Visitedset.empty in
  let node_get_all_lines n =
    let node_loc = Cfg.Node.get_loc n in
    let instrs_loc = IList.map Sil.instr_get_loc (Cfg.Node.get_instrs n) in
    let lines = IList.map (fun loc -> loc.Location.line) (node_loc :: instrs_loc) in
    IList.remove_duplicates int_compare (IList.sort int_compare lines) in
  let do_node n =
    res :=
      Specs.Visitedset.add (Cfg.Node.get_id n, node_get_all_lines n) !res in
  Cfg.NodeSet.iter do_node vset;
  !res

(** Extract specs from a pathset *)
let extract_specs tenv pdesc pathset : Prop.normal Specs.spec list =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let sub =
    let fav = Sil.fav_new () in
    Paths.PathSet.iter
      (fun prop _ -> Prop.prop_fav_add fav prop)
      pathset;
    let sub_list =
      IList.map
        (fun id -> (id, Sil.Var (Ident.create_fresh (Ident.knormal))))
        (Sil.fav_to_list fav) in
    Sil.sub_of_list sub_list in
  let pre_post_visited_list =
    let pplist = Paths.PathSet.elements pathset in
    let f (prop, path) =
      let _, prop' = Cfg.remove_locals_formals pdesc prop in
      let prop'' = Abs.abstract pname tenv prop' in
      let pre, post = Prop.extract_spec prop'' in
      let pre' = Prop.normalize (Prop.prop_sub sub pre) in
      if !Config.curr_language =
         Config.Java && Cfg.Procdesc.get_access pdesc <> Sil.Private then
        report_context_leaks pname (Prop.get_sigma post) tenv;
      let post' =
        if Prover.check_inconsistency_base prop then None
        else Some (Prop.normalize (Prop.prop_sub sub post), path) in
      let visited =
        let vset_ref = ref Cfg.NodeSet.empty in
        vset_ref_add_path vset_ref path;
        compute_visited !vset_ref in
      (pre', post', visited) in
    IList.map f pplist in
  let pre_post_map =
    let add map (pre, post, visited) =
      let current_posts, current_visited =
        try Pmap.find pre map
        with Not_found ->
          (Paths.PathSet.empty, Specs.Visitedset.empty) in
      let new_posts = match post with
        | None -> current_posts
        | Some (post, path) -> Paths.PathSet.add_renamed_prop post path current_posts in
      let new_visited = Specs.Visitedset.union visited current_visited in
      Pmap.add pre (new_posts, new_visited) map in
    IList.fold_left add Pmap.empty pre_post_visited_list in
  let specs = ref [] in
  let add_spec pre ((posts : Paths.PathSet.t), visited) =
    let posts' =
      IList.map
        (fun (p, path) -> (Cfg.remove_seed_vars p, path))
        (Paths.PathSet.elements (do_join_post pname tenv posts)) in
    let spec =
      { Specs.pre = Specs.Jprop.Prop (1, pre);
        Specs.posts = posts';
        Specs.visited = visited } in
    specs := spec :: !specs in
  Pmap.iter add_spec pre_post_map;
  !specs

let collect_postconditions wl tenv pdesc : Paths.PathSet.t * Specs.Visitedset.t =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let pathset = collect_analysis_result wl pdesc in
  L.d_strln
    ("#### [FUNCTION " ^ Procname.to_string pname ^ "] Analysis result ####");
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset pathset);
  L.d_ln ();
  let res =
    try
      let pathset = collect_do_abstract_post pname tenv pathset in
      let pathset_diverging = State.get_diverging_states_proc () in
      let visited =
        let vset_ref = ref Cfg.NodeSet.empty in
        vset_ref_add_pathset vset_ref pathset;
        (* nodes from diverging states were also visited *)
        vset_ref_add_pathset vset_ref pathset_diverging;
        compute_visited !vset_ref in
      do_join_post pname tenv pathset, visited with
    | exn when (match exn with Exceptions.Leak _ -> true | _ -> false) ->
        raise (Failure "Leak in post collecion") in
  L.d_strln
    ("#### [FUNCTION " ^ Procname.to_string pname ^ "] Postconditions after join ####");
  L.d_increase_indent 1;
  Propset.d Prop.prop_emp (Paths.PathSet.to_propset (fst res));
  L.d_decrease_indent 1;
  L.d_ln ();
  res

let create_seed_vars sigma =
  let hpred_add_seed sigma = function
    | Sil.Hpointsto (Sil.Lvar pv, se, typ) when not (Pvar.is_abducted pv) ->
        Sil.Hpointsto(Sil.Lvar (Pvar.to_seed pv), se, typ) :: sigma
    | _ -> sigma in
  IList.fold_left hpred_add_seed [] sigma

(** Initialize proposition for execution given formal and global
    parameters. The footprint is initialized according to the
    execution mode. The prop is not necessarily emp, so it
    should be incorporated when the footprint is constructed. *)
let prop_init_formals_seed tenv new_formals (prop : 'a Prop.t) : Prop.exposed Prop.t =
  let sigma_new_formals =
    let do_formal (pv, typ) =
      let texp = match !Config.curr_language with
        | Config.C_CPP -> Sil.Sizeof (typ, Sil.Subtype.exact)
        | Config.Java -> Sil.Sizeof (typ, Sil.Subtype.subtypes) in
      Prop.mk_ptsto_lvar (Some tenv) Prop.Fld_init Sil.inst_formal (pv, texp, None) in
    IList.map do_formal new_formals in
  let sigma_seed =
    create_seed_vars
      (* formals already there plus new ones *)
      (Prop.get_sigma prop @ sigma_new_formals) in
  let sigma = sigma_seed @ sigma_new_formals in
  let new_pi =
    Prop.get_pi prop in
  let prop' =
    Prop.replace_pi new_pi (Prop.prop_sigma_star prop sigma) in
  Prop.replace_sigma_footprint
    (Prop.get_sigma_footprint prop' @ sigma_new_formals)
    prop'

(** Construct an initial prop by extending [prop] with locals, and formals if [add_formals] is true
    as well as seed variables *)
let initial_prop
    tenv (curr_f: Cfg.Procdesc.t) (prop : 'a Prop.t) add_formals
  : Prop.normal Prop.t =
  let construct_decl (x, typ) =
    (Pvar.mk x (Cfg.Procdesc.get_proc_name curr_f), typ) in
  let new_formals =
    if add_formals
    then IList.map construct_decl (Cfg.Procdesc.get_formals curr_f)
    else [] in (** no new formals added *)
  let prop1 =
    Prop.prop_reset_inst
      (fun inst_old -> Sil.update_inst inst_old Sil.inst_formal)
      prop in
  let prop2 =
    prop_init_formals_seed tenv new_formals prop1 in
  Prop.prop_rename_primed_footprint_vars (Prop.normalize prop2)

(** Construct an initial prop from the empty prop *)
let initial_prop_from_emp tenv curr_f =
  initial_prop tenv curr_f Prop.prop_emp true

(** Construct an initial prop from an existing pre with formals *)
let initial_prop_from_pre tenv curr_f pre =
  if !Config.footprint then
    let vars = Sil.fav_to_list (Prop.prop_fav pre) in
    let sub_list =
      IList.map
        (fun id -> (id, Sil.Var (Ident.create_fresh (Ident.kfootprint))))
        vars in
    let sub = Sil.sub_of_list sub_list in
    let pre2 = Prop.prop_sub sub pre in
    let pre3 =
      Prop.replace_sigma_footprint
        (Prop.get_sigma pre2)
        (Prop.replace_pi_footprint (Prop.get_pure pre2) pre2) in
    initial_prop tenv curr_f pre3 false
  else
    initial_prop tenv curr_f pre false

(** Re-execute one precondition and return some spec if there was no re-execution error. *)
let execute_filter_prop wl tenv pdesc init_node (precondition : Prop.normal Specs.Jprop.t)
  : Prop.normal Specs.spec option =
  let proc_name = Cfg.Procdesc.get_proc_name pdesc in
  do_before_node 0 init_node;
  L.d_strln ("#### Start: RE-execution for " ^ Procname.to_string proc_name ^ " ####");
  L.d_indent 1;
  L.d_strln "Precond:"; Specs.Jprop.d_shallow precondition;
  L.d_ln (); L.d_ln ();
  let init_prop =
    initial_prop_from_pre tenv pdesc (Specs.Jprop.to_prop precondition) in
  let init_edgeset =
    Paths.PathSet.add_renamed_prop
      init_prop
      (Paths.Path.start init_node)
      Paths.PathSet.empty in
  do_after_node init_node;
  try
    Worklist.add wl init_node;
    ignore (path_set_put_todo wl init_node init_edgeset);
    forward_tabulate tenv wl;
    do_before_node 0 init_node;
    L.d_strln_color Green ("#### Finished: RE-execution for " ^ Procname.to_string proc_name ^ " ####");
    L.d_increase_indent 1;
    L.d_strln "Precond:"; Prop.d_prop (Specs.Jprop.to_prop precondition);
    L.d_ln ();
    let posts, visited =
      let pset, visited = collect_postconditions wl tenv pdesc in
      let plist =
        IList.map
          (fun (p, path) -> (Cfg.remove_seed_vars p, path))
          (Paths.PathSet.elements pset) in
      plist, visited in
    let pre =
      let p = Cfg.remove_locals_ret pdesc (Specs.Jprop.to_prop precondition) in
      match precondition with
      | Specs.Jprop.Prop (n, _) -> Specs.Jprop.Prop (n, p)
      | Specs.Jprop.Joined (n, _, jp1, jp2) -> Specs.Jprop.Joined (n, p, jp1, jp2) in
    let spec = { Specs.pre = pre; Specs.posts = posts; Specs.visited = visited } in
    L.d_decrease_indent 1;
    do_after_node init_node;
    Some spec
  with RE_EXE_ERROR ->
    do_before_node 0 init_node;
    Printer.force_delayed_prints ();
    L.d_strln_color Red ("#### [FUNCTION " ^ Procname.to_string proc_name ^ "] ...ERROR");
    L.d_increase_indent 1;
    L.d_strln "when starting from pre:";
    Prop.d_prop (Specs.Jprop.to_prop precondition);
    L.d_strln "This precondition is filtered out.";
    L.d_decrease_indent 1;
    do_after_node init_node;
    None

(** get all the nodes in the current call graph with their defined children *)
let get_procs_and_defined_children call_graph =
  IList.map
    (fun (n, ns) ->
       (n, Procname.Set.elements ns))
    (Cg.get_nodes_and_defined_children call_graph)

let pp_intra_stats wl proc_desc fmt _ =
  let nstates = ref 0 in
  let nodes = Cfg.Procdesc.get_nodes proc_desc in
  IList.iter
    (fun node ->
       nstates :=
         !nstates +
         Paths.PathSet.size
           (htable_retrieve wl.Worklist.path_set_visited (Cfg.Node.get_id node)))
    nodes;
  F.fprintf fmt "(%d nodes containing %d states)" (IList.length nodes) !nstates

(** Return functions to perform one phase of the analysis for a procedure.
    Given [proc_name], return [do, get_results] where [go ()] performs the analysis phase
    and [get_results ()] returns the results computed.
    This function is architected so that [get_results ()] can be called even after
    [go ()] was interrupted by and exception. *)
let perform_analysis_phase tenv (pname : Procname.t) (pdesc : Cfg.Procdesc.t)
  : (unit -> unit) * (unit -> Prop.normal Specs.spec list * Specs.phase) =
  let start_node = Cfg.Procdesc.get_start_node pdesc in

  let check_recursion_level () =
    let summary = Specs.get_summary_unsafe "check_recursion_level" pname in
    let recursion_level = Specs.get_timestamp summary in
    if recursion_level > !Config.max_recursion then
      begin
        L.err "Reached maximum level of recursion, raising a Timeout@.";
        raise (SymOp.Analysis_failure_exe (FKrecursion_timeout recursion_level))
      end in

  let compute_footprint : (unit -> unit) * (unit -> Prop.normal Specs.spec list * Specs.phase) =
    let go (wl : Worklist.t) () =
      let init_prop = initial_prop_from_emp tenv pdesc in
      (* use existing pre's (in recursion some might exist) as starting points *)
      let init_props_from_pres =
        let specs = Specs.get_specs pname in
        (* rename spec vars to footrpint vars, and copy current to footprint *)
        let mk_init precondition =
          initial_prop_from_pre tenv pdesc (Specs.Jprop.to_prop precondition) in
        IList.map (fun spec -> mk_init spec.Specs.pre) specs in
      let init_props = Propset.from_proplist (init_prop :: init_props_from_pres) in
      let init_edgeset =
        let add pset prop =
          Paths.PathSet.add_renamed_prop prop (Paths.Path.start start_node) pset in
        Propset.fold add Paths.PathSet.empty init_props in
      L.out "@.#### Start: Footprint Computation for %a ####@." Procname.pp pname;
      L.d_increase_indent 1;
      L.d_strln "initial props =";
      Propset.d Prop.prop_emp init_props; L.d_ln (); L.d_ln();
      L.d_decrease_indent 1;
      check_recursion_level ();
      Worklist.add wl start_node;
      Config.arc_mode :=
        Hashtbl.mem
          (Cfg.Procdesc.get_flags pdesc)
          Mleak_buckets.objc_arc_flag;
      ignore (path_set_put_todo wl start_node init_edgeset);
      forward_tabulate tenv wl in
    let get_results (wl : Worklist.t) () =
      State.process_execution_failures Reporting.log_warning pname;
      let results = collect_analysis_result wl pdesc in
      L.out "#### [FUNCTION %a] ... OK #####@\n" Procname.pp pname;
      L.out "#### Finished: Footprint Computation for %a %a ####@."
        Procname.pp pname
        (pp_intra_stats wl pdesc) pname;
      L.out "#### [FUNCTION %a] Footprint Analysis result ####@\n%a@."
        Procname.pp pname
        (Paths.PathSet.pp pe_text) results;
      let specs = try extract_specs tenv pdesc results with
        | Exceptions.Leak _ ->
            let exn =
              Exceptions.Internal_error
                (Localise.verbatim_desc "Leak_while_collecting_specs_after_footprint") in
            let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
            Reporting.log_error pname ~pre: pre_opt exn;
            [] (* retuning no specs *) in
      specs, Specs.FOOTPRINT in
    let wl = path_set_create_worklist pdesc in
    go wl, get_results wl in

  let re_execution proc_name
    : (unit -> unit) * (unit -> Prop.normal Specs.spec list * Specs.phase) =
    let candidate_preconditions =
      IList.map
        (fun spec -> spec.Specs.pre)
        (Specs.get_specs proc_name) in
    let valid_specs = ref [] in
    let go () =
      L.out "@.#### Start: Re-Execution for %a ####@." Procname.pp proc_name;
      check_recursion_level ();
      let filter p =
        let wl = path_set_create_worklist pdesc in
        let speco = execute_filter_prop wl tenv pdesc start_node p in
        let is_valid = match speco with
          | None -> false
          | Some spec ->
              valid_specs := !valid_specs @ [spec];
              true in
        let outcome = if is_valid then "pass" else "fail" in
        L.out "Finished re-execution for precondition %d %a (%s)@."
          (Specs.Jprop.to_number p)
          (pp_intra_stats wl pdesc) proc_name
          outcome;
        speco in
      if !Config.undo_join then
        ignore (Specs.Jprop.filter filter candidate_preconditions)
      else
        ignore (IList.map filter candidate_preconditions) in
    let get_results () =
      let specs = !valid_specs in
      L.out "#### [FUNCTION %a] ... OK #####@\n" Procname.pp proc_name;
      L.out "#### Finished: Re-Execution for %a ####@." Procname.pp proc_name;
      let valid_preconditions =
        IList.map (fun spec -> spec.Specs.pre) specs in
      let filename =
        DB.Results_dir.path_to_filename
          DB.Results_dir.Abs_source_dir
          [(Procname.to_filename proc_name)] in
      if !Config.write_dotty then
        Dotty.pp_speclist_dotty_file filename specs;
      L.out "@.@.================================================";
      L.out "@. *** CANDIDATE PRECONDITIONS FOR %a: " Procname.pp proc_name;
      L.out "@.================================================@.";
      L.out "@.%a @.@." (Specs.Jprop.pp_list pe_text false) candidate_preconditions;
      L.out "@.@.================================================";
      L.out "@. *** VALID PRECONDITIONS FOR %a: " Procname.pp proc_name;
      L.out "@.================================================@.";
      L.out "@.%a @.@." (Specs.Jprop.pp_list pe_text true) valid_preconditions;
      specs, Specs.RE_EXECUTION in
    go, get_results in

  match Specs.get_phase pname with
  | Specs.FOOTPRINT ->
      compute_footprint
  | Specs.RE_EXECUTION ->
      re_execution pname

let set_current_language proc_desc =
  let language = (Cfg.Procdesc.get_attributes proc_desc).ProcAttributes.language in
  Config.curr_language := language

(** reset global values before analysing a procedure *)
let reset_global_values proc_desc =
  Config.reset_abs_val ();
  Ident.NameGenerator.reset ();
  SymOp.reset_total ();
  reset_prop_metrics ();
  Abs.reset_current_rules ();
  set_current_language proc_desc

(* Collect all pairs of the kind (precondition, runtime exception) from a summary *)
let exception_preconditions tenv pname summary =
  let collect_exceptions pre exns (prop, _) =
    if Tabulation.prop_is_exn pname prop then
      let exn_name = Tabulation.prop_get_exn_name pname prop in
      if AndroidFramework.is_runtime_exception tenv exn_name then
        (pre, exn_name):: exns
      else exns
    else exns in
  let collect_spec errors spec =
    IList.fold_left (collect_exceptions spec.Specs.pre) errors spec.Specs.posts in
  IList.fold_left collect_spec [] (Specs.get_specs_from_payload summary)

(* Collect all pairs of the kind (precondition, custom error) from a summary *)
let custom_error_preconditions summary =
  let collect_errors pre errors (prop, _) =
    match Tabulation.lookup_custom_errors prop with
    | None -> errors
    | Some e -> (pre, e) :: errors in
  let collect_spec errors spec =
    IList.fold_left (collect_errors spec.Specs.pre) errors spec.Specs.posts in
  IList.fold_left collect_spec [] (Specs.get_specs_from_payload summary)


(* Remove the constrain of the form this != null which is true for all Java virtual calls *)
let remove_this_not_null prop =
  let collect_hpred (var_option, hpreds) = function
    | Sil.Hpointsto (Sil.Lvar pvar, Sil.Eexp (Sil.Var var, _), _)
      when !Config.curr_language = Config.Java && Pvar.is_this pvar ->
        (Some var, hpreds)
    | hpred -> (var_option, hpred:: hpreds) in
  let collect_atom var atoms = function
    | Sil.Aneq (Sil.Var v, e)
      when Ident.equal v var && Sil.exp_equal e Sil.exp_null -> atoms
    | a -> a:: atoms in
  match IList.fold_left collect_hpred (None, []) (Prop.get_sigma prop) with
  | None, _ -> prop
  | Some var, filtered_hpreds ->
      let filtered_atoms =
        IList.fold_left (collect_atom var) [] (Prop.get_pi prop) in
      let prop' = Prop.replace_pi filtered_atoms Prop.prop_emp in
      let prop'' = Prop.replace_sigma filtered_hpreds prop' in
      Prop.normalize prop''


(** Is true when the precondition does not contain constrains that can be false at call site.
    This means that the post-conditions associated with this precondition cannot be prevented
    by the calling context. *)
let is_unavoidable pre =
  let prop = remove_this_not_null (Specs.Jprop.to_prop pre) in
  match Prop.CategorizePreconditions.categorize [prop] with
  | Prop.CategorizePreconditions.NoPres
  | Prop.CategorizePreconditions.Empty -> true
  | _ -> false


(** Detects if there are specs of the form {precondition} proc {runtime exception} and report
    an error in that case, generating the trace that lead to the runtime exception if the method is
    called in the context { precondition } *)
let report_runtime_exceptions tenv pdesc summary =
  let pname = Specs.get_proc_name summary in
  let is_public_method =
    (Specs.get_attributes summary).ProcAttributes.access = Sil.Public in
  let is_main =
    is_public_method
    &&
    (match pname with
     | Procname.Java pname_java ->
         Procname.java_is_static pname
         && (Procname.java_get_method pname_java) = "main"
     | _ ->
         false) in
  let is_annotated =
    let proc_attributes = Specs.pdesc_resolve_attributes pdesc in
    let annotated_signature = Annotations.get_annotated_signature proc_attributes in
    let ret_annotation, _ = annotated_signature.Annotations.ret in
    Annotations.ia_is_verify ret_annotation in
  let should_report pre =
    is_main || is_annotated || is_unavoidable pre in
  let report (pre, runtime_exception) =
    if should_report pre then
      let pre_str =
        pp_to_string (Prop.pp_prop pe_text) (Specs.Jprop.to_prop pre) in
      let exn_desc = Localise.java_unchecked_exn_desc pname runtime_exception pre_str in
      let exn = Exceptions.Java_runtime_exception (runtime_exception, pre_str, exn_desc) in
      Reporting.log_error pname ~pre: (Some (Specs.Jprop.to_prop pre)) exn in
  IList.iter report (exception_preconditions tenv pname summary)


let report_custom_errors summary =
  let pname = Specs.get_proc_name summary in
  let report (pre, custom_error) =
    if is_unavoidable pre then
      let loc = summary.Specs.attributes.ProcAttributes.loc in
      let err_desc = Localise.desc_custom_error loc in
      let exn = Exceptions.Custom_error (custom_error, err_desc) in
      Reporting.log_error pname ~pre: (Some (Specs.Jprop.to_prop pre)) exn in
  IList.iter report (custom_error_preconditions summary)

module SpecMap = Map.Make (struct
    type t = Prop.normal Specs.Jprop.t
    let compare = Specs.Jprop.compare
  end)

(** Update the specs of the current proc after the execution of one phase *)
let update_specs proc_name phase (new_specs : Specs.NormSpec.t list)
  : Specs.NormSpec.t list * bool =
  let new_specs = Specs.normalized_specs_to_specs new_specs in
  let old_specs = Specs.get_specs proc_name in
  let changed = ref false in
  let current_specs =
    ref
      (IList.fold_left
         (fun map spec ->
            SpecMap.add
              spec.Specs.pre
              (Paths.PathSet.from_renamed_list spec.Specs.posts, spec.Specs.visited) map)
         SpecMap.empty old_specs) in
  let re_exe_filter old_spec = (* filter out pres which failed re-exe *)
    if phase == Specs.RE_EXECUTION &&
       not (IList.exists
              (fun new_spec -> Specs.Jprop.equal new_spec.Specs.pre old_spec.Specs.pre)
              new_specs)
    then begin
      changed:= true;
      L.out "Specs changed: removing pre of spec@\n%a@."
        (Specs.pp_spec pe_text None) old_spec;
      current_specs := SpecMap.remove old_spec.Specs.pre !current_specs end
    else () in
  let add_spec spec = (* add a new spec by doing union of the posts *)
    try
      let old_post, old_visited = SpecMap.find spec.Specs.pre !current_specs in
      let new_post, new_visited =
        Paths.PathSet.union
          old_post
          (Paths.PathSet.from_renamed_list spec.Specs.posts),
        Specs.Visitedset.union old_visited spec.Specs.visited in
      if not (Paths.PathSet.equal old_post new_post) then begin
        changed := true;
        L.out "Specs changed: added new post@\n%a@."
          (Propset.pp pe_text (Specs.Jprop.to_prop spec.Specs.pre))
          (Paths.PathSet.to_propset new_post);
        current_specs :=
          SpecMap.add spec.Specs.pre (new_post, new_visited)
            (SpecMap.remove spec.Specs.pre !current_specs) end

    with Not_found ->
      changed := true;
      L.out "Specs changed: added new pre@\n%a@."
        (Specs.Jprop.pp_short pe_text) spec.Specs.pre;
      current_specs :=
        SpecMap.add
          spec.Specs.pre
          ((Paths.PathSet.from_renamed_list spec.Specs.posts), spec.Specs.visited)
          !current_specs in
  let res = ref [] in
  let convert pre (post_set, visited) =
    res :=
      Specs.spec_normalize
        { Specs.pre = pre;
          Specs.posts = Paths.PathSet.elements post_set;
          Specs.visited = visited }:: !res in
  IList.iter re_exe_filter old_specs; (* filter out pre's which failed re-exe *)
  IList.iter add_spec new_specs; (* add new specs *)
  SpecMap.iter convert !current_specs;
  !res,!changed

(** update a summary after analysing a procedure *)
let update_summary prev_summary specs phase proc_name elapsed res =
  let normal_specs = IList.map Specs.spec_normalize specs in
  let new_specs, changed = update_specs proc_name phase normal_specs in
  let timestamp = max 1 (prev_summary.Specs.timestamp + if changed then 1 else 0) in
  let stats_time = prev_summary.Specs.stats.Specs.stats_time +. elapsed in
  let symops = prev_summary.Specs.stats.Specs.symops + SymOp.get_total () in
  let stats_failure = match res with
    | None -> prev_summary.Specs.stats.Specs.stats_failure
    | Some _ -> res in
  let stats =
    { prev_summary.Specs.stats with
      Specs.stats_time;
      symops;
      stats_failure;
    } in
  let payload =
    { prev_summary.Specs.payload with
      Specs.preposts = Some new_specs;
    } in
  { prev_summary with
    Specs.phase;
    stats;
    payload;
    timestamp;
  }


(** Analyze the procedure and return the resulting summary. *)
let analyze_proc exe_env proc_desc : Specs.summary =
  let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
  let init_time = Unix.gettimeofday () in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  reset_global_values proc_desc;
  let go, get_results = perform_analysis_phase tenv proc_name proc_desc in
  let res = Timeout.exe_timeout go () in
  let specs, phase = get_results () in
  let elapsed = Unix.gettimeofday () -. init_time in
  let prev_summary = Specs.get_summary_unsafe "analyze_proc" proc_name in
  let updated_summary =
    update_summary prev_summary specs phase proc_name elapsed res in
  if !Config.curr_language == Config.C_CPP && Config.report_custom_error then
    report_custom_errors updated_summary;
  if !Config.curr_language == Config.Java && !Config.report_runtime_exceptions then
    report_runtime_exceptions tenv proc_desc updated_summary;
  updated_summary

(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe proc_name joined_pres =
  L.out "Transition %a from footprint to re-exe@." Procname.pp proc_name;
  let summary = Specs.get_summary_unsafe "transition_footprint_re_exe" proc_name in
  let summary' =
    if !Config.only_footprint then
      { summary with
        Specs.phase = Specs.RE_EXECUTION;
      }
    else
      let specs =
        IList.map
          (fun jp ->
             Specs.spec_normalize
               { Specs.pre = jp;
                 posts = [];
                 visited = Specs.Visitedset.empty })
          joined_pres in
      let payload =
        { summary.Specs.payload with
          Specs.preposts = Some specs; } in
      let dependency_map =
        Specs.re_initialize_dependency_map summary.Specs.dependency_map in
      { summary with
        Specs.timestamp = 0;
        phase = Specs.RE_EXECUTION;
        dependency_map;
        payload;
      } in
  Specs.add_summary proc_name summary'

(** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for
    the procedures enabled after the analysis of [proc_name] *)
let perform_transition exe_env tenv proc_name =
  let transition () =
    (* disable exceptions for leaks and protect against any other errors *)
    let joined_pres =
      let allowleak = !Config.allowleak in
      (* apply the start node to f, and do nothing in case of exception *)
      let apply_start_node f =
        try
          match Exe_env.get_proc_desc exe_env proc_name with
          | Some pdesc ->
              let start_node = Cfg.Procdesc.get_start_node pdesc in
              f start_node
          | None -> ()
        with exn when SymOp.exn_not_failure exn -> () in
      apply_start_node (do_before_node 0);
      try
        Config.allowleak := true;
        let res = collect_preconditions tenv proc_name in
        Config.allowleak := allowleak;
        apply_start_node do_after_node;
        res
      with exn when SymOp.exn_not_failure exn ->
        apply_start_node do_after_node;
        Config.allowleak := allowleak;
        L.err "Error in collect_preconditions for %a@." Procname.pp proc_name;
        let err_name, _, ml_loc_opt, _, _, _, _ = Exceptions.recognize_exception exn in
        let err_str = "exception raised " ^ (Localise.to_string err_name) in
        L.err "Error: %s %a@." err_str L.pp_ml_loc_opt ml_loc_opt;
        [] in
    transition_footprint_re_exe proc_name joined_pres in
  if Specs.get_phase proc_name == Specs.FOOTPRINT
  then transition ()


let interprocedural_algorithm exe_env : unit =
  let call_graph = Exe_env.get_cg exe_env in
  let filter_initial proc_name =
    let summary = Specs.get_summary_unsafe "main_algorithm" proc_name in
    Specs.get_timestamp summary = 0 in
  let procs_to_analyze =
    IList.filter filter_initial (Cg.get_defined_nodes call_graph) in
  let to_analyze proc_name =
    match Exe_env.get_proc_desc exe_env proc_name with
    | Some proc_desc ->
        let reactive_changed =
          if !Config.reactive_mode
          then (Cfg.Procdesc.get_attributes proc_desc).ProcAttributes.changed
          else true in
        if
          reactive_changed && (* in reactive mode, only analyze changed procedures *)
          Ondemand.procedure_should_be_analyzed proc_name
        then
          Some proc_desc
        else
          None
    | None ->
        None in
  let process_one_proc proc_name =
    match to_analyze proc_name with
    | Some pdesc ->
        Ondemand.analyze_proc_name ~propagate_exceptions:false pdesc proc_name
    | None ->
        () in
  IList.iter process_one_proc procs_to_analyze

(** Perform the analysis of an exe_env *)
let do_analysis exe_env =
  let cg = Exe_env.get_cg exe_env in
  let procs_and_defined_children = get_procs_and_defined_children cg in
  let get_calls caller_pdesc =
    let calls = ref [] in
    let f (callee_pname, loc) = calls := (callee_pname, loc) :: !calls in
    Cfg.Procdesc.iter_calls f caller_pdesc;
    IList.rev !calls in
  let init_proc (pname, dep) =
    let pdesc = match Exe_env.get_proc_desc exe_env pname with
      | Some pdesc ->
          pdesc
      | None ->
          assert false in
    let nodes = IList.map (fun n -> Cfg.Node.get_id n) (Cfg.Procdesc.get_nodes pdesc) in
    let proc_flags = Cfg.Procdesc.get_flags pdesc in
    let static_err_log = Cfg.Procdesc.get_err_log pdesc in (** err log from translation *)
    let calls = get_calls pdesc in
    let attributes =
      { (Cfg.Procdesc.get_attributes pdesc) with
        ProcAttributes.err_log = static_err_log; } in
    Specs.init_summary (dep, nodes, proc_flags, calls, None, attributes) in

  IList.iter
    (fun ((pn, _) as x) ->
       let should_init () =
         Config.analyze_models ||
         Specs.get_summary pn = None in
       if should_init ()
       then init_proc x)
    procs_and_defined_children;

  let callbacks =
    let get_proc_desc proc_name =
      Exe_env.get_proc_desc exe_env proc_name in
    let analyze_ondemand proc_desc =
      let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
      let tenv = Exe_env.get_tenv exe_env proc_name in
      let summaryfp =
        run_in_footprint_mode (analyze_proc exe_env) proc_desc in
      Specs.add_summary proc_name summaryfp;

      perform_transition exe_env tenv proc_name;

      let summaryre =
        run_in_re_execution_mode (analyze_proc exe_env) proc_desc in
      Specs.add_summary proc_name summaryre in
    {
      Ondemand.analyze_ondemand;
      get_proc_desc;
    } in

  Ondemand.set_callbacks callbacks;
  interprocedural_algorithm exe_env;
  Ondemand.unset_callbacks ()


let visited_and_total_nodes cfg =
  let all_nodes =
    let add s n = Cfg.NodeSet.add n s in
    IList.fold_left add Cfg.NodeSet.empty (Cfg.Node.get_all_nodes cfg) in
  let filter_node n =
    Cfg.Procdesc.is_defined (Cfg.Node.get_proc_desc n) &&
    match Cfg.Node.get_kind n with
    | Cfg.Node.Stmt_node _ | Cfg.Node.Prune_node _
    | Cfg.Node.Start_node _ | Cfg.Node.Exit_node _ -> true
    | Cfg.Node.Skip_node _ | Cfg.Node.Join_node -> false in
  let counted_nodes = Cfg.NodeSet.filter filter_node all_nodes in
  let visited_nodes_re =
    Cfg.NodeSet.filter
      (fun node -> snd (Printer.node_is_visited node))
      counted_nodes in
  Cfg.NodeSet.elements visited_nodes_re, Cfg.NodeSet.elements counted_nodes

(** Print the stats for the given cfg.
    Consider every defined proc unless a proc with the same name
    was defined in another module, and was the one which was analyzed *)
let print_stats_cfg proc_shadowed cfg =
  let err_table = Errlog.create_err_table () in
  let nvisited, ntotal = visited_and_total_nodes cfg in
  let node_filter n =
    let node_procname = Cfg.Procdesc.get_proc_name (Cfg.Node.get_proc_desc n) in
    Specs.summary_exists node_procname && Specs.get_specs node_procname != [] in
  let nodes_visited = IList.filter node_filter nvisited in
  let nodes_total = IList.filter node_filter ntotal in
  let num_proc = ref 0 in
  let num_nospec_noerror_proc = ref 0 in
  let num_spec_noerror_proc = ref 0 in
  let num_nospec_error_proc = ref 0 in
  let num_spec_error_proc = ref 0 in
  let tot_specs = ref 0 in
  let tot_symops = ref 0 in
  let num_timeout = ref 0 in
  let compute_stats_proc proc_desc =
    let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
    if proc_shadowed proc_desc ||
       Specs.get_summary proc_name = None then
      L.out "print_stats: ignoring function %a which is also defined in another file@."
        Procname.pp proc_name
    else
      let summary = Specs.get_summary_unsafe "print_stats_cfg" proc_name in
      let stats = summary.Specs.stats in
      let err_log = summary.Specs.attributes.ProcAttributes.err_log in
      incr num_proc;
      let specs = Specs.get_specs_from_payload summary in
      tot_specs := (IList.length specs) + !tot_specs;
      let () =
        match specs,
              Errlog.size
                (fun ekind in_footprint -> ekind = Exceptions.Kerror && in_footprint)
                err_log with
        | [], 0 -> incr num_nospec_noerror_proc
        | _, 0 -> incr num_spec_noerror_proc
        | [], _ -> incr num_nospec_error_proc
        | _, _ -> incr num_spec_error_proc in
      tot_symops := !tot_symops + stats.Specs.symops;
      if Option.is_some stats.Specs.stats_failure then incr num_timeout;
      Errlog.extend_table err_table err_log in
  let print_file_stats fmt () =
    let num_errors = Errlog.err_table_size_footprint Exceptions.Kerror err_table in
    let num_warnings = Errlog.err_table_size_footprint Exceptions.Kwarning err_table in
    let num_infos = Errlog.err_table_size_footprint Exceptions.Kinfo err_table in
    let num_ok_proc = !num_spec_noerror_proc + !num_spec_error_proc in
    (* F.fprintf fmt "VISITED: %a@\n" (pp_seq pp_node) nodes_visited;
       F.fprintf fmt "TOTAL: %a@\n" (pp_seq pp_node) nodes_total; *)
    F.fprintf fmt "@\n++++++++++++++++++++++++++++++++++++++++++++++++++@\n";
    F.fprintf fmt "+ FILE: %s  LOC: %n  VISITED: %d/%d SYMOPS: %d@\n"
      (DB.source_file_to_string !DB.current_source)
      !Config.nLOC
      (IList.length nodes_visited)
      (IList.length nodes_total)
      !tot_symops;
    F.fprintf fmt "+  num_procs: %d (%d ok, %d timeouts, %d errors, %d warnings, %d infos)@\n"
      !num_proc num_ok_proc !num_timeout num_errors num_warnings num_infos;
    F.fprintf fmt "+  detail procs:@\n";
    F.fprintf fmt "+    - No Errors and No Specs: %d@\n" !num_nospec_noerror_proc;
    F.fprintf fmt "+    - Some Errors and No Specs: %d@\n" !num_nospec_error_proc;
    F.fprintf fmt "+    - No Errors and Some Specs: %d@\n" !num_spec_noerror_proc;
    F.fprintf fmt "+    - Some Errors and Some Specs: %d@\n" !num_spec_error_proc;
    F.fprintf fmt "+  errors: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kerror) err_table;
    F.fprintf fmt "+  warnings: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kwarning) err_table;
    F.fprintf fmt "+  infos: %a@\n" (Errlog.pp_err_table_stats Exceptions.Kinfo) err_table;
    F.fprintf fmt "+  specs: %d@\n" !tot_specs;
    F.fprintf fmt "++++++++++++++++++++++++++++++++++++++++++++++++++@\n";
    Errlog.print_err_table_details fmt err_table in
  let save_file_stats () =
    let source_dir = DB.source_dir_from_source_file !DB.current_source in
    let stats_file = DB.source_dir_get_internal_file source_dir ".stats" in
    try
      let outc = open_out (DB.filename_to_string stats_file) in
      let fmt = F.formatter_of_out_channel outc in
      print_file_stats fmt ();
      close_out outc
    with Sys_error _ -> () in
  IList.iter compute_stats_proc (Cfg.get_defined_procs cfg);
  L.out "%a" print_file_stats ();
  save_file_stats ()

(** Print the stats for all the files in the exe_env *)
let print_stats exe_env =
  if !Config.developer_mode then
    Exe_env.iter_files
      (fun fname cfg ->
         let proc_shadowed proc_desc =
           (** return true if a proc with the same name in another module was analyzed instead *)
           let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
           Exe_env.get_source exe_env proc_name <> Some fname in
         print_stats_cfg proc_shadowed cfg)
      exe_env
