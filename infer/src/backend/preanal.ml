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

module L = Logging

(** find all the predecessors of nodes, using exception links *)
module AllPreds = struct
  module NodeHash = Cfg.NodeHash
  let preds_table = NodeHash.create 3 (* table from node to set of predecessors *)

  let clear_table () =
    NodeHash.clear preds_table

  let mk_table cfg =
    let do_pdesc _ pdesc =
      let exit_node = Cfg.Procdesc.get_exit_node pdesc in
      let add_edge is_exn nfrom nto =
        if is_exn && Cfg.Node.equal nto exit_node then ()
        else
          try
            let preds = NodeHash.find preds_table nto in
            let preds' = Cfg.NodeSet.add nfrom preds in
            NodeHash.replace preds_table nto preds'
          with Not_found ->
            NodeHash.add preds_table nto (Cfg.NodeSet.singleton nfrom) in
      let do_node n =
        IList.iter (add_edge false n) (Cfg.Node.get_succs n);
        IList.iter (add_edge true n) (Cfg.Node.get_exn n) in
      let proc_nodes = Cfg.Procdesc.get_nodes pdesc in
      IList.iter do_node proc_nodes in
    clear_table ();
    Cfg.iter_proc_desc cfg do_pdesc

  let get_preds n =
    try
      let preds = NodeHash.find preds_table n in
      Cfg.NodeSet.elements preds
    with Not_found ->
      Cfg.Node.get_preds n
end

module Vset = AddressTaken.PvarSet

let is_not_function cfg x =
  let pname = Procname.from_string_c_fun (Mangled.to_string (Pvar.get_name x)) in
  Cfg.Procdesc.find_from_name cfg pname = None

(** variables read in the expression *)
let rec use_exp cfg pdesc (exp: Sil.exp) acc =
  match exp with
  | Sil.Var _ | Sil.Sizeof _ -> acc
  | Sil.Const (Cclosure { captured_vars; }) ->
      IList.fold_left
        (fun vset_acc (_, captured_pvar, _) -> Vset.add captured_pvar vset_acc)
        acc
        captured_vars
  | Sil.Const
      (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cattribute _ | Cexn _ | Cclass _ | Cptr_to_fld _) ->
      acc
  | Sil.Lvar x -> Vset.add x acc
  | Sil.Cast (_, e) | Sil.UnOp (_, e, _) | Sil.Lfield (e, _, _) -> use_exp cfg pdesc e acc
  | Sil.BinOp (_, e1, e2) | Sil.Lindex (e1, e2) -> use_exp cfg pdesc e1 (use_exp cfg pdesc e2 acc)

and use_etl cfg pdesc (etl: (Sil.exp * Sil.typ) list) acc =
  IList.fold_left (fun acc (e, _) -> use_exp cfg pdesc e acc) acc etl

and use_instr cfg (pdesc: Cfg.Procdesc.t) (instr: Sil.instr) acc =
  match instr with
  | Sil.Set (_, _, e, _)
  | Sil.Letderef (_, e, _, _) -> use_exp cfg pdesc e acc
  | Sil.Prune (e, _, _, _) -> use_exp cfg pdesc e acc
  | Sil.Call (_, _, etl, _, _) -> use_etl cfg pdesc etl acc
  | Sil.Nullify _ -> acc
  | Sil.Abstract _ | Sil.Remove_temps _ | Sil.Stackop _ | Sil.Declare_locals _ -> acc

(** variables written in the expression *)
let rec def_exp cfg (exp: Sil.exp) acc =
  match exp with
  | Sil.Lvar x -> if is_not_function cfg x then Vset.add x acc else acc
  | Sil.Cast (_, e) -> def_exp cfg e acc
  | _ -> acc

let rec def_instr cfg (instr: Sil.instr) acc =
  match instr with
  | Sil.Set (e, _, _, _) -> def_exp cfg e acc
  | Sil.Call _ | Sil.Letderef _ | Sil.Prune _ -> acc
  | Sil.Nullify (x, _, _) ->
      if is_not_function cfg x then Vset.add x acc else acc
  | Sil.Abstract _ | Sil.Remove_temps _ | Sil.Stackop _ | Sil.Declare_locals _ -> acc

and def_instrl cfg instrs acc =
  IList.fold_left (fun acc' i -> def_instr cfg i acc') acc instrs

(** variables written by instructions in the node *)
let def_node cfg node acc =
  match Cfg.Node.get_kind node with
  | Cfg.Node.Start_node _ | Cfg.Node.Exit_node _ | Cfg.Node.Join_node | Cfg.Node.Skip_node _ -> acc
  | Cfg.Node.Prune_node _
  | Cfg.Node.Stmt_node _ ->
      def_instrl cfg (Cfg.Node.get_instrs node) acc

let compute_live_instr cfg pdesc s instr =
  use_instr cfg pdesc instr (Vset.diff s (def_instr cfg instr Vset.empty))

let compute_live_instrl cfg pdesc instrs livel =
  IList.fold_left (compute_live_instr cfg pdesc) livel (IList.rev instrs)

module Worklist = struct
  module S = Cfg.NodeSet

  let worklist = ref S.empty

  let reset _ = worklist := S.empty
  let add node = worklist := S.add node !worklist
  let pick () =
    let min = S.min_elt !worklist in
    worklist := S.remove min !worklist;
    min
end

(** table of live variables *)
module Table: sig
  val reset: unit -> unit

  (** variables live after the last instruction in the current node *)
  val get_live: Cfg.node -> Vset.t

  (** propagate live variables to predecessor nodes *)
  val propagate_to_preds: Vset.t -> Cfg.node list -> unit

  val iter: Vset.t -> (Cfg.node -> Vset.t -> Vset.t -> unit) -> unit
end = struct
  module H = Cfg.NodeHash
  let table = H.create 1024
  let reset _ = H.clear table
  let get_live node = try H.find table node with Not_found -> Vset.empty
  let replace node set = H.replace table node set

  let propagate_to_preds set preds =
    let do_node node =
      try
        let oldset = H.find table node in
        let newset = Vset.union set oldset in
        replace node newset;
        if not (Vset.equal oldset newset) then Worklist.add node
      with Not_found ->
        replace node set; Worklist.add node in
    IList.iter do_node preds

  let iter init f =
    let get_live_preds init node = (** nodes live at predecessors *)
      match AllPreds.get_preds node with
      | [] -> init
      | preds -> IList.fold_left Vset.union Vset.empty (IList.map get_live preds) in
    H.iter (fun node live -> f node (get_live_preds init node) live) table
end

(** Compute condidate nullable variables amongst formals and locals *)
let compute_candidates procdesc : Vset.t * (Vset.t -> Vset.elt list) =
  let candidates = ref Vset.empty in
  let struct_array_cand = ref Vset.empty in
  let typ_is_struct_array = function
    | Sil.Tstruct _ | Sil.Tarray _ -> true
    | _ -> false in
  let add_vi (pvar, typ) =
    let pv = Pvar.mk pvar (Cfg.Procdesc.get_proc_name procdesc) in
    candidates := Vset.add pv !candidates;
    if typ_is_struct_array typ then struct_array_cand := Vset.add pv !struct_array_cand in
  IList.iter add_vi (Cfg.Procdesc.get_formals procdesc);
  IList.iter add_vi (Cfg.Procdesc.get_locals procdesc);
  let get_sorted_candidates vs =
    let priority, no_pri =
      IList.partition
        (fun pv -> Vset.mem pv !struct_array_cand)
        (Vset.elements vs) in
    IList.rev_append (IList.rev priority) no_pri in
  !candidates, get_sorted_candidates

(** Construct a table wich associates to each node a set of live variables *)
let analyze_proc cfg pdesc cand =
  let exit_node = Cfg.Procdesc.get_exit_node pdesc in
  Worklist.reset ();
  Table.reset ();
  Worklist.add exit_node;
  try
    while true do
      let node = Worklist.pick () in
      let curr_live = Table.get_live node in
      let preds = AllPreds.get_preds node in
      let live_at_predecessors =
        match Cfg.Node.get_kind node with
        | Cfg.Node.Start_node _
        | Cfg.Node.Exit_node _
        | Cfg.Node.Join_node
        | Cfg.Node.Skip_node _ -> curr_live
        | Cfg.Node.Prune_node _
        | Cfg.Node.Stmt_node _ ->
            compute_live_instrl cfg pdesc (Cfg.Node.get_instrs node) curr_live in
      Table.propagate_to_preds (Vset.inter live_at_predecessors cand) preds
    done
  with Not_found -> ()

(* Instruction i is nullifying a block variable *)
let is_block_nullify i =
  match i with
  | Sil.Nullify(pvar, _, true) -> Sil.is_block_pvar pvar
  | _ -> false

(** Add nullify instructions to the node given dead program variables  *)
let node_add_nullify_instrs n dead_vars_after dead_vars_before =
  let loc = Cfg.Node.get_last_loc n in
  let move_tmp_pvars_first pvars =
    let pvars_tmp, pvars_notmp = IList.partition Errdesc.pvar_is_frontend_tmp pvars in
    pvars_tmp @ pvars_notmp in
  let instrs_after =
    IList.map
      (fun pvar -> Sil.Nullify (pvar, loc, false))
      (move_tmp_pvars_first dead_vars_after) in
  let instrs_before =
    IList.map
      (fun pvar -> Sil.Nullify (pvar, loc, false))
      (move_tmp_pvars_first dead_vars_before) in
  (* Nullify(bloc_var,_,true) can be placed in the middle
     of the block because when we add this instruction*)
  (* we don't have already all the instructions of the node.
     Here we reorder the instructions to move *)
  (* nullification of blocks at the end of existing instructions. *)
  let block_nullify, no_block_nullify = IList.partition is_block_nullify (Cfg.Node.get_instrs n) in
  Cfg.Node.replace_instrs n (no_block_nullify @ block_nullify);
  Cfg.Node.append_instrs_temps n instrs_after [];
  Cfg.Node.prepend_instrs_temps n instrs_before []

(** return true if the node does not assign any variables *)
let node_assigns_no_variables cfg node =
  let instrs = Cfg.Node.get_instrs node in
  let assign_set = def_instrl cfg instrs (Vset.empty) in
  Vset.is_empty assign_set

(** Set the dead variables of a node, by default as dead_after.
    If the node is a prune or a join node, propagate as dead_before in the successors *)
let add_dead_pvars_after_conditionals_join cfg n deads =
  (* L.out "    node %d: %a@." (Cfg.Node.get_id n) (Sil.pp_list pe_text) deads; *)
  let seen = ref Cfg.NodeSet.empty in
  let rec add_after_prune_join is_after node =
    if Cfg.NodeSet.mem node !seen (* gone through a loop in the cfg *)
    then Cfg.Node.set_dead_pvars n true deads
    else
      begin
        seen := Cfg.NodeSet.add node !seen;
        let node_is_exit n = match Cfg.Node.get_kind n with
          | Cfg.Node.Exit_node _ -> true
          | _ -> false in
        let next_is_exit n = match Cfg.Node.get_succs n with
          | [n'] -> node_is_exit n'
          | _ -> false in
        match Cfg.Node.get_kind node with
        | Cfg.Node.Prune_node _
        | Cfg.Node.Join_node
          when node_assigns_no_variables cfg node && not (next_is_exit node) ->
            (* cannot push nullify instructions after an assignment,
               as they could nullify the same variable *)
            let succs = Cfg.Node.get_succs node in
            IList.iter (add_after_prune_join false) succs
        | _ ->
            let new_dead_pvs =
              let old_pvs = Cfg.Node.get_dead_pvars node is_after in
              let pv_is_new pv = not (IList.exists (Pvar.equal pv) old_pvs) in
              (IList.filter pv_is_new deads) @ old_pvs in
            Cfg.Node.set_dead_pvars node is_after new_dead_pvs
      end in
  add_after_prune_join true n

(** Find the set of dead variables for the procedure pname and add nullify instructions.
    The variables whose addresses may be taken are only considered just before the exit node. *)
let analyze_and_annotate_proc cfg tenv pname pdesc =
  let exit_node = Cfg.Procdesc.get_exit_node pdesc in
  let exit_node_is_succ node =
    match Cfg.Node.get_succs node with
    | [en] -> Cfg.Node.equal en exit_node
    | _ -> false in
  let cand, get_sorted_cand = compute_candidates pdesc in

  let addr_taken_vars =
    if !Config.curr_language = Config.Java
    then Vset.empty
    else
      match AddressTaken.Analyzer.compute_post (ProcData.make pdesc tenv) with
      | Some post -> post
      | None -> Vset.empty in

  analyze_proc cfg pdesc cand;

  let dead_pvars_added = ref 0 in
  let dead_pvars_limit = 100000 in
  let incr_dead_pvars_added pvars =

    let num = IList.length pvars in
    dead_pvars_added := num + !dead_pvars_added;
    if !dead_pvars_added > dead_pvars_limit && !dead_pvars_added - num <= dead_pvars_limit
    then
      L.err "WARNING: liveness: more than %d dead pvars added in procedure %a, stopping@."
        dead_pvars_limit Procname.pp pname in
  Table.iter cand (fun n live_at_predecessors live_current -> (* set dead variables on nodes *)
      (* live before, or assigned to *)
      let nonnull_pvars = Vset.inter (def_node cfg n live_at_predecessors) cand in
      (* only nullify when variables become live *)
      let dead_pvars = Vset.diff nonnull_pvars live_current in
      let dead_pvars_no_addr_taken = get_sorted_cand (Vset.diff dead_pvars addr_taken_vars) in
      let dead_pvars_to_add =
        if exit_node_is_succ n (* add dead address taken vars just before the exit node *)
        then dead_pvars_no_addr_taken @ (get_sorted_cand (Vset.inter cand addr_taken_vars))
        else dead_pvars_no_addr_taken in
      incr_dead_pvars_added dead_pvars_to_add;
      if !dead_pvars_added < dead_pvars_limit
      then add_dead_pvars_after_conditionals_join cfg n dead_pvars_to_add);

  IList.iter (fun n -> (* generate nullify instructions *)
      let dead_pvs_after = Cfg.Node.get_dead_pvars n true in
      let dead_pvs_before = Cfg.Node.get_dead_pvars n false in
      node_add_nullify_instrs n dead_pvs_after dead_pvs_before)
    (Cfg.Procdesc.get_nodes pdesc);
  Table.reset ()

(** mutate the cfg/cg to add dynamic dispatch handling *)
let add_dispatch_calls cfg cg tenv f_translate_typ_opt =
  let pname_translate_types pname =
    match f_translate_typ_opt with
    | Some f_translate_typ ->
        (match pname with
         | Procname.Java pname_java ->
             let param_type_strs =
               IList.map Procname.java_type_to_string (Procname.java_get_parameters pname_java) in
             let receiver_type_str = Procname.java_get_class_name pname_java in
             let return_type_str = Procname.java_get_return_type pname_java in
             IList.iter
               (fun typ_str -> f_translate_typ tenv typ_str)
               (return_type_str :: (receiver_type_str :: param_type_strs))
         | Procname.C _
         | Procname.ObjC_Cpp _
         | Procname.Block _ ->
             (* TODO: support this for C/CPP/Obj-C *)
             ())
    | None -> () in
  let node_add_dispatch_calls caller_pname node =
    (* TODO: handle dynamic dispatch for virtual calls as well *)
    let call_flags_is_dispatch call_flags =
      (* if sound dispatch is turned off, only consider dispatch for interface calls *)
      (Config.sound_dynamic_dispatch && call_flags.Sil.cf_virtual) ||
      call_flags.Sil.cf_interface in
    let instr_is_dispatch_call = function
      | Sil.Call (_, _, _, _, call_flags) -> call_flags_is_dispatch call_flags
      | _ -> false in
    let has_dispatch_call instrs =
      IList.exists instr_is_dispatch_call instrs in
    let replace_dispatch_calls = function
      | Sil.Call (ret_ids, (Sil.Const (Sil.Cfun callee_pname) as call_exp),
                  (((_, receiver_typ) :: _) as args), loc, call_flags) as instr
        when call_flags_is_dispatch call_flags ->
          (* the frontend should not populate the list of targets *)
          assert (call_flags.Sil.cf_targets = []);
          let receiver_typ_no_ptr = match receiver_typ with
            | Sil.Tptr (typ', _) ->
                typ'
            | _ ->
                receiver_typ in
          let sorted_overrides =
            let overrides = Prover.get_overrides_of tenv receiver_typ_no_ptr callee_pname in
            IList.sort (fun (_, p1) (_, p2) -> Procname.compare p1 p2) overrides in
          (match sorted_overrides with
           | ((_, target_pname) :: _) as all_targets ->
               let targets_to_add =
                 if Config.sound_dynamic_dispatch then
                   IList.map snd all_targets
                 else
                   (* if sound dispatch is turned off, consider only the first target. we do this
                      because choosing all targets is too expensive for everyday use *)
                   [target_pname] in
               IList.iter
                 (fun target_pname ->
                    pname_translate_types target_pname;
                    Cg.add_edge cg caller_pname target_pname)
                 targets_to_add;
               let call_flags' = { call_flags with Sil.cf_targets = targets_to_add; } in
               Sil.Call (ret_ids, call_exp, args, loc, call_flags')
           | [] -> instr)

      | instr -> instr in
    let instrs = Cfg.Node.get_instrs node in
    if has_dispatch_call instrs then
      IList.map replace_dispatch_calls instrs
      |> Cfg.Node.replace_instrs node in
  let proc_add_dispach_calls pname pdesc =
    Cfg.Procdesc.iter_nodes (node_add_dispatch_calls pname) pdesc in
  Cfg.iter_proc_desc cfg proc_add_dispach_calls

(** add instructions to perform abstraction *)
let add_abstraction_instructions cfg =
  let open Cfg in
  (* true if there is a succ node s.t.: it is an exit node, or the succ of >1 nodes *)
  let converging_node node =
    let is_exit node = match Node.get_kind node with
      | Node.Exit_node _ -> true
      | _ -> false in
    let succ_nodes = Node.get_succs node in
    if IList.exists is_exit succ_nodes then true
    else match succ_nodes with
      | [] -> false
      | [h] -> IList.length (Node.get_preds h) > 1
      | _ -> false in
  let node_requires_abstraction node =
    match Node.get_kind node with
    | Node.Start_node _
    | Node.Join_node ->
        false
    | Node.Exit_node _
    | Node.Stmt_node _
    | Node.Prune_node _
    | Node.Skip_node _ ->
        converging_node node in
  let all_nodes = Node.get_all_nodes cfg in
  let do_node node =
    let loc = Node.get_last_loc node in
    if node_requires_abstraction node then Node.append_instrs_temps node [Sil.Abstract loc] [] in
  IList.iter do_node all_nodes

(** add instructions to remove temporaries *)
let add_removetemps_instructions cfg =
  let open Cfg in
  let all_nodes = Node.get_all_nodes cfg in
  let do_node node =
    let loc = Node.get_last_loc node in
    let temps = Node.get_temps node in
    if temps != [] then Node.append_instrs_temps node [Sil.Remove_temps (temps, loc)] [] in
  IList.iter do_node all_nodes

let doit ?(f_translate_typ=None) cfg cg tenv =
  add_removetemps_instructions cfg;
  AllPreds.mk_table cfg;
  Cfg.iter_proc_desc cfg (analyze_and_annotate_proc cfg tenv);
  AllPreds.clear_table ();
  if !Config.curr_language = Config.Java
  then add_dispatch_calls cfg cg tenv f_translate_typ;
  add_abstraction_instructions cfg;
