(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
open Utils

(** find all the predecessors of nodes, using exception links *)
module AllPreds = struct
  module NodeHash = Cfg.NodeHash
  let preds_table = NodeHash.create 3 (* table from node to set of predecessors *)

  let clear_table () =
    NodeHash.clear preds_table

  let mk_table cfg =
    let do_pdesc pname pdesc =
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

module Vset = Set.Make (struct
    type t = Sil.pvar
    let compare = Sil.pvar_compare
  end)

let aliased_var = ref Vset.empty

let captured_var = ref Vset.empty

let is_not_function cfg x =
  let pname = Procname.from_string_c_fun (Mangled.to_string (Sil.pvar_get_name x)) in
  Cfg.Procdesc.find_from_name cfg pname = None

let is_captured_pvar pdesc x =
  let captured = Cfg.Procdesc.get_captured pdesc in
  IList.exists (fun (m, _) -> (Sil.pvar_to_string x) = (Mangled.to_string m)) captured

(** variables read in the expression *)
let rec use_exp cfg pdesc (exp: Sil.exp) acc =
  match exp with
  | Sil.Var _ | Sil.Sizeof _ -> acc
  | Sil.Const (Sil.Ctuple((Sil.Const (Sil.Cfun pname)):: _)) ->
      (* for tuples representing the assignment of a block we take the block name *)
      (* look for its procdesc and add its captured vars to the set of captured vars. *)
      let found_pd = ref None in
      Cfg.iter_proc_desc cfg (fun pn pd -> if Procname.equal pn pname then found_pd:= Some pd);
      let defining_proc = Cfg.Procdesc.get_proc_name pdesc in
      (match !found_pd with
       | Some pd ->
           IList.iter (fun (x, _) ->
               captured_var:= Vset.add (Sil.mk_pvar x defining_proc) !captured_var
             ) (Cfg.Procdesc.get_captured pd)
       | _ -> ());
      acc
  | Sil.Const _ -> acc
  | Sil.Lvar x ->
      (* If x is a captured var in the current procdesc don't add it to acc *)
      if is_captured_pvar pdesc x then acc else Vset.add x acc
  | Sil.Cast (_, e) | Sil.UnOp (_, e, _) | Sil.Lfield (e, _, _) -> use_exp cfg pdesc e acc
  | Sil.BinOp (_, e1, e2) | Sil.Lindex (e1, e2) -> use_exp cfg pdesc e1 (use_exp cfg pdesc e2 acc)

and use_etl cfg pdesc (etl: (Sil.exp * Sil.typ) list) acc =
  IList.fold_left (fun acc (e, _) -> use_exp cfg pdesc e acc) acc etl

and use_instrl cfg tenv (pdesc: Cfg.Procdesc.t) (il : Sil.instr list) acc =
  IList.fold_left (fun acc instr -> use_instr cfg tenv pdesc instr acc) acc il

and use_instr cfg tenv (pdesc: Cfg.Procdesc.t) (instr: Sil.instr) acc =
  match instr with
  | Sil.Set (_, _, e, _)
  | Sil.Letderef (_, e, _, _) -> use_exp cfg pdesc e acc
  | Sil.Prune (e, _, _, _) -> use_exp cfg pdesc e acc
  | Sil.Call (_, e, etl, _, _) -> use_etl cfg pdesc etl acc
  | Sil.Nullify _ -> acc
  | Sil.Abstract _ | Sil.Remove_temps _ | Sil.Stackop _ | Sil.Declare_locals _ -> acc
  | Sil.Goto_node (e, _) -> use_exp cfg pdesc e acc

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
  | Sil.Goto_node _ -> acc

and def_instrl cfg instrs acc =
  IList.fold_left (fun acc' i -> def_instr cfg i acc') acc instrs

(* computes the addresses that are assigned to something or passed as parameters to*)
(* a functions. These will be considered becoming possibly aliased *)
let rec aliasing_instr cfg pdesc (instr: Sil.instr) acc =
  match instr with
  | Sil.Set (_, _, e, _) -> use_exp cfg pdesc e acc
  | Sil.Call (_, _, argl, _, _) ->
      let argl'= fst (IList.split argl) in
      IList.fold_left (fun acc' e' -> use_exp cfg pdesc e' acc') acc argl'
  | Sil.Letderef _ | Sil.Prune _ -> acc
  | Sil.Nullify _ -> acc
  | Sil.Abstract _ | Sil.Remove_temps _ | Sil.Stackop _ | Sil.Declare_locals _ -> acc
  | Sil.Goto_node _ -> acc

and aliasing_instrl cfg pdesc (il : Sil.instr list) acc =
  IList.fold_left (fun acc instr -> aliasing_instr cfg pdesc instr acc) acc il

(* computes possible alisased var *)
let def_aliased_var cfg pdesc instrs acc =
  IList.fold_left (fun acc' i -> aliasing_instr cfg pdesc i acc') acc instrs

(** variables written by instructions in the node *)
let def_node cfg node acc =
  match Cfg.Node.get_kind node with
  | Cfg.Node.Start_node _ | Cfg.Node.Exit_node _ | Cfg.Node.Join_node | Cfg.Node.Skip_node _ -> acc
  | Cfg.Node.Prune_node _
  | Cfg.Node.Stmt_node _ ->
      def_instrl cfg (Cfg.Node.get_instrs node) acc

let compute_live_instr cfg tenv pdesc s instr =
  use_instr cfg tenv pdesc instr (Vset.diff s (def_instr cfg instr Vset.empty))

let compute_live_instrl cfg tenv pdesc instrs livel =
  IList.fold_left (compute_live_instr cfg tenv pdesc) livel (IList.rev instrs)

module Worklist = struct
  module S = Cfg.NodeSet

  let worklist = ref S.empty

  let reset _ = worklist := S.empty
  let add node = worklist := S.add node !worklist
  let add_list = IList.iter add
  let pick () =
    let min = S.min_elt !worklist in
    worklist := S.remove min !worklist;
    min
end

(** table of live variables *)
module Table: sig
  val reset: unit -> unit
  val get_live: Cfg.node -> Vset.t (** variables live after the last instruction in the current node *)
  val replace: Cfg.node -> Vset.t -> unit
  val propagate_to_preds: Vset.t -> Cfg.node list -> unit (** propagate live variables to predecessor nodes *)
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

(** compute the variables which are possibly aliased in node n *)
let compute_aliased cfg n aliased_var =
  match Cfg.Node.get_kind n with
  | Cfg.Node.Start_node _ | Cfg.Node.Exit_node _ | Cfg.Node.Join_node | Cfg.Node.Skip_node _ -> aliased_var
  | Cfg.Node.Prune_node _
  | Cfg.Node.Stmt_node _ ->
      def_aliased_var cfg (Cfg.Node.get_proc_desc n) (Cfg.Node.get_instrs n) aliased_var

(** Compute condidate nullable variables amongst formals and locals *)
let compute_candidates procdesc : Vset.t * (Vset.t -> Vset.elt list) =
  let candidates = ref Vset.empty in
  let struct_array_cand = ref Vset.empty in
  let typ_is_struct_array = function
    | Sil.Tstruct _ | Sil.Tarray _ -> true
    | _ -> false in
  let add_vi (pvar, typ) =
    let pv = Sil.mk_pvar pvar (Cfg.Procdesc.get_proc_name procdesc) in
    if is_captured_pvar procdesc pv then () (* don't add captured vars of the current pdesc to candidates *)
    else (
      candidates := Vset.add pv !candidates;
      if typ_is_struct_array typ then struct_array_cand := Vset.add pv !struct_array_cand
    ) in
  IList.iter add_vi (Cfg.Procdesc.get_formals procdesc);
  IList.iter add_vi (Cfg.Procdesc.get_locals procdesc);
  let get_sorted_candidates vs =
    let priority, no_pri = IList.partition (fun pv -> Vset.mem pv !struct_array_cand) (Vset.elements vs) in
    IList.rev_append (IList.rev priority) no_pri in
  !candidates, get_sorted_candidates

(** Construct a table wich associates to each node a set of live variables *)
let analyze_proc cfg tenv pdesc cand =
  let exit_node = Cfg.Procdesc.get_exit_node pdesc in
  Worklist.reset ();
  Table.reset ();
  Worklist.add exit_node;
  try
    while true do
      let node = Worklist.pick () in
      aliased_var := Vset.union (compute_aliased cfg node !aliased_var) !aliased_var;
      let curr_live = Table.get_live node in
      let preds = AllPreds.get_preds node in
      let live_at_predecessors =
        match Cfg.Node.get_kind node with
        | Cfg.Node.Start_node _ | Cfg.Node.Exit_node _ | Cfg.Node.Join_node | Cfg.Node.Skip_node _ -> curr_live
        | Cfg.Node.Prune_node _
        | Cfg.Node.Stmt_node _ ->
            compute_live_instrl cfg tenv pdesc (Cfg.Node.get_instrs node) curr_live in
      Table.propagate_to_preds (Vset.inter live_at_predecessors cand) preds
    done
  with Not_found -> ()

(* Printing function useful for debugging *)
let print_aliased_var s al_var =
  L.out s;
  Vset.iter (fun v -> L.out " %a, " (Sil.pp_pvar pe_text) v) al_var;
  L.out "@."

(* Printing function useful for debugging *)
let print_aliased_var_l s al_var =
  L.out s;
  IList.iter (fun v -> L.out " %a, " (Sil.pp_pvar pe_text) v) al_var;
  L.out "@."

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
    IList.map (fun pvar -> Sil.Nullify (pvar, loc, false)) (move_tmp_pvars_first dead_vars_after) in
  let instrs_before =
    IList.map (fun pvar -> Sil.Nullify (pvar, loc, false)) (move_tmp_pvars_first dead_vars_before) in
  (* Nullify(bloc_var,_,true) can be placed in the middle of the block because when we add this instruction*)
  (* we don't have already all the instructions of the node. Here we reorder the instructions to move *)
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
let add_dead_pvars_after_conditionals_join cfg n dead_pvars =
  (* L.out "    node %d: %a@." (Cfg.Node.get_id n) (Sil.pp_pvar_list pe_text) dead_pvars; *)
  let seen = ref Cfg.NodeSet.empty in
  let rec add_after_prune_join is_after node =
    if Cfg.NodeSet.mem node !seen (* gone through a loop in the cfg *)
    then Cfg.Node.set_dead_pvars n true dead_pvars
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
        | Cfg.Node.Prune_node _ | Cfg.Node.Join_node when node_assigns_no_variables cfg node && not (next_is_exit node) ->
            (* cannot push nullify instructions after an assignment, as they could nullify the same variable *)
            let succs = Cfg.Node.get_succs node in
            IList.iter (add_after_prune_join false) succs
        | _ ->
            let new_dead_pvs =
              let old_pvs = Cfg.Node.get_dead_pvars node is_after in
              let pv_is_new pv = not (IList.exists (Sil.pvar_equal pv) old_pvs) in
              (IList.filter pv_is_new dead_pvars) @ old_pvs in
            Cfg.Node.set_dead_pvars node is_after new_dead_pvs
      end in
  add_after_prune_join true n

(** Find the set of dead variables for the procedure pname and add nullify instructions.
    The variables that are possibly aliased are only considered just before the exit node. *)
let analyze_and_annotate_proc cfg tenv pname pdesc =
  let exit_node = Cfg.Procdesc.get_exit_node pdesc in
  let exit_node_is_succ node =
    match Cfg.Node.get_succs node with
    | [en] -> Cfg.Node.equal en exit_node
    | _ -> false in
  let cand, get_sorted_cand = compute_candidates pdesc in
  aliased_var:= Vset.empty;
  captured_var:= Vset.empty;
  analyze_proc cfg tenv pdesc cand; (* as side effect it coputes the set aliased_var  *)
  (* print_aliased_var "@.@.Aliased variable computed: " !aliased_var;
     L.out "  PROCEDURE %s@." (Procname.to_string pname); *)
  let dead_pvars_added = ref 0 in
  let dead_pvars_limit = 100000 in
  let incr_dead_pvars_added pvars =
    let num = IList.length pvars in
    dead_pvars_added := num + !dead_pvars_added;
    if !dead_pvars_added > dead_pvars_limit && !dead_pvars_added - num <= dead_pvars_limit
    then L.err "WARNING: liveness: more than %d dead pvars added in procedure %a, stopping@." dead_pvars_limit Procname.pp pname in
  Table.iter cand (fun n live_at_predecessors live_current -> (* set dead variables on nodes *)
      let nonnull_pvars = Vset.inter (def_node cfg n live_at_predecessors) cand in (* live before, or assigned to *)
      let dead_pvars = Vset.diff nonnull_pvars live_current in (* only nullify when variable become live *)
      (* L.out "    Node %s " (string_of_int (Cfg.Node.get_id n)); *)
      let dead_pvars_no_captured = Vset.diff dead_pvars !captured_var in
      (* print_aliased_var "@.@.Non-nullable variable computed: " nonnull_pvars;
         print_aliased_var "@.Dead variable computed: " dead_pvars;
         print_aliased_var "@.Captured variable computed: " !captured_var;
         print_aliased_var "@.Dead variable excluding captured computed: " dead_pvars_no_captured; *)
      let dead_pvars_no_alias = get_sorted_cand (Vset.diff dead_pvars_no_captured !aliased_var) in
      (* print_aliased_var_l "@. Final Dead variable computed: " dead_pvars_no_alias; *)
      let dead_pvars_to_add =
        if exit_node_is_succ n (* add dead aliased vars just before the exit node *)
        then dead_pvars_no_alias @ (get_sorted_cand (Vset.inter cand !aliased_var))
        else dead_pvars_no_alias in
      incr_dead_pvars_added dead_pvars_to_add;
      if !dead_pvars_added < dead_pvars_limit then add_dead_pvars_after_conditionals_join cfg n dead_pvars_to_add);
  IList.iter (fun n -> (* generate nullify instructions *)
      let dead_pvs_after = Cfg.Node.get_dead_pvars n true in
      let dead_pvs_before = Cfg.Node.get_dead_pvars n false in
      node_add_nullify_instrs n dead_pvs_after dead_pvs_before)
    (Cfg.Procdesc.get_nodes pdesc);
  Table.reset ()

let time = ref 0.0
let doit cfg tenv =
  let init = Unix.gettimeofday () in
  (* L.out "#### Dead variable nullification ####"; *)
  AllPreds.mk_table cfg;
  Cfg.iter_proc_desc cfg (analyze_and_annotate_proc cfg tenv);
  AllPreds.clear_table ();
  time := !time +. (Unix.gettimeofday () -. init)

let gettime () = !time
