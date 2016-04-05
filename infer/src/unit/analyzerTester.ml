(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

(** utilities for writing abstract domains/transfer function tests *)

(** structured language that makes it easy to write small test programs in OCaml *)
module StructuredSil = struct
  type assertion = string
  type label = int

  type structured_instr =
    | Cmd of Sil.instr
    | If of Sil.exp * structured_instr list * structured_instr list
    | While of Sil.exp * structured_instr list
    | Invariant of assertion * label (* gets autotranslated into assertions about abstract state *)

  type structured_program = structured_instr list

  let rec pp_structured_instr fmt = function
    | Cmd instr -> (Sil.pp_instr pe_text) fmt instr
    | If (exp, then_instrs, else_instrs) ->
        (* TODO (t10287763): indent bodies of if/while *)
        F.fprintf fmt "if (%a) {@.%a@.} else {@.%a@.}"
          (Sil.pp_exp pe_text) exp
          pp_structured_instr_list then_instrs
          pp_structured_instr_list else_instrs
    | While (exp, instrs) ->
        F.fprintf fmt "while (%a) {@.%a@.}" (Sil.pp_exp pe_text) exp pp_structured_instr_list instrs
    | Invariant (inv_str, label) ->
        F.fprintf fmt "invariant %d: %s" label inv_str

  and pp_structured_instr_list fmt instrs =
    F.pp_print_list
      ~pp_sep:F.pp_print_newline
      (fun fmt instr -> F.fprintf fmt "%a" pp_structured_instr instr)
      fmt
      instrs

  let pp_structured_program = pp_structured_instr_list

  let dummy_typ = Sil.Tvoid
  let dummy_loc = Location.dummy
  let dummy_procname = Procname.empty_block

  let label_counter = ref 0

  let fresh_label () =
    incr label_counter;
    !label_counter

  let invariant inv_str =
    Invariant (inv_str, fresh_label ())

  let pvar_of_str str =
    Pvar.mk (Mangled.from_string str) dummy_procname

  let var_of_str str =
    Sil.Lvar (pvar_of_str str)

  let ident_of_str str =
    Ident.create_normal (Ident.string_to_name str) 0

  let unknown_exp =
    var_of_str "__unknown__"

  let make_letderef ~rhs_typ lhs_id rhs_exp =
    Cmd (Sil.Letderef (lhs_id, rhs_exp, rhs_typ, dummy_loc))

  let make_set ~rhs_typ ~lhs_exp ~rhs_exp =
    Cmd (Sil.Set (lhs_exp, rhs_typ, rhs_exp, dummy_loc))

  let make_call ?(procname=dummy_procname) ret_ids args =
    let call_exp = Sil.Const (Sil.Cfun procname) in
    Cmd (Sil.Call (ret_ids, call_exp, args, dummy_loc, Sil.cf_default))

  let id_assign_id ?(rhs_typ=dummy_typ) lhs rhs =
    let lhs_id = ident_of_str lhs in
    let rhs_exp = Sil.Var (ident_of_str rhs) in
    make_letderef ~rhs_typ lhs_id rhs_exp

  let id_assign_var ?(rhs_typ=dummy_typ) lhs rhs =
    let lhs_id = ident_of_str lhs in
    let rhs_exp = var_of_str rhs in
    make_letderef ~rhs_typ lhs_id rhs_exp

  let var_assign_exp ~rhs_typ lhs rhs_exp =
    let lhs_exp = var_of_str lhs in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp

  let var_assign_int lhs rhs =
    let rhs_exp = Sil.exp_int (Sil.Int.of_int rhs) in
    let rhs_typ = Sil.Tint Sil.IInt in
    var_assign_exp ~rhs_typ lhs rhs_exp

  let var_assign_id ?(rhs_typ=dummy_typ) lhs rhs =
    let lhs_exp = var_of_str lhs in
    let rhs_exp = Sil.Var (ident_of_str rhs) in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp

  let var_assign_var ?(rhs_typ=dummy_typ) lhs rhs =
    let lhs_exp = var_of_str lhs in
    let rhs_exp = var_of_str rhs in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp

  let call_unknown ret_id_strs arg_strs =
    let args = IList.map (fun param_str -> (var_of_str param_str, dummy_typ)) arg_strs in
    let ret_ids = IList.map ident_of_str ret_id_strs in
    make_call ret_ids args

  let call_unknown_no_ret arg_strs =
    call_unknown [] arg_strs
end

module Make
    (C : ProcCfg.Wrapper with type node = Cfg.Node.t)
    (S : Scheduler.S)
    (A : AbstractDomain.S)
    (T : TransferFunctions.S with type astate = A.astate) = struct

  open StructuredSil

  module I = AbstractInterpreter.Make (C) (S) (A) (T)
  module M = ProcCfg.NodeIdMap (C)

  type assert_map = string M.t

  let structured_program_to_cfg program =
    let cfg = Cfg.Node.create_cfg () in
    let pdesc =
      Cfg.Procdesc.create cfg (ProcAttributes.default dummy_procname !Config.curr_language) in

    let create_node kind cmds =
      let no_tmp_idents = [] in
      Cfg.Node.create cfg dummy_loc kind cmds pdesc no_tmp_idents in
    let set_succs cur_node succs =
      let no_exc_succs = [] in
      Cfg.Node.set_succs_exn cur_node succs no_exc_succs in
    let mk_prune_nodes_for_cond cond_exp if_kind =
      let mk_prune_node cond_exp if_kind true_branch =
        let prune_instr = Sil.Prune (cond_exp, dummy_loc, true_branch, if_kind) in
        create_node (Cfg.Node.Prune_node (true_branch, if_kind, "")) [prune_instr] in
      let true_prune_node = mk_prune_node cond_exp if_kind true in
      let false_prune_node =
        let negated_cond_exp = Sil.UnOp (Sil.LNot, cond_exp, None) in
        mk_prune_node negated_cond_exp if_kind false in
      true_prune_node, false_prune_node in

    let rec structured_instr_to_node (last_node, assert_map) = function
      | Cmd cmd ->
          let node = create_node (Cfg.Node.Stmt_node "") [cmd] in
          set_succs last_node [node];
          node, assert_map
      | If (exp, then_instrs, else_instrs) ->
          let then_prune_node, else_prune_node = mk_prune_nodes_for_cond exp Sil.Ik_if in
          set_succs last_node [then_prune_node; else_prune_node];
          let then_branch_end_node, assert_map' =
            structured_instrs_to_node then_prune_node assert_map then_instrs in
          let else_branch_end_node, assert_map'' =
            structured_instrs_to_node else_prune_node assert_map' else_instrs in
          let join_node = create_node Cfg.Node.Join_node [] in
          set_succs then_branch_end_node [join_node];
          set_succs else_branch_end_node [join_node];
          join_node, assert_map''
      | While (exp, body_instrs) ->
          let loop_head_join_node = create_node Cfg.Node.Join_node [] in
          set_succs last_node [loop_head_join_node];
          let true_prune_node, false_prune_node = mk_prune_nodes_for_cond exp Sil.Ik_while in
          set_succs loop_head_join_node [true_prune_node; false_prune_node];
          let loop_body_end_node, assert_map' =
            structured_instrs_to_node true_prune_node assert_map body_instrs in
          let loop_exit_node = create_node (Cfg.Node.Skip_node "") [] in
          set_succs loop_body_end_node [loop_head_join_node];
          set_succs false_prune_node [loop_exit_node];
          loop_exit_node, assert_map'
      | Invariant (inv_str, inv_label) ->
          let node = create_node (Cfg.Node.Stmt_node "Invariant") [] in
          set_succs last_node [node];
          (* add the assertion to be checked after analysis converges *)
          node, M.add (C.node_id node) (inv_str, inv_label) assert_map
    and structured_instrs_to_node last_node assert_map instrs =
      IList.fold_left
        (fun acc instr -> structured_instr_to_node acc instr) (last_node, assert_map) instrs in

    let start_node = create_node (Cfg.Node.Start_node pdesc) [] in
    Cfg.Procdesc.set_start_node pdesc start_node;
    let last_node, assert_map = structured_instrs_to_node start_node M.empty program in
    let exit_node = create_node (Cfg.Node.Exit_node pdesc) [] in
    set_succs last_node [exit_node];
    Cfg.Procdesc.set_exit_node pdesc exit_node;
    pdesc, assert_map

  let create_test test_program _ =
    let pdesc, assert_map = structured_program_to_cfg test_program in
    let inv_map = I.exec_pdesc pdesc in

    let collect_invariant_mismatches node_id (inv_str, inv_label) error_msgs_acc =
      let post_str =
        try
          let state = M.find node_id inv_map in
          pp_to_string A.pp state.post
        with Not_found -> "_|_" in
      if inv_str <> post_str then
        let error_msg =
          F.fprintf F.str_formatter
            "> Expected state %s at invariant %d, but found state %s"
            inv_str inv_label post_str
          |> F.flush_str_formatter in
        error_msg :: error_msgs_acc
      else error_msgs_acc in

    match M.fold collect_invariant_mismatches assert_map [] with
    | [] -> () (* no mismatches, test passed *)
    | error_msgs ->
        let mismatches_str =
          F.pp_print_list
            (fun fmt error_msg -> F.fprintf fmt "%s" error_msg) F.str_formatter
            (IList.rev error_msgs)
          |> F.flush_str_formatter in
        let assert_fail_message =
          F.fprintf F.str_formatter "Error while analyzing@.%a:@.%s@."
            pp_structured_program test_program mismatches_str
          |> F.flush_str_formatter in
        OUnit2.assert_failure assert_fail_message

  let create_tests tests =
    let open OUnit2 in
    IList.map (fun (name, test_program) -> name>::create_test test_program) tests

end
