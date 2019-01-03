(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** utilities for writing abstract domains/transfer function tests *)

(** structured language that makes it easy to write small test programs in OCaml *)
module StructuredSil = struct
  type assertion = string

  type label = int

  type structured_instr =
    | Cmd of Sil.instr
    | If of Exp.t * structured_instr list * structured_instr list
    | While of Exp.t * structured_instr list
        (** try/catch/finally. note: there is no throw. the semantics are that every command in the try
       block is assumed to be possibly-excepting, and the catch block captures all exceptions *)
    | Try of structured_instr list * structured_instr list * structured_instr list
    | Invariant of assertion * label
        (** gets autotranslated into assertions about abstract state *)

  let rec pp_structured_instr fmt = function
    | Cmd instr ->
        Sil.pp_instr ~print_types:false Pp.text fmt instr
    | If (exp, then_instrs, else_instrs) ->
        (* TODO (t10287763): indent bodies of if/while *)
        F.fprintf fmt "if (%a) {@.%a@.} else {@.%a@.}" Exp.pp exp pp_structured_instr_list
          then_instrs pp_structured_instr_list else_instrs
    | While (exp, instrs) ->
        F.fprintf fmt "while (%a) {@.%a@.}" Exp.pp exp pp_structured_instr_list instrs
    | Try (try_, catch, finally) ->
        F.fprintf fmt "try {@.%a@.} catch (...) {@.%a@.} finally {@.%a@.}" pp_structured_instr_list
          try_ pp_structured_instr_list catch pp_structured_instr_list finally
    | Invariant (inv_str, label) ->
        F.fprintf fmt "invariant %d: %s" label inv_str


  and pp_structured_instr_list fmt instrs =
    F.pp_print_list ~pp_sep:F.pp_print_newline pp_structured_instr fmt instrs


  let pp_structured_program = pp_structured_instr_list

  let dummy_typ = Typ.mk (Tint IUChar)

  let dummy_loc = Location.dummy

  let dummy_procname = Typ.Procname.empty_block

  let label_counter = ref 0

  let fresh_label () = incr label_counter ; !label_counter

  let invariant inv_str = Invariant (inv_str, fresh_label ())

  let pvar_of_str str = Pvar.mk (Mangled.from_string str) dummy_procname

  let var_of_str str = Exp.Lvar (pvar_of_str str)

  let ident_of_str str = Ident.create_normal (Ident.string_to_name str) 0

  let unknown_exp = var_of_str "__unknown__"

  let make_load ~rhs_typ lhs_id rhs_exp = Cmd (Sil.Load (lhs_id, rhs_exp, rhs_typ, dummy_loc))

  let make_set ~rhs_typ ~lhs_exp ~rhs_exp = Cmd (Sil.Store (lhs_exp, rhs_typ, rhs_exp, dummy_loc))

  let make_call ?(procname = dummy_procname) ?return:return_opt args =
    let ret_id_typ =
      match return_opt with
      | Some ret_id_typ ->
          ret_id_typ
      | None ->
          (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
    in
    let call_exp = Exp.Const (Const.Cfun procname) in
    Cmd (Sil.Call (ret_id_typ, call_exp, args, dummy_loc, CallFlags.default))


  let make_store ~rhs_typ root_exp fld_str ~rhs_exp =
    let fld = AccessPathTestUtils.make_fieldname fld_str in
    let lhs_exp = Exp.Lfield (root_exp, fld, rhs_typ) in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp


  let make_load_fld ~rhs_typ lhs_str fld_str root_exp =
    let fld = AccessPathTestUtils.make_fieldname fld_str in
    let rhs_exp = Exp.Lfield (root_exp, fld, rhs_typ) in
    make_load ~rhs_typ (ident_of_str lhs_str) rhs_exp


  let id_assign_exp ?(rhs_typ = dummy_typ) lhs rhs_exp =
    let lhs_id = ident_of_str lhs in
    make_load ~rhs_typ lhs_id rhs_exp


  let id_assign_id ?(rhs_typ = dummy_typ) lhs rhs =
    id_assign_exp ~rhs_typ lhs (Exp.Var (ident_of_str rhs))


  let id_assign_var ?(rhs_typ = dummy_typ) lhs rhs =
    let lhs_id = ident_of_str lhs in
    let rhs_exp = var_of_str rhs in
    make_load ~rhs_typ lhs_id rhs_exp


  let id_set_id ?(rhs_typ = dummy_typ) lhs_id rhs_id =
    let lhs_exp = Exp.Var (ident_of_str lhs_id) in
    let rhs_exp = Exp.Var (ident_of_str rhs_id) in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp


  let var_assign_exp ~rhs_typ lhs rhs_exp =
    let lhs_exp = var_of_str lhs in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp


  let var_assign_int lhs rhs =
    let rhs_exp = Exp.int (IntLit.of_int rhs) in
    let rhs_typ = Typ.mk (Tint Typ.IInt) in
    var_assign_exp ~rhs_typ lhs rhs_exp


  let var_assign_id ?(rhs_typ = dummy_typ) lhs rhs =
    let lhs_exp = var_of_str lhs in
    let rhs_exp = Exp.Var (ident_of_str rhs) in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp


  (* x = &y *)
  let var_assign_addrof_var ?(rhs_typ = dummy_typ) lhs rhs =
    let lhs_exp = var_of_str lhs in
    let rhs_exp = var_of_str rhs in
    make_set ~rhs_typ ~lhs_exp ~rhs_exp


  let call_unknown ?return arg_strs =
    let args = List.map ~f:(fun param_str -> (var_of_str param_str, dummy_typ)) arg_strs in
    let return = Option.map return ~f:(fun (str, typ) -> (ident_of_str str, typ)) in
    make_call ?return args
end

module MakeMake
    (MakeAbstractInterpreter : AbstractInterpreter.Make)
    (T : TransferFunctions.SIL with type CFG.Node.t = Procdesc.Node.t) =
struct
  open StructuredSil
  module I = MakeAbstractInterpreter (T)
  module M = I.InvariantMap

  let structured_program_to_cfg program test_pname =
    let cfg = Cfg.create () in
    let pdesc =
      Cfg.create_proc_desc cfg (ProcAttributes.default (SourceFile.invalid __FILE__) test_pname)
    in
    let create_node kind cmds = Procdesc.create_node pdesc dummy_loc kind cmds in
    let set_succs cur_node succs ~exn_handlers =
      Procdesc.node_set_succs_exn pdesc cur_node succs exn_handlers
    in
    let mk_prune_nodes_for_cond cond_exp if_kind =
      let mk_prune_node cond_exp if_kind true_branch =
        let prune_instr = Sil.Prune (cond_exp, dummy_loc, true_branch, if_kind) in
        create_node
          (Procdesc.Node.Prune_node
             ( true_branch
             , if_kind
             , if true_branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch ))
          [prune_instr]
      in
      let true_prune_node = mk_prune_node cond_exp if_kind true in
      let false_prune_node =
        let negated_cond_exp = Exp.UnOp (Unop.LNot, cond_exp, None) in
        mk_prune_node negated_cond_exp if_kind false
      in
      (true_prune_node, false_prune_node)
    in
    let rec structured_instr_to_node (last_node, assert_map) exn_handlers = function
      | Cmd cmd ->
          let node = create_node (Procdesc.Node.Stmt_node (Skip "")) [cmd] in
          set_succs last_node [node] ~exn_handlers ;
          (node, assert_map)
      | If (exp, then_instrs, else_instrs) ->
          let then_prune_node, else_prune_node = mk_prune_nodes_for_cond exp Sil.Ik_if in
          set_succs last_node [then_prune_node; else_prune_node] ~exn_handlers ;
          let then_branch_end_node, assert_map' =
            structured_instrs_to_node then_prune_node assert_map exn_handlers then_instrs
          in
          let else_branch_end_node, assert_map'' =
            structured_instrs_to_node else_prune_node assert_map' exn_handlers else_instrs
          in
          let join_node = create_node Procdesc.Node.Join_node [] in
          set_succs then_branch_end_node [join_node] ~exn_handlers ;
          set_succs else_branch_end_node [join_node] ~exn_handlers ;
          (join_node, assert_map'')
      | While (exp, body_instrs) ->
          let loop_head_join_node = create_node Procdesc.Node.Join_node [] in
          set_succs last_node [loop_head_join_node] ~exn_handlers ;
          let true_prune_node, false_prune_node = mk_prune_nodes_for_cond exp Sil.Ik_while in
          set_succs loop_head_join_node [true_prune_node; false_prune_node] ~exn_handlers ;
          let loop_body_end_node, assert_map' =
            structured_instrs_to_node true_prune_node assert_map exn_handlers body_instrs
          in
          let loop_exit_node = create_node (Procdesc.Node.Skip_node "") [] in
          set_succs loop_body_end_node [loop_head_join_node] ~exn_handlers ;
          set_succs false_prune_node [loop_exit_node] ~exn_handlers ;
          (loop_exit_node, assert_map')
      | Try (try_instrs, catch_instrs, finally_instrs) ->
          let catch_start_node = create_node (Procdesc.Node.Skip_node "exn_handler") [] in
          (* use [catch_start_node] as the exn handler *)
          let try_end_node, assert_map' =
            structured_instrs_to_node last_node assert_map [catch_start_node] try_instrs
          in
          let catch_end_node, assert_map'' =
            structured_instrs_to_node catch_start_node assert_map' exn_handlers catch_instrs
          in
          let finally_start_node = create_node (Procdesc.Node.Skip_node "finally") [] in
          set_succs try_end_node [finally_start_node] ~exn_handlers ;
          set_succs catch_end_node [finally_start_node] ~exn_handlers ;
          structured_instrs_to_node finally_start_node assert_map'' exn_handlers finally_instrs
      | Invariant (inv_str, inv_label) ->
          let node = create_node (Procdesc.Node.Stmt_node (Skip "Invariant")) [] in
          set_succs last_node [node] ~exn_handlers ;
          (* add the assertion to be checked after analysis converges *)
          (node, M.add (T.CFG.Node.id node) (inv_str, inv_label) assert_map)
    and structured_instrs_to_node last_node assert_map exn_handlers instrs =
      List.fold
        ~f:(fun acc instr -> structured_instr_to_node acc exn_handlers instr)
        ~init:(last_node, assert_map) instrs
    in
    let start_node = create_node Procdesc.Node.Start_node [] in
    Procdesc.set_start_node pdesc start_node ;
    let no_exn_handlers = [] in
    let last_node, assert_map =
      structured_instrs_to_node start_node M.empty no_exn_handlers program
    in
    let exit_node = create_node Procdesc.Node.Exit_node [] in
    set_succs last_node [exit_node] ~exn_handlers:no_exn_handlers ;
    Procdesc.set_exit_node pdesc exit_node ;
    (pdesc, assert_map)


  let create_test test_program extras ~initial pp_opt test_pname _ =
    let pp_state = Option.value ~default:I.TransferFunctions.Domain.pp pp_opt in
    let pdesc, assert_map = structured_program_to_cfg test_program test_pname in
    let inv_map = I.exec_pdesc (ProcData.make pdesc (Tenv.create ()) extras) ~initial in
    let collect_invariant_mismatches node_id (inv_str, inv_label) error_msgs_acc =
      let post_str =
        try
          let {AbstractInterpreter.State.post} = M.find node_id inv_map in
          F.asprintf "%a" pp_state post
        with Caml.Not_found -> "_|_"
      in
      if inv_str <> post_str then
        let error_msg =
          F.fprintf F.str_formatter "> Expected state %s at invariant %d, but found state %s"
            inv_str inv_label post_str
          |> F.flush_str_formatter
        in
        error_msg :: error_msgs_acc
      else error_msgs_acc
    in
    match M.fold collect_invariant_mismatches assert_map [] with
    | [] ->
        () (* no mismatches, test passed *)
    | error_msgs ->
        let mismatches_str =
          F.pp_print_list F.pp_print_string F.str_formatter (List.rev error_msgs)
          |> F.flush_str_formatter
        in
        let assert_fail_message =
          F.fprintf F.str_formatter "Error while analyzing@.%a:@.%s@." pp_structured_program
            test_program mismatches_str
          |> F.flush_str_formatter
        in
        OUnit2.assert_failure assert_fail_message
end

module Make (T : TransferFunctions.SIL with type CFG.Node.t = Procdesc.Node.t) = struct
  module AI_RPO = MakeMake (AbstractInterpreter.MakeRPO) (T)
  module AI_WTO = MakeMake (AbstractInterpreter.MakeWTO) (T)

  let ai_list = [("ai_rpo", AI_RPO.create_test); ("ai_wto", AI_WTO.create_test)]

  let create_tests ?(test_pname = Typ.Procname.empty_block) ~initial ?pp_opt extras tests =
    let open OUnit2 in
    List.concat_map
      ~f:(fun (name, test_program) ->
        List.map ai_list ~f:(fun (ai_name, create_test) ->
            name ^ "_" ^ ai_name >:: create_test test_program extras ~initial pp_opt test_pname )
        )
      tests
end
