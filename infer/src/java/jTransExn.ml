(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
open Javalib_pack
open Sawja_pack

let create_handler_table impl =
  let handler_tb = Hashtbl.create 1 in
  let collect (pc, exn_handler) =
    try
      let handlers = Hashtbl.find handler_tb pc in
      Hashtbl.replace handler_tb pc (exn_handler :: handlers)
    with Caml.Not_found -> Hashtbl.add handler_tb pc [exn_handler]
  in
  List.iter ~f:collect (JBir.exception_edges impl) ;
  handler_tb


let java_lang_throwable = JBasics.make_cn "java.lang.Throwable"

let is_java_lang_throwable cn = JBasics.cn_equal cn java_lang_throwable

let translate_exceptions (context : JContext.t) exit_nodes get_body_nodes handler_table =
  let catch_block_table = Hashtbl.create 1 in
  let procdesc = context.procdesc in
  let create_node loc node_kind instrs = Procdesc.create_node procdesc loc node_kind instrs in
  let ret_var = Procdesc.get_ret_var procdesc in
  let ret_type = Procdesc.get_ret_type procdesc in
  let id_ret_val = Ident.create_fresh Ident.knormal in
  (* this is removed in the true branches, and in the false branch of the last handler *)
  let id_exn_val = Ident.create_fresh Ident.knormal in
  let create_entry_node loc =
    let instr_get_ret_val = Sil.Load {id= id_ret_val; e= Exp.Lvar ret_var; typ= ret_type; loc} in
    let instr_deactivate_exn = Sil.Store {e1= Exp.Lvar ret_var; typ= ret_type; e2= Exp.null; loc} in
    let instr_unwrap_ret_val =
      let unwrap_builtin = Exp.Const (Const.Cfun BuiltinDecl.__unwrap_exception) in
      Sil.Call
        ( (id_exn_val, ret_type)
        , unwrap_builtin
        , [(Exp.Var id_ret_val, ret_type)]
        , loc
        , CallFlags.default )
    in
    create_node loc Procdesc.Node.exn_handler_kind
      [instr_get_ret_val; instr_deactivate_exn; instr_unwrap_ret_val]
  in
  let create_entry_block handler_list =
    try ignore (Hashtbl.find catch_block_table handler_list)
    with Caml.Not_found ->
      let collect succ_nodes rethrow_exception handler =
        let catch_nodes = get_body_nodes handler.JBir.e_handler in
        let loc =
          match catch_nodes with
          | n :: _ ->
              Procdesc.Node.get_loc n
          | [] ->
              Location.none context.source_file
        in
        let finally_case () =
          let finally_node = create_node loc (Procdesc.Node.Stmt_node FinallyBranch) [] in
          Procdesc.node_set_succs procdesc finally_node ~normal:catch_nodes ~exn:exit_nodes ;
          [finally_node]
        in
        match handler.JBir.e_catch_type with
        | None ->
            finally_case ()
        | Some exn_class_name when is_java_lang_throwable exn_class_name ->
            finally_case ()
        | Some exn_class_name ->
            let exn_type =
              match
                JTransType.get_class_type context.program (JContext.get_tenv context) exn_class_name
              with
              | {Typ.desc= Tptr (typ, _)} ->
                  typ
              | _ ->
                  assert false
            in
            let id_instanceof = Ident.create_fresh Ident.knormal in
            (* Introduction of nullable flag here is so we can assert/prune that the exception value is >0 in both
               branches of the type test by passing false in the is_true branch and true in the is_false branch.
               Unfortunately that exposes some flakiness in one of the
               build system tests (or test reporting), so we only use true at the moment (which introduces a FP in a pulse test)
               TODO: come back and fix this
            *)
            let instr_call_instanceof ~is_nullable =
              let instanceof_builtin = Exp.Const (Const.Cfun BuiltinDecl.__instanceof) in
              let args =
                [ (Exp.Var id_exn_val, Typ.mk (Tptr (exn_type, Typ.Pk_pointer)))
                ; ( Exp.Sizeof
                      { typ= exn_type
                      ; nbytes= None
                      ; dynamic_length= None
                      ; subtype= Subtype.exact
                      ; nullable= is_nullable }
                  , StdTyp.void ) ]
              in
              Sil.Call
                ( (id_instanceof, Typ.mk (Tint IBool))
                , instanceof_builtin
                , args
                , loc
                , CallFlags.default )
            in
            let if_kind = Sil.Ik_switch in
            let instr_prune_true = Sil.Prune (Exp.Var id_instanceof, loc, true, if_kind) in
            let instr_prune_false =
              Sil.Prune (Exp.UnOp (Unop.LNot, Exp.Var id_instanceof, None), loc, false, if_kind)
            in
            let instr_set_catch_var =
              let catch_var = JContext.set_pvar context handler.JBir.e_catch_var ret_type in
              Sil.Store {e1= Exp.Lvar catch_var; typ= ret_type; e2= Exp.Var id_exn_val; loc}
            in
            let instr_rethrow_exn =
              Sil.Store {e1= Exp.Lvar ret_var; typ= ret_type; e2= Exp.Exn (Exp.Var id_exn_val); loc}
            in
            let node_kind_true =
              Procdesc.Node.Prune_node (true, if_kind, PruneNodeKind_ExceptionHandler)
            in
            let node_kind_false =
              Procdesc.Node.Prune_node (false, if_kind, PruneNodeKind_ExceptionHandler)
            in
            let node_true =
              let instrs_true =
                [instr_call_instanceof ~is_nullable:true; instr_prune_true; instr_set_catch_var]
              in
              create_node loc node_kind_true instrs_true
            in
            let node_false =
              let instrs_false =
                [instr_call_instanceof ~is_nullable:true; instr_prune_false]
                @ if rethrow_exception then [instr_rethrow_exn] else []
              in
              create_node loc node_kind_false instrs_false
            in
            Procdesc.node_set_succs procdesc node_true ~normal:catch_nodes ~exn:exit_nodes ;
            Procdesc.node_set_succs procdesc node_false ~normal:succ_nodes ~exn:exit_nodes ;
            [node_true; node_false]
      in
      let is_last_handler = ref true in
      let process_handler succ_nodes handler =
        (* process handlers starting from the last one *)
        let remove_temps = !is_last_handler in
        (* remove temporary variables on last handler *)
        is_last_handler := false ;
        collect succ_nodes remove_temps handler
      in
      let nodes_first_handler =
        List.fold ~f:process_handler ~init:exit_nodes (List.rev handler_list)
      in
      let loc =
        match nodes_first_handler with
        | n :: _ ->
            Procdesc.Node.get_loc n
        | [] ->
            Location.none context.source_file
      in
      let entry_node = create_entry_node loc in
      Procdesc.node_set_succs procdesc entry_node ~normal:nodes_first_handler ~exn:exit_nodes ;
      Hashtbl.add catch_block_table handler_list [entry_node]
  in
  Hashtbl.iter (fun _ handler_list -> create_entry_block handler_list) handler_table ;
  catch_block_table


let create_exception_handlers context exit_nodes get_body_nodes impl =
  match JBir.exc_tbl impl with
  | [] ->
      fun _ -> exit_nodes
  | _ -> (
      let handler_table = create_handler_table impl in
      let catch_block_table =
        translate_exceptions context exit_nodes get_body_nodes handler_table
      in
      fun pc ->
        try
          let handler_list = Hashtbl.find handler_table pc in
          Hashtbl.find catch_block_table handler_list
        with Caml.Not_found -> exit_nodes )
