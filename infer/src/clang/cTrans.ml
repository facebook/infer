(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Translates instructions: (statements and expressions) from the ast into sil *)

open CLocation
open CContext
open Utils
open CTrans_utils
open CFrontend_utils
open CFrontend_utils.General_utils
open Clang_ast_t
open CFrontend_config

open CTrans_utils.Nodes
module L = Logging

module type CTrans = sig
(** Translates instructions: (statements and expressions) from the ast into sil *)

(** It receives the context, a list of statements and the exit node and it returns a list of cfg nodes *)
(** that reporesent the translation of the stmts into sil. *)
  val instructions_trans : CContext.t -> Clang_ast_t.stmt list -> Cfg.Node.t -> Cfg.Node.t list

  (** It receives the context and a statement and a warning string and returns the translated sil expression *)
  (** that represents the translation of the stmts into sil. *)
  val expression_trans : CContext.t -> Clang_ast_t.stmt -> string -> Sil.exp

end

module CTrans_funct(M: CModule_type.CMethod_declaration) : CTrans =
struct

  let add_autorelease_call context exp typ sil_loc =
    let method_name = Procname.clang_get_method (Cfg.Procdesc.get_proc_name context.procdesc) in
    if !Config.arc_mode &&
    not (CTrans_utils.is_owning_name method_name) &&
    ObjcInterface_decl.is_pointer_to_objc_class context.CContext.tenv typ then
      let fname = SymExec.ModelBuiltins.__set_autorelease_attribute in
      let ret_id = Ident.create_fresh Ident.knormal in
      let stmt_call = Sil.Call([ret_id], (Sil.Const (Sil.Cfun fname)), [(exp, typ)], sil_loc, Sil.cf_default) in
      ([ret_id], [stmt_call])
    else ([], [])

  let rec is_block_expr s =
    match s with
    | BlockExpr _ -> true
    (* the block can be wrapped in ExprWithCleanups  or ImplicitCastExpr*)
    | ImplicitCastExpr(_, [s'], _, _)
    | ExprWithCleanups(_, [s'], _, _) -> is_block_expr s'
    | _ -> false

  let objc_exp_of_type_block fun_exp_stmt =
    let is_block_qt qt =
      match Str.split (Str.regexp_string "(^)") qt.Clang_ast_t.qt_raw with
      | [_; _] -> true
      | _ -> false in
    match fun_exp_stmt with
    | ImplicitCastExpr(_, _, ei, _) when is_block_qt ei.Clang_ast_t.ei_qual_type -> true
    | _ -> false

  (* This function add in tenv a class representing an objc block. *)
  (* An object of this class has type:*)
  (* name_of_block |-> {capture_var1:typ_of_capture_var1,... capture_varn:typ_of_capture_varn} *)
  (* It allocates one element and sets its fields with the current values of the *)
  (* captured variables. This allocated instance is used to detect retain cycles involving the block.*)
  let allocate_block trans_state block_name captured_vars loc =
    let tenv = trans_state.context.tenv in
    let procdesc = trans_state.context.procdesc in
    let procname = Cfg.Procdesc.get_proc_name procdesc in
    let mk_field_from_captured_var (vname, typ, b) =
      let fname = CField_decl.mk_class_field_name block_name (Mangled.to_string vname) in
      let item_annot = Sil.item_annotation_empty in
      fname, typ, item_annot in
    let fields = list_map mk_field_from_captured_var captured_vars in
    let fields = CFrontend_utils.General_utils.sort_fields fields in
    Printing.log_out "Block %s field:\n" block_name;
    list_iter (fun (fn, ft, _) ->
            Printing.log_out "-----> field: '%s'\n" (Ident.fieldname_to_string fn)) fields;
    let mblock = Mangled.from_string block_name in
    let block_type = Sil.Tstruct(fields, [], Sil.Class, Some mblock, [], [], []) in
    let block_name = Sil.TN_csu(Sil.Class, mblock) in
    Sil.tenv_add tenv block_name block_type;
    let trans_res = CTrans_utils.alloc_trans trans_state loc (Ast_expressions.dummy_stmt_info ()) block_type true in
    let id_block = match trans_res.exps with
      | [(Sil.Var id, t)] -> id
      | _ -> assert false in
    let block_var = Sil.mk_pvar mblock procname in
    let declare_block_local =
      Sil.Declare_locals([(block_var, Sil.Tptr(block_type, Sil.Pk_pointer))], loc) in
    (* Adds Nullify of the temp block variable in the predecessors of the exit node. *)
    let pred_exit = Cfg.Node.get_preds (Cfg.Procdesc.get_exit_node procdesc) in
    let block_nullify_instr =
      if pred_exit = [] then
        [Sil.Nullify(block_var, loc, true)]
      else (list_iter (fun n -> let loc = Cfg.Node.get_loc n in
                Cfg.Node.append_instrs_temps n [Sil.Nullify(block_var, loc, true)] []) pred_exit;
        []) in
    let set_instr = Sil.Set(Sil.Lvar block_var, block_type, Sil.Var id_block, loc) in
    let ids, captured_instrs = list_split (list_map (fun (vname, typ, _) ->
                  let id = Ident.create_fresh Ident.knormal in
                  id, Sil.Letderef(id, Sil.Lvar (Sil.mk_pvar vname procname), typ, loc)
            ) captured_vars) in
    let fields_ids = list_combine fields ids in
    let set_fields = list_map (fun ((f, t, _), id) ->
              Sil.Set(Sil.Lfield(Sil.Var id_block, f, block_type), t, Sil.Var id, loc)) fields_ids in
    (declare_block_local :: trans_res.instrs) @ [set_instr] @ captured_instrs @ set_fields @ block_nullify_instr , id_block:: ids

  (* From a list of expression extract blocks from tuples and *)
  (* returns block names and assignment to temp vars  *)
  let extract_block_from_tuple procname exps loc =
    let insts = ref [] in
    let ids = ref [] in
    let is_function_name t e =
      match e with
      | Sil.Const(Sil.Cfun bn) ->
          let bn'= Procname.to_string bn in
          let bn''= Mangled.from_string bn' in
          let block = Sil.Lvar(Sil.mk_pvar bn'' procname) in
          let id = Ident.create_fresh Ident.knormal in
          ids := id::!ids;
          insts := Sil.Letderef(id, block, t, loc)::!insts;
          [(Sil.Var id, t)]
      | _ -> [(e, t)] in
    let get_function_name t el = list_flatten(list_map (is_function_name t) el) in
    let rec f es =
      match es with
      | [] -> []
      | (Sil.Const(Sil.Ctuple el), (Sil.Tptr((Sil.Tfun _), _ ) as t)):: es' ->
          get_function_name t el @ (f es')
      | e:: es' -> e:: f es' in
    (f exps, !insts, !ids)

  (* If e is a block and the calling node has the priority then *)
  (* we need to release the priority to allow*)
  (* creation of nodes inside the block.*)
  (* At the end of block translation, we need to get the proirity back.*)
  (* the parameter f will be called with function instruction *)
  let exec_with_block_priority_exception f trans_state e stmt_info =
    if (is_block_expr e) && (PriorityNode.own_priority_node trans_state.priority stmt_info) then
      f { trans_state with priority = Free } e
    else f trans_state e

  (* This is the standard way of dealing with self:Class or a call [a class]. We translate it as sizeof(<type pf a>) *)
  (* The only time when we want to translate those expressions differently is when they are the first argument of    *)
  (* method calls. In that case they are not translated as expressions, but we take the type and create a static     *)
  (* method call from it. This is done in objcMessageExpr_trans. *)
  let exec_with_self_exception f trans_state stmt =
    try
      f trans_state stmt
    with Self.SelfClassException class_name ->
        let typ = CTypes_decl.type_name_to_sil_type trans_state.context.tenv class_name in
        { empty_res_trans with
          exps = [(Sil.Sizeof(typ, Sil.Subtype.exact), typ)]}

  (* Execute translation of e forcing to release priority (if it's not free) and then setting it back.*)
  (* This is used in conditional operators where we need to force the priority to be free for the *)
  (* computation of the expressions*)
  let exec_with_priority_exception trans_state e f =
    if PriorityNode.is_priority_free trans_state then
      f trans_state e
    else f { trans_state with priority = Free } e

  let breakStmt_trans trans_state =
    match trans_state.continuation with
    | Some bn -> { empty_res_trans with root_nodes = bn.break }
    | _ -> assert false

  let continueStmt_trans trans_state =
    match trans_state.continuation with
    | Some bn -> { empty_res_trans with root_nodes = bn.continue }
    | _ -> assert false

  let stringLiteral_trans trans_state stmt_info expr_info str =
    Printing.log_out "Passing from StringLiteral '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp = Sil.Const (Sil.Cstr (str)) in
    { empty_res_trans with exps = [(exp, typ)]}

  (* FROM CLANG DOCS: "Implements the GNU __null extension, which is a name for a null pointer constant *)
  (* that has integral type (e.g., int or long) and is the same size and alignment as a pointer. The __null *)
  (* extension is typically  only used by system headers, which define NULL as __null in C++ rather than using 0 *)
  (* (which is an integer that may not match the size of a pointer)". So we implement it as the constant zero *)
  let gNUNullExpr_trans trans_state stmt_info expr_info =
    Printing.log_out "Passing from GNUNullExpr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp = Sil.Const (Sil.Cint (Sil.Int.zero)) in
    { empty_res_trans with exps = [(exp, typ)]}

  let nullPtrExpr_trans trans_state stmt_info expr_info =
    Printing.log_out "Passing from nullptr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    { empty_res_trans with exps = [(Sil.exp_null, typ)]}

  let objCSelectorExpr_trans trans_state stmt_info expr_info selector =
    stringLiteral_trans trans_state stmt_info expr_info selector

  let objCEncodeExpr_trans trans_state stmt_info expr_info qual_type =
    Printing.log_out "Passing from ObjCEncodeExpr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    stringLiteral_trans trans_state stmt_info expr_info (CTypes.get_type qual_type)

  let objCProtocolExpr_trans trans_state stmt_info expr_info decl_ref =
    Printing.log_out "Passing from ObjCProtocolExpr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let name = (match decl_ref.Clang_ast_t.dr_name with
        | Some s -> s
        | _ -> "") in
    stringLiteral_trans trans_state stmt_info expr_info name

  let characterLiteral_trans trans_state stmt_info expr_info n =
    Printing.log_out "Passing from CharacterLiteral '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp = Sil.Const (Sil.Cint (Sil.Int.of_int n)) in
    { empty_res_trans with exps = [(exp, typ)]}

  let floatingLiteral_trans trans_state stmt_info expr_info float_string =
    Printing.log_out "Passing from FloatingLiteral '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp = Sil.Const (Sil.Cfloat (float_of_string float_string)) in
    { empty_res_trans with exps = [(exp, typ)]}

  (* Note currently we don't have support for different qual     *)
  (* type like long, unsigned long, etc                                *)
  and integerLiteral_trans trans_state stmt_info expr_info integer_literal_info =
    Printing.log_out "Passing from IntegerLiteral '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp, ids =
      try
        let i = Int64.of_string integer_literal_info.Clang_ast_t.ili_value in
        let exp = Sil.exp_int (Sil.Int.of_int64 i) in
        exp, []
      with
      | Failure _ ->
      (* Parse error: return a nondeterministic value *)
          let id = Ident.create_fresh Ident.knormal in
          let exp = Sil.Var id in
          exp, [id] in
    { empty_res_trans with
      exps = [(exp, typ)];
      ids = ids; }

  let nullStmt_trans succ_nodes stmt_info =
    Printing.log_out "Passing from NullStmt '%s'.\n" stmt_info.Clang_ast_t.si_pointer;
    { empty_res_trans with root_nodes = succ_nodes }

  (* The stmt seems to be always empty *)
  let unaryExprOrTypeTraitExpr_trans trans_state stmt_info expr_info unary_expr_or_type_trait_expr_info =
    Printing.log_out "Passing from UnaryExprOrTypeTraitExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.qual_type_to_sil_type trans_state.context.tenv expr_info.Clang_ast_t.ei_qual_type in
    match unary_expr_or_type_trait_expr_info.Clang_ast_t.uttei_kind with
    | `SizeOf ->
        let qt = Ast_utils.type_from_unary_expr_or_type_trait_expr_info unary_expr_or_type_trait_expr_info in
        let sizeof_typ =
          match qt with
          | Some qt -> CTypes_decl.qual_type_to_sil_type trans_state.context.tenv qt
          | None -> typ in (* Some default type since the type is missing *)
        { empty_res_trans with exps = [(Sil.Sizeof(sizeof_typ, Sil.Subtype.exact), sizeof_typ)]}
    | k -> Printing.log_stats
          "\nWARNING: Missing translation of Uniry_Expression_Or_Trait of kind: %s . Expression ignored, returned -1... \n"
          (Clang_ast_j.string_of_unary_expr_or_type_trait_kind k);
        { empty_res_trans with exps =[(Sil.exp_minus_one, typ)]}

  (* search the label into the hashtbl - create a fake node eventually *)
  (* connect that node with this stmt *)
  let gotoStmt_trans trans_state stmt_info label_name =
    Printing.log_out "\nPassing from `GotoStmt '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    { empty_res_trans with root_nodes = [root_node']; leaf_nodes = trans_state.succ_nodes }

  let declRefExpr_trans trans_state stmt_info expr_info decl_ref_expr_info d =
    Printing.log_out "Passing from DeclRefExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let context = trans_state.context in
    let typ = CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
    let name = get_name_decl_ref_exp_info decl_ref_expr_info stmt_info in
    let procname = Cfg.Procdesc.get_proc_name context.procdesc in
    let is_function =
      match get_decl_kind decl_ref_expr_info with
      | `Function -> true
      | _ -> false in
    if is_enumeration_constant d then (
      let const_exp = (match CTypes.search_enum_type_by_name context.tenv name with
          | Some v ->
              let ce = Sil.Const v in
              Printing.log_out "   ....Found enum constant '%s', " name;
              Printing.log_out "replacing with integer '%s' \n" (Sil.exp_to_string ce); ce
          | None ->
              Printing.log_stats
                "   WARNING: Found enum constant '%s', but its value was not found in the tenv. Returning 0.\n" name;
              (Sil.Const(Sil.Cint Sil.Int.zero))) in
      { root_nodes = []; leaf_nodes = []; ids = []; instrs = []; exps = [(const_exp, typ)]}
    ) else if is_function then (
      let name =
        if name = CFrontend_config.builtin_expect then ("infer"^CFrontend_config.builtin_expect)
        else name in
      let qt = CTypes.get_raw_qual_type_decl_ref_exp_info decl_ref_expr_info in
      let pname, type_opt =
        match qt with
        | Some v ->
            CMethod_trans.mk_procname_from_function name v, CTypes_decl.parse_func_type name v
        | None -> Procname.from_string name, None in
      CMethod_trans.create_external_procdesc context.cfg pname false type_opt;
      let address_of_function = not context.CContext.is_callee_expression in
      (* If we are not translating a callee expression, then the address of the function is being taken.*)
      (* As e.g. in fun_ptr = foo; *)
      let non_mangled_func_name =
        if name = CFrontend_config.malloc &&
        (!CFrontend_config.language = CFrontend_config.OBJC ||
          !CFrontend_config.language = CFrontend_config.OBJCPP) then
          SymExec.ModelBuiltins.malloc_no_fail
        else Procname.from_string name in
      let is_builtin = SymExec.function_is_builtin non_mangled_func_name in
      if is_builtin then (* malloc, free, exit, scanf, ... *)
      { empty_res_trans with exps = [(Sil.Const (Sil.Cfun non_mangled_func_name), typ)]}
      else
        begin
          if address_of_function then Cfg.set_procname_priority context.cfg pname;
          { empty_res_trans with exps = [(Sil.Const (Sil.Cfun pname), typ)]}
        end
    ) else (
      let pvar =
        if not (Utils.string_is_prefix pointer_prefix stmt_info.si_pointer) then
          try
            CContext.LocalVars.find_var_with_pointer context stmt_info.Clang_ast_t.si_pointer
          with _ -> assert false
        else Sil.mk_pvar (Mangled.from_string name) procname in
      let e = Sil.Lvar pvar in
      let exps =
        if Self.is_var_self pvar (CContext.is_objc_method context) then
          if (CTypes.is_class typ) then
            raise (Self.SelfClassException (CContext.get_curr_class_name trans_state.context.curr_class))
          else
            let typ = CTypes.add_pointer_to_typ
                (CTypes_decl.get_type_curr_class context.tenv (CContext.get_curr_class context)) in
            [(e, typ)]
        else [(e, typ)] in
      Printing.log_out "\n\n PVAR ='%s'\n\n" (Sil.pvar_to_string pvar);
      { empty_res_trans with exps = exps }
    )

  let rec labelStmt_trans trans_state stmt_info stmt_list label_name =
    Printing.log_out "\nPassing from `LabelStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    (* go ahead with the translation *)
    let res_trans = match stmt_list with
      | [stmt] ->
          instruction trans_state stmt
      | _ -> assert false (* expected a stmt or at most a compoundstmt *) in
    (* create the label root node into the hashtbl *)
    let sil_loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    Cfg.Node.set_succs_exn root_node' res_trans.root_nodes [];
    { empty_res_trans with root_nodes = [root_node']; leaf_nodes = trans_state.succ_nodes }

  and arraySubscriptExpr_trans trans_state stmt_info expr_info stmt_list =
    Printing.log_out
      "Passing from ArraySubscriptExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let array_stmt, idx_stmt = (match stmt_list with
        | [a; i] -> a, i  (* Assumption: the statement list contains 2 elements,
        the first is the array expr and the second the index *)
        | _ -> assert false) in (* Let's get notified if the assumption is wrong...*)
    let line_number = get_line stmt_info trans_state.parent_line_number in
    let trans_state'= { trans_state with parent_line_number = line_number } in
    let res_trans_a = instruction trans_state' array_stmt in
    let res_trans_idx = instruction trans_state' idx_stmt in
    let (a_exp, a_typ) = extract_exp_from_list res_trans_a.exps
        "WARNING: In ArraySubscriptExpr there was a problem in translating array exp.\n" in
    let (i_exp, i_typ) = extract_exp_from_list res_trans_idx.exps
        "WARNING: In ArraySubscriptExpr there was a problem in translating index exp.\n" in
    let array_exp = Sil.Lindex (a_exp, i_exp) in

    let root_nodes =
      if res_trans_a.root_nodes <> []
      then res_trans_a.root_nodes
      else res_trans_idx.root_nodes in
    let leaf_nodes =
      if res_trans_idx.leaf_nodes <> []
      then res_trans_idx.leaf_nodes
      else res_trans_a.leaf_nodes in

    if res_trans_idx.root_nodes <> []
    then
      list_iter
        (fun n -> Cfg.Node.set_succs_exn n res_trans_idx.root_nodes [])
        res_trans_a.leaf_nodes;

    (* Note the order of res_trans_idx.ids @ res_trans_a.ids is important. *)
    (* We expect to use only res_trans_idx.ids in construction of other operation.                  *)
    (* res_trans_a.ids is passed to be Removed.*)
    { root_nodes = root_nodes;
      leaf_nodes = leaf_nodes;
      ids = res_trans_idx.ids @ res_trans_a.ids;
      instrs = res_trans_a.instrs @ res_trans_idx.instrs;
      exps = [(array_exp, typ)]}

  and binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list =
    let bok = (Clang_ast_j.string_of_binary_operator_kind binary_operator_info.Clang_ast_t.boi_kind) in
    Printing.log_out "Passing from BinaryOperator '%s' " bok;
    Printing.log_out "pointer = '%s'  " stmt_info.Clang_ast_t.si_pointer;
    Printing.log_out "priority node free = '%s'.\n" (string_of_bool (PriorityNode.is_priority_free trans_state));
    let context = trans_state.context in
    let parent_line_number = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = get_sil_location stmt_info parent_line_number context in
    let typ = CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
    (match stmt_list with
      | [s1; ImplicitCastExpr (stmt, [CompoundLiteralExpr (cle_stmt_info, stmts, expr_info)], _, cast_expr_info)] ->
          let di, line_number = get_decl_ref_info s1 parent_line_number in
          let line_number = get_line cle_stmt_info line_number in
          let trans_state' = { trans_state with parent_line_number = line_number } in
          let res_trans_tmp = initListExpr_trans trans_state' stmt_info expr_info di stmts in
          { res_trans_tmp with leaf_nodes =[]}
      | [s1; s2] -> (* Assumption: We expect precisely 2 stmt corresponding to the 2 operands*)
          let rhs_owning_method = CTrans_utils.is_owning_method s2 in
          (* NOTE: we create a node only if required. In that case this node *)
          (* becomes the successor of the nodes that may be created when     *)
          (* translating the operands.                                       *)
          let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
          let line_number = get_line stmt_info parent_line_number in
          let trans_state'' = { trans_state_pri with parent_line_number = line_number; succ_nodes =[]} in
          let res_trans_e1 = exec_with_self_exception instruction trans_state'' s1 in
          let res_trans_e2 =
            (* translation of s2 is done taking care of block special case *)
            exec_with_block_priority_exception (exec_with_self_exception instruction) trans_state'' s2 stmt_info in
          let (sil_e1, sil_typ1) = extract_exp_from_list res_trans_e1.exps "\nWARNING: Missing LHS operand in BinOp. Returning -1. Fix needed...\n" in
          let (sil_e2, sil_typ2) = extract_exp_from_list res_trans_e2.exps "\nWARNING: Missing RHS operand in BinOp. Returning -1. Fix needed...\n" in
          let exp_op, instr, ids_bin =
            CArithmetic_trans.binary_operation_instruction context binary_operator_info sil_e1 typ sil_e2 sil_loc rhs_owning_method in
          let instrs = res_trans_e1.instrs@res_trans_e2.instrs@instr in
          let ids = res_trans_e1.ids@res_trans_e2.ids@ids_bin in

          (* Create a node if the priority if free and there are instructions *)
          let creating_node =
            (PriorityNode.own_priority_node trans_state_pri.priority stmt_info) &&
            (list_length instrs >0) in

          let instrs_after_assign, assign_ids, exp_to_parent =
            if (is_binary_assign_op binary_operator_info)
            && ((not creating_node) || (is_return_temp trans_state.continuation)) then (
              (* We are in this case when an assignment is inside        *)
              (* another operator that creates a node. Eg. another       *)
              (* assignment.  *)
              (* As no node is created here ids are passed to the parent *)
              let id = Ident.create_fresh Ident.knormal in
              let res_instr = [Sil.Letderef (id, sil_e1, sil_typ1, sil_loc)] in
              instrs@res_instr, ids@[id], Sil.Var id
            ) else (
              instrs, ids, exp_op) in

          let instruction_to_ancestor, ids_to_ancestor, succ_nodes' =
            if creating_node then (
              let node_kind =
                Cfg.Node.Stmt_node ("BinaryOperatorStmt: "^
                  (CArithmetic_trans.bin_op_to_string binary_operator_info)) in
              let node_bin_op = create_node node_kind [] [] sil_loc context in
              Cfg.Node.set_succs_exn node_bin_op succ_nodes [];
              let succ_nodes'' = [node_bin_op] in
              (* If a node was created, ids are passed to the parent*)
              (* if the binop is in the translation of a condition.*)
              (* Otherwise ids are added to the node. *)
              (* ids_parent/ids_nodes are the list of ids for the parent/node respectively.*)
              (* They are computed with continuation which tells us *)
              (* if we are translating a condition or not *)
              let ids_parent = ids_to_parent trans_state.continuation assign_ids in
              let ids_node = ids_to_node trans_state.continuation assign_ids in
              list_iter (fun n -> Cfg.Node.append_instrs_temps n instrs_after_assign ids_node) succ_nodes'';
              [], ids_parent, succ_nodes''
            ) else (
              instrs_after_assign, assign_ids, succ_nodes) in

          let e1_has_nodes = res_trans_e1.root_nodes <> []
          and e2_has_nodes = res_trans_e2.root_nodes <> [] in

          let e1_succ_nodes =
            if e2_has_nodes then res_trans_e2.root_nodes else succ_nodes' in
          list_iter (fun n -> Cfg.Node.set_succs_exn n e1_succ_nodes []) res_trans_e1.leaf_nodes;
          list_iter (fun n -> Cfg.Node.set_succs_exn n succ_nodes' []) res_trans_e2.leaf_nodes;

          let root_nodes_to_ancestor = match e1_has_nodes, e2_has_nodes with
            | false, false -> succ_nodes'
            | true, _ -> res_trans_e1.root_nodes
            | false, true -> res_trans_e2.root_nodes in

          let leaf_nodes_to_ancestor =
            if creating_node then succ_nodes'
            else if e2_has_nodes then res_trans_e2.leaf_nodes
            else res_trans_e1.leaf_nodes in

          Printing.log_out "....BinaryOperator '%s' " bok;
          Printing.log_out "has ids_to_ancestor |ids_to_ancestor|=%s "
            (string_of_int (list_length ids_to_ancestor));
          Printing.log_out " |nodes_e1|=%s .\n"
            (string_of_int (list_length res_trans_e1.root_nodes));
          Printing.log_out " |nodes_e2|=%s .\n"
            (string_of_int (list_length res_trans_e2.root_nodes));
          list_iter (fun id -> Printing.log_out "     ... '%s'\n"
                    (Ident.to_string id)) ids_to_ancestor;
          { root_nodes = root_nodes_to_ancestor;
            leaf_nodes = leaf_nodes_to_ancestor;
            ids = ids_to_ancestor;
            instrs = instruction_to_ancestor;
            exps = [(exp_to_parent, sil_typ1)] }
      | _ -> assert false) (* Binary operator should have two operands*)

  and callExpr_trans trans_state si stmt_list expr_info =
    let pln = trans_state.parent_line_number in
    let context = trans_state.context in
    let function_type = CTypes_decl.get_type_from_expr_info expr_info context.tenv in
    Printing.log_out "Passing from CallExpr '%s'.\n" si.Clang_ast_t.si_pointer;
    let procname = Cfg.Procdesc.get_proc_name context.procdesc in
    let sil_loc = get_sil_location si pln context in
    let fun_exp_stmt, params_stmt = (match stmt_list with (* First stmt is the function expr and the rest are params*)
        | fe:: params -> fe, params
        | _ -> assert false) in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    (* claim priority if no ancestors has claimed priority before *)
    let line_number = get_line si pln in
    let context_callee = { context with CContext.is_callee_expression = true } in
    let trans_state_callee = { trans_state_pri with context = context_callee; parent_line_number = line_number; succ_nodes = []} in
    let is_call_to_block = objc_exp_of_type_block fun_exp_stmt in
    let res_trans_callee = instruction trans_state_callee fun_exp_stmt in
    (* As we may have nodes coming from different parameters we need to  *)
    (* call instruction for each parameter and collect the results       *)
    (* afterwards. The 'instructions' function does not do that          *)
    let trans_state_param =
      { trans_state_pri with parent_line_number = line_number; succ_nodes = [] } in
    let res_trans_par =
      let l = list_map (fun i -> exec_with_self_exception instruction trans_state_param i) params_stmt in
      let rt = collect_res_trans (res_trans_callee :: l) in
      { rt with exps = list_tl rt.exps } in
    let (sil_fe, typ_fe) = extract_exp_from_list res_trans_callee.exps
        "WARNING: The translation of fun_exp did not return an expression. Returning -1. NEED TO BE FIXED" in
    let sil_fe, is_cf_retain_release = CTrans_models.builtin_predefined_model fun_exp_stmt sil_fe in
    if CTrans_models.is_assert_log sil_fe then
      if Config.report_assertion_failure then
        CTrans_utils.trans_assertion_failure sil_loc context
      else
        CTrans_utils.trans_assume_false sil_loc context trans_state.succ_nodes
    else
      let callee_pname_opt =
        match sil_fe with
        | Sil.Const (Sil.Cfun pn) ->
            Some pn
        | _ -> None (* function pointer *) in
      let act_params = if list_length res_trans_par.exps = list_length params_stmt then
          res_trans_par.exps
        else (Printing.log_err
            "WARNING: stmt_list and res_trans_par.exps must have same size. NEED TO BE FIXED\n\n";
          fix_param_exps_mismatch params_stmt res_trans_par.exps) in
      let act_params = if is_cf_retain_release then
          (Sil.Const (Sil.Cint Sil.Int.one), Sil.Tint Sil.IBool):: act_params
        else act_params in
      match CTrans_utils.builtin_trans trans_state_pri sil_loc si function_type callee_pname_opt with
      | Some builtin -> builtin
      | None ->
          let ret_id, call_instr =
            match cast_trans context act_params sil_loc callee_pname_opt function_type with
            | Some (id, instr, _) -> [id], instr
            | None ->
                let ret_id = if (Sil.typ_equal function_type Sil.Tvoid) then []
                  else [Ident.create_fresh Ident.knormal] in
                let call_flags = { Sil.cf_virtual = false; Sil.cf_noreturn = false; Sil.cf_is_objc_block = is_call_to_block; } in
                let call_instr = Sil.Call(ret_id, sil_fe, act_params, sil_loc, call_flags) in
                ret_id, call_instr in
          let ids = res_trans_par.ids@ret_id in
          let instrs = res_trans_par.instrs @ [call_instr] in
          let nname = "Call "^(Sil.exp_to_string sil_fe) in
          let res_trans_tmp = { res_trans_par with ids = ids; instrs = instrs; exps =[]} in
          let res_trans_to_parent =
            PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname si res_trans_tmp in
          (match callee_pname_opt with
            | Some callee_pname ->
                if not (SymExec.function_is_builtin callee_pname) then
                  begin
                    Cg.add_edge context.cg procname callee_pname;
                    try
                      let callee_ms = CMethod_signature.find callee_pname in
                      CMethod_trans.create_local_procdesc context.cfg context.tenv callee_ms [] [] false
                    with Not_found ->
                        CMethod_trans.create_external_procdesc context.cfg callee_pname false None
                  end
            | None -> ());
          (match ret_id with
            | [] -> { res_trans_to_parent with exps =[] }
            | [ret_id'] -> { res_trans_to_parent with exps =[(Sil.Var ret_id', function_type)] }
            | _ -> assert false) (* by construction of red_id, we cannot be in this case *)

  and objCMessageExpr_trans trans_state si obj_c_message_expr_info stmt_list expr_info =
    Printing.log_out "Passing from ObjMessageExpr '%s'.\n" si.Clang_ast_t.si_pointer;
    let context = trans_state.context in
    let parent_line_number = trans_state.parent_line_number in
    let sil_loc = get_sil_location si parent_line_number context in
    let selector, receiver_kind = get_selector_receiver obj_c_message_expr_info in
    let is_alloc_or_new = (selector = CFrontend_config.alloc) || (selector = CFrontend_config.new_str) in
    Printing.log_out "\n!!!!!!! Calling with selector = '%s' " selector;
    Printing.log_out "    receiver_kind= '%s'\n\n" (Clang_ast_j.string_of_receiver_kind receiver_kind);
    let method_type = CTypes_decl.get_type_from_expr_info expr_info context.tenv in
    let ret_id = if Sil.typ_equal method_type Sil.Tvoid then []
      else [Ident.create_fresh Ident.knormal] in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let line_number = get_line si parent_line_number in
    let trans_state_param =
      { trans_state_pri with parent_line_number = line_number; succ_nodes = [] } in
    let obj_c_message_expr_info, res_trans_par =
      (match stmt_list with
        | stmt:: rest ->
            let obj_c_message_expr_info, fst_res_trans =
              (try
                let fst_res_trans = instruction trans_state_param stmt in
                obj_c_message_expr_info, fst_res_trans
              with Self.SelfClassException class_name ->
                  let obj_c_message_expr_info = Ast_expressions.make_obj_c_message_expr_info_class selector class_name in
                  obj_c_message_expr_info, empty_res_trans) in
            let l = list_map (fun i -> exec_with_self_exception instruction trans_state_param i) rest in
            obj_c_message_expr_info, collect_res_trans (fst_res_trans :: l)
        | [] -> obj_c_message_expr_info, empty_res_trans) in
    let (class_type, _, _) = CMethod_trans.get_class_selector_instance context obj_c_message_expr_info res_trans_par.exps in
    if (selector = CFrontend_config.class_method && CTypes.is_class method_type) then
      raise (Self.SelfClassException class_type)
    else if is_alloc_or_new then
      new_or_alloc_trans trans_state_pri sil_loc si class_type selector
    else if (CTrans_models.is_handleFailureInMethod selector) then
      if Config.report_assertion_failure then
        CTrans_utils.trans_assertion_failure sil_loc context
      else
        CTrans_utils.trans_assume_false sil_loc context trans_state.succ_nodes
    else
      let procname = Cfg.Procdesc.get_proc_name context.procdesc in
      let callee_name, method_call_type =
        CMethod_trans.get_callee_objc_method context obj_c_message_expr_info res_trans_par.exps in
      let res_trans_par = Self.add_self_parameter_for_super_instance context procname sil_loc
          obj_c_message_expr_info res_trans_par in
      let is_virtual = method_call_type = CMethod_trans.MCVirtual in
      Cg.add_edge context.cg procname callee_name;
      let call_flags = { Sil.cf_virtual = is_virtual; Sil.cf_noreturn = false; Sil.cf_is_objc_block = false; } in
      let param_exps, instr_block_param, ids_block_param = extract_block_from_tuple procname res_trans_par.exps sil_loc in
      let stmt_call = Sil.Call(ret_id, (Sil.Const (Sil.Cfun callee_name)), param_exps, sil_loc, call_flags) in
      let nname = "Message Call: "^selector in
      let res_trans_tmp = {
        res_trans_par with
        ids = ret_id @ res_trans_par.ids @ids_block_param ;
        instrs = res_trans_par.instrs@instr_block_param@[stmt_call];
        exps =[]
      } in
      let res_trans_to_parent =
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname si res_trans_tmp in
      (match ret_id with
        | [] -> { res_trans_to_parent with exps = [] }
        | [ret_id'] -> { res_trans_to_parent with exps = [(Sil.Var ret_id', method_type)] }
        | _ -> assert false) (* by construction of red_id, we cannot be in this case *)

  and dispatch_function_trans trans_state stmt_info stmt_list ei n =
    Printing.log_out "\n Call to a dispatch function treated as special case...\n";
    let procname = Cfg.Procdesc.get_proc_name trans_state.context.procdesc in
    let pvar = CFrontend_utils.General_utils.get_next_block_pvar procname in
    CContext.LocalVars.add_pointer_var stmt_info.Clang_ast_t.si_pointer pvar trans_state.context;
    let transformed_stmt, qt =
      Ast_expressions.translate_dispatch_function (Sil.pvar_to_string pvar) stmt_info stmt_list ei n in
    let typ = CTypes_decl.qual_type_to_sil_type trans_state.context.tenv qt in
    let loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let res_state = instruction trans_state transformed_stmt in
    (* Add declare locals to the first node *)
    list_iter (fun n -> Cfg.Node.prepend_instrs_temps n [Sil.Declare_locals([(pvar, typ)], loc)] []) res_state.root_nodes;
    let preds = list_flatten (list_map (fun n -> Cfg.Node.get_preds n) trans_state.succ_nodes) in
    (* Add nullify of the temp block var to the last node (predecessor or the successor nodes)*)
    list_iter (fun n -> Cfg.Node.append_instrs_temps n [Sil.Nullify(pvar, loc, true)] []) preds;
    res_state

  and compoundStmt_trans trans_state stmt_info stmt_list =
    Printing.log_out "Passing from CompoundStmt '%s'.\n" stmt_info.Clang_ast_t.si_pointer;
    let line_number = get_line stmt_info trans_state.parent_line_number in
    let trans_state' = { trans_state with parent_line_number = line_number } in
    instructions trans_state' (list_rev stmt_list)

  and conditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let parent_line_number = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    let procname = Cfg.Procdesc.get_proc_name context.procdesc in
    let mk_temp_var id =
      Sil.mk_pvar (Mangled.from_string ("SIL_temp_conditional___"^(string_of_int id))) procname in
    Printing.log_out "Passing from ConditionalOperator '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info parent_line_number context in
    let line_number = get_line stmt_info parent_line_number in
    (* We have two different kind of join type for conditional operator. *)
    (* If it's a simple conditional operator then we use a standard join *)
    (* node. If it's a nested conditional operator then we need to       *)
    (* assign the temp variable of the inner conditional to the outer    *)
    (* conditional. In that case we use a statement node for join. This   *)
    (* node make the assignment of the temp variables.                   *)
    let compute_join_node typ =
      let join_node' = match succ_nodes with
        | [n] when is_join_node n ->
            let n'= create_node (Cfg.Node.Stmt_node "Temp Join Node") [] [] sil_loc context in
            let id = Ident.create_fresh Ident.knormal in
            let pvar = mk_temp_var (Cfg.Node.get_id n) in
            let pvar' = mk_temp_var (Cfg.Node.get_id n') in
            let ilist =[Sil.Letderef (id, Sil.Lvar pvar', typ, sil_loc);
              Sil.Declare_locals([(pvar, typ)], sil_loc);
              Sil.Set (Sil.Lvar pvar, typ, Sil.Var id, sil_loc);
              Sil.Nullify(pvar', sil_loc, true)] in
            Cfg.Node.append_instrs_temps n' ilist [id]; n'
        | _ -> create_node (Cfg.Node.Join_node) [] [] sil_loc context in
      Cfg.Node.set_succs_exn join_node' succ_nodes [];
      join_node' in
    let do_branch branch stmt typ prune_nodes join_node pvar =
      let trans_state' = { trans_state with succ_nodes = [join_node]; parent_line_number = line_number } in
      let res_trans_b =
        exec_with_priority_exception trans_state' stmt instruction in
      let node_b = res_trans_b.root_nodes in
      let (e', e'_typ) = extract_exp_from_list res_trans_b.exps
          "\nWARNING: Missing branch expression for Conditional operator. Need to be fixed\n" in
      (* If e' is the address of a prog var, we need to get its value in a temp before assign it to temp_var*)
      let e'', instr_e'', id_e'' = match e' with
        | Sil.Lvar _ ->
            let id = Ident.create_fresh Ident.knormal in
            Sil.Var id,[Sil.Letderef (id, e', typ, sil_loc)], [id]
        | _ -> e', [], [] in
      let set_temp_var = [Sil.Declare_locals([(pvar, typ)], sil_loc); Sil.Set (Sil.Lvar pvar, typ, e'', sil_loc)] in
      let nodes_branch = (match node_b, is_join_node join_node with
          | [], _ -> let n = create_node (Cfg.Node.Stmt_node "ConditinalStmt Branch" ) (res_trans_b.ids@id_e'') (res_trans_b.instrs @ instr_e'' @ set_temp_var) sil_loc context in
              Cfg.Node.set_succs_exn n [join_node] [];
              [n]
          | _, true ->
              list_iter
                (fun n' ->
                  (* If there is a node with instructions we need to only *)
                  (* add the set of the temp variable *)
                      if not (is_prune_node n') then
                        Cfg.Node.append_instrs_temps n'
                          (res_trans_b.instrs @ instr_e''@ set_temp_var)
                          (res_trans_b.ids @ id_e'')
                ) node_b;
              node_b
          | _, false -> node_b) in
      let prune_nodes_t, prune_nodes_f = list_partition is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      list_iter (fun n -> Cfg.Node.set_succs_exn n nodes_branch []) prune_nodes' in
    (match stmt_list with
      | [cond; exp1; exp2] ->
          let typ =
            CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
          let join_node = compute_join_node typ in
          let pvar = mk_temp_var (Cfg.Node.get_id join_node) in
          let continuation' = mk_cond_continuation trans_state.continuation in
          let trans_state' = { trans_state with continuation = continuation'; parent_line_number = line_number; succ_nodes =[]} in
          let res_trans_cond = exec_with_priority_exception trans_state' cond cond_trans in
          (* Note: by contruction prune nodes are leafs_nodes_cond *)
          do_branch true exp1 typ res_trans_cond.leaf_nodes join_node pvar;
          do_branch false exp2 typ res_trans_cond.leaf_nodes join_node pvar;
          let id = Ident.create_fresh Ident.knormal in
          let instrs =[Sil.Letderef (id, Sil.Lvar pvar, typ, sil_loc); Sil.Nullify (pvar, sil_loc, true)] in
          { root_nodes = res_trans_cond.root_nodes;
            leaf_nodes = [join_node];
            ids = [id];
            instrs = instrs;
            exps = [(Sil.Var id, typ)]
          }
      | _ -> assert false)

  (* Translate a condition for if/loops statement. It shorts-circuit and/or. *)
  (* The invariant is that the translation of a condition always contains (at least) *)
  (* the prune nodes. Moreover these are always the leaf nodes of the translation. *)
  and cond_trans trans_state cond =
    let context = trans_state.context in
    let parent_line_number = trans_state.parent_line_number in
    let si, _ = Clang_ast_proj.get_stmt_tuple cond in
    let sil_loc = get_sil_location si parent_line_number context in
    let mk_prune_node b e ids ins =
      create_prune_node b e ids ins sil_loc (Sil.Ik_if) context in
    let extract_exp el =
      extract_exp_from_list el
        "\nWARNING: Missing expression for Conditional operator. Need to be fixed" in
    (* this function translate cond without doing shortcircuit *)
    let no_short_circuit_cond () =
      Printing.log_out " No short-circuit condition\n";
      let res_trans_cond =
        if is_null_stmt cond then {
            empty_res_trans with exps = [(Sil.Const (Sil.Cint Sil.Int.one), (Sil.Tint Sil.IBool))]
          }
        (* Assumption: If it's a null_stmt, it is a loop with no bound, so we set condition to 1 *)
        else
          instruction trans_state cond in
      let e', instrs' = define_condition_side_effects context res_trans_cond.exps res_trans_cond.instrs sil_loc in
      let prune_t = mk_prune_node true e' res_trans_cond.ids instrs' in
      let prune_f = mk_prune_node false e' res_trans_cond.ids instrs' in
      list_iter (fun n' -> Cfg.Node.set_succs_exn n' [prune_t; prune_f] []) res_trans_cond.leaf_nodes;
      let rnodes = if (list_length res_trans_cond.root_nodes) = 0 then
          [prune_t; prune_f]
        else res_trans_cond.root_nodes in
      { root_nodes = rnodes; leaf_nodes =[prune_t; prune_f]; ids = res_trans_cond.ids; instrs = instrs'; exps = e' } in

    (* This function translate (s1 binop s2) doing shortcircuit for '&&' and '||' *)
    (* At the high level it does cond_trans s1; cond_trans s2; glue_nodes *)
    (* The glue_nodes partitions the prune nodes of s1's translation.*)
    (* Some of them need to go to the statement to be executed after the *)
    (* condition (prune_to_short_c) and others to the root nodes of the *)
    (* translation of s2 (i.e., the case when we need to fully evaluate*)
    (* the condition to decide its truth value). *)
    let short_circuit binop s1 s2 =
      let res_trans_s1 = cond_trans trans_state s1 in
      let prune_nodes_t, prune_nodes_f = list_partition is_true_prune_node res_trans_s1.leaf_nodes in
      let res_trans_s2 = cond_trans trans_state s2 in
      (* prune_to_s2 is the prune node that is connected with the root node of the *)
      (* translation of s2.*)
      (* prune_to_short_c is the prune node that is connected directly with the branch *)
      (* where the control flow goes in case of short circuit *)
      let prune_to_s2, prune_to_short_c = (match binop with
          | Sil.LAnd -> prune_nodes_t, prune_nodes_f
          | Sil.LOr -> prune_nodes_f, prune_nodes_t
          | _ -> assert false) in
      list_iter (fun n -> Cfg.Node.set_succs_exn n res_trans_s2.root_nodes []) prune_to_s2;
      let root_nodes_to_parent =
        if (list_length res_trans_s1.root_nodes) = 0 then res_trans_s1.leaf_nodes else res_trans_s1.root_nodes in
      let (exp1, typ1) = extract_exp res_trans_s1.exps in
      let (exp2, typ2) = extract_exp res_trans_s2.exps in
      let e_cond = Sil.BinOp (binop, exp1, exp2) in
      { root_nodes = root_nodes_to_parent;
        leaf_nodes = prune_to_short_c@res_trans_s2.leaf_nodes;
        ids = res_trans_s1.ids@res_trans_s2.ids;
        instrs = res_trans_s1.instrs@res_trans_s2.instrs;
        exps = [(e_cond, typ1)] } in
    Printing.log_out "Translating Condition for Conditional/Loop \n";
    match cond with
    | BinaryOperator(si, [s1; s2], expr_info, boi) ->
        (match boi.Clang_ast_t.boi_kind with
          | `LAnd -> short_circuit (Sil.LAnd) s1 s2
          | `LOr -> short_circuit (Sil.LOr) s1 s2
          | _ -> no_short_circuit_cond ())
    | ParenExpr(_,[s], _) -> (* condition can be wrapped in parenthesys *)
        cond_trans trans_state s
    | _ -> no_short_circuit_cond ()

  and ifStmt_trans trans_state stmt_info stmt_list =
    Printing.log_out "Passing from IfStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = get_sil_location stmt_info pln context in
    let line_number = get_line stmt_info pln in
    let join_node = create_node (Cfg.Node.Join_node) [] [] sil_loc context in
    Cfg.Node.set_succs_exn join_node succ_nodes [];
    let trans_state' = { trans_state with parent_line_number = line_number; succ_nodes = [join_node]} in
    let do_branch branch stmt_branch prune_nodes =
      (* leaf nodes are ignored here as they will be already attached to join_node *)
      let res_trans_b = instruction trans_state' stmt_branch in
      let nodes_branch = (match res_trans_b.root_nodes with
          | [] -> [create_node (Cfg.Node.Stmt_node "IfStmt Branch" ) res_trans_b.ids res_trans_b.instrs sil_loc context]
          | _ -> res_trans_b.root_nodes) in
      let prune_nodes_t, prune_nodes_f = list_partition is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      list_iter (fun n -> Cfg.Node.set_succs_exn n nodes_branch []) prune_nodes';
      res_trans_b.ids in
    (match stmt_list with
      | [null_stmt; cond; stmt1; stmt2] -> (* Note: for the moment we don't do anything with the null_stmt/decl*)
      (* set the flat to inform that we are translating a condition of a if *)
          let continuation' = mk_cond_continuation trans_state.continuation in
          let trans_state'' = { trans_state with continuation = continuation'; succ_nodes = []; parent_line_number = line_number } in
          let res_trans_cond = cond_trans trans_state'' cond in
          (* Note: by contruction prune nodes are leafs_nodes_cond *)
          let ids_t = do_branch true stmt1 res_trans_cond.leaf_nodes in
          let ids_f = do_branch false stmt2 res_trans_cond.leaf_nodes in
          { root_nodes = res_trans_cond.root_nodes; leaf_nodes =[join_node]; ids = res_trans_cond.ids@ids_t@ids_f; instrs =[]; exps =[] }
      | _ -> assert false)

  (* Assumption: the CompoundStmt can be made of different stmts, not just CaseStmts *)
  and switchStmt_trans trans_state stmt_info switch_stmt_list =
    Printing.log_out "\nPassing from `SwitchStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    let continuation = trans_state.continuation in
    let sil_loc = get_sil_location stmt_info pln context in
    (match switch_stmt_list with
      | [_; cond; CompoundStmt(stmt_info, stmt_list)] ->
          let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
          let trans_state' ={ trans_state_pri with succ_nodes = []} in
          let res_trans_cond = instruction trans_state' cond in
          let switch_special_cond_node =
            create_node (Cfg.Node.Stmt_node "Switch_stmt") [] res_trans_cond.instrs sil_loc context in
          let trans_state_no_pri = if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then
              { trans_state_pri with priority = Free }
            else trans_state_pri in
          let (switch_e_cond', switch_e_cond'_typ) =
            extract_exp_from_list res_trans_cond.exps
              "\nWARNING: The condition of the SwitchStmt is not singleton. Need to be fixed\n" in
          let switch_exit_point = succ_nodes in
          let continuation' =
            match continuation with
            | Some cont -> Some { cont with break = switch_exit_point }
            | None -> Some { break = switch_exit_point; continue = []; return_temp = false } in
          let trans_state'' = { trans_state_no_pri with continuation = continuation'} in
          let merge_into_cases stmt_list = (* returns list_of_cases * before_any_case_instrs *)
            let rec aux rev_stmt_list acc cases =
              (match rev_stmt_list with
                | CaseStmt(info, a :: b :: (CaseStmt x) :: c) :: rest -> (* case x: case y: ... *)
                    if c <> [] then assert false; (* empty case with nested case, then followed by some instructions *)
                    let rest' = [CaseStmt(info, a :: b :: [])] @ rest in
                    let rev_stmt_list' = (CaseStmt x) :: rest' in
                    aux rev_stmt_list' acc cases
                | CaseStmt(info, a :: b :: (DefaultStmt x) :: c) :: rest ->
                (* case x: default: ... *)
                    if c <> [] then assert false; (* empty case with nested case, then followed by some instructions *)
                    let rest' = [CaseStmt(info, a :: b :: [])] @ rest in
                    let rev_stmt_list' = (DefaultStmt x) :: rest' in
                    aux rev_stmt_list' acc cases
                | DefaultStmt(info, (CaseStmt x) :: c) :: rest -> (* default: case x: ... *)
                    if c <> [] then assert false; (* empty case with nested case, then followed by some instructions *)
                    let rest' = [DefaultStmt(info, [])] @ rest in
                    let rev_stmt_list' = (CaseStmt x) :: rest' in
                    aux rev_stmt_list' acc cases
                | CaseStmt(info, a :: b :: c) :: rest ->
                    aux rest [] (CaseStmt(info, a :: b :: c@acc):: cases)
                | DefaultStmt(info, c) :: rest -> (* default is always the last in the list *)
                    aux rest [] (DefaultStmt(info, c@acc) :: cases)
                | x :: rest ->
                    aux rest (x:: acc) cases
                | [] ->
                    cases, acc) in
            aux (list_rev stmt_list) [] [] in
          let list_of_cases, pre_case_stmts = merge_into_cases stmt_list in
          let rec connected_instruction rev_instr_list successor_nodes =
            (* returns the entry point of the translated set of instr *)
            match rev_instr_list with
            | [] -> successor_nodes
            | instr :: rest ->
                let trans_state''' = { trans_state'' with succ_nodes = successor_nodes } in
                let res_trans_instr = instruction trans_state''' instr in
                let instr_entry_points = res_trans_instr.root_nodes in
                connected_instruction rest instr_entry_points in
          let rec translate_and_connect_cases cases next_nodes next_prune_nodes =
            let create_prune_nodes_for_case case =
              match case with
              | CaseStmt(stmt_info, case_const:: _:: _) ->
                  let trans_state_pri =
                    PriorityNode.try_claim_priority_node trans_state'' stmt_info in
                  let res_trans_case_const = instruction trans_state_pri case_const in
                  let e_const = res_trans_case_const.exps in
                  let e_const' =
                    match e_const with
                    | [(head, typ)] -> head
                    | _ -> assert false in
                  let sil_eq_cond = Sil.BinOp(Sil.Eq, switch_e_cond', e_const') in
                  let sil_loc = get_sil_location stmt_info pln context in
                  let true_prune_node =
                    create_prune_node true [(sil_eq_cond, switch_e_cond'_typ)]
                      res_trans_case_const.ids res_trans_case_const.instrs
                      sil_loc (Sil.Ik_switch) context in
                  let false_prune_node =
                    create_prune_node false [(sil_eq_cond, switch_e_cond'_typ)]
                      res_trans_case_const.ids res_trans_case_const.instrs
                      sil_loc (Sil.Ik_switch) context in
                  (true_prune_node, false_prune_node)
              | _ -> assert false in
            match cases with (* top-down to handle default cases *)
            | [] -> next_nodes, next_prune_nodes
            | CaseStmt(stmt_info, _ :: _ :: case_content) as case :: rest ->
                let last_nodes, last_prune_nodes = translate_and_connect_cases rest next_nodes next_prune_nodes in
                let case_entry_point = connected_instruction (list_rev case_content) last_nodes in
                (* connects between cases, then continuation has priority about breaks *)
                let prune_node_t, prune_node_f = create_prune_nodes_for_case case in
                Cfg.Node.set_succs_exn prune_node_t case_entry_point [];
                Cfg.Node.set_succs_exn prune_node_f last_prune_nodes [];
                case_entry_point, [prune_node_t; prune_node_f]
            | DefaultStmt(stmt_info, default_content) :: rest ->
                let sil_loc = get_sil_location stmt_info pln context in
                let placeholder_entry_point =
                  create_node (Cfg.Node.Stmt_node "DefaultStmt_placeholder") [] [] sil_loc context in
                let last_nodes, last_prune_nodes = translate_and_connect_cases rest next_nodes [placeholder_entry_point] in
                let default_entry_point = connected_instruction (list_rev default_content) last_nodes in
                Cfg.Node.set_succs_exn placeholder_entry_point default_entry_point [];
                default_entry_point, last_prune_nodes
            | _ -> assert false in
          let top_entry_point, top_prune_nodes = translate_and_connect_cases list_of_cases succ_nodes succ_nodes in
          let _ = connected_instruction (list_rev pre_case_stmts) top_entry_point in
          Cfg.Node.set_succs_exn switch_special_cond_node top_prune_nodes [];
          let top_nodes =
            match res_trans_cond.root_nodes with
            | [] -> (* here if no root or if the translation of cond needed priority *)
                [switch_special_cond_node]
            | _ ->
                list_iter (fun n' -> Cfg.Node.set_succs_exn n' [switch_special_cond_node] []) res_trans_cond.leaf_nodes;
                res_trans_cond.root_nodes in
          list_iter (fun n' -> Cfg.Node.append_instrs_temps n' [] res_trans_cond.ids) succ_nodes; (* succ_nodes will remove the temps *)
          { root_nodes = top_nodes; leaf_nodes = succ_nodes; ids = []; instrs = []; exps =[]}
      | _ -> assert false)

  and stmtExpr_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    Printing.log_out "Passing from StmtExpr '%s'.\n" stmt_info.Clang_ast_t.si_pointer;
    let stmt = extract_stmt_from_singleton stmt_list "ERROR: StmtExpr should have only one statement.\n" in
    let res_trans_stmt = instruction trans_state stmt in
    let idl = res_trans_stmt.ids in
    let exps'= list_rev res_trans_stmt.exps in
    (match exps' with
      | (last, typ):: _ ->
      (* The StmtExpr contains a single CompoundStmt node, which it evaluates and *)
      (* takes the value of the last subexpression.*)
      (* Exp returned by StmtExpr is always a RValue. So we need to assign to a temp and return the temp.*)
          let id = Ident.create_fresh Ident.knormal in
          let loc = get_sil_location stmt_info trans_state.parent_line_number context in
          let instr' = Sil.Letderef (id, last, typ, loc) in
          { root_nodes = res_trans_stmt.root_nodes;
            leaf_nodes = res_trans_stmt.leaf_nodes;
            ids = id:: idl;
            instrs = res_trans_stmt.instrs@[instr'];
            exps = [(Sil.Var id, typ)]}
      | _ -> assert false)

  and loop_instruction trans_state loop_kind stmt_info =
    let outer_continuation = trans_state.continuation in
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = get_sil_location stmt_info pln context in
    let line_number = get_line stmt_info pln in
    let join_node = create_node (Cfg.Node.Join_node) [] [] sil_loc context in
    let continuation = Some { break = succ_nodes; continue = [join_node]; return_temp = false } in
    (* set the flat to inform that we are translating a condition of a if *)
    let continuation_cond = mk_cond_continuation outer_continuation in
    let init_incr_nodes =
      match loop_kind with
      | Loops.For (init, cond, incr, body) ->
          let trans_state' = { trans_state with succ_nodes = [join_node]; continuation = continuation; parent_line_number = line_number } in
          let res_trans_init = instruction trans_state' init in
          let res_trans_incr = instruction trans_state' incr in
          Some (res_trans_init.root_nodes, res_trans_incr.root_nodes)
      | _ -> None in
    let cond_stmt = Loops.get_cond loop_kind in
    let cond_line_number = get_line (fst (Clang_ast_proj.get_stmt_tuple cond_stmt)) line_number in
    let trans_state_cond = { trans_state with continuation = continuation_cond; parent_line_number = cond_line_number; succ_nodes = [] } in
    let res_trans_cond = cond_trans trans_state_cond cond_stmt in
    let body_succ_nodes =
      match loop_kind with
      | Loops.For _ -> (match init_incr_nodes with | Some (nodes_init, nodes_incr) -> nodes_incr | None -> assert false)
      | Loops.While _ -> [join_node]
      | Loops.DoWhile _ -> res_trans_cond.root_nodes in
    let body_continuation = match continuation, init_incr_nodes with
      | Some c, Some (nodes_init, nodes_incr) ->
          Some { c with continue = nodes_incr }
      | _ -> continuation in
    let res_trans_body =
      let trans_state_body =
        { trans_state with
          succ_nodes = body_succ_nodes;
          continuation = body_continuation;
          parent_line_number = line_number } in
      instruction trans_state_body (Loops.get_body loop_kind) in
    let join_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ -> res_trans_cond.root_nodes
      | Loops.DoWhile _ -> res_trans_body.root_nodes in
    (* Note: prune nodes are by contruction the res_trans_cond.leaf_nodes *)
    let prune_nodes_t, prune_nodes_f = list_partition is_true_prune_node res_trans_cond.leaf_nodes in
    let prune_t_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ -> res_trans_body.root_nodes
      | Loops.DoWhile _ -> [join_node] in
    Cfg.Node.set_succs_exn join_node join_succ_nodes [];
    list_iter (fun n -> Cfg.Node.set_succs_exn n prune_t_succ_nodes []) prune_nodes_t;
    list_iter (fun n -> Cfg.Node.set_succs_exn n succ_nodes []) prune_nodes_f;
    let root_nodes =
      match loop_kind with
      | Loops.For _ ->
          (match init_incr_nodes with | Some (nodes_init, nodes_incr) -> nodes_init | None -> assert false)
      | Loops.While _ | Loops.DoWhile _ -> [join_node] in
    root_nodes, prune_nodes_f

  and forStmt_trans trans_state init cond incr body stmt_info =
    let for_kind = Loops.For (init, cond, incr, body) in
    let root_nodes, leaf_nodes = loop_instruction trans_state for_kind stmt_info in
    { empty_res_trans with root_nodes = root_nodes; leaf_nodes = leaf_nodes }

  and whileStmt_trans trans_state cond body stmt_info =
    let while_kind = Loops.While (cond, body) in
    let root_nodes, leaf_nodes = loop_instruction trans_state while_kind stmt_info in
    { empty_res_trans with root_nodes = root_nodes; leaf_nodes = leaf_nodes }

  and doStmt_trans trans_state stmt_info cond body =
    let dowhile_kind = Loops.DoWhile (cond, body) in
    let root_nodes, leaf_nodes = loop_instruction trans_state dowhile_kind stmt_info in
    { empty_res_trans with root_nodes = root_nodes; leaf_nodes = leaf_nodes }

  and objCForCollectionStmt_trans trans_state item items body stmt_info =
    let bin_op = Ast_expressions.make_next_object_exp stmt_info item items in
    let while_kind = Loops.While (bin_op, body) in
    let root_nodes, leaf_nodes = loop_instruction trans_state while_kind stmt_info in
    { empty_res_trans with root_nodes = root_nodes; leaf_nodes = leaf_nodes }

  (* Assumption: We expect precisely 2 stmt corresponding to the 2 operands. *)
  and compoundAssignOperator trans_state stmt_info binary_operator_info expr_info stmt_list =
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    Printing.log_out "Passing from CompoundAssignOperator '%s'" stmt_info.Clang_ast_t.si_pointer;
    Printing.log_out "'%s' .\n"
      (Clang_ast_j.string_of_binary_operator_kind binary_operator_info.Clang_ast_t.boi_kind);
    (* claim priority if no ancestors has claimed priority before *)
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let sil_loc = get_sil_location stmt_info pln context in
    let line_number = get_line stmt_info pln in
    let sil_typ =
      CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
    (match stmt_list with
      | [s1; s2] ->
          let trans_state' = { trans_state_pri with succ_nodes = []; parent_line_number = line_number } in
          let res_trans_s1 = instruction trans_state' s1 in
          let res_trans_s2 = instruction trans_state' s2 in
          let (lhs_e, lhs_typ) = extract_exp_from_list res_trans_s1.exps
              "\nWARNING: Missing LHS operand in Compount Assign operator. Need Fixing.\n" in
          let (sil_e2, sil_typ2) = extract_exp_from_list res_trans_s2.exps
              "\nWARNING: Missing RHS operand in Compount Assign operator. Need Fixing.\n" in
          let id_op, exp_op, instr_op = CArithmetic_trans.compound_assignment_binary_operation_instruction
              binary_operator_info lhs_e sil_typ sil_e2 sil_loc in
          let ids = res_trans_s1.ids@res_trans_s2.ids@id_op in
          let instrs = res_trans_s1.instrs@res_trans_s2.instrs@instr_op in
          let res_trans_tmp = { res_trans_s2 with ids = ids; instrs = instrs; exps =[]} in
          let res_trans_to_parent =
            PriorityNode.compute_results_to_parent trans_state_pri sil_loc "ComppoundAssignStmt" stmt_info res_trans_tmp in

          let trans_s1_succs =
            if res_trans_to_parent.root_nodes <> []
            then res_trans_to_parent.root_nodes
            else trans_state_pri.succ_nodes in
          list_iter
            (fun n -> Cfg.Node.set_succs_exn n trans_s1_succs [])
            res_trans_s1.leaf_nodes;

          let instrs_to_parent', ids_to_parent', exp_to_parent' =
            compute_instr_ids_exp_to_parent stmt_info res_trans_to_parent.instrs res_trans_to_parent.ids
              [(exp_op, sil_typ)] lhs_e sil_typ sil_loc trans_state_pri.priority in

          let root_nodes =
            if res_trans_s1.root_nodes <> []
            then res_trans_s1.root_nodes
            else res_trans_to_parent.root_nodes in
          { root_nodes = root_nodes;
            leaf_nodes = res_trans_to_parent.leaf_nodes;
            ids = ids_to_parent';
            instrs = instrs_to_parent';
            exps = exp_to_parent' }
      | _ -> assert false) (* Compound assign statement should have two operands*)

  and initListExpr_trans trans_state stmt_info expr_info di_pointer stmts =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let rec collect_right_hand_exprs ts stmt = match stmt with
      | InitListExpr (stmt_info , stmts , expr_info) -> list_flatten (list_map (collect_right_hand_exprs ts) stmts)
      | _ ->
          let trans_state' = { ts with succ_nodes = []} in
          let res_trans_stmt = instruction trans_state' stmt in
          let (exp, typ) = extract_exp_from_list res_trans_stmt.exps
              "WARNING: in InitListExpr we expect the translation of each stmt to return one expression.\n" in
          let is_owning_method = CTrans_utils.is_owning_method stmt in
          let is_method_call = CTrans_utils.is_method_call stmt in
          [(res_trans_stmt.ids, res_trans_stmt.instrs, exp, is_method_call, is_owning_method, typ)] in
    let rec collect_left_hand_exprs e typ tns = match typ with
      | (Sil.Tvar tn) ->
          (match Sil.tenv_lookup context.tenv tn with
            | Some (Sil.Tstruct _ as str) -> collect_left_hand_exprs e str tns
            | Some ((Sil.Tvar typename) as tvar) ->
                if (StringSet.mem (Sil.typename_to_string typename) tns) then ([[(e, typ)]])
                else (collect_left_hand_exprs e tvar (StringSet.add (Sil.typename_to_string typename) tns));
            | _ -> [[(e, typ)]] (*This case is an error, shouldn't happen.*))
      | (Sil.Tstruct (struct_fields, _, _, _, _, _, _) as type_struct) ->
          let lh_exprs = list_map ( fun (fieldname, fieldtype, _) ->
                    Sil.Lfield (e, fieldname, type_struct) )
              struct_fields in
          let lh_types = list_map ( fun (fieldname, fieldtype, _) -> fieldtype)
              struct_fields in
          list_map (fun (e, t) -> list_flatten (collect_left_hand_exprs e t tns)) (zip lh_exprs lh_types)
      | Sil.Tarray (arrtyp, Sil.Const(Sil.Cint(n))) ->
          let size = Sil.Int.to_int n in
          let indices = list_range 0 (size - 1) in
          let index_constants = list_map
              (fun i -> (Sil.Const (Sil.Cint (Sil.Int.of_int i))))
              indices in
          let lh_exprs = list_map
              (fun index_expr -> Sil.Lindex (e, index_expr))
              index_constants in
          let lh_types = replicate size arrtyp in
          list_map (fun (e, t) -> list_flatten (collect_left_hand_exprs e t tns)) (zip lh_exprs lh_types)
      | _ -> [ [(e, typ)] ] in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let var_type = CTypes_decl.qual_type_to_sil_type context.tenv expr_info.ei_qual_type in
    let pvar = CContext.LocalVars.find_var_with_pointer context di_pointer in
    let lh = list_flatten (collect_left_hand_exprs (Sil.Lvar pvar) var_type Utils.StringSet.empty) in
    let rh = list_flatten (list_map (collect_right_hand_exprs trans_state_pri) stmts ) in
    if (list_length rh != list_length lh) then (
      (* If the right hand expressions are not as many as the left hand expressions something's wrong *)
      { empty_res_trans with root_nodes = succ_nodes }
    ) else (
      (* Creating new instructions by assigning right hand side to left hand side expressions *)
      let sil_loc = get_sil_location stmt_info trans_state_pri.parent_line_number context in
      let big_zip = list_map
          (fun ( (lh_exp, lh_t), (_, _, rh_exp, is_method_call, rhs_owning_method, rh_t) ) ->
                let is_pointer_object = ObjcInterface_decl.is_pointer_to_objc_class context.CContext.tenv rh_t in
                if !Config.arc_mode && (is_method_call || is_pointer_object) then
                  (* In arc mode, if it's a method call or we are initializing with a pointer to objc class *)
                  (* we need to add retain/release *)
                  let (e, instrs, ids) =
                    CArithmetic_trans.assignment_arc_mode context lh_exp lh_t rh_exp sil_loc rhs_owning_method true in
                  ([(e, lh_t)], instrs, ids)
                else
                  ([], [Sil.Set (lh_exp, lh_t, rh_exp, sil_loc)], []))
          (zip lh rh) in
      let rh_instrs = list_flatten ( list_map (fun (_, instrs, _, _, _, _) -> instrs) rh) in
      let assign_instrs = list_flatten(list_map (fun (_, instrs, _) -> instrs) big_zip) in
      let assign_ids = list_flatten(list_map (fun (_, _, ids) -> ids) big_zip) in
      let instructions = list_append (rh_instrs) assign_instrs in
      let rh_ids = list_flatten ( list_map (fun (ids, _, _, _, _, _) -> ids) rh) in
      let ids = list_append (rh_ids) assign_ids in
      if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then (
        let node_kind = Cfg.Node.Stmt_node "InitListExp" in
        let node = create_node node_kind (ids) (instructions) sil_loc context in
        Cfg.Node.set_succs_exn node succ_nodes [];
        { root_nodes =[node]; leaf_nodes =[]; ids = rh_ids; instrs = instructions; exps = [(Sil.Lvar pvar, var_type)]}
      ) else { root_nodes =[]; leaf_nodes =[]; ids = rh_ids; instrs = instructions; exps = [(Sil.Lvar pvar, var_type)]})

  and collect_all_decl trans_state var_decls next_nodes stmt_info =
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let do_var_dec (di, var_name, qtype, vdi) next_node =
      (match vdi.Clang_ast_t.vdi_init_expr with
        | None -> { empty_res_trans with root_nodes = next_node } (* Nothing to do if no init expression *)
        | Some (ImplicitValueInitExpr (_, stmt_list, _)) ->
        (* Seems unclear what it does, so let's keep an eye on the stmts *)
        (* and report a warning if it finds a non empty list of stmts *)
            (match stmt_list with
              | [] -> ()
              | _ -> Printing.log_stats "\n!!!!WARNING: found statement <\"ImplicitValueInitExpr\"> with non-empty stmt_list.\n");
            { empty_res_trans with root_nodes = next_node }
        | Some (InitListExpr (stmt_info , stmts , expr_info))
        | Some (ExprWithCleanups(_, [InitListExpr (stmt_info , stmts , expr_info)], _, _)) ->
            initListExpr_trans trans_state stmt_info expr_info di.Clang_ast_t.di_pointer stmts
        | Some ie -> (*For init expr, translate how to compute it and assign to the var*)
            let sil_loc = get_sil_location stmt_info pln context in
            let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
            let next_node =
              if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then (
                let node_kind = Cfg.Node.Stmt_node "DeclStmt" in
                let node = create_node node_kind [] [] sil_loc context in
                Cfg.Node.set_succs_exn node next_node [];
                [node]
              ) else next_node in
            let pvar = CContext.LocalVars.find_var_with_pointer context di.Clang_ast_t.di_pointer in
            let line_number = get_line stmt_info pln in
            (* if ie is a block the translation need to be done with the block special cases by exec_with_block_priority*)
            let res_trans_ie =
              let trans_state' = { trans_state_pri with succ_nodes = next_node; parent_line_number = line_number } in
              exec_with_block_priority_exception (exec_with_self_exception instruction) trans_state' ie stmt_info in
            let root_nodes = res_trans_ie.root_nodes in
            let leaf_nodes = res_trans_ie.leaf_nodes in
            let (sil_e1', ie_typ) = extract_exp_from_list res_trans_ie.exps
                "WARNING: In DeclStmt we expect only one expression returned in recursive call\n" in
            let rhs_owning_method = CTrans_utils.is_owning_method ie in
            let _, instrs_assign, ids_assign =
              if !Config.arc_mode &&
              (CTrans_utils.is_method_call ie || ObjcInterface_decl.is_pointer_to_objc_class context.CContext.tenv ie_typ) then
                (* In arc mode, if it's a method call or we are initializing with a pointer to objc class *)
                (* we need to add retain/release *)
                let (e, instrs, ids) =
                  CArithmetic_trans.assignment_arc_mode context (Sil.Lvar pvar) ie_typ sil_e1' sil_loc rhs_owning_method true in
                ([(e, ie_typ)], instrs, ids)
              else ([], [Sil.Set (Sil.Lvar pvar, ie_typ, sil_e1', sil_loc)], []) in
            let ids = res_trans_ie.ids@ids_assign in
            let instrs = res_trans_ie.instrs@instrs_assign in
            if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then (
              let node = list_hd next_node in
              Cfg.Node.append_instrs_temps node instrs ids;
              list_iter (fun n -> Cfg.Node.set_succs_exn n [node] []) leaf_nodes;

              let root_nodes = if (list_length root_nodes) = 0 then next_node else root_nodes in
              { root_nodes = root_nodes; leaf_nodes =[]; ids = ids; instrs = instrs; exps = [(Sil.Lvar pvar, ie_typ)]}
            ) else { root_nodes = root_nodes; leaf_nodes =[]; ids = ids; instrs = instrs; exps =[(Sil.Lvar pvar, ie_typ)]}) in
    match var_decls with
    | [] -> { empty_res_trans with root_nodes = next_nodes }
    | VarDecl(di, n, qt, vdi):: var_decls' ->
    (* Var are defined when procdesc is created, here we only take care of initialization*)
        let res_trans_vd = collect_all_decl trans_state var_decls' next_nodes stmt_info in
        let res_trans_tmp = do_var_dec (di, n, qt, vdi) res_trans_vd.root_nodes in
        { root_nodes = res_trans_tmp.root_nodes; leaf_nodes = [];
          ids = res_trans_tmp.ids @ res_trans_vd.ids;
          instrs = res_trans_tmp.instrs @ res_trans_vd.instrs;
          exps = res_trans_tmp.exps @ res_trans_vd.exps }
    | CXXRecordDecl _ :: var_decls' (*C++/C record decl treated in the same way *)
    | RecordDecl _ :: var_decls' ->
    (* Record declaration is done in the beginning when procdesc is defined.*)
        collect_all_decl trans_state var_decls' next_nodes stmt_info
    | _ -> assert false

  (* stmt_list is ignored because it contains the same instructions as *)
  (* the init expression. We use the latter info.                      *)
  and declStmt_trans trans_state decl_list stmt_info =
    let succ_nodes = trans_state.succ_nodes in
    Printing.log_out "Passing from DeclStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let line_number = get_line stmt_info trans_state.parent_line_number in
    let trans_state' = { trans_state with parent_line_number = line_number } in
    let res_trans = (match decl_list with
        | VarDecl _ :: _ ->  (* Case for simple variable declarations*)
            collect_all_decl trans_state' decl_list succ_nodes stmt_info
        | CXXRecordDecl _ :: var_decls (*C++/C record decl treated in the same way *)
        | RecordDecl _:: var_decls -> (* Case for struct *)
            collect_all_decl trans_state' decl_list succ_nodes stmt_info
        | _ ->
            Printing.log_stats
              "WARNING: In DeclStmt found an unknown declaration type. RETURNING empty list of declaration. NEED TO BE FIXED";
            empty_res_trans) in
    { res_trans with leaf_nodes = []}

  and objCPropertyRefExpr_trans trans_state stmt_info stmt_list =
    Printing.log_out "Passing from ObjCPropertyRefExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    (match stmt_list with
      | [stmt] -> instruction trans_state stmt
      | _ -> assert false)

  (* For OpaqueValueExpr we return the translation generated from its source expression*)
  and opaqueValueExpr_trans trans_state stmt_info opaque_value_expr_info =
    Printing.log_out "Passing from OpaqueValueExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    (match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
      | Some stmt -> instruction trans_state stmt
      | _ -> assert false)

  (* NOTE: This translation has several hypothesis. Need to be verified when we have more*)
  (* experience with this construct. Assert false will help to see if we encounter programs*)
  (* that do not conform with this hypothesis.*)
  (* Hypotheses:*)
  (* 1. stmt_list is composed by 2 parts: the first element is a syntactic description of the*)
  (* expression. The rest of the list has a semantic caracterization of the expression and*)
  (* defines how that expression is going to be implemented at runtime. *)
  (* 2. the semantic description is composed by a list of OpaqueValueExpr that define the *)
  (* various expressions involved and one finale expression that define how the final value of*)
  (* the PseudoObjectExpr is obtained. All the OpaqueValueExpr will be part of the last expression.*)
  (* So they can be skipped. *)
  (* For example: 'x.f = a' when 'f' is a property will be translated with a call to f's setter [x f:a]*)
  (* the stmt_list will be [x.f = a; x; a; CallToSetter] Among all element of the list we only need*)
  (* to translate the CallToSetter which is how x.f = a is actually implemented by the runtime.*)
  and pseudoObjectExpr_trans trans_state stmt_info stmt_list =
    let line_number = get_line stmt_info trans_state.parent_line_number in
    let trans_state' = { trans_state with parent_line_number = line_number } in
    Printing.log_out "Passing from PseudoObjectExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let rec do_semantic_elements el =
      (match el with
        | OpaqueValueExpr _ :: el' -> do_semantic_elements el'
        | stmt:: _ -> instruction trans_state' stmt
        | _ -> assert false) in
    (match stmt_list with
      | syntactic_form:: semantic_form ->
          do_semantic_elements semantic_form
      | _ -> assert false)

  (* Cast expression are treated the same apart from the cast operation kind*)
  and cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_expr_info is_objc_bridged =
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    Printing.log_out "Passing from CastExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info pln context in
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: In CastExpr There must be only one stmt defining the expression to be cast.\n" in
    let line_number = get_line stmt_info pln in
    let trans_state' = { trans_state with parent_line_number = line_number } in
    let res_trans_stmt = instruction trans_state' stmt in
    let typ = CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
    let cast_kind = cast_expr_info.Clang_ast_t.cei_cast_kind in
    (* This gives the differnece among cast operations kind*)
    let cast_ids, cast_inst, cast_exp = cast_operation context cast_kind res_trans_stmt.exps typ sil_loc is_objc_bridged in
    { root_nodes = res_trans_stmt.root_nodes;
      leaf_nodes = res_trans_stmt.leaf_nodes;
      ids = res_trans_stmt.ids @ cast_ids;
      instrs = res_trans_stmt.instrs @ cast_inst;
      exps = [(cast_exp, typ)] }

  (* function used in the computation for both Member_Expr and ObjCIVarRefExpr *)
  and do_memb_ivar_ref_exp trans_state expr_info exp_stmt sil_loc nfield =
    Printing.log_out "!!!!! Dealing with field '%s' @." nfield;
    let res_trans_exp_stmt = instruction trans_state exp_stmt in
    let (e, class_typ) = extract_exp_from_list res_trans_exp_stmt.exps
        "WARNING: in MemberExpr we expect the translation of the stmt to return an expression\n" in
    let class_typ =
      (match class_typ with
        | Sil.Tptr (t, _) -> CTypes_decl.expand_structured_type trans_state.context.tenv t
        | t -> t) in
    let typ = CTypes_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
    let exp =
      (match class_typ with
        | Sil.Tvoid -> Sil.exp_minus_one
        | _ ->
            Printing.log_out "Type is  '%s' @." (Sil.typ_to_string class_typ);
            ( match ObjcInterface_decl.find_field trans_state.context.tenv nfield (Some class_typ) false with
              | Some (fn, _, _) -> Sil.Lfield (e, fn, class_typ)
              | None -> assert false)) in
    { res_trans_exp_stmt with
      exps = [(exp, typ)] }

  and objCIvarRefExpr_trans trans_state stmt_info expr_info stmt_list obj_c_ivar_ref_expr_info =
    Printing.log_out "Passing from ObjCIvarRefExpr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let exp_stmt = extract_stmt_from_singleton stmt_list
        "WARNING: in MemberExpr there must be only one stmt defining its expression.\n" in
    let name_field = (match obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref.Clang_ast_t.dr_name with
        | Some s -> s
        | _ -> assert false) in
    do_memb_ivar_ref_exp trans_state expr_info exp_stmt sil_loc name_field

  and memberExpr_trans trans_state stmt_info expr_info stmt_list member_expr_info =
    Printing.log_out "Passing from MemberExpr '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let exp_stmt = extract_stmt_from_singleton stmt_list
        "WARNING: in MemberExpr there must be only one stmt defining its expression.\n" in
    let name_field = member_expr_info.Clang_ast_t.mei_name in
    do_memb_ivar_ref_exp trans_state expr_info exp_stmt sil_loc name_field

  and unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info =
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    Printing.log_out "Passing from UnaryOperator '%s'\n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info pln context in
    let line_number = get_line stmt_info pln in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: We expect only one element in stmt list defining the operand in UnaryOperator. NEED FIXING\n" in
    let trans_state' = { trans_state_pri with succ_nodes =[]; parent_line_number = line_number } in
    let res_trans_stmt = instruction trans_state' stmt in
    (* Assumption: the operand does not create a cfg node*)
    let (sil_e', _) = extract_exp_from_list res_trans_stmt.exps "\nWARNING: Missing operand in unary operator. NEED FIXING.\n" in
    let ret_typ = CTypes_decl.qual_type_to_sil_type context.tenv expr_info.Clang_ast_t.ei_qual_type in
    let ids_op, exp_op, instr_op =
      CArithmetic_trans.unary_operation_instruction unary_operator_info sil_e' ret_typ sil_loc in
    let node_kind = Cfg.Node.Stmt_node "UnaryOperator" in
    let ids' = res_trans_stmt.ids@ids_op in
    let instrs = res_trans_stmt.instrs @ instr_op in
    let root_nodes_to_parent, leaf_nodes_to_parent, ids_to_parent, instr_to_parent, exp_to_parent =
      if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then
        (* Create a node. *)
        let ids_parent = ids_to_parent trans_state_pri.continuation ids' in
        let ids_node = ids_to_node trans_state_pri.continuation ids' in
        let node = create_node node_kind ids_node instrs sil_loc context in

        Cfg.Node.set_succs_exn node trans_state_pri.succ_nodes [];
        list_iter (fun n -> Cfg.Node.set_succs_exn n [node] []) res_trans_stmt.leaf_nodes;

        let root_nodes =
          if res_trans_stmt.root_nodes <> [] then res_trans_stmt.root_nodes
          else [node] in
        let leaf_nodes = [node] in

        root_nodes, leaf_nodes, ids_parent, [], exp_op
      else
        res_trans_stmt.root_nodes, res_trans_stmt.leaf_nodes, ids', instrs, exp_op in
    { root_nodes = root_nodes_to_parent;
      leaf_nodes = leaf_nodes_to_parent;
      ids = ids_to_parent;
      instrs = instr_to_parent;
      exps = [(exp_to_parent, ret_typ)]
    }

  and returnStmt_trans trans_state stmt_info stmt_list =
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let succ_nodes = trans_state.succ_nodes in
    Printing.log_out "Passing from ReturnOperator '%s'.\n" stmt_info.Clang_ast_t.si_pointer;
    let sil_loc = get_sil_location stmt_info pln context in
    let line_number = get_line stmt_info pln in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let ret_node = create_node (Cfg.Node.Stmt_node "Return Stmt") [] [] sil_loc context in
    Cfg.Node.set_succs_exn ret_node [(Cfg.Procdesc.get_exit_node context.procdesc)] [];
    let trans_result = (match stmt_list with
        | [stmt] -> (* return exp; *)
            let trans_state' = { trans_state_pri with succ_nodes = [ret_node]; parent_line_number = line_number } in
            let res_trans_stmt = exec_with_self_exception instruction trans_state' stmt in
            let (sil_expr, sil_typ) = extract_exp_from_list res_trans_stmt.exps
                "WARNING: There should be only one return expression.\n" in
            let ret_var = Cfg.Procdesc.get_ret_var context.procdesc in
            let ret_type = Cfg.Procdesc.get_ret_type context.procdesc in
            let ret_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_expr, sil_loc) in
            let autorelease_ids, autorelease_instrs = add_autorelease_call context sil_expr ret_type sil_loc in
            let instrs = res_trans_stmt.instrs @ [ret_instr] @ autorelease_instrs in
            let ids = res_trans_stmt.ids@autorelease_ids in
            Cfg.Node.append_instrs_temps ret_node instrs ids;
            list_iter (fun n -> Cfg.Node.set_succs_exn n [ret_node] []) res_trans_stmt.leaf_nodes;
            let root_nodes_to_parent =
              if list_length res_trans_stmt.root_nodes >0 then res_trans_stmt.root_nodes else [ret_node] in
            { root_nodes = root_nodes_to_parent; leaf_nodes =[ret_node]; ids = ids; instrs = instrs; exps =[]}
        | [] -> (* return; *)
            { empty_res_trans with root_nodes =[ret_node]; leaf_nodes =[ret_node]}
        | _ -> Printing.log_out
              "\nWARNING: Missing translation of Return Expression. Return Statement ignored. Need fixing!\n";
            { empty_res_trans with root_nodes = succ_nodes }) in (* We expect a return with only one expression *)
    trans_result

  (* We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. *)
  (*  It may be that later on (when we treat ARC) some info can be taken from it. *)
  (* For ParenExpression we translate its body composed by the stmt_list.  *)
  (* In paren expression there should be only one stmt that defines the  expression  *)
  and parenExpr_trans trans_state stmt_info stmt_list =
    Printing.log_out "Passing from ParenExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let line_number = get_line stmt_info trans_state.parent_line_number in
    let trans_state'= { trans_state with parent_line_number = line_number } in
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: In ParenExpression there should be only one stmt.\n" in
    instruction trans_state' stmt

  and objCBoxedExpr_trans trans_state info sel stmt_info stmts =
    let typ = CTypes_decl.class_from_pointer_type trans_state.context.tenv info.Clang_ast_t.ei_qual_type in
    let obj_c_message_expr_info = Ast_expressions.make_obj_c_message_expr_info_class sel typ in
    let message_stmt = ObjCMessageExpr(stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  and objCArrayLiteral_trans trans_state info stmt_info stmts =
    let typ = CTypes_decl.class_from_pointer_type trans_state.context.tenv info.Clang_ast_t.ei_qual_type in
    let obj_c_message_expr_info =
      Ast_expressions.make_obj_c_message_expr_info_class CFrontend_config.array_with_objects_count_m typ in
    let stmts = stmts@[Ast_expressions.create_nil stmt_info] in
    let message_stmt = ObjCMessageExpr(stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  and objCDictionaryLiteral_trans trans_state info stmt_info stmts =
    let typ = CTypes_decl.class_from_pointer_type trans_state.context.tenv info.Clang_ast_t.ei_qual_type in
    let obj_c_message_expr_info =
      Ast_expressions.make_obj_c_message_expr_info_class CFrontend_config.dict_with_objects_and_keys_m typ in
    let stmts = swap_elements_list stmts in
    let stmts = stmts@[Ast_expressions.create_nil stmt_info] in
    let message_stmt = ObjCMessageExpr(stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  and objCStringLiteral_trans trans_state stmt_info stmts info =
    let stmts = [Ast_expressions.create_implicit_cast_expr stmt_info stmts
        (Ast_expressions.create_char_type ()) `ArrayToPointerDecay] in
    let typ = CTypes_decl.class_from_pointer_type trans_state.context.tenv info.Clang_ast_t.ei_qual_type in
    let obj_c_message_expr_info =
      Ast_expressions.make_obj_c_message_expr_info_class CFrontend_config.string_with_utf8_m typ in
    let message_stmt = ObjCMessageExpr(stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  (** When objects are autoreleased, they get added a flag AUTORELEASE. All these objects will be
  ignored when checking for memory leaks. When the end of the block autoreleasepool is reached,
  then those objects are released and the autorelease flag is removed. *)
  and objcAutoreleasePool_trans trans_state stmt_info stmts =
    let sil_loc = get_sil_location stmt_info trans_state.parent_line_number trans_state.context in
    let fname = SymExec.ModelBuiltins.__objc_release_autorelease_pool in
    let ret_id = Ident.create_fresh Ident.knormal in
    let autorelease_pool_vars = compute_autorelease_pool_vars trans_state.context stmts in
    let stmt_call = Sil.Call([ret_id], (Sil.Const (Sil.Cfun fname)), autorelease_pool_vars, sil_loc, Sil.cf_default) in
    let node_kind = Cfg.Node.Stmt_node ("Release the autorelease pool") in
    let call_node = create_node node_kind ([ret_id]) ([stmt_call]) sil_loc trans_state.context in
    Cfg.Node.set_succs_exn call_node trans_state.succ_nodes [];
    let trans_state'={ trans_state with continuation = None; succ_nodes =[call_node] } in
    instructions trans_state' stmts

  (* Assumption: stmt_list contains 2 items, the first can be ObjCMessageExpr or ParenExpr *)
  (* We ignore this item since we don't deal with the concurrency problem yet *)
  (* For the same reason we also ignore the stmt_info that is related with the ObjCAtSynchronizedStmt construct *)
  (* Finally we recursively work on the CompoundStmt, the second item of stmt_list *)
  and objCAtSynchronizedStmt_trans trans_state stmt_info stmt_list =
    Printing.log_out "Passing from `ObjCAtSynchronizedStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    (match stmt_list with
      | [_; compound_stmt] -> instruction trans_state compound_stmt
      | _ -> assert false)

  and blockExpr_trans trans_state stmt_info expr_info decl =
    Printing.log_out "Passing from BlockExpr '%s' \n" stmt_info.Clang_ast_t.si_pointer;
    let context = trans_state.context in
    let pln = trans_state.parent_line_number in
    let procname = Cfg.Procdesc.get_proc_name context.procdesc in
    let loc =
      (match stmt_info.Clang_ast_t.si_source_range with (l1, l2) ->
            CLocation.clang_to_sil_location l1 pln (Some context.procdesc)) in
    (* Given a mangled name (possibly full) returns a plain mangled name *)
    let ensure_plain_mangling m =
      Mangled.from_string (Mangled.to_string m) in
    (* Given a captured var, return the instruction to assign it to a temp *)
    let assign_captured_var cv =
      let cvar, typ = (match cv with
          | (cvar, typ, false) -> cvar, typ
          | (cvar, typ, true) -> (* static case *)
              let formals = Cfg.Procdesc.get_formals context.procdesc in
              let cvar' = ensure_plain_mangling cvar in
              (* we check if cvar' is a formal. In that case we need this plain mangled name *)
              (* otherwise it's a static variable defined among the locals *)
              (* and therefore we need the full mangled name *)
              let cvar''=
                if (list_exists(fun (s, t) -> Mangled.from_string s = cvar') formals) then cvar'
                else cvar in
              (cvar'', typ)) in
      let id = Ident.create_fresh Ident.knormal in
      let instr = Sil.Letderef (id, Sil.Lvar (Sil.mk_pvar cvar procname), typ, loc) in
      (id, instr) in
    (match decl with
      | BlockDecl(decl_info, decl_list, decl_context_info, block_decl_info) ->
          let qual_type = expr_info.Clang_ast_t.ei_qual_type in
          let block_pname = CFrontend_utils.General_utils.mk_fresh_block_procname procname in
          let typ = CTypes_decl.qual_type_to_sil_type context.tenv qual_type in
          (* We need to set the explicit dependency between the newly created block and the *)
          (* defining procedure. We add an edge in the call graph.*)
          Cg.add_edge context.cg procname block_pname;
          let function_decl_info = CFrontend_utils.General_utils.mk_function_decl_info_from_block block_decl_info in
          let static_locals = list_filter (fun (v, t, s) -> s = true) context.local_vars in
          (*list_iter (fun (v, _, _) -> L.err "Static Locals %s@." (Mangled.to_string v)) static_locals;*)
          let static_formals = list_filter (fun (v, t, s) -> s = true) context.captured_vars in
          (*list_iter (fun (v, _, _) -> L.err "Formal Static %s@." (Mangled.to_string v)) static_formals;*)
          let static_vars = static_locals @ static_formals in
          let captured_vars =
            (CMethod_trans.captured_vars_from_block_info context block_decl_info.Clang_ast_t.bdi_captured_variables) in
          let all_captured_vars = captured_vars @ static_vars in
          let ids_instrs = list_map assign_captured_var all_captured_vars in
          let ids, instrs = list_split ids_instrs in
          M.function_decl context.tenv context.cfg context.cg context.namespace context.is_instance decl_info
            (Procname.to_string block_pname) qual_type function_decl_info all_captured_vars (Some block_pname) context.curr_class;
          Cfg.set_procname_priority context.cfg block_pname;
          let captured_exps = list_map (fun id -> Sil.Var id) ids in
          let tu = Sil.Ctuple ((Sil.Const (Sil.Cfun block_pname)):: captured_exps) in
          let alloc_block_instr, ids_block = allocate_block trans_state (Procname.to_string block_pname) all_captured_vars loc in
          { empty_res_trans with ids = ids_block @ ids; instrs = alloc_block_instr @ instrs; exps = [(Sil.Const tu, typ)]}
      | _ -> assert false)

  (* Translates a clang instruction into SIL instructions. It takes a       *)
  (* a trans_state containing current info on the translation and it returns *)
  (* a result_state.*)
  and instruction trans_state instr =
    match instr with
    | GotoStmt(stmt_info, _, { Clang_ast_t.gsi_label = label_name; _ }) ->
        gotoStmt_trans trans_state stmt_info label_name

    | LabelStmt(stmt_info, stmt_list, label_name) ->
        Printing.log_out "\nPassing from `LabelStmt '%s' \n" stmt_info.Clang_ast_t.si_pointer;
        labelStmt_trans trans_state stmt_info stmt_list label_name

    | ArraySubscriptExpr(stmt_info, stmt_list, expr_info) ->
        arraySubscriptExpr_trans trans_state stmt_info expr_info stmt_list

    | BinaryOperator(stmt_info, stmt_list, expr_info, binary_operator_info) ->
        binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list

    | CallExpr(stmt_info, stmt_list, ei) ->
        (match is_dispatch_function stmt_list with
          | Some block_arg_pos ->
              dispatch_function_trans trans_state stmt_info stmt_list ei block_arg_pos
          | None ->
              callExpr_trans trans_state stmt_info stmt_list ei)

    | ObjCMessageExpr(stmt_info, stmt_list, expr_info, obj_c_message_expr_info) ->
        objCMessageExpr_trans trans_state stmt_info obj_c_message_expr_info stmt_list expr_info

    | CompoundStmt (stmt_info, stmt_list) ->
    (* No node for this statement. We just collect its statement list*)
        compoundStmt_trans trans_state stmt_info stmt_list

    | ConditionalOperator(stmt_info, stmt_list, expr_info) ->
    (* Ternary operator "cond ? exp1 : exp2" *)
        conditionalOperator_trans trans_state stmt_info stmt_list expr_info

    | IfStmt(stmt_info, stmt_list) ->
        ifStmt_trans trans_state stmt_info stmt_list

    | SwitchStmt (stmt_info, switch_stmt_list) ->
        switchStmt_trans trans_state stmt_info switch_stmt_list

    | CaseStmt (stmt_info, stmt_list) ->
        Printing.log_out "FATAL: Passing from CaseStmt outside of SwitchStmt, terminating.\n"; assert false

    | StmtExpr(stmt_info, stmt_list, expr_info) ->
        stmtExpr_trans trans_state stmt_info stmt_list expr_info

    | ForStmt(stmt_info, [init; null_stmt; cond; incr; body]) ->
    (* Note: we ignore null_stmt at the moment.*)
        forStmt_trans trans_state init cond incr body stmt_info

    | WhileStmt(stmt_info, [_; cond; body]) ->  (* Note: we ignore null_stmt at the moment.*)
        whileStmt_trans trans_state cond body stmt_info

    | DoStmt(stmt_info, [body; cond]) ->
        doStmt_trans trans_state stmt_info cond body

    | ObjCForCollectionStmt(stmt_info, [item; items; body]) ->
        objCForCollectionStmt_trans trans_state item items body stmt_info

    | NullStmt(stmt_info, stmt_list) ->
        nullStmt_trans trans_state.succ_nodes stmt_info

    | CompoundAssignOperator(stmt_info, stmt_list, expr_info, binary_operator_info, caoi) ->
        compoundAssignOperator trans_state stmt_info binary_operator_info expr_info stmt_list

    | DeclStmt(stmt_info, stmt_list, decl_list) ->
        declStmt_trans trans_state decl_list stmt_info

    | DeclRefExpr(stmt_info, stmt_list, expr_info, decl_ref_expr_info) as d ->
        declRefExpr_trans trans_state stmt_info expr_info decl_ref_expr_info d

    | ObjCPropertyRefExpr(stmt_info, stmt_list, expr_info, property_ref_expr_info) ->
        objCPropertyRefExpr_trans trans_state stmt_info stmt_list

    | OpaqueValueExpr(stmt_info, stmt_list, expr_info, opaque_value_expr_info) ->
        opaqueValueExpr_trans trans_state stmt_info opaque_value_expr_info

    | PseudoObjectExpr(stmt_info, stmt_list, expr_info) ->
        pseudoObjectExpr_trans trans_state stmt_info stmt_list

    | UnaryExprOrTypeTraitExpr(stmt_info, stmt_list, expr_info, ei) ->
        unaryExprOrTypeTraitExpr_trans trans_state stmt_info expr_info ei

    | ObjCBridgedCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _) ->
        cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind true
    | ImplicitCastExpr(stmt_info, stmt_list, expr_info, cast_kind)
    | CStyleCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _)
    | CXXStaticCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _, _) ->
        cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind false

    | IntegerLiteral(stmt_info, _, expr_info, integer_literal_info) ->
        integerLiteral_trans trans_state stmt_info expr_info integer_literal_info

    | StringLiteral(stmt_info, stmt_list, expr_info, str) ->
        stringLiteral_trans trans_state stmt_info expr_info str

    | GNUNullExpr(stmt_info, stmt_list, expr_info) ->
        gNUNullExpr_trans trans_state stmt_info expr_info

    | CXXNullPtrLiteralExpr(stmt_info, stmt_list, expr_info) ->
        nullPtrExpr_trans trans_state stmt_info expr_info

    | ObjCSelectorExpr(stmt_info, stmt_list, expr_info, selector) ->
        objCSelectorExpr_trans trans_state stmt_info expr_info selector

    | ObjCEncodeExpr(stmt_info, stmt_list, expr_info, qual_type) ->
        objCEncodeExpr_trans trans_state stmt_info expr_info qual_type

    | ObjCProtocolExpr(stmt_info, stmt_list, expr_info, decl_ref) ->
        objCProtocolExpr_trans trans_state stmt_info expr_info decl_ref

    | ObjCIvarRefExpr(stmt_info, stmt_list, expr_info, obj_c_ivar_ref_expr_info) ->
        objCIvarRefExpr_trans trans_state stmt_info expr_info stmt_list obj_c_ivar_ref_expr_info

    | MemberExpr(stmt_info, stmt_list, expr_info, member_expr_info) ->
        memberExpr_trans trans_state stmt_info expr_info stmt_list member_expr_info

    | UnaryOperator(stmt_info, stmt_list, expr_info, unary_operator_info) ->
        if is_logical_negation_of_int trans_state.context.tenv expr_info unary_operator_info then
          let conditional = Ast_expressions.trans_negation_with_conditional stmt_info expr_info stmt_list in
          instruction trans_state conditional
        else
          unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info

    | ReturnStmt (stmt_info, stmt_list) ->
        returnStmt_trans trans_state stmt_info stmt_list

    (* We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. *)
    (*  It may be that later on (when we treat ARC) some info can be taken from it. *)
    | ExprWithCleanups(stmt_info, stmt_list, expr_info, _)
    | ParenExpr(stmt_info, stmt_list, expr_info) ->
        parenExpr_trans trans_state stmt_info stmt_list

    | ObjCBoolLiteralExpr (stmt_info, stmts, expr_info, n)
    | CharacterLiteral (stmt_info, stmts, expr_info, n)
    | CXXBoolLiteralExpr (stmt_info, stmts, expr_info, n) ->
        characterLiteral_trans trans_state stmt_info expr_info n

    | FloatingLiteral (stmt_info, stmts, expr_info, float_string) ->
        floatingLiteral_trans trans_state stmt_info expr_info float_string

    | ObjCBoxedExpr (stmt_info, stmts, info, sel) ->
        objCBoxedExpr_trans trans_state info sel stmt_info stmts

    | ObjCArrayLiteral (stmt_info, stmts, info) ->
        objCArrayLiteral_trans trans_state info stmt_info stmts

    | ObjCDictionaryLiteral (stmt_info, stmts, info) ->
        objCDictionaryLiteral_trans trans_state info stmt_info stmts

    | ObjCStringLiteral(stmt_info, stmts, info) ->
        objCStringLiteral_trans trans_state stmt_info stmts info

    | BreakStmt(stmt_info, lstmt) -> breakStmt_trans trans_state

    | ContinueStmt(stmt_infr, lstmt) -> continueStmt_trans trans_state

    | ObjCAtSynchronizedStmt(stmt_info, stmt_list) ->
        objCAtSynchronizedStmt_trans trans_state stmt_info stmt_list

    | ObjCIndirectCopyRestoreExpr (stmt_info, stmt_list, _) ->
        instructions trans_state stmt_list

    | BlockExpr(stmt_info, _ , expr_info, decl) ->
        blockExpr_trans trans_state stmt_info expr_info decl

    | ObjCAutoreleasePoolStmt (stmt_info, stmts) ->
        objcAutoreleasePool_trans trans_state stmt_info stmts

    | ObjCAtTryStmt (stmt_info, stmts) ->
        compoundStmt_trans trans_state stmt_info stmts

    | ObjCAtThrowStmt (stmt_info, stmts) ->
        returnStmt_trans trans_state stmt_info stmts

    | ObjCAtFinallyStmt (stmt_info, stmts) ->
        compoundStmt_trans trans_state stmt_info stmts

    | ObjCAtCatchStmt (stmt_info, stmts, obj_c_message_expr_kind) ->
        compoundStmt_trans trans_state stmt_info []

    | PredefinedExpr (stmt_info, stmts, expr_info, predefined_expr_type) ->
        stringLiteral_trans trans_state stmt_info expr_info ""

    | BinaryConditionalOperator (stmt_info, stmts, expr_info) ->
        (match stmts with
          | [stmt1; ostmt1; ostmt2; stmt2] when contains_opaque_value_expr ostmt1 && contains_opaque_value_expr ostmt2 ->
              conditionalOperator_trans trans_state stmt_info [stmt1; stmt1; stmt2] expr_info
          | _ -> Printing.log_stats
                "BinaryConditionalOperator not translated %s @."
                (Ast_utils.string_of_stmt instr);
              assert false)
    | s -> (Printing.log_stats
            "\n!!!!WARNING: found statement %s. \nACTION REQUIRED: Translation need to be defined. Statement ignored.... \n"
            (Ast_utils.string_of_stmt s);
          assert false)

  (* Given a translation state, this function traslates a list of clang statements. *)
  and instructions trans_state clang_stmt_list =
    (* Printing.log_err "\n instruction list %i" (List.length clang_stmt_list); *)
    match clang_stmt_list with
    | [] -> { empty_res_trans with root_nodes = trans_state.succ_nodes }
    | s:: clang_stmt_list' ->
        let res_trans_s = instruction trans_state s in
        let trans_state' =
          if res_trans_s.root_nodes <> []
          then { trans_state with succ_nodes = res_trans_s.root_nodes }
          else trans_state in
        let res_trans_tail = instructions trans_state' clang_stmt_list' in
        { root_nodes = res_trans_tail.root_nodes;
          leaf_nodes =[];
          ids = res_trans_s.ids @ res_trans_tail.ids;
          instrs = res_trans_s.instrs @ res_trans_tail.instrs;
          exps = res_trans_s.exps @ res_trans_tail.exps }

  let expression_trans context stmt warning =
    let trans_state = { context = context; succ_nodes =[]; continuation = None; parent_line_number = -1; priority = Free } in
    let res_trans_stmt = instruction trans_state stmt in
    fst (CTrans_utils.extract_exp_from_list res_trans_stmt.exps warning)

  let instructions_trans context clang_stmt_list exit_node =
    let trans_state = { context = context; succ_nodes =[exit_node]; continuation = None; parent_line_number = -1; priority = Free } in
    let res_trans = instructions trans_state clang_stmt_list in
    res_trans.root_nodes

end
