(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Utility methods to support the translation of clang ast constructs into sil instructions.  *)

module L = Logging

(** Extract the element of a singleton list. If the list is not a singleton it crashes. *)
let extract_item_from_singleton l pp source_range warning_string =
  match l with
  | [item] ->
      item
  | _ ->
      L.die InternalError "At %a: List has %d elements, 1 expected:@\n[@[<h>%a@]]@\n%s"
        (Pp.to_string ~f:Clang_ast_j.string_of_source_range)
        source_range (List.length l) (Pp.semicolon_seq pp) l warning_string


let source_range_of_stmt stmt =
  let {Clang_ast_t.si_source_range}, _ = Clang_ast_proj.get_stmt_tuple stmt in
  si_source_range


module Nodes = struct
  let prune_kind b if_kind =
    Procdesc.Node.Prune_node
      (b, if_kind, if b then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch)


  let is_true_prune_node n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Prune_node (true, _, _) ->
        true
    | _ ->
        false


  let create_prune_node proc_desc ~branch ~negate_cond e_cond instrs_cond loc if_kind =
    let e_cond = if negate_cond then Exp.UnOp (Unop.LNot, e_cond, None) else e_cond in
    let instrs_cond' = instrs_cond @ [Sil.Prune (e_cond, loc, branch, if_kind)] in
    Procdesc.create_node proc_desc loc (prune_kind branch if_kind) instrs_cond'


  (** Check if this binary opertor requires the creation of a node in the cfg. *)
  let is_binary_assign_op boi =
    match boi.Clang_ast_t.boi_kind with
    | `Assign
    | `MulAssign
    | `DivAssign
    | `RemAssign
    | `AddAssign
    | `SubAssign
    | `ShlAssign
    | `ShrAssign
    | `AndAssign
    | `XorAssign
    | `OrAssign ->
        true
    | `PtrMemD
    | `PtrMemI
    | `Mul
    | `Div
    | `Rem
    | `Add
    | `Sub
    | `Shl
    | `Shr
    | `LT
    | `GT
    | `LE
    | `GE
    | `EQ
    | `NE
    | `And
    | `Xor
    | `Or
    | `LAnd
    | `LOr
    | `Cmp
    | `Comma ->
        false
end

module GotoLabel = struct
  let find_goto_label context label sil_loc =
    try Hashtbl.find context.CContext.label_map label with Caml.Not_found ->
      let node_name = Format.sprintf "GotoLabel_%s" label in
      let new_node =
        Procdesc.create_node context.CContext.procdesc sil_loc (Procdesc.Node.Skip_node node_name)
          []
      in
      Hashtbl.add context.CContext.label_map label new_node ;
      new_node
end

type continuation =
  { break: Procdesc.Node.t list
  ; continue: Procdesc.Node.t list
  ; return_temp: bool
  (* true if temps should not be removed in the node but returned to ancestors *) }

let is_return_temp continuation =
  match continuation with Some cont -> cont.return_temp | _ -> false


let mk_cond_continuation cont =
  match cont with
  | Some cont' ->
      Some {cont' with return_temp= true}
  | None ->
      Some {break= []; continue= []; return_temp= true}


type priority_node = Free | Busy of Clang_ast_t.pointer

(** A translation state. It provides the translation function with the info it needs to carry on the
   translation. *)
type trans_state =
  { context: CContext.t  (** current context of the translation *)
  ; succ_nodes: Procdesc.Node.t list  (** successor nodes in the cfg *)
  ; continuation: continuation option  (** current continuation *)
  ; priority: priority_node
  ; var_exp_typ: (Exp.t * Typ.t) option
  ; opaque_exp: (Exp.t * Typ.t) option
  ; is_fst_arg_objc_instance_method_call: bool }

let default_trans_state context =
  { context
  ; succ_nodes= []
  ; continuation= None
  ; priority= Free
  ; var_exp_typ= None
  ; opaque_exp= None
  ; is_fst_arg_objc_instance_method_call= false }


type control =
  { root_nodes: Procdesc.Node.t list
  ; leaf_nodes: Procdesc.Node.t list
  ; instrs: Sil.instr list
  ; initd_exps: Exp.t list }

type trans_result =
  { control: control
  ; return: Exp.t * Typ.t
  ; method_name: Typ.Procname.t option
  ; is_cpp_call_virtual: bool }

let empty_control = {root_nodes= []; leaf_nodes= []; instrs= []; initd_exps= []}

let mk_trans_result ?method_name ?(is_cpp_call_virtual = false) return control =
  {control; return; method_name; is_cpp_call_virtual}


let undefined_expression () = Exp.Var (Ident.create_fresh Ident.knormal)

(** Collect the results of translating a list of instructions, and link up the nodes created. *)
let collect_controls pdesc l =
  let collect_one result_rev {root_nodes; leaf_nodes; instrs; initd_exps} =
    if root_nodes <> [] then
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn pdesc n root_nodes [])
        result_rev.leaf_nodes ;
    let root_nodes = if result_rev.root_nodes <> [] then result_rev.root_nodes else root_nodes in
    let leaf_nodes = if leaf_nodes <> [] then leaf_nodes else result_rev.leaf_nodes in
    { root_nodes
    ; leaf_nodes
    ; instrs= List.rev_append instrs result_rev.instrs
    ; initd_exps= List.rev_append initd_exps result_rev.initd_exps }
  in
  let rev_result = List.fold l ~init:empty_control ~f:collect_one in
  {rev_result with instrs= List.rev rev_result.instrs; initd_exps= List.rev rev_result.initd_exps}


let collect_trans_results pdesc ~return trans_results =
  List.map trans_results ~f:(fun {control} -> control)
  |> collect_controls pdesc |> mk_trans_result return


module PriorityNode = struct
  type t = priority_node

  let try_claim_priority_node trans_state stmt_info =
    match trans_state.priority with
    | Free ->
        L.(debug Capture Verbose)
          "Priority is free. Locking priority node in %d@\n@." stmt_info.Clang_ast_t.si_pointer ;
        {trans_state with priority= Busy stmt_info.Clang_ast_t.si_pointer}
    | _ ->
        L.(debug Capture Verbose)
          "Priority busy in %d. No claim possible@\n@." stmt_info.Clang_ast_t.si_pointer ;
        trans_state


  let force_claim_priority_node trans_state stmt_info =
    {trans_state with priority= Busy stmt_info.Clang_ast_t.si_pointer}


  let is_priority_free trans_state = match trans_state.priority with Free -> true | _ -> false

  let own_priority_node pri stmt_info =
    match pri with Busy p when Int.equal p stmt_info.Clang_ast_t.si_pointer -> true | _ -> false


  (* Used by translation functions to handle potential cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)
  let compute_controls_to_parent trans_state loc ~node_name stmt_info res_states_children =
    let res_state = collect_controls trans_state.context.procdesc res_states_children in
    let create_node = own_priority_node trans_state.priority stmt_info && res_state.instrs <> [] in
    if create_node then (
      (* We need to create a node *)
      let node_kind = Procdesc.Node.Stmt_node node_name in
      let node =
        Procdesc.create_node trans_state.context.CContext.procdesc loc node_kind res_state.instrs
      in
      Procdesc.node_set_succs_exn trans_state.context.procdesc node trans_state.succ_nodes [] ;
      List.iter
        ~f:(fun leaf -> Procdesc.node_set_succs_exn trans_state.context.procdesc leaf [node] [])
        res_state.leaf_nodes ;
      (* Invariant: if root_nodes is empty then the params have not created a node.*)
      let root_nodes = if res_state.root_nodes <> [] then res_state.root_nodes else [node] in
      {res_state with root_nodes; leaf_nodes= [node]; instrs= []} )
    else (* The node is created by the parent. We just pass back nodes/leafs params *)
      res_state


  let compute_results_to_parent trans_state loc ~node_name stmt_info ~return trans_results =
    List.map trans_results ~f:(fun trans_result -> trans_result.control)
    |> compute_controls_to_parent trans_state loc ~node_name stmt_info
    |> mk_trans_result return


  let compute_control_to_parent trans_state loc ~node_name stmt_info control =
    compute_controls_to_parent trans_state loc ~node_name stmt_info [control]


  let compute_result_to_parent trans_state loc ~node_name stmt_info trans_result =
    compute_control_to_parent trans_state loc ~node_name stmt_info trans_result.control
    |> mk_trans_result trans_result.return
end

module Loops = struct
  type loop_kind =
    | For of
        { init: Clang_ast_t.stmt
        ; decl_stmt: Clang_ast_t.stmt
        ; condition: Clang_ast_t.stmt
        ; increment: Clang_ast_t.stmt
        ; body: Clang_ast_t.stmt }
    | While of {decl_stmt: Clang_ast_t.stmt; condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}
    | DoWhile of {condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}

  let get_body loop_kind =
    match loop_kind with For {body} | While {body} | DoWhile {body} -> body


  let get_cond loop_kind =
    match loop_kind with For {condition} | While {condition} | DoWhile {condition} -> condition
end

module Scope = struct
  module StmtMap = ClangPointers.Map

  let add_scope_vars_to_destroy var_map stmt_info vars =
    let ptr = stmt_info.Clang_ast_t.si_pointer in
    StmtMap.set var_map ~key:ptr ~data:vars


  let rec compute_vars vars_in_scope break_count var_map stmt =
    (* vars_in_scope corresponds to the list of all variables existing in the current scope *)
    (* break_count saves the number of variables in the current scope when entering the most recent loop *)
    (* there is an assumption that break can only be used in iteration statements *)
    let open Clang_ast_t in
    let get_var_info_from_decl = function VarDecl _ as decl -> Some decl | _ -> None in
    let get_new_vars = function
      | DeclStmt (_, _, decl_list) ->
          List.filter_map ~f:get_var_info_from_decl decl_list
      | _ ->
          []
    in
    let rec handle_instructions_block var_map vars_in_scope break_count instrs =
      match instrs with
      | [] ->
          (vars_in_scope, var_map)
      | stmt :: rest ->
          let new_var_map = compute_vars vars_in_scope break_count var_map stmt in
          let new_vars_in_stmt = get_new_vars stmt in
          handle_instructions_block new_var_map (new_vars_in_stmt @ vars_in_scope) break_count rest
    in
    (* TODO handle following stmts: *)
    (* GotoStmt _ |  | LabelStmt_ *)
    match stmt with
    | CompoundStmt (stmt_info, stmt_list) ->
        let vars, new_var_map =
          handle_instructions_block var_map vars_in_scope break_count stmt_list
        in
        (* vars contains the variables defined in the current compound statement + vars_in_scope *)
        let vars_to_destroy = List.take vars (List.length vars - List.length vars_in_scope) in
        add_scope_vars_to_destroy new_var_map stmt_info vars_to_destroy
    | ReturnStmt (stmt_info, _) ->
        add_scope_vars_to_destroy var_map stmt_info vars_in_scope
    | BreakStmt (stmt_info, _) | ContinueStmt (stmt_info, _) ->
        let vars_to_destroy = List.take vars_in_scope (List.length vars_in_scope - break_count) in
        add_scope_vars_to_destroy var_map stmt_info vars_to_destroy
    | WhileStmt (_, stmt_list)
    | DoStmt (_, stmt_list)
    | SwitchStmt (_, stmt_list)
    (* TODO handle variable declarations inside for / foreach *)
    | ForStmt (_, stmt_list)
    | CXXForRangeStmt (_, stmt_list) ->
        let break_count = List.length vars_in_scope in
        List.fold_left ~f:(compute_vars vars_in_scope break_count) stmt_list ~init:var_map
    | _ ->
        let stmt_list = snd (Clang_ast_proj.get_stmt_tuple stmt) in
        List.fold_left ~f:(compute_vars vars_in_scope break_count) stmt_list ~init:var_map


  let compute_vars_to_destroy body =
    List.fold_left ~f:(compute_vars [] 0) ~init:StmtMap.empty [body]
end

(** This function handles ObjC new/alloc and C++ new calls *)
let create_alloc_instrs integer_type_widths ~alloc_builtin ?size_exp ?placement_args_exps sil_loc
    function_type =
  let function_type, function_type_np =
    match function_type.Typ.desc with
    | Tptr (styp, Typ.Pk_pointer)
    | Tptr (styp, Typ.Pk_objc_weak)
    | Tptr (styp, Typ.Pk_objc_unsafe_unretained)
    | Tptr (styp, Typ.Pk_objc_autoreleasing) ->
        (function_type, styp)
    | _ ->
        (CType.add_pointer_to_typ function_type, function_type)
  in
  let ret_id = Ident.create_fresh Ident.knormal in
  let args =
    let nbytes =
      match function_type_np.Typ.desc with
      | Tint ikind ->
          Some (Typ.width_of_ikind integer_type_widths ikind / 8)
      | _ ->
          None
    in
    let sizeof_exp_ =
      Exp.Sizeof {typ= function_type_np; nbytes; dynamic_length= None; subtype= Subtype.exact}
    in
    let sizeof_exp =
      match size_exp with
      | Some exp ->
          Exp.BinOp (Binop.Mult (Some Typ.size_t), sizeof_exp_, exp)
      | None ->
          sizeof_exp_
    in
    let exp = (sizeof_exp, Typ.mk (Tint Typ.IULong)) in
    match placement_args_exps with Some args -> exp :: args | None -> [exp]
  in
  let ret_id_typ = (ret_id, function_type) in
  let stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun alloc_builtin), args, sil_loc, CallFlags.default)
  in
  (function_type, [stmt_call], Exp.Var ret_id)


let alloc_trans trans_state ~alloc_builtin loc stmt_info function_type =
  let integer_type_widths = trans_state.context.translation_unit_context.integer_type_widths in
  let function_type, instrs, exp =
    create_alloc_instrs integer_type_widths ~alloc_builtin loc function_type
  in
  let control_tmp = {empty_control with instrs} in
  PriorityNode.compute_control_to_parent trans_state loc ~node_name:(Call "alloc") stmt_info
    control_tmp
  |> mk_trans_result (exp, function_type)


let objc_new_trans trans_state ~alloc_builtin loc stmt_info cls_name function_type =
  let integer_type_widths = trans_state.context.translation_unit_context.integer_type_widths in
  let alloc_ret_type, alloc_stmt_call, alloc_ret_exp =
    create_alloc_instrs integer_type_widths ~alloc_builtin loc function_type
  in
  let init_ret_id = Ident.create_fresh Ident.knormal in
  let is_instance = true in
  let call_flags = {CallFlags.default with CallFlags.cf_virtual= is_instance} in
  let method_kind = ClangMethodKind.OBJC_INSTANCE in
  let pname =
    CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind cls_name CFrontend_config.init
      Typ.Procname.ObjC_Cpp.ObjCInstanceMethod
  in
  CMethod_trans.create_external_procdesc trans_state.context.CContext.translation_unit_context
    trans_state.context.CContext.cfg pname method_kind None ;
  let args = [(alloc_ret_exp, alloc_ret_type)] in
  let ret_id_typ = (init_ret_id, alloc_ret_type) in
  let init_stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun pname), args, loc, call_flags)
  in
  let instrs = alloc_stmt_call @ [init_stmt_call] in
  let res_trans_tmp = {empty_control with instrs} in
  let node_name = Procdesc.Node.CallObjCNew in
  PriorityNode.compute_control_to_parent trans_state loc ~node_name stmt_info res_trans_tmp
  |> mk_trans_result (Exp.Var init_ret_id, alloc_ret_type)


let new_or_alloc_trans trans_state loc stmt_info qual_type class_name_opt selector =
  let tenv = trans_state.context.CContext.tenv in
  let function_type = CType_decl.qual_type_to_sil_type tenv qual_type in
  let class_name =
    match class_name_opt with
    | Some class_name ->
        class_name
    | None ->
        CType.objc_classname_of_type function_type
  in
  if String.equal selector CFrontend_config.alloc then
    alloc_trans trans_state ~alloc_builtin:BuiltinDecl.__objc_alloc_no_fail loc stmt_info
      function_type
  else if String.equal selector CFrontend_config.new_str then
    objc_new_trans trans_state ~alloc_builtin:BuiltinDecl.__objc_alloc_no_fail loc stmt_info
      class_name function_type
  else Logging.die InternalError "Expected selector new or alloc but got, %s" selector


let cpp_new_trans integer_type_widths sil_loc function_type size_exp placement_args_exps =
  let alloc_builtin =
    match placement_args_exps with
    | [] -> (
      match size_exp with Some _ -> BuiltinDecl.__new_array | None -> BuiltinDecl.__new )
    | _ ->
        (* TODO: call user defined `new` when there is more than one placement argument *)
        BuiltinDecl.__placement_new
  in
  let function_type, stmt_call, exp =
    create_alloc_instrs integer_type_widths ~alloc_builtin ?size_exp ~placement_args_exps sil_loc
      function_type
  in
  mk_trans_result (exp, function_type) {empty_control with instrs= stmt_call}


let create_call_to_free_cf sil_loc exp typ =
  let pname = BuiltinDecl.__free_cf in
  let stmt_call =
    Sil.Call
      ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
      , Exp.Const (Const.Cfun pname)
      , [(exp, typ)]
      , sil_loc
      , CallFlags.default )
  in
  stmt_call


let dereference_var_sil (exp, typ) sil_loc =
  let id = Ident.create_fresh Ident.knormal in
  let sil_instr = Sil.Load (id, exp, typ, sil_loc) in
  ([sil_instr], Exp.Var id)


let dereference_value_from_result ?(strip_pointer = false) source_range sil_loc trans_result =
  let obj_sil, class_typ = trans_result.return in
  let typ_no_ptr =
    match class_typ.Typ.desc with
    | Tptr (typ, _) ->
        typ
    | _ ->
        CFrontend_config.incorrect_assumption __POS__ source_range
          "Expected pointer type but found type %a" (Typ.pp_full Pp.text) class_typ
  in
  let cast_typ = if strip_pointer then typ_no_ptr else class_typ in
  let cast_inst, cast_exp = dereference_var_sil (obj_sil, cast_typ) sil_loc in
  { trans_result with
    control= {trans_result.control with instrs= trans_result.control.instrs @ cast_inst}
  ; return= (cast_exp, cast_typ) }


let cast_operation cast_kind ((exp, typ) as exp_typ) cast_typ sil_loc =
  match cast_kind with
  | `NoOp | `DerivedToBase | `UncheckedDerivedToBase ->
      (* These casts ignore change of type *)
      ([], exp_typ)
  | `BitCast when Typ.is_pointer_to_int cast_typ ->
      ([], (Exp.Cast (cast_typ, exp), cast_typ))
  | `IntegralCast when Typ.is_unsigned_int cast_typ ->
      ([], (Exp.Cast (cast_typ, exp), cast_typ))
  | `BitCast | `IntegralCast | `IntegralToBoolean ->
      (* This is treated as a nop by returning the same expressions exps*)
      ([], (exp, cast_typ))
  | `CPointerToObjCPointerCast when Objc_models.is_core_lib_type typ ->
      (* Translation of __bridge_transfer *)
      let instr = create_call_to_free_cf sil_loc exp typ in
      ([instr], (exp, cast_typ))
  | `LValueToRValue ->
      (* Takes an LValue and allow it to use it as RValue. *)
      (* So we assign the LValue to a temp and we pass it to the parent.*)
      let instrs, deref_exp = dereference_var_sil (exp, cast_typ) sil_loc in
      (instrs, (deref_exp, cast_typ))
  | `NullToPointer ->
      if Exp.is_zero exp then ([], (Exp.null, cast_typ)) else ([], (exp, cast_typ))
  | `ToVoid ->
      (* If exp is not used later, i.e. as in `(void) exp;` we miss reads *)
      (* We create a call to skip function passing the exp as a parameter *)
      let skip_builtin = Exp.Const (Const.Cfun BuiltinDecl.__infer_skip) in
      let args = [(exp, typ)] in
      let no_id = Ident.create_none () in
      let call_instr =
        Sil.Call ((no_id, cast_typ), skip_builtin, args, sil_loc, CallFlags.default)
      in
      ([call_instr], (exp, cast_typ))
  | _ ->
      L.(debug Capture Verbose)
        "@\nWARNING: Missing translation for Cast Kind %s. The construct has been ignored...@\n"
        (Clang_ast_j.string_of_cast_kind cast_kind) ;
      ([], (exp, cast_typ))


let trans_assertion_failure sil_loc (context : CContext.t) =
  let assert_fail_builtin = Exp.Const (Const.Cfun BuiltinDecl.__infer_fail) in
  let args = [(Exp.Const (Const.Cstr Config.default_failure_name), Typ.mk Tvoid)] in
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_typ = Typ.mk Tvoid in
  let call_instr =
    Sil.Call ((ret_id, ret_typ), assert_fail_builtin, args, sil_loc, CallFlags.default)
  in
  let exit_node = Procdesc.get_exit_node context.procdesc
  and failure_node =
    Procdesc.create_node context.CContext.procdesc sil_loc
      (Procdesc.Node.Stmt_node AssertionFailure) [call_instr]
  in
  Procdesc.node_set_succs_exn context.procdesc failure_node [exit_node] [] ;
  mk_trans_result (Exp.Var ret_id, ret_typ) {empty_control with root_nodes= [failure_node]}


let trans_assume_false sil_loc (context : CContext.t) succ_nodes =
  let if_kind = Sil.Ik_land_lor in
  let instrs_cond = [Sil.Prune (Exp.zero, sil_loc, true, if_kind)] in
  let prune_node =
    Procdesc.create_node context.CContext.procdesc sil_loc (Nodes.prune_kind true if_kind)
      instrs_cond
  in
  Procdesc.node_set_succs_exn context.procdesc prune_node succ_nodes [] ;
  mk_trans_result
    (Exp.zero, Typ.(mk (Tint IInt)))
    {empty_control with root_nodes= [prune_node]; leaf_nodes= [prune_node]}


let trans_assertion trans_state sil_loc =
  let context = trans_state.context in
  if Config.report_custom_error then trans_assertion_failure sil_loc context
  else trans_assume_false sil_loc context trans_state.succ_nodes


let trans_builtin_expect params_trans_res =
  (* Translate call to __builtin_expect as the first argument *)
  (* for simpler symbolic execution *)
  match params_trans_res with [_; fst_arg_res; _] -> Some fst_arg_res | _ -> None


let trans_std_addressof params_trans_res =
  (* Translate call to std::addressof as the first argument *)
  (* for simpler symbolic execution. *)
  match params_trans_res with [_; fst_arg_res] -> Some fst_arg_res | _ -> assert false


let trans_replace_with_deref_first_arg source_range sil_loc params_trans_res ~cxx_method_call =
  let first_arg_res_trans =
    if cxx_method_call then
      match params_trans_res with
      | fst_arg_res :: _ ->
          assert (Option.is_some fst_arg_res.method_name) ;
          fst_arg_res
      | [] ->
          assert false
    else
      match params_trans_res with _ :: fst_arg_res :: _ -> fst_arg_res | [] | [_] -> assert false
  in
  dereference_value_from_result ~strip_pointer:true source_range sil_loc first_arg_res_trans


let builtin_trans trans_state source_range loc params_trans_res pname =
  if CTrans_models.is_assert_log pname then Some (trans_assertion trans_state loc)
  else if CTrans_models.is_builtin_expect pname then trans_builtin_expect params_trans_res
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some
      (trans_replace_with_deref_first_arg source_range loc params_trans_res ~cxx_method_call:false)
  else if CTrans_models.is_std_addressof pname then trans_std_addressof params_trans_res
  else None


let cxx_method_builtin_trans trans_state source_range loc params_trans_res pname =
  if CTrans_models.is_assert_log pname then Some (trans_assertion trans_state loc)
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some
      (trans_replace_with_deref_first_arg source_range loc params_trans_res ~cxx_method_call:true)
  else None


let define_condition_side_effects ((e_cond_exp, e_cond_typ) as e_cond) instrs_cond sil_loc =
  match e_cond_exp with
  | Exp.Lvar pvar ->
      let id = Ident.create_fresh Ident.knormal in
      ((Exp.Var id, e_cond_typ), [Sil.Load (id, Exp.Lvar pvar, e_cond_typ, sil_loc)])
  | _ ->
      (e_cond, instrs_cond)


let is_superinstance mei =
  match mei.Clang_ast_t.omei_receiver_kind with `SuperInstance -> true | _ -> false


let is_null_stmt s = match s with Clang_ast_t.NullStmt _ -> true | _ -> false

let extract_stmt_from_singleton stmt_list source_range warning_string =
  extract_item_from_singleton stmt_list
    (Pp.to_string ~f:Clang_ast_j.string_of_stmt)
    source_range warning_string


module Self = struct
  exception
    SelfClassException of
      { class_name: Typ.Name.t
      ; position: Logging.ocaml_pos
      ; source_range: Clang_ast_t.source_range }

  let add_self_parameter_for_super_instance stmt_info context procname loc mei =
    if is_superinstance mei then
      let typ, self_expr, instrs =
        let t' =
          CType.add_pointer_to_typ
            (Typ.mk (Tstruct (CContext.get_curr_class_typename stmt_info context)))
        in
        let e = Exp.Lvar (Pvar.mk (Mangled.from_string CFrontend_config.self) procname) in
        let id = Ident.create_fresh Ident.knormal in
        (t', Exp.Var id, [Sil.Load (id, e, t', loc)])
      in
      Some (mk_trans_result (self_expr, typ) {empty_control with instrs})
    else None


  let is_var_self pvar is_objc_method =
    let is_self = String.equal (Mangled.to_string (Pvar.get_name pvar)) CFrontend_config.self in
    is_self && is_objc_method
end

let rec contains_opaque_value_expr s =
  match s with
  | Clang_ast_t.OpaqueValueExpr _ ->
      true
  | _ -> (
    match snd (Clang_ast_proj.get_stmt_tuple s) with
    | [] ->
        false
    | s'' :: _ ->
        contains_opaque_value_expr s'' )


(* checks if a unary operator is a logic negation applied to integers*)
let is_logical_negation_of_int tenv ei uoi =
  match
    ( (CType_decl.qual_type_to_sil_type tenv ei.Clang_ast_t.ei_qual_type).desc
    , uoi.Clang_ast_t.uoi_kind )
  with
  | Typ.Tint _, `LNot ->
      true
  | _, _ ->
      false


let mk_fresh_void_exp_typ () = (Exp.Var (Ident.create_fresh Ident.knormal), Typ.mk Tvoid)

let mk_fresh_void_id_typ () = (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)

let mk_fresh_void_return () =
  let id = Ident.create_fresh Ident.knormal and void = Typ.mk Tvoid in
  ((id, void), (Exp.Var id, void))


let last_or_mk_fresh_void_exp_typ exp_typs =
  match List.last exp_typs with
  | Some last_exp_typ ->
      last_exp_typ
  | None ->
      mk_fresh_void_exp_typ ()
