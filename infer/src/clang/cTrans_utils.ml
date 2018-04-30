(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Utility methods to support the translation of clang ast constructs into sil instructions.  *)

module L = Logging

(** Extract the element of a singleton list. If the list is not a singleton It stops the computation
   giving a warning. We use this because we assume in many places that a list is just a
   singleton. We use the warning if to see which assumption was not correct. *)
let extract_item_from_singleton l warning_string failure_val =
  match l with
  | [item] ->
      item
  | _ ->
      L.(debug Capture Medium) "%s" warning_string ;
      failure_val


let dummy_exp = (Exp.minus_one, Typ.mk (Tint Typ.IInt))

(** Extract the element of a singleton list. If the list is not a singleton Gives a warning and
   return -1 as standard value indicating something went wrong. *)
let extract_exp_from_list el warning_string =
  extract_item_from_singleton el warning_string dummy_exp


module Nodes = struct
  let prune_kind b if_kind = Procdesc.Node.Prune_node (b, if_kind, string_of_bool b ^ " Branch")

  let is_true_prune_node n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Prune_node (true, _, _) ->
        true
    | _ ->
        false


  let create_node node_kind instrs loc context =
    let procdesc = CContext.get_procdesc context in
    Procdesc.create_node procdesc loc node_kind instrs


  let create_prune_node ~branch ~negate_cond e_cond instrs_cond loc if_kind context =
    let e_cond', _ =
      extract_exp_from_list e_cond
        "@\nWARNING: Missing expression for Conditional operator. Need to be fixed"
    in
    let e_cond'' = if negate_cond then Exp.UnOp (Unop.LNot, e_cond', None) else e_cond' in
    let instrs_cond' = instrs_cond @ [Sil.Prune (e_cond'', loc, branch, if_kind)] in
    create_node (prune_kind branch if_kind) instrs_cond' loc context


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
      let new_node = Nodes.create_node (Procdesc.Node.Skip_node node_name) [] sil_loc context in
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

(* A translation state. It provides the translation function with the info*)
(* it need to carry on the tranlsation. *)
type trans_state =
  { context: CContext.t
  ; (* current context of the translation *)
    succ_nodes: Procdesc.Node.t list
  ; (* successor nodes in the cfg *)
    continuation: continuation option
  ; (* current continuation *)
    priority: priority_node
  ; var_exp_typ: (Exp.t * Typ.t) option
  ; opaque_exp: (Exp.t * Typ.t) option }

(* A translation result. It is returned by the translation function. *)
type trans_result =
  { root_nodes: Procdesc.Node.t list
  ; (* Top cfg nodes (root) created by the translation *)
    leaf_nodes: Procdesc.Node.t list
  ; (* Bottom cfg nodes (leaf) created by the translate *)
    instrs: Sil.instr list
  ; (* list of SIL instruction that need to be placed in cfg nodes of the parent*)
    exps: (Exp.t * Typ.t) list
  ; (* SIL expressions resulting from translation of clang stmt *)
    initd_exps: Exp.t list
  ; is_cpp_call_virtual: bool }

(* Empty result translation *)
let empty_res_trans =
  {root_nodes= []; leaf_nodes= []; instrs= []; exps= []; initd_exps= []; is_cpp_call_virtual= false}


let undefined_expression () = Exp.Var (Ident.create_fresh Ident.knormal)

(** Collect the results of translating a list of instructions, and link up the nodes created. *)
let collect_res_trans pdesc l =
  let rec collect l rt =
    match l with
    | [] ->
        rt
    | rt' :: l' ->
        let root_nodes = if rt.root_nodes <> [] then rt.root_nodes else rt'.root_nodes in
        let leaf_nodes = if rt'.leaf_nodes <> [] then rt'.leaf_nodes else rt.leaf_nodes in
        if rt'.root_nodes <> [] then
          List.iter
            ~f:(fun n -> Procdesc.node_set_succs_exn pdesc n rt'.root_nodes [])
            rt.leaf_nodes ;
        collect l'
          { root_nodes
          ; leaf_nodes
          ; instrs= List.rev_append rt'.instrs rt.instrs
          ; exps= List.rev_append rt'.exps rt.exps
          ; initd_exps= List.rev_append rt'.initd_exps rt.initd_exps
          ; is_cpp_call_virtual= false }
  in
  let rt = collect l empty_res_trans in
  {rt with instrs= List.rev rt.instrs; exps= List.rev rt.exps; initd_exps= List.rev rt.initd_exps}


(* priority_node is used to enforce some kind of policy for creating nodes *)
(* in the cfg. Certain elements of the AST must__ create nodes therefore   *)
(* there is no need for them to use priority_node. Certain elements        *)
(* instead need or need not to create a node depending of certain factors. *)
(* When an element of the latter kind wants to create a node it must claim *)
(* priority first (like taking a lock). priority can be claimes only when  *)
(* it is free. If an element of AST succedes in claiming priority its id   *)
(* (pointer) is recorded in priority. After an element has finished it     *)
(* frees the priority. In general an AST element E checks if an ancestor   *)
(* has claimed priority. If priority is already claimed E does not have to *)
(* create a node. If priority is free then it means E has to create the    *)
(* node. Then E claims priority and release it afterward.                  *)
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


  (* Used by translation functions to handle potenatial cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)
  let compute_results_to_parent trans_state loc nd_name stmt_info res_states_children =
    let res_state = collect_res_trans trans_state.context.procdesc res_states_children in
    let create_node = own_priority_node trans_state.priority stmt_info && res_state.instrs <> [] in
    if create_node then (
      (* We need to create a node *)
      let node_kind = Procdesc.Node.Stmt_node nd_name in
      let node = Nodes.create_node node_kind res_state.instrs loc trans_state.context in
      Procdesc.node_set_succs_exn trans_state.context.procdesc node trans_state.succ_nodes [] ;
      List.iter
        ~f:(fun leaf -> Procdesc.node_set_succs_exn trans_state.context.procdesc leaf [node] [])
        res_state.leaf_nodes ;
      (* Invariant: if root_nodes is empty then the params have not created a node.*)
      let root_nodes = if res_state.root_nodes <> [] then res_state.root_nodes else [node] in
      {res_state with root_nodes; leaf_nodes= [node]; instrs= []; exps= []} )
    else
      (* The node is created by the parent. We just pass back nodes/leafs params *)
      {res_state with exps= []}
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
let create_alloc_instrs ~alloc_builtin ?size_exp ?placement_args_exps sil_loc function_type =
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
    let sizeof_exp_ =
      Exp.Sizeof {typ= function_type_np; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
    in
    let sizeof_exp =
      match size_exp with
      | Some exp ->
          Exp.BinOp (Binop.Mult, sizeof_exp_, exp)
      | None ->
          sizeof_exp_
    in
    let exp = (sizeof_exp, Typ.mk (Tint Typ.IULong)) in
    match placement_args_exps with Some args -> exp :: args | None -> [exp]
  in
  let ret_id_typ = Some (ret_id, function_type) in
  let stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun alloc_builtin), args, sil_loc, CallFlags.default)
  in
  (function_type, [stmt_call], Exp.Var ret_id)


let alloc_trans trans_state ~alloc_builtin loc stmt_info function_type =
  let function_type, instrs, exp = create_alloc_instrs ~alloc_builtin loc function_type in
  let res_trans_tmp = {empty_res_trans with instrs} in
  let res_trans =
    let nname = "Call alloc" in
    PriorityNode.compute_results_to_parent trans_state loc nname stmt_info [res_trans_tmp]
  in
  {res_trans with exps= [(exp, function_type)]}


let objc_new_trans trans_state ~alloc_builtin loc stmt_info cls_name function_type =
  let alloc_ret_type, alloc_stmt_call, alloc_ret_exp =
    create_alloc_instrs ~alloc_builtin loc function_type
  in
  let init_ret_id = Ident.create_fresh Ident.knormal in
  let is_instance = true in
  let call_flags = {CallFlags.default with CallFlags.cf_virtual= is_instance} in
  let method_kind = ProcAttributes.OBJC_INSTANCE in
  let pname =
    CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind cls_name CFrontend_config.init
      Typ.Procname.ObjC_Cpp.ObjCInstanceMethod
  in
  CMethod_trans.create_external_procdesc trans_state.context.CContext.cfg pname method_kind None ;
  let args = [(alloc_ret_exp, alloc_ret_type)] in
  let ret_id_typ = Some (init_ret_id, alloc_ret_type) in
  let init_stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun pname), args, loc, call_flags)
  in
  let instrs = alloc_stmt_call @ [init_stmt_call] in
  let res_trans_tmp = {empty_res_trans with instrs} in
  let res_trans =
    let nname = "Call objC new" in
    PriorityNode.compute_results_to_parent trans_state loc nname stmt_info [res_trans_tmp]
  in
  {res_trans with exps= [(Exp.Var init_ret_id, alloc_ret_type)]}


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


let cpp_new_trans sil_loc function_type size_exp placement_args_exps =
  let alloc_builtin =
    match placement_args_exps with
    | [] -> (
      match size_exp with Some _ -> BuiltinDecl.__new_array | None -> BuiltinDecl.__new )
    | _ ->
        (* TODO: call user defined `new` when there is more than one placement argument *)
        BuiltinDecl.__placement_new
  in
  let function_type, stmt_call, exp =
    create_alloc_instrs ~alloc_builtin ?size_exp ~placement_args_exps sil_loc function_type
  in
  {empty_res_trans with instrs= stmt_call; exps= [(exp, function_type)]}


let create_call_to_free_cf sil_loc exp typ =
  let pname = BuiltinDecl.__free_cf in
  let stmt_call =
    Sil.Call (None, Exp.Const (Const.Cfun pname), [(exp, typ)], sil_loc, CallFlags.default)
  in
  stmt_call


let dereference_var_sil (exp, typ) sil_loc =
  let id = Ident.create_fresh Ident.knormal in
  let sil_instr = Sil.Load (id, exp, typ, sil_loc) in
  ([sil_instr], Exp.Var id)


(** Given trans_result with ONE expression, create temporary variable with value of an expression
    assigned to it *)
let dereference_value_from_result sil_loc trans_result ~strip_pointer =
  let obj_sil, class_typ = extract_exp_from_list trans_result.exps "" in
  let typ_no_ptr =
    match class_typ.Typ.desc with
    | Tptr (typ, _) ->
        typ
    | _ ->
        CFrontend_config.incorrect_assumption __POS__
          (CAst_utils.dummy_source_range ())
          "Expected pointer type but found type %a" (Typ.pp_full Pp.text) class_typ
  in
  let cast_typ = if strip_pointer then typ_no_ptr else class_typ in
  let cast_inst, cast_exp = dereference_var_sil (obj_sil, cast_typ) sil_loc in
  {trans_result with instrs= trans_result.instrs @ cast_inst; exps= [(cast_exp, cast_typ)]}


let cast_operation cast_kind exps cast_typ sil_loc =
  let exp, typ = extract_exp_from_list exps "" in
  match cast_kind with
  | `NoOp | `DerivedToBase | `UncheckedDerivedToBase ->
      (* These casts ignore change of type *)
      ([], (exp, typ))
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
  | _ ->
      L.(debug Capture Verbose)
        "@\nWARNING: Missing translation for Cast Kind %s. The construct has been ignored...@\n"
        (Clang_ast_j.string_of_cast_kind cast_kind) ;
      ([], (exp, cast_typ))


let trans_assertion_failure sil_loc (context: CContext.t) =
  let assert_fail_builtin = Exp.Const (Const.Cfun BuiltinDecl.__infer_fail) in
  let args = [(Exp.Const (Const.Cstr Config.default_failure_name), Typ.mk Tvoid)] in
  let call_instr = Sil.Call (None, assert_fail_builtin, args, sil_loc, CallFlags.default) in
  let exit_node = Procdesc.get_exit_node (CContext.get_procdesc context)
  and failure_node =
    Nodes.create_node (Procdesc.Node.Stmt_node "Assertion failure") [call_instr] sil_loc context
  in
  Procdesc.node_set_succs_exn context.procdesc failure_node [exit_node] [] ;
  {empty_res_trans with root_nodes= [failure_node]}


let trans_assume_false sil_loc (context: CContext.t) succ_nodes =
  let if_kind = Sil.Ik_land_lor in
  let instrs_cond = [Sil.Prune (Exp.zero, sil_loc, true, if_kind)] in
  let prune_node = Nodes.create_node (Nodes.prune_kind true if_kind) instrs_cond sil_loc context in
  Procdesc.node_set_succs_exn context.procdesc prune_node succ_nodes [] ;
  {empty_res_trans with root_nodes= [prune_node]; leaf_nodes= [prune_node]}


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


let trans_replace_with_deref_first_arg sil_loc params_trans_res ~cxx_method_call =
  let first_arg_res_trans =
    match params_trans_res with
    | _ :: fst_arg_res :: _ when not cxx_method_call ->
        fst_arg_res
    | ({exps= _method_exp :: this_exp} as fst_arg_res) :: _ when cxx_method_call ->
        (* method_deref_trans uses different format to store first argument - it stores
           two things in exps: [method_exp; this_exp].
           We need to get rid of first exp before calling dereference_value_from_result *)
        {fst_arg_res with exps= this_exp}
    | _ ->
        assert false
  in
  dereference_value_from_result sil_loc first_arg_res_trans ~strip_pointer:true


let builtin_trans trans_state loc params_trans_res pname =
  if CTrans_models.is_assert_log pname then Some (trans_assertion trans_state loc)
  else if CTrans_models.is_builtin_expect pname then trans_builtin_expect params_trans_res
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some (trans_replace_with_deref_first_arg loc params_trans_res ~cxx_method_call:false)
  else if CTrans_models.is_std_addressof pname then trans_std_addressof params_trans_res
  else None


let cxx_method_builtin_trans trans_state loc params_trans_res pname =
  if CTrans_models.is_assert_log pname then Some (trans_assertion trans_state loc)
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some (trans_replace_with_deref_first_arg loc params_trans_res ~cxx_method_call:true)
  else None


let define_condition_side_effects e_cond instrs_cond sil_loc =
  let e', typ =
    extract_exp_from_list e_cond "@\nWARNING: Missing expression in IfStmt. Need to be fixed@\n"
  in
  match e' with
  | Exp.Lvar pvar ->
      let id = Ident.create_fresh Ident.knormal in
      ([(Exp.Var id, typ)], [Sil.Load (id, Exp.Lvar pvar, typ, sil_loc)])
  | _ ->
      ([(e', typ)], instrs_cond)


let is_superinstance mei =
  match mei.Clang_ast_t.omei_receiver_kind with `SuperInstance -> true | _ -> false


let is_null_stmt s = match s with Clang_ast_t.NullStmt _ -> true | _ -> false

let extract_stmt_from_singleton stmt_list warning_string =
  extract_item_from_singleton stmt_list warning_string (CAst_utils.dummy_stmt ())


module Self = struct
  exception
    SelfClassException of
      { class_name: Typ.Name.t
      ; position: Logging.ocaml_pos
      ; source_range: Clang_ast_t.source_range }

  let add_self_parameter_for_super_instance stmt_info context procname loc mei =
    if is_superinstance mei then
      let typ, self_expr, ins =
        let t' =
          CType.add_pointer_to_typ
            (Typ.mk (Tstruct (CContext.get_curr_class_typename stmt_info context)))
        in
        let e = Exp.Lvar (Pvar.mk (Mangled.from_string CFrontend_config.self) procname) in
        let id = Ident.create_fresh Ident.knormal in
        (t', Exp.Var id, [Sil.Load (id, e, t', loc)])
      in
      {empty_res_trans with exps= [(self_expr, typ)]; instrs= ins}
    else empty_res_trans


  let is_var_self pvar is_objc_method =
    let is_self = String.equal (Mangled.to_string (Pvar.get_name pvar)) CFrontend_config.self in
    is_self && is_objc_method
end

let rec contains_opaque_value_expr s =
  match s with
  | Clang_ast_t.OpaqueValueExpr _ ->
      true
  | _ ->
    match snd (Clang_ast_proj.get_stmt_tuple s) with
    | [] ->
        false
    | s'' :: _ ->
        contains_opaque_value_expr s''


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
