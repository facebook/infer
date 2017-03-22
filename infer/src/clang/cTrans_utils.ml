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

exception TemplatedCodeException of Clang_ast_t.stmt

(* Extract the element of a singleton list. If the list is not a singleton *)
(* It stops the computation giving a warning. We use this because we       *)
(* assume in many places that a list is just a singleton. We use the       *)
(* warning if to see which assumption was not correct                      *)
let extract_item_from_singleton l warning_string failure_val =
  match l with
  | [item] -> item
  | _ -> Logging.err_debug "%s" warning_string; failure_val

let dummy_exp = (Exp.minus_one, Typ.Tint Typ.IInt)

(* Extract the element of a singleton list. If the list is not a singleton *)
(* Gives a warning and return -1 as standard value indicating something    *)
(* went wrong.                                                             *)
let extract_exp_from_list el warning_string =
  extract_item_from_singleton el warning_string dummy_exp

module Nodes =
struct

  let prune_kind b = Procdesc.Node.Prune_node(b, Sil.Ik_bexp , ((string_of_bool b)^" Branch"))

  let is_join_node n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Join_node -> true
    | _ -> false

  let is_prune_node n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Prune_node _ -> true
    | _ -> false

  let is_true_prune_node n =
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Prune_node(true, _, _) -> true
    | _ -> false

  let create_node node_kind instrs loc context =
    let procdesc = CContext.get_procdesc context in
    Procdesc.create_node procdesc loc node_kind instrs

  let create_prune_node branch e_cond instrs_cond loc ik context =
    let (e_cond', _) = extract_exp_from_list e_cond
        "\nWARNING: Missing expression for Conditional operator. Need to be fixed" in
    let e_cond'' =
      if branch then
        Exp.BinOp(Binop.Ne, e_cond', Exp.zero)
      else
        Exp.BinOp(Binop.Eq, e_cond', Exp.zero) in
    let instrs_cond'= instrs_cond @ [Sil.Prune(e_cond'', loc, branch, ik)] in
    create_node (prune_kind branch) instrs_cond' loc context

  (** Check if this binary opertor requires the creation of a node in the cfg. *)
  let is_binary_assign_op boi =
    match boi.Clang_ast_t.boi_kind with
    | `Assign | `MulAssign | `DivAssign | `RemAssign | `AddAssign | `SubAssign
    | `ShlAssign | `ShrAssign | `AndAssign | `XorAssign | `OrAssign -> true
    | `PtrMemD | `PtrMemI | `Mul | `Div | `Rem | `Add | `Sub | `Shl | `Shr
    | `LT | `GT | `LE | `GE | `EQ | `NE | `And | `Xor | `Or | `LAnd | `LOr
    | `Comma -> false

  (** Check if this unary opertor requires the creation of a node in the cfg. *)
  let need_unary_op_node uoi =
    match uoi.Clang_ast_t.uoi_kind with
    | `PostInc | `PostDec | `PreInc | `PreDec | `AddrOf | `Deref | `Plus -> true
    | `Minus | `Not | `LNot | `Real | `Imag | `Extension | `Coawait -> false

end

module GotoLabel =
struct

  let find_goto_label context label sil_loc =
    try
      Hashtbl.find context.CContext.label_map label
    with Not_found ->
      let node_name = Format.sprintf "GotoLabel_%s" label in
      let new_node = Nodes.create_node (Procdesc.Node.Skip_node node_name) [] sil_loc context in
      Hashtbl.add context.CContext.label_map label new_node;
      new_node
end

type continuation = {
  break: Procdesc.Node.t list;
  continue: Procdesc.Node.t list;
  return_temp : bool; (* true if temps should not be removed in the node but returned to ancestors *)
}

let is_return_temp continuation =
  match continuation with
  | Some cont -> cont.return_temp
  | _ -> false

let ids_to_parent cont ids =
  if is_return_temp cont then ids else []

let ids_to_node cont ids =
  if is_return_temp cont then [] else ids

let mk_cond_continuation cont =
  match cont with
  | Some cont' -> Some { cont' with return_temp = true; }
  | None -> Some { break =[]; continue =[]; return_temp = true;}

type priority_node =
  | Free
  | Busy of Clang_ast_t.pointer

(* A translation state. It provides the translation function with the info*)
(* it need to carry on the tranlsation. *)
type trans_state = {
  context: CContext.t; (* current context of the translation *)
  succ_nodes: Procdesc.Node.t list; (* successor nodes in the cfg *)
  continuation: continuation option; (* current continuation *)
  priority: priority_node;
  var_exp_typ: (Exp.t * Typ.t) option;
  opaque_exp: (Exp.t * Typ.t) option;
  obj_bridged_cast_typ : Typ.t option
}

(* A translation result. It is returned by the translation function. *)
type trans_result = {
  root_nodes: Procdesc.Node.t list; (* Top cfg nodes (root) created by the translation *)
  leaf_nodes: Procdesc.Node.t list; (* Bottom cfg nodes (leaf) created by the translate *)
  instrs: Sil.instr list; (* list of SIL instruction that need to be placed in cfg nodes of the parent*)
  exps: (Exp.t * Typ.t) list; (* SIL expressions resulting from translation of clang stmt *)
  initd_exps: Exp.t list;
  is_cpp_call_virtual : bool;
}

(* Empty result translation *)
let empty_res_trans = {
  root_nodes = [];
  leaf_nodes = [];
  instrs = [];
  exps = [];
  initd_exps = [];
  is_cpp_call_virtual = false;
}

let undefined_expression () = Exp.Var (Ident.create_fresh Ident.knormal)

(** Collect the results of translating a list of instructions, and link up the nodes created. *)
let collect_res_trans pdesc l =
  let rec collect l rt =
    match l with
    | [] -> rt
    | rt':: l' ->
        let root_nodes =
          if rt.root_nodes <> [] then rt.root_nodes
          else rt'.root_nodes in
        let leaf_nodes =
          if rt'.leaf_nodes <> [] then rt'.leaf_nodes
          else rt.leaf_nodes in
        if rt'.root_nodes <> [] then
          List.iter
            ~f:(fun n -> Procdesc.node_set_succs_exn pdesc n rt'.root_nodes [])
            rt.leaf_nodes;
        collect l'
          { root_nodes = root_nodes;
            leaf_nodes = leaf_nodes;
            instrs = List.rev_append rt'.instrs rt.instrs;
            exps = List.rev_append rt'.exps rt.exps;
            initd_exps = List.rev_append rt'.initd_exps rt.initd_exps;
            is_cpp_call_virtual = false; } in
  let rt = collect l empty_res_trans in
  {
    rt with
    instrs = List.rev rt.instrs;
    exps = List.rev rt.exps;
    initd_exps = List.rev rt.initd_exps;
  }

let extract_var_exp_or_fail transt_state =
  match transt_state.var_exp_typ with
  | Some var_exp_typ -> var_exp_typ
  | None -> assert false

(* priority_node is used to enforce some kind of policy for creating nodes *)
(* in the cfg. Certain elements of the AST _must_ create nodes therefore   *)
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
module PriorityNode =
struct

  type t = priority_node

  let try_claim_priority_node trans_state stmt_info =
    match trans_state.priority with
    | Free ->
        Logging.out_debug "Priority is free. Locking priority node in %d\n@."
          stmt_info.Clang_ast_t.si_pointer;
        { trans_state with priority = Busy stmt_info.Clang_ast_t.si_pointer }
    | _ ->
        Logging.out_debug "Priority busy in %d. No claim possible\n@."
          stmt_info.Clang_ast_t.si_pointer;
        trans_state

  let force_claim_priority_node trans_state stmt_info =
    { trans_state with priority = Busy stmt_info.Clang_ast_t.si_pointer }


  let is_priority_free trans_state =
    match trans_state.priority with
    | Free -> true
    | _ -> false

  let own_priority_node pri stmt_info =
    match pri with
    | Busy p when Int.equal p stmt_info.Clang_ast_t.si_pointer -> true
    | _ -> false

  (* Used by translation functions to handle potenatial cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)
  let compute_results_to_parent trans_state loc nd_name stmt_info res_states_children =
    let res_state = collect_res_trans trans_state.context.procdesc res_states_children in
    let create_node = own_priority_node trans_state.priority stmt_info && res_state.instrs <> [] in
    if create_node then
      (* We need to create a node *)
      let node_kind = Procdesc.Node.Stmt_node (nd_name) in
      let node = Nodes.create_node node_kind res_state.instrs loc trans_state.context in
      Procdesc.node_set_succs_exn trans_state.context.procdesc node trans_state.succ_nodes [];
      List.iter
        ~f:(fun leaf -> Procdesc.node_set_succs_exn trans_state.context.procdesc leaf [node] [])
        res_state.leaf_nodes;
      (* Invariant: if root_nodes is empty then the params have not created a node.*)
      let root_nodes = (if res_state.root_nodes <> [] then res_state.root_nodes
                        else [node]) in
      { res_state with
        root_nodes = root_nodes;
        leaf_nodes = [node];
        instrs = [];
        exps = [];
      }
    else
      (* The node is created by the parent. We just pass back nodes/leafs params *)
      { res_state with exps = []}

end

module Loops =
struct

  type loop_kind =
    | For of Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt
    (* init, decl_stmt, condition, increment and body *)
    | While of Clang_ast_t.stmt option * Clang_ast_t.stmt * Clang_ast_t.stmt
    (* decl_stmt, condition and body *)
    | DoWhile of Clang_ast_t.stmt * Clang_ast_t.stmt  (* condition and body *)

  let loop_kind_to_if_kind loop_kind =
    match loop_kind with
    | For _ -> Sil.Ik_for
    | While _ -> Sil.Ik_while
    | DoWhile _ -> Sil.Ik_dowhile

  let get_body loop_kind =
    match loop_kind with
    | For (_, _, _, _, body) | While (_, _, body) | DoWhile (_, body) -> body

  let get_cond loop_kind =
    match loop_kind with
    | For (_, _, cond, _, _) | While (_, cond, _) | DoWhile (cond, _) -> cond
end

(** This function handles ObjC new/alloc and C++ new calls *)
let create_alloc_instrs sil_loc function_type fname size_exp_opt procname_opt =
  let function_type, function_type_np =
    match function_type with
    | Typ.Tptr (styp, Typ.Pk_pointer)
    | Typ.Tptr (styp, Typ.Pk_objc_weak)
    | Typ.Tptr (styp, Typ.Pk_objc_unsafe_unretained)
    | Typ.Tptr (styp, Typ.Pk_objc_autoreleasing) ->
        function_type, styp
    | _ -> Typ.Tptr (function_type, Typ.Pk_pointer), function_type in
  let sizeof_exp_ = Exp.Sizeof (function_type_np, None, Subtype.exact) in
  let sizeof_exp = match size_exp_opt with
    | Some exp -> Exp.BinOp (Binop.Mult, sizeof_exp_, exp)
    | None -> sizeof_exp_ in
  let exp = (sizeof_exp, Typ.Tint Typ.IULong) in
  let procname_arg = match procname_opt with
    | Some procname -> [Exp.Const (Const.Cfun (procname)), Typ.Tvoid]
    | None -> [] in
  let args = exp :: procname_arg in
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_id_typ = Some (ret_id, function_type) in
  let stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun fname), args, sil_loc, CallFlags.default) in
  (function_type, stmt_call, Exp.Var ret_id)

let alloc_trans trans_state loc stmt_info function_type is_cf_non_null_alloc procname_opt =
  let fname = if is_cf_non_null_alloc then
      BuiltinDecl.__objc_alloc_no_fail
    else
      BuiltinDecl.__objc_alloc in
  let (function_type, stmt_call, exp) =
    create_alloc_instrs loc function_type fname None procname_opt in
  let res_trans_tmp = { empty_res_trans with instrs =[stmt_call]} in
  let res_trans =
    let nname = "Call alloc" in
    PriorityNode.compute_results_to_parent trans_state loc nname stmt_info [res_trans_tmp] in
  { res_trans with exps =[(exp, function_type)]}

let objc_new_trans trans_state loc stmt_info cls_name function_type =
  let fname = BuiltinDecl.__objc_alloc_no_fail in
  let (alloc_ret_type, alloc_stmt_call, alloc_ret_exp) =
    create_alloc_instrs loc function_type fname None None in
  let init_ret_id = Ident.create_fresh Ident.knormal in
  let is_instance = true in
  let call_flags = { CallFlags.default with CallFlags.cf_virtual = is_instance; } in
  let pname =
    CProcname.NoAstDecl.objc_method_of_string_kind
      cls_name CFrontend_config.init Typ.Procname.ObjCInstanceMethod in
  CMethod_trans.create_external_procdesc trans_state.context.CContext.cfg pname is_instance None;
  let args = [(alloc_ret_exp, alloc_ret_type)] in
  let ret_id_typ = Some (init_ret_id, alloc_ret_type) in
  let init_stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun pname), args, loc, call_flags) in
  let instrs = [alloc_stmt_call; init_stmt_call] in
  let res_trans_tmp = { empty_res_trans with instrs = instrs } in
  let res_trans =
    let nname = "Call objC new" in
    PriorityNode.compute_results_to_parent trans_state loc nname stmt_info [res_trans_tmp] in
  { res_trans with exps = [(Exp.Var init_ret_id, alloc_ret_type)]}

let new_or_alloc_trans trans_state loc stmt_info type_ptr class_name_opt selector =
  let tenv = trans_state.context.CContext.tenv in
  let function_type = CType_decl.type_ptr_to_sil_type tenv type_ptr in
  let class_name =
    match class_name_opt with
    | Some class_name -> class_name
    | None -> CType.objc_classname_of_type function_type in
  if String.equal selector CFrontend_config.alloc then
    alloc_trans trans_state loc stmt_info function_type true None
  else if String.equal selector CFrontend_config.new_str then
    objc_new_trans trans_state loc stmt_info class_name function_type
  else assert false

let cpp_new_trans sil_loc function_type size_exp_opt =
  let fname =
    match size_exp_opt with
    | Some _ -> BuiltinDecl.__new_array
    | None -> BuiltinDecl.__new in
  let (function_type, stmt_call, exp) =
    create_alloc_instrs sil_loc function_type fname size_exp_opt None in
  { empty_res_trans with instrs = [stmt_call]; exps = [(exp, function_type)] }

let create_cast_instrs exp cast_from_typ cast_to_typ sil_loc =
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_id_typ = Some (ret_id, cast_to_typ) in
  let typ = CType.remove_pointer_to_typ cast_to_typ in
  let sizeof_exp = Exp.Sizeof (typ, None, Subtype.exact) in
  let pname = BuiltinDecl.__objc_cast in
  let args = [(exp, cast_from_typ); (sizeof_exp, Typ.Tint Typ.IULong)] in
  let stmt_call =
    Sil.Call (ret_id_typ, Exp.Const (Const.Cfun pname), args, sil_loc, CallFlags.default) in
  (stmt_call, Exp.Var ret_id)

let cast_trans exps sil_loc function_type pname =
  if CTrans_models.is_toll_free_bridging pname then
    match exps with
    | [exp, typ] ->
        Some (create_cast_instrs exp typ function_type sil_loc)
    | _ -> assert false
  else None

let dereference_var_sil (exp, typ) sil_loc =
  let id = Ident.create_fresh Ident.knormal in
  let sil_instr = Sil.Load (id, exp, typ, sil_loc) in
  ([sil_instr], Exp.Var id)

(** Given trans_result with ONE expression, create temporary variable with value of an expression
    assigned to it *)
let dereference_value_from_result sil_loc trans_result ~strip_pointer =
  let (obj_sil, class_typ) = extract_exp_from_list trans_result.exps "" in
  let typ_no_ptr = match class_typ with | Typ.Tptr (typ, _) -> typ | _ -> assert false in
  let cast_typ = if strip_pointer then typ_no_ptr else class_typ in
  let cast_inst, cast_exp = dereference_var_sil (obj_sil, cast_typ) sil_loc in
  { trans_result with
    instrs = trans_result.instrs @ cast_inst;
    exps = [(cast_exp, cast_typ)]
  }


let cast_operation trans_state cast_kind exps cast_typ sil_loc is_objc_bridged =
  let (exp, typ) = extract_exp_from_list exps "" in
  let is_objc_bridged = Option.is_some trans_state.obj_bridged_cast_typ || is_objc_bridged in
  match cast_kind with
  | `NoOp
  | `DerivedToBase
  | `UncheckedDerivedToBase ->  (* These casts ignore change of type *)
      ([], (exp, typ))
  | `BitCast
  | `IntegralCast
  | `IntegralToBoolean -> (* This is treated as a nop by returning the same expressions exps*)
      ([], (exp, cast_typ))
  | `CPointerToObjCPointerCast
  | `ARCProduceObject
  | `ARCConsumeObject when is_objc_bridged ->
      (* Translation of __bridge_transfer or __bridge_retained *)
      let objc_cast_typ =
        match trans_state.obj_bridged_cast_typ with
        | Some typ -> typ
        | None -> cast_typ in
      let instr, exp = create_cast_instrs exp typ objc_cast_typ sil_loc in
      [instr], (exp, cast_typ)
  | `LValueToRValue ->
      (* Takes an LValue and allow it to use it as RValue. *)
      (* So we assign the LValue to a temp and we pass it to the parent.*)
      let instrs, deref_exp = dereference_var_sil (exp, cast_typ) sil_loc in
      instrs, (deref_exp, cast_typ)
  | _ ->
      Logging.err_debug
        "\nWARNING: Missing translation for Cast Kind %s. The construct has been ignored...\n"
        (Clang_ast_j.string_of_cast_kind cast_kind);
      ([], (exp, cast_typ))

let trans_assertion_failure sil_loc (context : CContext.t) =
  let assert_fail_builtin = Exp.Const (Const.Cfun BuiltinDecl.__infer_fail) in
  let args = [Exp.Const (Const.Cstr Config.default_failure_name), Typ.Tvoid] in
  let call_instr = Sil.Call (None, assert_fail_builtin, args, sil_loc, CallFlags.default) in
  let exit_node = Procdesc.get_exit_node (CContext.get_procdesc context)
  and failure_node =
    Nodes.create_node (Procdesc.Node.Stmt_node "Assertion failure") [call_instr] sil_loc context in
  Procdesc.node_set_succs_exn context.procdesc failure_node [exit_node] [];
  { empty_res_trans with root_nodes = [failure_node]; }

let trans_assume_false sil_loc (context : CContext.t) succ_nodes =
  let instrs_cond = [Sil.Prune (Exp.zero, sil_loc, true, Sil.Ik_land_lor)] in
  let prune_node = Nodes.create_node (Nodes.prune_kind true) instrs_cond sil_loc context in
  Procdesc.node_set_succs_exn context.procdesc prune_node succ_nodes [];
  { empty_res_trans with root_nodes = [prune_node]; leaf_nodes = [prune_node] }

let trans_assertion trans_state sil_loc =
  let context = trans_state.context in
  if Config.report_custom_error then
    trans_assertion_failure sil_loc context
  else trans_assume_false sil_loc context trans_state.succ_nodes

let trans_builtin_expect params_trans_res =
  (* Translate call to __builtin_expect as the first argument *)
  (* for simpler symbolic execution *)
  match params_trans_res with
  | [_; fst_arg_res; _] -> Some fst_arg_res
  | _ -> None

let trans_replace_with_deref_first_arg sil_loc params_trans_res ~cxx_method_call =
  let first_arg_res_trans = match params_trans_res with
    | _ :: fst_arg_res :: _ when not cxx_method_call -> fst_arg_res
    | ({exps= _method_exp :: this_exp} as fst_arg_res) :: _ when cxx_method_call ->
        (* method_deref_trans uses different format to store first argument - it stores
           two things in exps: [method_exp; this_exp].
           We need to get rid of first exp before calling dereference_value_from_result *)
        { fst_arg_res with exps = this_exp }
    | _ -> assert false in
  dereference_value_from_result sil_loc first_arg_res_trans ~strip_pointer:true

let builtin_trans trans_state loc stmt_info function_type params_trans_res pname =
  if CTrans_models.is_cf_non_null_alloc pname ||
     CTrans_models.is_alloc_model function_type pname then
    Some (alloc_trans trans_state loc stmt_info function_type true (Some pname))
  else if CTrans_models.is_alloc pname then
    Some (alloc_trans trans_state loc stmt_info function_type false None)
  else if CTrans_models.is_assert_log pname then
    Some (trans_assertion trans_state loc)
  else if CTrans_models.is_builtin_expect pname then
    trans_builtin_expect params_trans_res
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some (trans_replace_with_deref_first_arg loc params_trans_res ~cxx_method_call:false)
  else None

let cxx_method_builtin_trans trans_state loc params_trans_res pname =
  if CTrans_models.is_assert_log pname then
    Some (trans_assertion trans_state loc)
  else if CTrans_models.is_replace_with_deref_first_arg pname then
    Some (trans_replace_with_deref_first_arg loc params_trans_res ~cxx_method_call:true)
  else
    None

let define_condition_side_effects e_cond instrs_cond sil_loc =
  let (e', typ) = extract_exp_from_list e_cond "\nWARNING: Missing expression in IfStmt. Need to be fixed\n" in
  match e' with
  | Exp.Lvar pvar ->
      let id = Ident.create_fresh Ident.knormal in
      [(Exp.Var id, typ)],
      [Sil.Load (id, Exp.Lvar pvar, typ, sil_loc)]
  | _ -> [(e', typ)], instrs_cond

let is_superinstance mei =
  match mei.Clang_ast_t.omei_receiver_kind with
  | `SuperInstance -> true
  | _ -> false

let get_selector_receiver obj_c_message_expr_info =
  obj_c_message_expr_info.Clang_ast_t.omei_selector, obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind

let is_member_exp stmt =
  match stmt with
  | Clang_ast_t.MemberExpr _ -> true
  | _ -> false

let is_enumeration_constant stmt =
  match stmt with
  | Clang_ast_t.DeclRefExpr(_, _, _, drei) ->
      (match drei.Clang_ast_t.drti_decl_ref with
       | Some d -> (match d.Clang_ast_t.dr_kind with
           | `EnumConstant -> true
           | _ -> false)
       | _ -> false)
  | _ -> false

let is_null_stmt s =
  match s with
  | Clang_ast_t.NullStmt _ -> true
  | _ -> false

let extract_stmt_from_singleton stmt_list warning_string =
  extract_item_from_singleton stmt_list warning_string (Ast_expressions.dummy_stmt ())

let rec get_type_from_exp_stmt stmt =
  let do_decl_ref_exp i =
    match i.Clang_ast_t.drti_decl_ref with
    | Some d -> (match d.Clang_ast_t.dr_type_ptr with
        | Some n -> n
        | _ -> assert false )
    | _ -> assert false in
  let open Clang_ast_t in
  match stmt with
  | CXXOperatorCallExpr(_, _, ei)
  | CallExpr(_, _, ei) -> ei.Clang_ast_t.ei_type_ptr
  | MemberExpr (_, _, ei, _) -> ei.Clang_ast_t.ei_type_ptr
  | ParenExpr (_, _, ei) -> ei.Clang_ast_t.ei_type_ptr
  | ArraySubscriptExpr(_, _, ei) -> ei.Clang_ast_t.ei_type_ptr
  | ObjCIvarRefExpr (_, _, ei, _) -> ei.Clang_ast_t.ei_type_ptr
  | ObjCMessageExpr (_, _, ei, _ ) -> ei.Clang_ast_t.ei_type_ptr
  | PseudoObjectExpr(_, _, ei) -> ei.Clang_ast_t.ei_type_ptr
  | CStyleCastExpr(_, stmt_list, _, _, _)
  | UnaryOperator(_, stmt_list, _, _)
  | ImplicitCastExpr(_, stmt_list, _, _) ->
      get_type_from_exp_stmt (extract_stmt_from_singleton stmt_list "WARNING: We expect only one stmt.")
  | DeclRefExpr(_, _, _, info) -> do_decl_ref_exp info
  | _ ->
      Logging.err_debug "Failing with: %s@\n%!" (Clang_ast_j.string_of_stmt stmt);
      assert false

module Self =
struct

  exception SelfClassException of Typ.Name.t

  let add_self_parameter_for_super_instance context procname loc mei =
    if is_superinstance mei then
      let typ, self_expr, ins =
        let t' =
          CType.add_pointer_to_typ
            (Typ.Tstruct (CContext.get_curr_class_typename context)) in
        let e = Exp.Lvar (Pvar.mk (Mangled.from_string CFrontend_config.self) procname) in
        let id = Ident.create_fresh Ident.knormal in
        t', Exp.Var id, [Sil.Load (id, e, t', loc)] in
      { empty_res_trans with
        exps = [(self_expr, typ)];
        instrs = ins }
    else empty_res_trans

  let is_var_self pvar is_objc_method =
    let is_self = String.equal (Mangled.to_string (Pvar.get_name pvar)) CFrontend_config.self in
    is_self && is_objc_method

end

(* From the manual: A selector is in a certain selector family if, ignoring any leading underscores, *)
(* the first component of the selector either consists entirely of the name of *)
(* the method family or it begins with that followed by character other than lower case letter.*)
(* For example: '__perform:with' and 'performWith:' would fall into the 'perform' family (if we had one),*)
(* but 'performing:with' would not.  *)
let is_owning_name n =
  let is_family fam s'=
    if String.length s' < String.length fam then false
    else (
      let prefix = Str.string_before s' (String.length fam) in
      let suffix = Str.string_after s' (String.length fam) in
      String.equal prefix fam && not (Str.string_match (Str.regexp "[a-z]") suffix 0)
    ) in
  match Str.split (Str.regexp_string ":") n with
  | fst:: _ ->
      (match Str.split (Str.regexp "['_']+") fst with
       | [no_und]
       | _:: no_und:: _ ->
           is_family CFrontend_config.alloc no_und ||
           is_family CFrontend_config.copy no_und ||
           is_family CFrontend_config.new_str no_und ||
           is_family CFrontend_config.mutableCopy no_und ||
           is_family CFrontend_config.init no_und
       | _ -> assert false)
  | _ -> assert false

let rec is_owning_method s =
  match s with
  | Clang_ast_t.ObjCMessageExpr(_, _ , _, mei) ->
      is_owning_name mei.Clang_ast_t.omei_selector
  | _ -> (match snd (Clang_ast_proj.get_stmt_tuple s) with
      | [] -> false
      | s'':: _ -> is_owning_method s'')

let rec is_method_call s =
  match s with
  | Clang_ast_t.ObjCMessageExpr _ -> true
  | _ -> (match snd (Clang_ast_proj.get_stmt_tuple s) with
      | [] -> false
      | s'':: _ -> is_method_call s'')

let rec get_decl_ref_info s =
  match s with
  | Clang_ast_t.DeclRefExpr (_, _, _, decl_ref_expr_info) ->
      (match decl_ref_expr_info.Clang_ast_t.drti_decl_ref with
       | Some decl_ref -> decl_ref
       | None -> assert false)
  | _ ->
      match Clang_ast_proj.get_stmt_tuple s with
      | _, [] -> assert false
      | _, s'':: _ ->
          get_decl_ref_info s''

let rec contains_opaque_value_expr s =
  match s with
  | Clang_ast_t.OpaqueValueExpr _ -> true
  | _ -> match snd (Clang_ast_proj.get_stmt_tuple s) with
    | [] -> false
    | s'':: _ -> contains_opaque_value_expr s''

(* checks if a unary operator is a logic negation applied to integers*)
let is_logical_negation_of_int tenv ei uoi =
  match CType_decl.type_ptr_to_sil_type tenv ei.Clang_ast_t.ei_type_ptr, uoi.Clang_ast_t.uoi_kind with
  | Typ.Tint _,`LNot -> true
  | _, _ -> false

let rec is_block_stmt stmt =
  let open Clang_ast_t in
  match stmt with
  | BlockExpr _ -> true
  | DeclRefExpr (_, _, expr_info, _) ->
      let tp = expr_info.Clang_ast_t.ei_type_ptr in
      CType.is_block_type tp
  | _ -> (match snd (Clang_ast_proj.get_stmt_tuple stmt) with
      | [sub_stmt] -> is_block_stmt sub_stmt
      | _ -> false)

(* Checks if stmt_list is a call to a special dispatch function *)
let is_dispatch_function stmt_list =
  let open Clang_ast_t in
  let rec is_dispatch_function stmt arg_stmts =
    match stmt with
    | DeclRefExpr (_, _, _, di) ->
        (match di.Clang_ast_t.drti_decl_ref with
         | None -> None
         | Some d ->
             (match d.Clang_ast_t.dr_kind, d.Clang_ast_t.dr_name with
              | `Function, Some name_info ->
                  let s = name_info.Clang_ast_t.ni_name in
                  (match (CTrans_models.is_dispatch_function_name s) with
                   | None -> None
                   | Some (_, block_arg_pos) ->
                       try
                         let arg_stmt = List.nth_exn arg_stmts block_arg_pos in
                         if is_block_stmt arg_stmt then Some block_arg_pos else None
                       with Failure _ -> None)
              | _ -> None))
    | _ -> match snd (Clang_ast_proj.get_stmt_tuple stmt) with
      | [sub_stmt] -> is_dispatch_function sub_stmt arg_stmts
      | _ -> None in
  match stmt_list with
  | stmt:: arg_stmts -> is_dispatch_function stmt arg_stmts
  | _ -> None

let is_block_enumerate_function mei =
  String.equal mei.Clang_ast_t.omei_selector CFrontend_config.enumerateObjectsUsingBlock

(* This takes a variable of type struct or array and returns a list of expressions *)
(* for each of its fields (also recursively, such that each field access is of a basic type) *)
(* If the flag return_zero is true, the list will be a list of zero values, otherwise it will *)
(* be a list of LField expressions *)
let var_or_zero_in_init_list tenv e typ ~return_zero:return_zero =
  let rec var_or_zero_in_init_list' e typ tns =
    let open CGeneral_utils in
    match typ with
    | Typ.Tstruct tn -> (
        match Tenv.lookup tenv tn with
        | Some { fields } ->
            let lh_exprs =
              List.map ~f:(fun (fieldname, _, _) -> Exp.Lfield (e, fieldname, typ)) fields in
            let lh_types = List.map ~f:(fun (_, fieldtype, _) -> fieldtype) fields in
            let exp_types = zip lh_exprs lh_types in
            List.map ~f:(fun (e, t) -> List.concat (var_or_zero_in_init_list' e t tns)) exp_types
        | None ->
            assert false
      )
    | Typ.Tarray (arrtyp, Some n) ->
        let size = IntLit.to_int n in
        let indices = list_range 0 (size - 1) in
        let index_constants =
          List.map ~f:(fun i -> (Exp.Const (Const.Cint (IntLit.of_int i)))) indices in
        let lh_exprs =
          List.map ~f:(fun index_expr -> Exp.Lindex (e, index_expr)) index_constants in
        let lh_types = replicate size arrtyp in
        let exp_types = zip lh_exprs lh_types in
        List.map ~f:(fun (e, t) ->
            List.concat (var_or_zero_in_init_list' e t tns)) exp_types
    | Typ.Tint _ | Typ.Tfloat _  | Typ.Tptr _ ->
        let exp = if return_zero then Sil.zero_value_of_numerical_type typ else e in
        [ [(exp, typ)] ]
    | Typ.Tfun _ | Typ.Tvoid | Typ.Tarray _ -> assert false in
  List.concat (var_or_zero_in_init_list' e typ String.Set.empty)

(*
(** Similar to extract_item_from_singleton but for option type *)
let extract_item_from_option op warning_string =
  match op with
  | Some item -> item
  | _ -> Logging.err_debug warning_string; assert false

let extract_id_from_singleton id_list warning_string =
  extract_item_from_singleton id_list warning_string (dummy_id ())

let get_decl_pointer decl_ref_expr_info =
  match decl_ref_expr_info.Clang_ast_t.drti_decl_ref with
  | Some decl_ref -> decl_ref.Clang_ast_t.dr_decl_pointer
  | None -> assert false
*)
