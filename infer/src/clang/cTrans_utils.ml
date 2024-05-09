(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hashtbl = Caml.Hashtbl

(** Utility methods to support the translation of clang ast constructs into sil instructions. *)

module L = Logging

(** Extract the element of a singleton list. If the list is not a singleton it crashes. *)
let extract_item_from_singleton l pp source_range warning_string =
  match l with
  | [item] ->
      item
  | _ ->
      L.die InternalError "At %a: List has %d elements, 1 expected:@\n[@[<h>%a@]]@\n%s"
        (Pp.of_string ~f:Clang_ast_j.string_of_source_range)
        source_range (List.length l) (Pp.semicolon_seq pp) l warning_string


let source_range_of_stmt stmt =
  let {Clang_ast_t.si_source_range}, _ = Clang_ast_proj.get_stmt_tuple stmt in
  si_source_range


module Nodes = struct
  let prune_kind b if_kind =
    Procdesc.Node.Prune_node
      (b, if_kind, if b then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch)


  let is_true_prune_node n =
    match Procdesc.Node.get_kind n with Procdesc.Node.Prune_node (true, _, _) -> true | _ -> false


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
    try Hashtbl.find context.CContext.label_map label
    with Caml.Not_found ->
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

let pp_continuation fmt ({break; continue; return_temp} [@warning "+missing-record-field-pattern"])
    =
  if List.is_empty break then F.pp_print_string fmt "empty"
  else
    F.fprintf fmt "@[{break=[%a];@;continue=[%a];@;return_temp=%b}@]"
      (Pp.seq ~sep:";" Procdesc.Node.pp)
      break
      (Pp.seq ~sep:";" Procdesc.Node.pp)
      continue return_temp


let is_return_temp continuation =
  match continuation with Some cont -> cont.return_temp | _ -> false


let mk_cond_continuation cont =
  match cont with
  | Some cont' ->
      Some {cont' with return_temp= true}
  | None ->
      Some {break= []; continue= []; return_temp= true}


type priority_node = Free | Busy of Clang_ast_t.pointer

let pp_priority_node fmt = function
  | Free ->
      F.pp_print_string fmt "Free"
  | Busy pointer ->
      F.fprintf fmt "Busy(%d)" pointer


(** A translation state. It provides the translation function with the info it needs to carry on the
    translation. *)
type trans_state =
  { context: CContext.t  (** current context of the translation *)
  ; succ_nodes: Procdesc.Node.t list  (** successor nodes in the cfg *)
  ; continuation: continuation option  (** current continuation *)
  ; priority: priority_node
  ; var_exp_typ: (Exp.t * Typ.t) option
  ; opaque_exp: (Exp.t * Typ.t) option
  ; is_fst_arg_objc_instance_method_call: bool
  ; block_as_arg_attributes: ProcAttributes.block_as_arg_attributes option
        (** Current to-be-translated instruction is being passed as argument to the given method in
            a position annotated with NS_NOESCAPE *) }

let pp_trans_state fmt
    ({ context= _
     ; succ_nodes
     ; continuation
     ; priority
     ; var_exp_typ
     ; opaque_exp
     ; is_fst_arg_objc_instance_method_call
     ; block_as_arg_attributes } [@warning "+missing-record-field-pattern"] ) =
  F.fprintf fmt
    "{@[succ_nodes=[%a];@;\
     continuation=%a@;\
     priority=%a;@;\
     var_exp_typ=%a;@;\
     opaque_exp=%a;@;\
     is_fst_arg_objc_instance_method_call=%b;@;\
     passed_as_noescape_block_to=%a@]}"
    (Pp.seq ~sep:";" Procdesc.Node.pp)
    succ_nodes (Pp.option pp_continuation) continuation pp_priority_node priority
    (Pp.option (Pp.pair ~fst:Exp.pp ~snd:(Typ.pp_full Pp.text_break)))
    var_exp_typ
    (Pp.option (Pp.pair ~fst:Exp.pp ~snd:(Typ.pp_full Pp.text_break)))
    opaque_exp is_fst_arg_objc_instance_method_call
    (Pp.option ProcAttributes.pp_block_as_arg_attributes)
    block_as_arg_attributes


let default_trans_state context =
  { context
  ; succ_nodes= []
  ; continuation= None
  ; priority= Free
  ; var_exp_typ= None
  ; opaque_exp= None
  ; is_fst_arg_objc_instance_method_call= false
  ; block_as_arg_attributes= None }


type control =
  { root_nodes: Procdesc.Node.t list
  ; leaf_nodes: Procdesc.Node.t list
  ; instrs: Sil.instr list
  ; initd_exps: Exp.t list
  ; cxx_temporary_markers_set: Pvar.t list }

let pp_control fmt {root_nodes; leaf_nodes; instrs; initd_exps; cxx_temporary_markers_set} =
  let pp_cxx_temporary_markers_set fmt =
    if not (List.is_empty cxx_temporary_markers_set) then
      F.fprintf fmt ";@;cxx_temporary_markers_set=[%a]"
        (Pp.seq ~sep:";" (Pvar.pp Pp.text))
        cxx_temporary_markers_set
  in
  F.fprintf fmt "@[{root_nodes=[%a];@;leaf_nodes=[%a];@;instrs=[%a];@;initd_exps=[%a]%t}@]"
    (Pp.seq ~sep:";" Procdesc.Node.pp)
    root_nodes
    (Pp.seq ~sep:";" Procdesc.Node.pp)
    leaf_nodes
    (Pp.seq ~sep:";" (Sil.pp_instr ~print_types:false Pp.text_break))
    instrs (Pp.seq ~sep:";" Exp.pp) initd_exps pp_cxx_temporary_markers_set


type trans_result =
  { control: control
  ; return: Exp.t * Typ.t
  ; method_name: Procname.t option
  ; method_signature: CMethodSignature.t option
  ; is_cpp_call_virtual: bool }

let empty_control =
  {root_nodes= []; leaf_nodes= []; instrs= []; initd_exps= []; cxx_temporary_markers_set= []}


let mk_trans_result ?method_name ?method_signature ?(is_cpp_call_virtual = false) return control =
  {control; return; method_name; method_signature; is_cpp_call_virtual}


let undefined_expression () = Exp.Var (Ident.create_fresh Ident.knormal)

(** Collect the results of translating a list of instructions, and link up the nodes created. *)
let collect_controls pdesc l =
  let collect_one result_rev {root_nodes; leaf_nodes; instrs; initd_exps; cxx_temporary_markers_set}
      =
    if not (List.is_empty root_nodes) then
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs pdesc n ~normal:root_nodes ~exn:[])
        result_rev.leaf_nodes ;
    let root_nodes =
      if List.is_empty result_rev.root_nodes then root_nodes else result_rev.root_nodes
    in
    let leaf_nodes = if List.is_empty leaf_nodes then result_rev.leaf_nodes else leaf_nodes in
    { root_nodes
    ; leaf_nodes
    ; instrs= List.rev_append instrs result_rev.instrs
    ; initd_exps= List.rev_append initd_exps result_rev.initd_exps
    ; cxx_temporary_markers_set=
        List.rev_append cxx_temporary_markers_set result_rev.cxx_temporary_markers_set }
  in
  let rev_result = List.fold l ~init:empty_control ~f:collect_one in
  {rev_result with instrs= List.rev rev_result.instrs}


let collect_trans_results pdesc ~return trans_results =
  List.map trans_results ~f:(fun {control} -> control)
  |> collect_controls pdesc |> mk_trans_result return


module PriorityNode = struct
  type t = priority_node

  let try_claim_priority_node trans_state stmt_info =
    match trans_state.priority with
    | Free ->
        L.debug Capture Verbose "Priority is free. Locking priority node in %d@\n"
          stmt_info.Clang_ast_t.si_pointer ;
        {trans_state with priority= Busy stmt_info.Clang_ast_t.si_pointer}
    | Busy _ ->
        L.debug Capture Verbose "Priority is %a. No claim possible in %d@\n" pp_priority_node
          trans_state.priority stmt_info.Clang_ast_t.si_pointer ;
        trans_state


  let force_claim_priority_node trans_state stmt_info =
    L.debug Capture Verbose "Force-locking priority node in %d (was %a)@\n"
      stmt_info.Clang_ast_t.si_pointer pp_priority_node trans_state.priority ;
    {trans_state with priority= Busy stmt_info.Clang_ast_t.si_pointer}


  let is_priority_free trans_state = match trans_state.priority with Free -> true | _ -> false

  let own_priority_node pri stmt_info =
    match pri with Busy p when Int.equal p stmt_info.Clang_ast_t.si_pointer -> true | _ -> false


  (* Used by translation functions to handle potential cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)
  let compute_controls_to_parent trans_state loc node_name stmt_info res_states_children =
    let res_state = collect_controls trans_state.context.procdesc res_states_children in
    L.debug Capture Verbose "collected controls: %a@\n" pp_control res_state ;
    let create_node =
      own_priority_node trans_state.priority stmt_info && not (List.is_empty res_state.instrs)
    in
    if create_node then (
      (* We need to create a node *)
      let node_kind = Procdesc.Node.Stmt_node node_name in
      let node_instrs = res_state.instrs in
      let node =
        Procdesc.create_node trans_state.context.CContext.procdesc loc node_kind node_instrs
      in
      Procdesc.node_set_succs trans_state.context.procdesc node ~normal:trans_state.succ_nodes
        ~exn:[] ;
      List.iter
        ~f:(fun leaf ->
          Procdesc.node_set_succs trans_state.context.procdesc leaf ~normal:[node] ~exn:[] )
        res_state.leaf_nodes ;
      (* Invariant: if root_nodes is empty then the params have not created a node.*)
      let root_nodes =
        if List.is_empty res_state.root_nodes then [node] else res_state.root_nodes
      in
      let res_state = {res_state with root_nodes; leaf_nodes= [node]; instrs= []} in
      L.debug Capture Verbose "Created node %a with instrs [%a], returning control %a@\n"
        Procdesc.Node.pp node
        (Pp.seq ~sep:";" (Sil.pp_instr ~print_types:false Pp.text_break))
        node_instrs pp_control res_state ;
      res_state )
    else (
      (* The node is created by the parent. We just pass back nodes/leafs params *)
      L.debug Capture Verbose "Delegating node creation to parent with control %a@\n" pp_control
        res_state ;
      res_state )


  let compute_results_to_parent trans_state loc node_name stmt_info ~return trans_results =
    List.map trans_results ~f:(fun trans_result -> trans_result.control)
    |> compute_controls_to_parent trans_state loc node_name stmt_info
    |> mk_trans_result return


  let compute_control_to_parent trans_state loc node_name stmt_info control =
    compute_controls_to_parent trans_state loc node_name stmt_info [control]


  let compute_result_to_parent trans_state loc node_name stmt_info trans_result =
    compute_control_to_parent trans_state loc node_name stmt_info trans_result.control
    |> mk_trans_result trans_result.return


  let mk_sequential loc node_name trans_state stmt_info return ~first_result ~second_result =
    (* force node creation for just the first result if needed *)
    let first_result =
      if
        List.is_empty second_result.control.root_nodes
        && List.is_empty second_result.control.leaf_nodes
      then first_result
      else compute_result_to_parent trans_state loc node_name stmt_info first_result
    in
    L.debug Capture Verbose "sequential composition :@\n@[<hv2>  %a@]@\n;@\n@[<hv2>  %a@]@\n"
      pp_control first_result.control pp_control second_result.control ;
    compute_results_to_parent trans_state loc node_name stmt_info ~return
      [first_result; second_result]


  let force_sequential loc node_name trans_state stmt_info ~mk_first_opt ~mk_second ~mk_return =
    let trans_state = force_claim_priority_node trans_state stmt_info in
    let second_result =
      let stmt_info = {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
      mk_second trans_state stmt_info
    in
    match mk_first_opt trans_state stmt_info with
    | None ->
        L.debug Capture Verbose "empty result for first instruction, skipping@\n" ;
        second_result
    | Some first_result ->
        mk_sequential loc node_name trans_state stmt_info
          (mk_return ~fst:first_result ~snd:second_result)
          ~first_result ~second_result


  let force_sequential_with_acc loc node_name trans_state stmt_info ~mk_first ~mk_second ~mk_return
      =
    let trans_state = force_claim_priority_node trans_state stmt_info in
    let first_result, acc = mk_first {trans_state with succ_nodes= []} stmt_info in
    let second_result =
      let stmt_info = {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
      mk_second acc trans_state stmt_info
    in
    mk_sequential loc node_name trans_state stmt_info
      (mk_return ~fst:first_result ~snd:second_result)
      ~first_result ~second_result
end

module Loops = struct
  type loop_kind =
    | For of
        { init: Clang_ast_t.stmt
        ; decl_stmt: Clang_ast_t.stmt
        ; condition: Clang_ast_t.stmt
        ; increment: Clang_ast_t.stmt
        ; body: Clang_ast_t.stmt }
    | While of
        {decl_stmt: Clang_ast_t.stmt option; condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}
    | DoWhile of {condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}

  let get_body loop_kind = match loop_kind with For {body} | While {body} | DoWhile {body} -> body

  let get_cond loop_kind =
    match loop_kind with For {condition} | While {condition} | DoWhile {condition} -> condition
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
          Some (IntegerWidths.width_of_ikind integer_type_widths ikind / 8)
      | _ ->
          None
    in
    let sizeof_exp_ =
      Exp.Sizeof
        { typ= function_type_np
        ; nbytes
        ; dynamic_length= None
        ; subtype= Subtype.exact
        ; nullable= false }
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
  PriorityNode.compute_control_to_parent trans_state loc (Call "alloc") stmt_info control_tmp
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
      Procname.ObjC_Cpp.ObjCInstanceMethod []
  in
  CMethod_trans.create_external_procdesc trans_state.context.CContext.translation_unit_context
    trans_state.context.CContext.cfg pname method_kind None ;
  let args = [(alloc_ret_exp, alloc_ret_type)] in
  let ret_id_typ = (init_ret_id, alloc_ret_type) in
  let init_stmt_call = Sil.Call (ret_id_typ, Exp.Const (Const.Cfun pname), args, loc, call_flags) in
  let instrs = alloc_stmt_call @ [init_stmt_call] in
  let res_trans_tmp = {empty_control with instrs} in
  let node_name = Procdesc.Node.CallObjCNew in
  PriorityNode.compute_control_to_parent trans_state loc node_name stmt_info res_trans_tmp
  |> mk_trans_result (Exp.Var init_ret_id, alloc_ret_type)


let new_or_alloc_trans trans_state loc stmt_info function_type class_name_opt selector =
  let class_name =
    match class_name_opt with
    | Some class_name ->
        class_name
    | None ->
        CType.objc_classname_of_type function_type
  in
  if
    String.equal selector CFrontend_config.alloc
    || String.equal selector CFrontend_config.allocWithZone
  then
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


let create_call_to_objc_bridge_transfer sil_loc exp typ =
  let pname = BuiltinDecl.__objc_bridge_transfer in
  let stmt_call =
    Sil.Call
      ( (Ident.create_fresh Ident.knormal, StdTyp.void)
      , Exp.Const (Const.Cfun pname)
      , [(exp, typ)]
      , sil_loc
      , CallFlags.default )
  in
  stmt_call


let dereference_var_sil (exp, typ) sil_loc =
  let id = Ident.create_fresh Ident.knormal in
  let sil_instr = Sil.Load {id; e= exp; typ; loc= sil_loc} in
  (sil_instr, Exp.Var id)


let dereference_value_from_result ?(strip_pointer = false) source_range sil_loc trans_result =
  let obj_sil, class_typ = trans_result.return in
  let typ_no_ptr =
    match class_typ.Typ.desc with
    | Tptr (typ, _) ->
        typ
    | _ ->
        CFrontend_errors.incorrect_assumption __POS__ source_range
          "Expected pointer type but found type %a" (Typ.pp_full Pp.text) class_typ
  in
  let cast_typ = if strip_pointer then typ_no_ptr else class_typ in
  let cast_inst, cast_exp = dereference_var_sil (obj_sil, cast_typ) sil_loc in
  { trans_result with
    control= {trans_result.control with instrs= trans_result.control.instrs @ [cast_inst]}
  ; return= (cast_exp, cast_typ) }


let cast_operation ?objc_bridge_cast_kind cast_kind ((exp, typ) as exp_typ) cast_typ sil_loc =
  match cast_kind with
  | `NoOp when Typ.is_rvalue_reference cast_typ ->
      ([], (Exp.Cast (cast_typ, exp), cast_typ))
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
  | `LValueToRValue -> (
    match typ with
    | {Typ.desc= Tstruct _} ->
        (* Avoid dereference of C struct on cast *)
        ([], exp_typ)
    | _ ->
        (* Takes an LValue and allow it to use it as RValue. *)
        (* So we assign the LValue to a temp and we pass it to the parent.*)
        let instr, deref_exp = dereference_var_sil (exp, cast_typ) sil_loc in
        ([instr], (deref_exp, cast_typ)) )
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
  | _ -> (
    match objc_bridge_cast_kind with
    | Some `OBC_BridgeTransfer ->
        let instr = create_call_to_objc_bridge_transfer sil_loc exp typ in
        ([instr], (exp, cast_typ))
    | Some cast_kind ->
        L.debug Capture Verbose
          "@\n\
           WARNING: Missing translation for ObjC Bridge Cast Kind %a. The construct has been \
           ignored...@\n"
          (Pp.of_string ~f:Clang_ast_j.string_of_obj_c_bridge_cast_kind)
          cast_kind ;
        ([], (exp, cast_typ))
    | _ ->
        L.debug Capture Verbose
          "@\nWARNING: Missing translation for Cast Kind %a. The construct has been ignored...@\n"
          (Pp.of_string ~f:Clang_ast_j.string_of_cast_kind)
          cast_kind ;
        ([], (exp, cast_typ)) )


let trans_assertion_failure sil_loc (context : CContext.t) =
  let assert_fail_builtin = Exp.Const (Const.Cfun BuiltinDecl.__infer_fail) in
  let args = [(Exp.Const (Const.Cstr Config.default_failure_name), StdTyp.void)] in
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_typ = StdTyp.void in
  let call_instr =
    Sil.Call ((ret_id, ret_typ), assert_fail_builtin, args, sil_loc, CallFlags.default)
  in
  let exit_node = Procdesc.get_exit_node context.procdesc
  and failure_node =
    Procdesc.create_node context.CContext.procdesc sil_loc
      (Procdesc.Node.Stmt_node AssertionFailure) [call_instr]
  in
  Procdesc.node_set_succs context.procdesc failure_node ~normal:[exit_node] ~exn:[] ;
  mk_trans_result (Exp.Var ret_id, ret_typ) {empty_control with root_nodes= [failure_node]}


let trans_assume_false sil_loc (context : CContext.t) succ_nodes =
  let if_kind = Sil.Ik_land_lor in
  let instrs_cond = [Sil.Prune (Exp.zero, sil_loc, true, if_kind)] in
  let prune_node =
    Procdesc.create_node context.CContext.procdesc sil_loc (Nodes.prune_kind true if_kind)
      instrs_cond
  in
  Procdesc.node_set_succs context.procdesc prune_node ~normal:succ_nodes ~exn:[] ;
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
      ((Exp.Var id, e_cond_typ), [Sil.Load {id; e= Exp.Lvar pvar; typ= e_cond_typ; loc= sil_loc}])
  | _ ->
      (e_cond, instrs_cond)


let is_superinstance mei =
  match mei.Clang_ast_t.omei_receiver_kind with `SuperInstance -> true | _ -> false


let is_null_stmt s = match s with Clang_ast_t.NullStmt _ -> true | _ -> false

let extract_stmt_from_singleton stmt_list source_range warning_string =
  extract_item_from_singleton stmt_list
    (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
    source_range warning_string


module Self = struct
  let add_self_parameter_for_super_instance stmt_info context procname loc mei =
    if is_superinstance mei then
      let typ, self_expr, instrs =
        let t' =
          CType.add_pointer_to_typ
            (Typ.mk (Tstruct (CContext.get_curr_class_typename stmt_info context)))
        in
        let e = Exp.Lvar (Pvar.mk (Mangled.from_string CFrontend_config.self) procname) in
        let id = Ident.create_fresh Ident.knormal in
        (t', Exp.Var id, [Sil.Load {id; e; typ= t'; loc}])
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


let mk_fresh_void_exp_typ () = (Exp.Var (Ident.create_fresh Ident.knormal), StdTyp.void)

let mk_fresh_void_id_typ () = (Ident.create_fresh Ident.knormal, StdTyp.void)

let mk_fresh_void_return () =
  let id = Ident.create_fresh Ident.knormal and void = StdTyp.void in
  ((id, void), (Exp.Var id, void))


let last_or_mk_fresh_void_exp_typ exp_typs =
  match List.last exp_typs with
  | Some last_exp_typ ->
      last_exp_typ
  | None ->
      mk_fresh_void_exp_typ ()


let should_remove_first_param {context= {tenv} as context; is_fst_arg_objc_instance_method_call}
    stmt =
  let some_class_name stmt_info = Some (CContext.get_curr_class_typename stmt_info context) in
  match (stmt : Clang_ast_t.stmt) with
  | ImplicitCastExpr
      ( _
      , [ DeclRefExpr
            ( stmt_info
            , _
            , _
            , {drti_decl_ref= Some {dr_name= Some {ni_name= name}; dr_qual_type= Some qual_type}} )
        ]
      , _
      , {cei_cast_kind= `LValueToRValue}
      , _ )
    when is_fst_arg_objc_instance_method_call && String.equal name "self"
         && CType.is_class (CType_decl.qual_type_to_sil_type tenv qual_type) ->
      some_class_name stmt_info
  | ObjCMessageExpr
      ( _
      , [ ImplicitCastExpr
            (_, [DeclRefExpr (stmt_info, _, _, _)], _, {cei_cast_kind= `LValueToRValue}, _) ]
      , _
      , {omei_selector= selector} )
    when is_fst_arg_objc_instance_method_call && String.equal selector CFrontend_config.class_method
    ->
      some_class_name stmt_info
  | _ ->
      None
