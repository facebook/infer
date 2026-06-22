(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Frontend recognition for the Swift Optional force-unwrap (postfix [!]) lowering.

    Postfix [!] (and member access on an IUO [T!]) lowers to a 2-block diamond in SIL:

    {v
      Block A:  if eq(<disc>, 0) then jmp <trap> else jmp <continue>
      <trap>:   _ = __sil_assert_fail(null) ; jmp <unreachable>
    v}

    where [<disc>] is loaded from the Optional storage's discriminator field ([field_1] for
    [Sg]-class shape or [__infer_tuple_field_1] for 2-component tuple shape). Pulse fires
    [PULSE_ASSERTION_ERROR] on the trap call -- right detection, wrong issue type.

    This pass walks each [Textual.ProcDesc] and, when it recognises the diamond on a verified
    Optional discriminator, rewrites the trap block's [__sil_assert_fail(...)] call to
    [__swift_optional_force_unwrap_trap(<opt>)] where [<opt>] is the Optional storage. The companion
    Pulse model fires [SWIFT_NPE] via the existing [OPTIONAL_EMPTY_ACCESS] -> [SWIFT_NPE] remap. *)

open! IStd
module IdentMap = Textual.Ident.Map
module NodeNameTbl = Stdlib.Hashtbl.Make (Textual.NodeName)

(* Collect all definitions per ident.  Textual allows a [Let id = exp] to
   appear multiple times under the same [id] (one per predecessor branch
   of a non-SSA-ssa_param phi-merge) -- common for ObjC-bridging shapes
   where the merge block uses a single id with two writes upstream rather
   than an ssa_parameter.  Walk back from any definition (the first one
   that reveals an Optional-discriminator load wins for the recognition
   purpose). *)
let collect_id_definitions (nodes : Textual.Node.t list) : Textual.Instr.t list IdentMap.t =
  List.fold nodes ~init:IdentMap.empty ~f:(fun acc (node : Textual.Node.t) ->
      List.fold node.instrs ~init:acc ~f:(fun acc (instr : Textual.Instr.t) ->
          let add_to acc id =
            let prev = match IdentMap.find_opt id acc with Some xs -> xs | None -> [] in
            IdentMap.add id (instr :: prev) acc
          in
          match instr with
          | Load {id; _} ->
              add_to acc id
          | Let {id= Some id; _} ->
              add_to acc id
          | _ ->
              acc ) )


(* [True] iff [field] is a Swift Optional discriminator field. Mirrors the
   shape checks in [Llair2Textual.try_rewrite_optional_discriminator_store]. *)
let is_optional_discriminator_field (field : Textual.qualified_fieldname) =
  let class_is_sg =
    Textual.TypeName.swift_mangled_name_of_type_name field.enclosing_class
    |> Option.exists ~f:(String.is_suffix ~suffix:"Sg")
  in
  let is_two_component_tuple =
    Textual.BaseTypeName.equal field.enclosing_class.name
      Textual.BaseTypeName.swift_tuple_class_name
    && match field.enclosing_class.args with [_; _] -> true | _ -> false
  in
  (class_is_sg && String.equal field.name.value "field_1")
  || (is_two_component_tuple && String.equal field.name.value "__infer_tuple_field_1")


(* Walk back from [id] through Load-of-Var and cast-shaped Let chains until
   we hit a Load whose pointer is an Optional discriminator [Field]. Returns
   the base expression of that Field (i.e. the Optional storage). *)
let trace_disc_to_optional_storage ~defs id =
  let try_instr visited walk_id_aux (instr : Textual.Instr.t) =
    match instr with
    | Load {exp= Textual.Exp.Field {exp= base; field}; _} when is_optional_discriminator_field field
      ->
        Some base
    | Load {exp= Textual.Exp.Var inner; _} ->
        walk_id_aux visited inner
    | Let {exp= Textual.Exp.Call {proc; args; _}; _} when Textual.ProcDecl.is_cast_builtin proc -> (
      match args with [_typ; Textual.Exp.Var inner] -> walk_id_aux visited inner | _ -> None )
    | _ ->
        None
  in
  let rec walk_id visited id =
    if Textual.Ident.Set.mem id visited then None
    else
      let visited = Textual.Ident.Set.add id visited in
      match IdentMap.find_opt id defs with
      | None ->
          None
      | Some instrs ->
          List.find_map instrs ~f:(try_instr visited walk_id)
  in
  walk_id Textual.Ident.Set.empty id


(* Match [eq(<expr>, K)] or [eq(K, <expr>)] for [K] in [{0, 1}] (the [__sil_eq] / [eq]
   cast-builtins).  Returns the [<expr>] on success; the caller traces further through any Load /
   cast chain.  Both constants are accepted because the [.none] tag value depends on the Optional
   shape: [Sg]-class uses [tag = 1] for [.none], 2-component tuple uses [tag = 0]. *)
let extract_eq_disc (exp : Textual.Exp.t) =
  match exp with
  | Textual.Exp.Call {proc; args; _} when Textual.ProcDecl.to_binop proc |> Option.is_some -> (
      let is_eq_op =
        Textual.ProcDecl.to_binop proc |> Option.exists ~f:(fun op -> Binop.equal op Binop.Eq)
      in
      let is_zero_or_one z = Z.equal z Z.zero || Z.equal z Z.one in
      if not is_eq_op then None
      else
        match args with
        | [lhs; Textual.Exp.Const (Textual.Const.Int z)] when is_zero_or_one z ->
            Some lhs
        | [Textual.Exp.Const (Textual.Const.Int z); lhs] when is_zero_or_one z ->
            Some lhs
        | _ ->
            None )
  | _ ->
      None


(* If [term] is an If terminator whose [bexp] tests [eq(<id>, 0)] and both
   branches are immediate Jumps to single labels, return
   [Some (disc_id, trap_label, continue_label)] where [trap_label] is the
   branch reached when the equality holds (key is true, i.e. disc is zero). *)
let extract_force_unwrap_branch (term : Textual.Terminator.t) =
  let open Textual in
  let extract_single_jump = function Terminator.Jump [{label; _}] -> Some label | _ -> None in
  match term with
  | If {bexp= Exp eq_exp; then_; else_} ->
      let open IOption.Let_syntax in
      let* disc_exp = extract_eq_disc eq_exp in
      let* trap_label = extract_single_jump then_ in
      let* continue_label = extract_single_jump else_ in
      Some (disc_exp, trap_label, continue_label)
  | _ ->
      None


(* [True] iff the procname is the [__sil_assert_fail] builtin emitted by
   [Llair2Textual.to_textual_call] for the Swift [_assertionFailure] runtime. *)
let is_assert_fail_call (proc : Textual.QualifiedProcName.t) =
  Textual.ProcName.equal proc.name Textual.ProcDecl.assert_fail_name.name


(* [True] iff [node] is a trap-only block: a single
   [_ = __sil_assert_fail(...)] Let followed by a Jump to an unreachable block
   (or a similarly-shaped trap proxy). *)
let is_trap_block ~nodes_by_label (node : Textual.Node.t) =
  let succ_block_is_unreachable label =
    match NodeNameTbl.find_opt nodes_by_label label with
    | Some (succ : Textual.Node.t) -> (
      match (succ.instrs, succ.last) with [], Textual.Terminator.Unreachable -> true | _ -> false )
    | None ->
        false
  in
  match (node.instrs, node.last) with
  | [Textual.Instr.Let {exp= Textual.Exp.Call {proc; _}; _}], Textual.Terminator.Jump [{label; _}]
    when is_assert_fail_call proc ->
      succ_block_is_unreachable label
  | _ ->
      false


(* Build the SIL builtin call [__swift_optional_force_unwrap_trap(opt)]. *)
let build_force_unwrap_trap_call ~loc opt_exp =
  let proc =
    Llair2TextualModels.builtin_qual_proc_name
      (SwiftProcname.show_builtin SwiftProcname.OptionalForceUnwrapTrap)
  in
  let exp = Textual.Exp.call_non_virtual proc [opt_exp] in
  Textual.Instr.Let {id= None; exp; loc}


(* Rewrite a single trap block's [__sil_assert_fail(...)] call to a
   [__swift_optional_force_unwrap_trap(<opt>)] call. *)
let rewrite_trap_block opt_exp (node : Textual.Node.t) =
  match node.instrs with
  | (Textual.Instr.Let {loc; _} as _old_call) :: rest ->
      let new_call = build_force_unwrap_trap_call ~loc opt_exp in
      {node with instrs= new_call :: rest}
  | _ ->
      node


(* [True] iff the procname matches the [__swift_optional_init_none] builtin. *)
let is_init_none_call (proc : Textual.QualifiedProcName.t) =
  Textual.ProcName.equal proc.name
    (Llair2TextualModels.builtin_qual_proc_name
       (SwiftProcname.show_builtin SwiftProcname.OptionalInitNone) )
      .name


(* In-block peephole: drop the literal-null payload store the frontend emits for a
   [.none] construction.  A [.none] payload is meaningless, but the frontend writes
   it as [store <opt>.field_0 <- 0]; on a by-value return that null is marshaled
   into the [.some] branch of safe-unwrap lowering ([guard let] / [if let]) and
   dereferenced there (a spurious NULLPTR_DEREFERENCE).  Removing the store leaves
   the payload symbolic, so the (infeasible) [.some] branch reads an unknown value
   and stays silent.  [.none]-ness is still recorded by [__swift_optional_init_none]. *)
let drop_none_payload_store_in_node (node : Textual.Node.t) =
  (* SSA id -> local var, for [Load {id; exp= Lvar v}] in this node. *)
  let var_of_id id =
    List.find_map node.instrs ~f:(function
      | Textual.Instr.Load {id= lid; exp= Textual.Exp.Lvar v; _} when Textual.Ident.equal lid id ->
          Some v
      | _ ->
          None )
  in
  (* Locals constructed as [.none] in this node. *)
  let none_vars =
    List.filter_map node.instrs ~f:(function
      | Textual.Instr.Let {exp= Textual.Exp.Call {proc; args= [Textual.Exp.Var id]; _}; _}
        when is_init_none_call proc ->
          var_of_id id
      | _ ->
          None )
  in
  if List.is_empty none_vars then node
  else
    let is_none_payload_store instr =
      match (instr : Textual.Instr.t) with
      | Store
          { exp1= Textual.Exp.Field {exp= Textual.Exp.Var base; field= {name= {value; _}; _}}
          ; exp2
          ; _ }
        when (String.equal value "field_0" || String.equal value "__infer_tuple_field_0")
             && Textual.Exp.is_zero_exp exp2 ->
          Option.value_map (var_of_id base) ~default:false ~f:(fun v ->
              List.mem none_vars v ~equal:Textual.VarName.equal )
      | _ ->
          false
    in
    {node with instrs= List.filter node.instrs ~f:(fun i -> not (is_none_payload_store i))}


(* [True] iff [node] terminates at an unreachable block (directly or via
   a single Jump to a block whose only terminator is Unreachable). *)
let node_terminates_at_unreachable ~nodes_by_label (node : Textual.Node.t) =
  match node.last with
  | Textual.Terminator.Unreachable ->
      true
  | Textual.Terminator.Jump [{label; _}] -> (
    match NodeNameTbl.find_opt nodes_by_label label with
    | Some (succ : Textual.Node.t) -> (
      match (succ.instrs, succ.last) with [], Textual.Terminator.Unreachable -> true | _ -> false )
    | None ->
        false )
  | _ ->
      false


(* In-block peephole: scan [node.instrs] for the constant-folded
   force-unwrap shape

     _ = __swift_optional_init_none(<opt>) ; _ = __sil_assert_fail(<...>)

   (immediately-adjacent), and replace the [__sil_assert_fail] call with
   [__swift_optional_force_unwrap_trap(<opt>)].  Only fires if [node]
   terminates at an unreachable block, so the rewrite never affects
   instrs reachable beyond the trap. *)
let rewrite_init_none_trap_in_node ~nodes_by_label (node : Textual.Node.t) =
  if not (node_terminates_at_unreachable ~nodes_by_label node) then node
  else
    let rec rewrite acc = function
      | ( Textual.Instr.Let {exp= Textual.Exp.Call {proc= init_proc; args= [opt_exp]; _}; _} as
          init_instr )
        :: ( Textual.Instr.Let {id= _; exp= Textual.Exp.Call {proc= trap_proc; _}; loc= trap_loc} as
             _trap )
        :: rest
        when is_init_none_call init_proc && is_assert_fail_call trap_proc ->
          let new_trap = build_force_unwrap_trap_call ~loc:trap_loc opt_exp in
          List.rev_append acc (init_instr :: new_trap :: rest)
      | instr :: rest ->
          rewrite (instr :: acc) rest
      | [] ->
          List.rev acc
    in
    {node with instrs= rewrite [] node.instrs}


(* [True] iff the procname matches one of the symbolic-discriminator
   path-split init builtins [__swift_optional_init_sg] or
   [__swift_optional_init_tuple] emitted by [Llair2Textual] on
   symbolic-discriminator construction sites. *)
let is_init_path_split_call (proc : Textual.QualifiedProcName.t) =
  let names =
    [SwiftProcname.OptionalInitSg; SwiftProcname.OptionalInitTuple]
    |> List.map ~f:(fun b ->
        (Llair2TextualModels.builtin_qual_proc_name (SwiftProcname.show_builtin b)).name )
  in
  List.exists names ~f:(fun name -> Textual.ProcName.equal proc.name name)


(* Scan [node.instrs] for an [__swift_optional_init_{sg,tuple}(opt, _)] call and return
   [Some opt] if found.  The first argument is the Optional storage; the second is the runtime
   tag (whose value the path-split model prunes on).  When a force-unwrap diamond's discriminator
   trace fails (e.g. the discriminator is the tag arg of a path-split call rather than a load of
   [field_1]), this is the fallback that recovers the Optional storage. *)
let find_path_split_storage_in_node (node : Textual.Node.t) =
  List.find_map node.instrs ~f:(fun (instr : Textual.Instr.t) ->
      match instr with
      | Textual.Instr.Let {exp= Textual.Exp.Call {proc; args= opt_exp :: _; _}; _}
        when is_init_path_split_call proc ->
          Some opt_exp
      | _ ->
          None )


let rewrite_proc (proc : Textual.ProcDesc.t) : Textual.ProcDesc.t =
  let nodes = proc.nodes in
  let nodes_by_label = NodeNameTbl.create (List.length nodes) in
  List.iter nodes ~f:(fun (n : Textual.Node.t) -> NodeNameTbl.add nodes_by_label n.label n) ;
  let defs = collect_id_definitions nodes in
  (* For each If-terminated node, recognise the force-unwrap diamond and
     collect the (trap_label -> opt_exp) rewrites. Multiple predecessors
     converging on the same trap block agree on the same [<opt>] iff every
     such convergence traces back to the same storage, so we keep the
     first match. *)
  let trap_rewrites : Textual.Exp.t NodeNameTbl.t = NodeNameTbl.create 4 in
  List.iter nodes ~f:(fun (node : Textual.Node.t) ->
      match extract_force_unwrap_branch node.last with
      | None ->
          ()
      | Some (disc_exp, trap_label, _continue_label) -> (
        match NodeNameTbl.find_opt nodes_by_label trap_label with
        | Some trap_node when is_trap_block ~nodes_by_label trap_node -> (
            let from_field_load =
              match disc_exp with
              | Textual.Exp.Var disc_id ->
                  trace_disc_to_optional_storage ~defs disc_id
              | Textual.Exp.Call {proc; args= [_typ; Textual.Exp.Var inner]; _}
                when Textual.ProcDecl.is_cast_builtin proc ->
                  trace_disc_to_optional_storage ~defs inner
              | _ ->
                  None
            in
            (* Fallback for the symbolic-discriminator path: when the discriminator is the tag arg
               of a [__swift_optional_init_{sg,tuple}(opt, tag)] call in this same block, the
               path-split model has already pruned on [tag] so the trap branch IS the [.none]
               branch.  Recover the Optional storage from the init call's first arg. *)
            let from_path_split =
              match from_field_load with
              | Some _ ->
                  from_field_load
              | None ->
                  find_path_split_storage_in_node node
            in
            (* Last-resort fallback for the raw-pointer null-check shape ([as!] force-cast over a
               dynamic-dispatch result; user-written [precondition(x != nil)] patterns).  The
               discriminator is [cast(<int>, X)] with no surrounding Optional storage in the SIL --
               [X] is the raw pointer being checked.  The trap model's [check_valid] fires
               [SWIFT_NPE] regardless of [opt]'s validity, so passing [X] works even when [X] is
               constrained to be null. *)
            let from_raw_pointer =
              match disc_exp with
              | Textual.Exp.Call {proc; args= [_typ; (Textual.Exp.Var _ as raw)]; _}
                when Textual.ProcDecl.is_cast_builtin proc ->
                  Some raw
              | _ ->
                  None
            in
            let opt_storage =
              match from_path_split with Some _ -> from_path_split | None -> from_raw_pointer
            in
            match opt_storage with
            | Some opt_exp ->
                if not (NodeNameTbl.mem trap_rewrites trap_label) then
                  NodeNameTbl.add trap_rewrites trap_label opt_exp
            | None ->
                () )
        | _ ->
            () ) ) ;
  let new_nodes =
    List.map nodes ~f:(fun (node : Textual.Node.t) ->
        match NodeNameTbl.find_opt trap_rewrites node.label with
        | Some opt_exp ->
            rewrite_trap_block opt_exp node
        | None ->
            node )
  in
  let new_nodes = List.map new_nodes ~f:(rewrite_init_none_trap_in_node ~nodes_by_label) in
  let new_nodes = List.map new_nodes ~f:drop_none_payload_store_in_node in
  if phys_equal new_nodes nodes then proc else {proc with nodes= new_nodes}


let rewrite_module (module_ : Textual.Module.t) : Textual.Module.t =
  if not (Textual.Lang.is_swift (Textual.Module.lang module_)) then module_
  else
    let decls =
      List.map module_.decls ~f:(function
        | Textual.Module.Proc proc ->
            Textual.Module.Proc (rewrite_proc proc)
        | decl ->
            decl )
    in
    {module_ with decls}
