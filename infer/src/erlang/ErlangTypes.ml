(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module Block = ErlangBlock
module Env = ErlangEnvironment
module L = Logging
module Node = ErlangNode

let mk_fresh_id () = Ident.create_fresh Ident.knormal

let true_const = Exp.Const (Cint IntLit.one)

let succ_true env = (Block.make_success env, true_const)

let any_typ = Env.ptr_typ_of_name Any

let combine_bool ~op ~default exprs =
  let f e1 e2 = Exp.BinOp (op, e1, e2) in
  match List.reduce exprs ~f with Some expr -> expr | None -> default


let rec type_condition (env : (_, _) Env.t) constraints ((arg_id, type_) : Ident.t * Ast.type_) :
    Block.t * Exp.t =
  let simple_condition typ id =
    let is_typ = mk_fresh_id () in
    let start = Node.make_stmt env [Env.has_type_instr env ~result:is_typ ~value:(Var id) typ] in
    ({Block.start; exit_success= start; exit_failure= Node.make_nop env}, Exp.Var is_typ)
  in
  let userdef_condition module_ name =
    let procname = Env.procname_for_user_type module_ name in
    let condition = mk_fresh_id () in
    let call_instr =
      let any_typ = Env.ptr_typ_of_name Any in
      Sil.Call
        ( (condition, any_typ)
        , Exp.Const (Cfun procname)
        , [(Exp.Var arg_id, any_typ)]
        , env.location
        , CallFlags.default )
    in
    (Block.make_instruction env [call_instr], Exp.Var condition)
  in
  let atom_literal atom id =
    let is_atom_block, is_atom_cond = simple_condition Atom id in
    let actual_hash = mk_fresh_id () in
    let load_instr =
      Env.load_field_from_expr env actual_hash (Var id) ErlangTypeName.atom_hash Atom
    in
    let expected_hash = Exp.Const (Cint (IntLit.of_int (ErlangTypeName.calculate_hash atom))) in
    let condition =
      Exp.BinOp (Binop.LAnd, is_atom_cond, Exp.BinOp (Binop.Eq, Var actual_hash, expected_hash))
    in
    (Block.all env [is_atom_block; Block.make_instruction env [load_instr]], condition)
  in
  match type_ with
  | Any ->
      succ_true env
  | Atom Any ->
      simple_condition Atom arg_id
  | Atom (Literal a) ->
      atom_literal a arg_id
  | List (Proper _) ->
      let block1, expr1 = simple_condition Cons arg_id in
      let block2, expr2 = simple_condition Nil arg_id in
      (Block.all env [block1; block2], Exp.BinOp (LOr, expr1, expr2))
  | Map ->
      simple_condition Map arg_id
  | Nil ->
      simple_condition Nil arg_id
  | Record name ->
      let record_info = String.Map.find_exn env.records name in
      let tuple_size = 1 + List.length record_info.field_names in
      let tuple_typ = ErlangTypeName.Tuple tuple_size in
      let is_tuple_block, is_tuple_cond = simple_condition tuple_typ arg_id in
      let name_id = mk_fresh_id () in
      let load_name_instr =
        Env.load_field_from_expr env name_id (Var arg_id) (ErlangTypeName.tuple_elem 1) tuple_typ
      in
      let is_atom_block, is_atom_cond = atom_literal name name_id in
      let condition = Exp.BinOp (Binop.LAnd, is_tuple_cond, is_atom_cond) in
      ( Block.all env [is_tuple_block; Block.make_instruction env [load_name_instr]; is_atom_block]
      , condition )
  | Remote {module_; type_} ->
      userdef_condition module_ type_
  | Tuple (FixedSize types) ->
      let tuple_size = List.length types in
      let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
      let is_tuple_block, is_tuple_cond = simple_condition tuple_typ arg_id in
      let fields = ErlangTypeName.tuple_field_names tuple_size in
      let fields_and_types = List.zip_exn fields types in
      let f (field, type_) =
        let id = mk_fresh_id () in
        let load_instr = Env.load_field_from_expr env id (Var arg_id) field tuple_typ in
        let load_block = Block.make_instruction env [load_instr] in
        let sub_block, sub_expr = type_condition env constraints (id, type_) in
        (Block.all env [load_block; sub_block], sub_expr)
      in
      let blocks, exprs = List.unzip (List.map ~f fields_and_types) in
      ( Block.all env (is_tuple_block :: blocks)
      , combine_bool ~op:Binop.LAnd ~default:true_const (is_tuple_cond :: exprs) )
  | Union types ->
      let f t = type_condition env constraints (arg_id, t) in
      let blocks, exprs = List.unzip (List.map ~f types) in
      (* Union shouldn't be empty, but if it somehow happens, just return true. *)
      (Block.all env blocks, combine_bool ~op:Binop.LOr ~default:true_const exprs)
  | UserDefined name ->
      userdef_condition env.current_module name
  | Var v -> (
    (* Simple substitution. Can go into infinite loop. For now we assume that the type checker rejects
       such cases before. TODO: check for cycles in a validation step (T115271156) *)
    match Map.find constraints v with
    | Some subtyp ->
        type_condition env constraints (arg_id, subtyp)
    | None ->
        L.debug Capture Verbose
          "@[No constraint found, or type is not supported for type variable %s, treating as \
           any()@."
          v ;
        succ_true env )
  | t ->
      L.debug Capture Verbose "@[The following type is not supported and is ignored: %s@."
        (Sexp.to_string (Ast.sexp_of_type_ t)) ;
      succ_true env


let disjunct_condition (env : (_, _) Env.t) arg_ids (function_ : Ast.function_)
    (specd : Ast.spec_disjunct) : Block.t * Exp.t =
  (* Generate condition for the type of each argument and form a conjunction. *)
  match List.zip arg_ids specd.arguments with
  | Ok args_with_types ->
      let blocks, exprs =
        List.unzip (List.map ~f:(type_condition env specd.constraints) args_with_types)
      in
      (Block.all env blocks, combine_bool ~op:Binop.LAnd ~default:true_const exprs)
  | Unequal_lengths ->
      L.debug Capture Verbose
        "@[Number of arguments and specs do not match in module %s function %s@." env.current_module
        (Sexp.to_string (ErlangAst.sexp_of_function_ function_)) ;
      succ_true env


let prune_spec (env : (_, _) Env.t) arg_ids (spec : Ast.spec) : Block.t =
  (* Generate condition for each disjunct recursively and form a disjunction in a prune node. *)
  let blocks, conditions =
    List.unzip (List.map ~f:(disjunct_condition env arg_ids spec.function_) spec.specs)
  in
  (* We could also use nondeterminism among blocks which could give more precision. *)
  (* Overloads shouldn't be empty, but if it somehow happens, just return true. *)
  let condition = combine_bool ~op:Binop.LOr ~default:true_const conditions in
  (* We could put the condition directly in the prune node, but we introduce a temp variable to help
     Pulse figure out the unsatisfiability of some formulas. In short, the limitation is that if
     a Pulse model for a function introduces a disjunction being false, it has (A or B) = C and C = 0.
     If we put the disjunction directly in the prune node, it will add (A or B) != 0 in the formula,
     and somehow Infer can't figure out that this is unsat. Adding a temp variable helps. T115354480 *)
  let cond_id = mk_fresh_id () in
  let load_block = Block.make_load env cond_id condition any_typ in
  let prune_block =
    let prune_node = Node.make_if env true (Var cond_id) in
    {Block.start= prune_node; exit_success= prune_node; exit_failure= Node.make_nop env}
  in
  Block.all env (blocks @ [load_block; prune_block])
