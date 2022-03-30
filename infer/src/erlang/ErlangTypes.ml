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

let combine_bool ~op ~default exprs =
  let f e1 e2 = Exp.BinOp (op, e1, e2) in
  match List.reduce exprs ~f with Some expr -> expr | None -> default


let any_typ = Env.ptr_typ_of_name Any

let rec type_condition (env : (_, _) Env.t) constraints ((arg_id, type_) : Ident.t * Ast.type_) :
    Block.t * Exp.t =
  let simple_condition typ =
    let is_typ = mk_fresh_id () in
    let start =
      Node.make_stmt env [Env.has_type_instr env ~result:is_typ ~value:(Exp.Var arg_id) typ]
    in
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
  match type_ with
  | Any ->
      succ_true env
  | Atom Any ->
      simple_condition Atom
  | List (Proper _) ->
      let block1, expr1 = simple_condition Cons in
      let block2, expr2 = simple_condition Nil in
      (Block.all env [block1; block2], Exp.BinOp (LOr, expr1, expr2))
  | Map ->
      simple_condition Map
  | Nil ->
      simple_condition Nil
  | Remote {module_; type_} ->
      userdef_condition module_ type_
  | Tuple (FixedSize types) ->
      let n = List.length types in
      simple_condition (Tuple n)
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
