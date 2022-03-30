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

let succ_true env = (Block.make_success env, Exp.Const (Cint IntLit.one))

let combine_bool exprs op =
  let f e1 e2 = Exp.BinOp (op, e1, e2) in
  match List.reduce exprs ~f with Some expr -> expr | None -> Exp.Const (Cint IntLit.one)


let rec assume_type (env : (_, _) Env.t) constraints ((arg_id, type_) : Ident.t * Ast.type_) :
    Block.t * Exp.t =
  let assume_simple typ =
    let is_typ = mk_fresh_id () in
    let start =
      Node.make_stmt env [Env.has_type_instr env ~result:is_typ ~value:(Exp.Var arg_id) typ]
    in
    ({Block.start; exit_success= start; exit_failure= Node.make_nop env}, Exp.Var is_typ)
  in
  match type_ with
  | Any ->
      succ_true env
  | Atom Any ->
      assume_simple Atom
  | List (Proper _) ->
      let block1, expr1 = assume_simple Cons in
      let block2, expr2 = assume_simple Nil in
      (Block.all env [block1; block2], Exp.BinOp (LOr, expr1, expr2))
  | Map ->
      assume_simple Map
  | Nil ->
      assume_simple Nil
  | Tuple (FixedSize types) ->
      let n = List.length types in
      assume_simple (Tuple n)
  | Union types ->
      let f t = assume_type env constraints (arg_id, t) in
      let blocks, exprs = List.unzip (List.map ~f types) in
      (Block.all env blocks, combine_bool exprs Binop.LOr)
  | UserDefined name ->
      let procname = Env.procname_for_user_type env.current_module name in
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
  | Var v -> (
    (* Simple substitution. Can go into infinite loop. For now we assume that the type checker rejects
       such cases before. TODO: check for cycles in a validation step (T115271156) *)
    match Map.find constraints v with
    | Some subtyp ->
        assume_type env constraints (arg_id, subtyp)
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


let assume_spec_disjunct (env : (_, _) Env.t) arg_ids (function_ : Ast.function_)
    (specd : Ast.spec_disjunct) : Block.t * Exp.t =
  (* Assume the type of each argument and form a conjunction. *)
  match List.zip arg_ids specd.arguments with
  | Ok args_with_types ->
      let blocks, exprs =
        List.unzip (List.map ~f:(assume_type env specd.constraints) args_with_types)
      in
      (Block.all env blocks, combine_bool exprs Binop.LAnd)
  | Unequal_lengths ->
      L.debug Capture Verbose
        "@[Number of arguments and specs do not match in module %s function %s@." env.current_module
        (Sexp.to_string (ErlangAst.sexp_of_function_ function_)) ;
      succ_true env


let assume_spec (env : (_, _) Env.t) arg_ids (spec : Ast.spec) : Block.t =
  (* Assume each disjunct recursively and form a disjunction in a prune node. *)
  let blocks, exprs =
    List.unzip (List.map ~f:(assume_spec_disjunct env arg_ids spec.function_) spec.specs)
  in
  let prune_block =
    (* We could also use nondeterminism among blocks which could give more precision. *)
    let condition = combine_bool exprs Binop.LOr in
    let prune_node = Node.make_if env true condition in
    {Block.start= prune_node; exit_success= prune_node; exit_failure= Node.make_nop env}
  in
  Block.all env (blocks @ [prune_block])
