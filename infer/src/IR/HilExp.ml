(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

type t =
  | AccessExpression of AccessExpression.t
  | UnaryOperator of Unop.t * t * Typ.t option
  | BinaryOperator of Binop.t * t * t
  | Exception of t
  | Closure of Typ.Procname.t * (AccessPath.base * t) list
  | Constant of Const.t
  | Cast of Typ.t * t
  | Sizeof of Typ.t * t option
[@@deriving compare]

let rec pp fmt = function
  | AccessExpression access_expr ->
      AccessExpression.pp fmt access_expr
  | UnaryOperator (op, e, _) ->
      F.fprintf fmt "%s%a" (Unop.str op) pp e
  | BinaryOperator (op, e1, e2) ->
      F.fprintf fmt "%a %s %a" pp e1 (Binop.str Pp.text op) pp e2
  | Exception e ->
      F.fprintf fmt "exception %a" pp e
  | Closure (pname, captured) ->
      let pp_item fmt (base, exp) =
        match exp with
        | AccessExpression (Base b) when AccessPath.equal_base b base ->
            F.fprintf fmt "%a captured" AccessPath.pp_base b
        | _ ->
            F.fprintf fmt "%a captured as %a" AccessPath.pp_base base pp exp
      in
      F.fprintf fmt "closure(%a, %a)" Typ.Procname.pp pname
        (PrettyPrintable.pp_collection ~pp_item)
        captured
  | Constant c ->
      Const.pp Pp.text fmt c
  | Cast (typ, e) ->
      F.fprintf fmt "(%a) %a" (Typ.pp_full Pp.text) typ pp e
  | Sizeof (typ, length) ->
      let pp_length fmt = Option.iter ~f:(F.fprintf fmt "[%a]" pp) in
      F.fprintf fmt "sizeof(%a%a)" (Typ.pp_full Pp.text) typ pp_length length


let rec get_typ tenv = function
  | AccessExpression access_expr ->
      AccessExpression.get_typ access_expr tenv
  | UnaryOperator (_, _, typ_opt) ->
      typ_opt
  | BinaryOperator ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _) ->
      Some (Typ.mk (Typ.Tint Typ.IBool))
  | BinaryOperator (_, e1, e2) -> (
    match
      (* TODO: doing this properly will require taking account of language-specific coercion
          semantics. Only return a type when the operands have the same type for now *)
      (get_typ tenv e1, get_typ tenv e2)
    with
    | Some typ1, Some typ2 when Typ.equal typ1 typ2 ->
        Some typ1
    | _ ->
        None )
  | Exception t ->
      get_typ tenv t
  | Closure _ | Constant (Cfun _) ->
      (* We don't have a way to represent function types *)
      None
  | Constant (Cint _) ->
      (* TODO: handle signedness *)
      Some (Typ.mk (Typ.Tint Typ.IInt))
  | Constant (Cfloat _) ->
      Some (Typ.mk (Typ.Tfloat Typ.FFloat))
  | Constant (Cclass _) ->
      (* TODO: this only happens in Java. We probably need to change it to `Cclass of Typ.Name.t`
         to give a useful result here *)
      None
  | Constant (Cstr _) ->
      (* TODO: this will need to behave differently depending on whether we're in C++ or Java *)
      None
  | Cast (typ, _) ->
      Some typ
  | Sizeof _ ->
      (* sizeof returns a size_t, which is an unsigned int *)
      Some (Typ.mk (Typ.Tint Typ.IUInt))


let get_access_exprs exp0 =
  let rec get_access_exprs_ exp acc =
    match exp with
    | AccessExpression ae ->
        ae :: acc
    | Cast (_, e) | UnaryOperator (_, e, _) | Exception e | Sizeof (_, Some e) ->
        get_access_exprs_ e acc
    | BinaryOperator (_, e1, e2) ->
        get_access_exprs_ e1 acc |> get_access_exprs_ e2
    | Closure (_, captured) ->
        List.fold captured ~f:(fun acc (_, e) -> get_access_exprs_ e acc) ~init:acc
    | Constant _ | Sizeof _ ->
        acc
  in
  get_access_exprs_ exp0 []


(* convert an SIL expression into an HIL expression. the [f_resolve_id] function should map an SSA
   temporary variable to the access path it represents. evaluating the HIL expression should
   produce the same result as evaluating the SIL expression and replacing the temporary variables
   using [f_resolve_id] *)
let of_sil ~include_array_indexes ~f_resolve_id ~add_deref exp typ =
  let rec of_sil_ (exp: Exp.t) typ =
    match exp with
    | Var id ->
        let ae =
          match f_resolve_id (Var.of_id id) with
          | Some access_expr ->
              if add_deref then AccessExpression.normalize (Dereference access_expr)
              else access_expr
          | None ->
              let access_expr = AccessExpression.of_id id typ in
              if add_deref then AccessExpression.normalize (Dereference access_expr)
              else access_expr
        in
        AccessExpression ae
    | UnOp (op, e, typ_opt) ->
        UnaryOperator (op, of_sil_ e typ, typ_opt)
    | BinOp (op, e0, e1) ->
        BinaryOperator (op, of_sil_ e0 typ, of_sil_ e1 typ)
    | Exn e ->
        Exception (of_sil_ e typ)
    | Const c ->
        Constant c
    | Cast (cast_typ, e) ->
        Cast (cast_typ, of_sil_ e typ)
    | Sizeof {typ; dynamic_length} ->
        Sizeof (typ, Option.map ~f:(fun e -> of_sil_ e typ) dynamic_length)
    | Closure closure ->
        let environment =
          List.map
            ~f:(fun (value, pvar, typ) ->
              (* TODO: this is a hack than can disappear once we have proper AccessExpressions (t23176430) *)
              let typ' =
                match value with
                | Exp.Lvar pvar1 when Pvar.equal pvar1 pvar ->
                    (* captured by reference, change type to pointer *)
                    {typ with Typ.desc= Typ.Tptr (typ, Typ.Pk_pointer)}
                | _ ->
                    (* capture by value *)
                    typ
              in
              (AccessPath.base_of_pvar pvar typ', of_sil_ value typ') )
            closure.captured_vars
        in
        Closure (closure.name, environment)
    | Lfield (root_exp, fld, root_exp_typ) -> (
      match
        AccessExpression.of_lhs_exp ~include_array_indexes ~add_deref exp typ ~f_resolve_id
      with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          (* unsupported field expression: represent with a dummy variable *)
          of_sil_
            (Exp.Lfield
               ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
               , fld
               , root_exp_typ )) typ )
    | Lindex (Const (Cstr s), index_exp) ->
        (* indexed string literal (e.g., "foo"[1]). represent this by introducing a dummy variable
         for the string literal. if you actually need to see the value of the string literal in the
         analysis, you should probably be using SIL. this is unsound if the code modifies the
         literal, e.g. using `const_cast<char*>` *)
        of_sil_ (Exp.Lindex (Var (Ident.create_normal (Ident.string_to_name s) 0), index_exp)) typ
    | Lindex (root_exp, index_exp) -> (
      match
        AccessExpression.of_lhs_exp ~include_array_indexes ~add_deref exp typ ~f_resolve_id
      with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          (* unsupported index expression: represent with a dummy variable *)
          of_sil_
            (Exp.Lindex
               ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
               , index_exp )) typ )
    | Lvar _ ->
      match
        AccessExpression.of_lhs_exp ~include_array_indexes ~add_deref exp typ ~f_resolve_id
      with
      | Some access_expr ->
          AccessExpression access_expr
      | None ->
          L.(die InternalError) "Couldn't convert var expression %a to access path" Exp.pp exp
  in
  of_sil_ exp typ


let is_null_literal = function Constant (Cint n) -> IntLit.isnull n | _ -> false

let rec eval_arithmetic_binop op e1 e2 =
  match (eval e1, eval e2) with
  | Some (Const.Cint i1), Some (Const.Cint i2) ->
      Some (Const.Cint (op i1 i2))
  | _ ->
      None

and eval = function
  | Constant c ->
      Some c
  | BinaryOperator (Binop.Div, e1, e2) -> (
    try eval_arithmetic_binop IntLit.div e1 e2 with Division_by_zero -> None )
  | BinaryOperator (Binop.MinusA, e1, e2) ->
      eval_arithmetic_binop IntLit.sub e1 e2
  | BinaryOperator (Binop.Mod, e1, e2) ->
      eval_arithmetic_binop IntLit.rem e1 e2
  | BinaryOperator (Binop.Mult, e1, e2) ->
      eval_arithmetic_binop IntLit.mul e1 e2
  | BinaryOperator (Binop.PlusA, e1, e2) ->
      eval_arithmetic_binop IntLit.add e1 e2
  | _ ->
      (* TODO: handle bitshifting cases, port eval_binop from RacerD.ml *)
      None
