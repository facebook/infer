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
  | AccessPath of AccessPath.t
  | UnaryOperator of Unop.t * t * Typ.t option
  | BinaryOperator of Binop.t * t * t
  | Exception of t
  | Closure of Typ.Procname.t * (AccessPath.base * t) list
  | Constant of Const.t
  | Cast of Typ.t * t
  | Sizeof of Typ.t * t option
  [@@deriving compare]

let rec pp fmt = function
  | AccessPath access_path
   -> AccessPath.pp fmt access_path
  | UnaryOperator (op, e, _)
   -> F.fprintf fmt "%s%a" (Unop.str op) pp e
  | BinaryOperator (op, e1, e2)
   -> F.fprintf fmt "%a %s %a" pp e1 (Binop.str Pp.text op) pp e2
  | Exception e
   -> F.fprintf fmt "exception %a" pp e
  | Closure (pname, _)
   -> F.fprintf fmt "closure(%a)" Typ.Procname.pp pname
  | Constant c
   -> Const.pp Pp.text fmt c
  | Cast (typ, e)
   -> F.fprintf fmt "(%a) %a" (Typ.pp_full Pp.text) typ pp e
  | Sizeof (typ, length)
   -> let pp_length fmt = Option.iter ~f:(F.fprintf fmt "[%a]" pp) in
      F.fprintf fmt "sizeof(%a%a)" (Typ.pp_full Pp.text) typ pp_length length

let rec get_typ tenv = function
  | AccessPath access_path
   -> AccessPath.get_typ access_path tenv
  | UnaryOperator (_, _, typ_opt)
   -> typ_opt
  | BinaryOperator ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _)
   -> Some (Typ.mk (Typ.Tint Typ.IBool))
  | BinaryOperator (_, e1, e2) -> (
    match
      (* TODO: doing this properly will require taking account of language-specific coercion
          semantics. Only return a type when the operands have the same type for now *)
      (get_typ tenv e1, get_typ tenv e2)
    with
    | Some typ1, Some typ2 when Typ.equal typ1 typ2
     -> Some typ1
    | _
     -> None )
  | Exception t
   -> get_typ tenv t
  | Closure _ | Constant Cfun _
   -> (* We don't have a way to represent function types *)
      None
  | Constant Cint _
   -> (* TODO: handle signedness *)
      Some (Typ.mk (Typ.Tint Typ.IInt))
  | Constant Cfloat _
   -> Some (Typ.mk (Typ.Tfloat Typ.FFloat))
  | Constant Cclass _
   -> (* TODO: this only happens in Java. We probably need to change it to `Cclass of Typ.Name.t`
         to give a useful result here *)
      None
  | Constant Cstr _
   -> (* TODO: this will need to behave differently depending on whether we're in C++ or Java *)
      None
  | Cast (typ, _)
   -> Some typ
  | Sizeof _
   -> (* sizeof returns a size_t, which is an unsigned int *)
      Some (Typ.mk (Typ.Tint Typ.IUInt))

let get_access_paths exp0 =
  let rec get_access_paths_ exp acc =
    match exp with
    | AccessPath ap
     -> ap :: acc
    | Cast (_, e) | UnaryOperator (_, e, _) | Exception e | Sizeof (_, Some e)
     -> get_access_paths_ e acc
    | BinaryOperator (_, e1, e2)
     -> get_access_paths_ e1 acc |> get_access_paths_ e2
    | Closure _ | Constant _ | Sizeof _
     -> acc
  in
  get_access_paths_ exp0 []

(* convert an SIL expression into an HIL expression. the [f_resolve_id] function should map an SSA
   temporary variable to the access path it represents. evaluating the HIL expression should
   produce the same result as evaluating the SIL expression and replacing the temporary variables
   using [f_resolve_id] *)
let rec of_sil ~f_resolve_id (exp: Exp.t) typ =
  match exp with
  | Var id
   -> let ap =
        match f_resolve_id (Var.of_id id) with
        | Some access_path
         -> access_path
        | None
         -> AccessPath.of_id id typ
      in
      AccessPath ap
  | UnOp (op, e, typ_opt)
   -> UnaryOperator (op, of_sil ~f_resolve_id e typ, typ_opt)
  | BinOp (op, e0, e1)
   -> BinaryOperator (op, of_sil ~f_resolve_id e0 typ, of_sil ~f_resolve_id e1 typ)
  | Exn e
   -> Exception (of_sil ~f_resolve_id e typ)
  | Const c
   -> Constant c
  | Cast (cast_typ, e)
   -> Cast (cast_typ, of_sil ~f_resolve_id e typ)
  | Sizeof {typ; dynamic_length}
   -> Sizeof (typ, Option.map ~f:(fun e -> of_sil ~f_resolve_id e typ) dynamic_length)
  | Closure closure
   -> let environment =
        List.map
          ~f:(fun (value, pvar, typ) ->
            (AccessPath.base_of_pvar pvar typ, of_sil ~f_resolve_id value typ))
          closure.captured_vars
      in
      Closure (closure.name, environment)
  | Lfield (root_exp, fld, root_exp_typ) -> (
    match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
    | Some access_path
     -> AccessPath access_path
    | None
     -> (* unsupported field expression: represent with a dummy variable *)
        of_sil ~f_resolve_id
          (Exp.Lfield
             ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
             , fld
             , root_exp_typ )) typ )
  | Lindex (Const Cstr s, index_exp)
   -> (* indexed string literal (e.g., "foo"[1]). represent this by introducing a dummy variable
         for the string literal. if you actually need to see the value of the string literal in the
         analysis, you should probably be using SIL. this is unsound if the code modifies the
         literal, e.g. using `const_cast<char*>` *)
      of_sil ~f_resolve_id
        (Exp.Lindex (Var (Ident.create_normal (Ident.string_to_name s) 0), index_exp)) typ
  | Lindex (root_exp, index_exp) -> (
    match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
    | Some access_path
     -> AccessPath access_path
    | None
     -> (* unsupported index expression: represent with a dummy variable *)
        of_sil ~f_resolve_id
          (Exp.Lindex
             ( Var (Ident.create_normal (Ident.string_to_name (Exp.to_string root_exp)) 0)
             , index_exp )) typ )
  | Lvar _ ->
    match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
    | Some access_path
     -> AccessPath access_path
    | None
     -> failwithf "Couldn't convert var expression %a to access path" Exp.pp exp

let is_null_literal = function Constant Cint n -> IntLit.isnull n | _ -> false
