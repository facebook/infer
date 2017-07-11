(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Expressions *)
open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

(* reverse the natural order on Var *)
type _ident = Ident.t

let compare__ident x y = Ident.compare y x

type closure = {name: Typ.Procname.t; captured_vars: (t * Pvar.t * Typ.t) list}

(** This records information about a [sizeof(typ)] expression.

    [nbytes] represents the result of the evaluation of [sizeof(typ)] if it is statically known.

    If [typ] is of the form [Tarray elt (Some static_length)], then [dynamic_length] is the number
    of elements of type [elt] in the array. The [dynamic_length], tracked by symbolic execution, may
    differ from the [static_length] obtained from the type definition, e.g. when an array is
    over-allocated.

    If [typ] is a struct type, the [dynamic_length] is that of the final extensible array, if any.*)
and sizeof_data = {typ: Typ.t; nbytes: int option; dynamic_length: t option; subtype: Subtype.t}

(** Program expressions. *)
and t =
  (** Pure variable: it is not an lvalue *)
  | Var of _ident  (** Unary operator with type of the result if known *)
  | UnOp of Unop.t * t * Typ.t option  (** Binary operator *)
  | BinOp of Binop.t * t * t  (** Exception *)
  | Exn of t  (** Anonymous function *)
  | Closure of closure  (** Constants *)
  | Const of Const.t  (** Type cast *)
  | Cast of Typ.t * t  (** The address of a program variable *)
  | Lvar of Pvar.t  (** A field offset, the type is the surrounding struct type *)
  | Lfield of t * Typ.Fieldname.t * Typ.t  (** An array index offset: [exp1\[exp2\]] *)
  | Lindex of t * t
  | Sizeof of sizeof_data
  [@@deriving compare]

let equal = [%compare.equal : t]

let hash = Hashtbl.hash

module Set = Caml.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Caml.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Hash = Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = hash
end)

let rec is_array_index_of exp1 exp2 =
  match exp1 with Lindex (exp, _) -> is_array_index_of exp exp2 | _ -> equal exp1 exp2

let is_null_literal = function Const Cint n -> IntLit.isnull n | _ -> false

let is_this = function Lvar pvar -> Pvar.is_this pvar | _ -> false

let is_zero = function Const Cint n -> IntLit.iszero n | _ -> false

(** {2 Utility Functions for Expressions} *)

(** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception *)
let texp_to_typ default_opt = function
  | Sizeof {typ}
   -> typ
  | _
   -> Typ.unsome "texp_to_typ" default_opt

(** Return the root of [lexp]. *)
let rec root_of_lexp lexp =
  match (lexp : t) with
  | Var _
   -> lexp
  | Const _
   -> lexp
  | Cast (_, e)
   -> root_of_lexp e
  | UnOp _ | BinOp _ | Exn _ | Closure _
   -> lexp
  | Lvar _
   -> lexp
  | Lfield (e, _, _)
   -> root_of_lexp e
  | Lindex (e, _)
   -> root_of_lexp e
  | Sizeof _
   -> lexp

(** Checks whether an expression denotes a location by pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. *)
let rec pointer_arith = function
  | Lfield (e, _, _)
   -> pointer_arith e
  | Lindex _
   -> true
  | _
   -> false

let get_undefined footprint =
  Var (Ident.create_fresh (if footprint then Ident.kfootprint else Ident.kprimed))

(** returns true if the expression represents a stack-directed address *)
let rec is_stack_addr e =
  match (e : t) with
  | Lvar pv
   -> not (Pvar.is_global pv)
  | UnOp (_, e', _) | Cast (_, e') | Lfield (e', _, _) | Lindex (e', _)
   -> is_stack_addr e'
  | _
   -> false

(** returns true if the express operates on address of local variable *)
let rec has_local_addr e =
  match (e : t) with
  | Lvar pv
   -> Pvar.is_local pv
  | UnOp (_, e', _) | Cast (_, e') | Lfield (e', _, _)
   -> has_local_addr e'
  | BinOp (_, e0, e1) | Lindex (e0, e1)
   -> has_local_addr e0 || has_local_addr e1
  | _
   -> false

(** Create integer constant *)
let int i = Const (Cint i)

(** Create float constant *)
let float v = Const (Cfloat v)

(** Integer constant 0 *)
let zero = int IntLit.zero

(** Null constant *)
let null = int IntLit.null

(** Integer constant 1 *)
let one = int IntLit.one

(** Integer constant -1 *)
let minus_one = int IntLit.minus_one

(** Create integer constant corresponding to the boolean value *)
let bool b = if b then one else zero

(** Create expresstion [e1 == e2] *)
let eq e1 e2 = BinOp (Eq, e1, e2)

(** Create expresstion [e1 != e2] *)
let ne e1 e2 = BinOp (Ne, e1, e2)

(** Create expression [e1 <= e2] *)
let le e1 e2 = BinOp (Le, e1, e2)

(** Create expression [e1 < e2] *)
let lt e1 e2 = BinOp (Lt, e1, e2)

(** Extract the ids and pvars from an expression *)
let get_vars exp =
  let rec get_vars_ exp vars =
    match exp with
    | Lvar pvar
     -> (fst vars, pvar :: snd vars)
    | Var id
     -> (id :: fst vars, snd vars)
    | Cast (_, e) | UnOp (_, e, _) | Lfield (e, _, _) | Exn e
     -> get_vars_ e vars
    | BinOp (_, e1, e2) | Lindex (e1, e2)
     -> get_vars_ e1 vars |> get_vars_ e2
    | Closure {captured_vars}
     -> List.fold
          ~f:(fun vars_acc (captured_exp, _, _) -> get_vars_ captured_exp vars_acc)
          ~init:vars captured_vars
    | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _)
     -> vars
    (* TODO: Sizeof dynamic length expressions may contain variables, do not ignore them. *)
    | Sizeof _
     -> vars
  in
  get_vars_ exp ([], [])

(** Pretty print an expression. *)
let rec pp_ pe pp_t f e =
  let pp_exp = pp_ pe pp_t in
  let print_binop_stm_output e1 op e2 =
    match (op : Binop.t) with
    | Eq | Ne | PlusA | Mult
     -> F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe op) pp_exp e1
    | Lt
     -> F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Gt) pp_exp e1
    | Gt
     -> F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Lt) pp_exp e1
    | Le
     -> F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Ge) pp_exp e1
    | Ge
     -> F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Le) pp_exp e1
    | _
     -> F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  in
  match (e : t) with
  | Var id
   -> Ident.pp pe f id
  | Const c
   -> F.fprintf f "%a" (Const.pp pe) c
  | Cast (typ, e)
   -> F.fprintf f "(%a)%a" pp_t typ pp_exp e
  | UnOp (op, e, _)
   -> F.fprintf f "%s%a" (Unop.str op) pp_exp e
  | BinOp (op, Const c, e2) when Config.smt_output
   -> print_binop_stm_output (Const c) op e2
  | BinOp (op, e1, e2)
   -> F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  | Exn e
   -> F.fprintf f "EXN %a" pp_exp e
  | Closure {name; captured_vars}
   -> let id_exps = List.map ~f:(fun (id_exp, _, _) -> id_exp) captured_vars in
      F.fprintf f "(%a)" (Pp.comma_seq pp_exp) (Const (Cfun name) :: id_exps)
  | Lvar pv
   -> Pvar.pp pe f pv
  | Lfield (e, fld, _)
   -> F.fprintf f "%a.%a" pp_exp e Typ.Fieldname.pp fld
  | Lindex (e1, e2)
   -> F.fprintf f "%a[%a]" pp_exp e1 pp_exp e2
  | Sizeof {typ; nbytes; dynamic_length; subtype}
   -> let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" pp_exp) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      F.fprintf f "sizeof(%a%a%a%a)" pp_t typ pp_size nbytes pp_len dynamic_length Subtype.pp
        subtype

let pp_printenv pe pp_typ f e = pp_ pe (pp_typ pe) f e

let pp f e = pp_printenv Pp.text Typ.pp f e

let to_string e = F.asprintf "%a" pp e
