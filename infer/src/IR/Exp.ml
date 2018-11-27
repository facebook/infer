(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Expressions *)

open! IStd
module Hashtbl = Caml.Hashtbl
module F = Format

(* reverse the natural order on Var *)
type ident_ = Ident.t

let compare_ident_ x y = Ident.compare y x

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
  | Var of ident_  (** Pure variable: it is not an lvalue *)
  | UnOp of Unop.t * t * Typ.t option  (** Unary operator with type of the result if known *)
  | BinOp of Binop.t * t * t  (** Binary operator *)
  | Exn of t  (** Exception *)
  | Closure of closure  (** Anonymous function *)
  | Const of Const.t  (** Constants *)
  | Cast of Typ.t * t  (** Type cast *)
  | Lvar of Pvar.t  (** The address of a program variable *)
  | Lfield of t * Typ.Fieldname.t * Typ.t
      (** A field offset, the type is the surrounding struct type *)
  | Lindex of t * t  (** An array index offset: [exp1\[exp2\]] *)
  | Sizeof of sizeof_data
[@@deriving compare]

let equal = [%compare.equal: t]

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

let is_null_literal = function Const (Cint n) -> IntLit.isnull n | _ -> false

let is_this = function Lvar pvar -> Pvar.is_this pvar | _ -> false

let is_zero = function Const (Cint n) -> IntLit.iszero n | _ -> false

(** {2 Utility Functions for Expressions} *)

(** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception *)
let texp_to_typ default_opt = function
  | Sizeof {typ} ->
      typ
  | _ ->
      Typ.unsome "texp_to_typ" default_opt


(** Return the root of [lexp]. *)
let rec root_of_lexp lexp =
  match (lexp : t) with
  | Var _ ->
      lexp
  | Const _ ->
      lexp
  | Cast (_, e) ->
      root_of_lexp e
  | UnOp _ | BinOp _ | Exn _ | Closure _ ->
      lexp
  | Lvar _ ->
      lexp
  | Lfield (e, _, _) ->
      root_of_lexp e
  | Lindex (e, _) ->
      root_of_lexp e
  | Sizeof _ ->
      lexp


(** Checks whether an expression denotes a location by pointer arithmetic.
    Currently, catches array-indexing expressions such as a[i] only. *)
let rec pointer_arith = function
  | Lfield (e, _, _) ->
      pointer_arith e
  | Lindex _ ->
      true
  | _ ->
      false


let get_undefined footprint =
  Var (Ident.create_fresh (if footprint then Ident.kfootprint else Ident.kprimed))


(** returns true if the express operates on address of local variable *)
let rec has_local_addr e =
  match (e : t) with
  | Lvar pv ->
      Pvar.is_local pv
  | UnOp (_, e', _) | Cast (_, e') | Lfield (e', _, _) ->
      has_local_addr e'
  | BinOp (_, e0, e1) | Lindex (e0, e1) ->
      has_local_addr e0 || has_local_addr e1
  | _ ->
      false


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

(** Create expression [e1 == e2] *)
let eq e1 e2 = BinOp (Eq, e1, e2)

(** Create expression [e1 != e2] *)
let ne e1 e2 = BinOp (Ne, e1, e2)

(** Create expression [e1 <= e2] *)
let le e1 e2 = BinOp (Le, e1, e2)

(** Create expression [e1 < e2] *)
let lt e1 e2 = BinOp (Lt, e1, e2)

let fold_captured ~f exp acc =
  let rec fold_captured_ exp captured_acc =
    match exp with
    | Cast (_, e) | UnOp (_, e, _) | Lfield (e, _, _) | Exn e | Sizeof {dynamic_length= Some e} ->
        fold_captured_ e captured_acc
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        fold_captured_ e1 captured_acc |> fold_captured_ e2
    | Closure {captured_vars} ->
        List.fold captured_vars
          ~f:(fun acc (captured_exp, _, _) -> f acc captured_exp)
          ~init:captured_acc
    | Const _ | Lvar _ | Var _ | Sizeof _ ->
        captured_acc
  in
  fold_captured_ exp acc


(** Pretty print an expression. *)
let rec pp_ pe pp_t f e =
  let pp_exp = pp_ pe pp_t in
  let print_binop_stm_output e1 op e2 =
    match (op : Binop.t) with
    | Eq | Ne | PlusA _ | Mult _ ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe op) pp_exp e1
    | Lt ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Gt) pp_exp e1
    | Gt ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Lt) pp_exp e1
    | Le ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Ge) pp_exp e1
    | Ge ->
        F.fprintf f "(%a %s %a)" pp_exp e2 (Binop.str pe Le) pp_exp e1
    | _ ->
        F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  in
  match (e : t) with
  | Var id ->
      Ident.pp f id
  | Const c ->
      (Const.pp pe) f c
  | Cast (typ, e) ->
      F.fprintf f "(%a)%a" pp_t typ pp_exp e
  | UnOp (op, e, _) ->
      F.fprintf f "%s%a" (Unop.to_string op) pp_exp e
  | BinOp (op, Const c, e2) when Config.smt_output ->
      print_binop_stm_output (Const c) op e2
  | BinOp (op, e1, e2) ->
      F.fprintf f "(%a %s %a)" pp_exp e1 (Binop.str pe op) pp_exp e2
  | Exn e ->
      F.fprintf f "EXN %a" pp_exp e
  | Closure {name; captured_vars} ->
      if List.is_empty captured_vars then F.fprintf f "(%a)" pp_exp (Const (Cfun name))
      else
        F.fprintf f "(%a,%a)" pp_exp (Const (Cfun name))
          (Pp.comma_seq (pp_captured_var pe pp_t))
          captured_vars
  | Lvar pv ->
      Pvar.pp pe f pv
  | Lfield (e, fld, _) ->
      F.fprintf f "%a.%a" pp_exp e Typ.Fieldname.pp fld
  | Lindex (e1, e2) ->
      F.fprintf f "%a[%a]" pp_exp e1 pp_exp e2
  | Sizeof {typ; nbytes; dynamic_length; subtype} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" pp_exp) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      let pp_if b pp label f v = if b then F.fprintf f ";%s=%a" label pp v in
      let pp_if_some pp_opt label f opt = pp_if (Option.is_some opt) pp_opt label f opt in
      let subt_s = F.asprintf "%a" Subtype.pp subtype in
      F.fprintf f "sizeof(t=%a%a%a%a)" pp_t typ (pp_if_some pp_size "nbytes") nbytes
        (pp_if_some pp_len "len") dynamic_length
        (pp_if (not (String.equal "" subt_s)) Subtype.pp "sub_t")
        subtype


and pp_captured_var pe pp_t f (exp, var, typ) =
  match exp with
  | Lvar evar when Pvar.equal var evar ->
      (Pvar.pp pe) f var
  | _ ->
      F.fprintf f "(%a %a:%a)" (pp_ pe pp_t) exp (Pvar.pp pe) var (Typ.pp pe) typ


let pp_printenv ~print_types pe f e =
  let pp_typ = if print_types then Typ.pp_full else Typ.pp in
  pp_ pe (pp_typ pe) f e


let pp f e = pp_printenv ~print_types:false Pp.text f e

let to_string e = F.asprintf "%a" pp e

let is_objc_block_closure = function
  | Closure {name} ->
      Typ.Procname.is_objc_block name
  | _ ->
      false


let rec gen_free_vars =
  let open Sequence.Generator in
  function
  | Var id ->
      yield id
  | Cast (_, e) | Exn e | Lfield (e, _, _) | Sizeof {dynamic_length= Some e} | UnOp (_, e, _) ->
      gen_free_vars e
  | Closure {captured_vars} ->
      ISequence.gen_sequence_list captured_vars ~f:(fun (e, _, _) -> gen_free_vars e)
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _) | Lvar _ | Sizeof {dynamic_length= None}
    ->
      return ()
  | BinOp (_, e1, e2) | Lindex (e1, e2) ->
      gen_free_vars e1 >>= fun () -> gen_free_vars e2


let free_vars e = Sequence.Generator.run (gen_free_vars e)

let ident_mem e id = free_vars e |> Sequence.exists ~f:(Ident.equal id)

let rec gen_program_vars =
  let open Sequence.Generator in
  function
  | Lvar name ->
      yield name
  | Const _ | Var _ | Sizeof {dynamic_length= None} ->
      return ()
  | Cast (_, e) | Exn e | Lfield (e, _, _) | Sizeof {dynamic_length= Some e} | UnOp (_, e, _) ->
      gen_program_vars e
  | BinOp (_, e1, e2) | Lindex (e1, e2) ->
      gen_program_vars e1 >>= fun () -> gen_program_vars e2
  | Closure {captured_vars} ->
      ISequence.gen_sequence_list captured_vars ~f:(fun (e, _, _) -> gen_program_vars e)


let program_vars e = Sequence.Generator.run (gen_program_vars e)

let zero_of_type typ =
  match typ.Typ.desc with
  | Typ.Tint _ ->
      Some (Const (Cint IntLit.zero))
  | Typ.Tfloat _ ->
      Some (Const (Cfloat 0.0))
  | Typ.Tptr _ ->
      Some (Const (Cint IntLit.null))
  | _ ->
      None


let zero_of_type_exn typ = Option.value_exn (zero_of_type typ)

let rec ignore_cast e = match e with Cast (_, e) -> ignore_cast e | _ -> e

let rec ignore_integer_cast e =
  match e with Cast (t, e) when Typ.is_int t -> ignore_integer_cast e | _ -> e
