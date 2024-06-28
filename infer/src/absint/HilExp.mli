(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | AccessExpression of access_expression  (** access path (e.g., x.f.g or x[i]) *)
  | UnaryOperator of Unop.t * t * Typ.t option
      (** Unary operator with type of the result if known *)
  | BinaryOperator of Binop.t * t * t  (** Binary operator *)
  | Exception of t  (** Exception *)
  | Closure of Procname.t * (AccessPath.base * t) list  (** Name of function + environment *)
  | Constant of Const.t  (** Constants *)
  | Cast of Typ.t * t  (** Type cast *)
  | Sizeof of Typ.t * t option
      (** C-style sizeof(), and also used to treate a type as an expression. Refer to [Exp] module
          for canonical documentation *)

and access_expression = private
  | Base of AccessPath.base
  | FieldOffset of access_expression * Fieldname.t  (** field access *)
  | ArrayOffset of access_expression * Typ.t * t option  (** array access *)
  | AddressOf of access_expression  (** "address of" operator [&] *)
  | Dereference of access_expression  (** "dereference" operator [*] *)
[@@deriving compare, equal]

module AccessExpression : sig
  val of_id : Ident.t -> Typ.t -> access_expression [@@warning "-unused-value-declaration"]

  val base : AccessPath.base -> access_expression

  val field_offset : access_expression -> Fieldname.t -> access_expression

  val array_offset : access_expression -> Typ.t -> t option -> access_expression

  val address_of : access_expression -> access_expression option
  [@@warning "-unused-value-declaration"]
  (** address_of doesn't always make sense, eg [address_of (Dereference t)] is [None] *)

  val address_of_base : AccessPath.base -> access_expression [@@warning "-unused-value-declaration"]

  val to_access_path : access_expression -> AccessPath.t

  val get_base : access_expression -> AccessPath.base

  val replace_base :
    remove_deref_after_base:bool -> AccessPath.base -> access_expression -> access_expression

  val is_return_var : access_expression -> bool

  val get_typ : access_expression -> Tenv.t -> Typ.t option

  val pp : Format.formatter -> access_expression -> unit

  val equal : access_expression -> access_expression -> bool

  val to_accesses : access_expression -> access_expression * t option MemoryAccess.t list
  (** return the base and a list of accesses equivalent to the input expression *)

  val add_access : access_expression -> t option MemoryAccess.t -> access_expression option

  val truncate : access_expression -> (access_expression * t option MemoryAccess.t) option
  (** remove and return the prefix and the last access of the expression if it's a base; otherwise
      return None *)

  val append : onto:access_expression -> access_expression -> access_expression option
  (** [append ~onto y] replaces the base of [y] with [onto] itself; this makes sense if no
      [Dereference (AddressOf _)] instances are introduced *)

  type nonrec t = access_expression = private
    | Base of AccessPath.base
    | FieldOffset of access_expression * Fieldname.t
    | ArrayOffset of access_expression * Typ.t * t option
    | AddressOf of access_expression
    | Dereference of access_expression
  [@@deriving compare]

  val fold_vars : (t, Var.t, 'accum) Container.fold
end

val pp : F.formatter -> t -> unit

val of_sil :
     include_array_indexes:bool
  -> f_resolve_id:(Var.t -> AccessExpression.t option)
  -> add_deref:bool
  -> Exp.t
  -> Typ.t
  -> t
(** Convert SIL expression to HIL expression *)

val get_access_exprs : t -> AccessExpression.t list
(** Get all the access paths used in the given HIL expression, including duplicates if a path is
    used more than once. *)

val is_null_literal : t -> bool

val is_int_zero : t -> bool

(* val eval : t -> Const.t option *)

val eval_boolean_exp : AccessExpression.t -> t -> bool option
(** [eval_boolean_exp var exp] returns [Some bool_value] if the given boolean expression [exp]
    evaluates to [bool_value] when [var] is set to true. Return None if it has free variables that
    stop us from evaluating it, or is not a boolean expression. *)

val access_expr_of_exp :
     include_array_indexes:bool
  -> f_resolve_id:(Var.t -> AccessExpression.t option)
  -> Exp.t
  -> Typ.t
  -> access_expression option
(** best effort translating a SIL expression to an access path, not semantics preserving in
    particular in the presence of pointer arithmetic *)
