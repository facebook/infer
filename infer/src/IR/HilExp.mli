(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Access : sig
  type 'array_index t =
    | FieldAccess of Typ.Fieldname.t
    | ArrayAccess of Typ.t * 'array_index
    | TakeAddress
    | Dereference
  [@@deriving compare]

  val pp : (Format.formatter -> 'array_index -> unit) -> Format.formatter -> 'array_index t -> unit
end

type t =
  | AccessExpression of access_expression  (** access path (e.g., x.f.g or x[i]) *)
  | UnaryOperator of Unop.t * t * Typ.t option
      (** Unary operator with type of the result if known *)
  | BinaryOperator of Binop.t * t * t  (** Binary operator *)
  | Exception of t  (** Exception *)
  | Closure of Typ.Procname.t * (AccessPath.base * t) list  (** Name of function + environment *)
  | Constant of Const.t  (** Constants *)
  | Cast of Typ.t * t  (** Type cast *)
  | Sizeof of Typ.t * t option
      (** C-style sizeof(), and also used to treate a type as an expression. Refer to [Exp] module for
      canonical documentation *)

and access_expression = private
  | Base of AccessPath.base
  | FieldOffset of access_expression * Typ.Fieldname.t  (** field access *)
  | ArrayOffset of access_expression * Typ.t * t option  (** array access *)
  | AddressOf of access_expression  (** "address of" operator [&] *)
  | Dereference of access_expression  (** "dereference" operator [*] *)
[@@deriving compare]

module AccessExpression : sig
  val base : AccessPath.base -> access_expression

  val field_offset : access_expression -> Typ.Fieldname.t -> access_expression

  val array_offset : access_expression -> Typ.t -> t option -> access_expression

  val dereference : access_expression -> access_expression
  (** guarantees that we never build [Dereference (AddressOf t)] expressions: these become [t] *)

  val to_accesses_fold :
       access_expression
    -> init:'accum
    -> f_array_offset:('accum -> t option -> 'accum * 'array_index)
    -> 'accum * AccessPath.base * 'array_index Access.t list

  val to_access_path : access_expression -> AccessPath.t

  val to_access_paths : access_expression list -> AccessPath.t list

  val get_base : access_expression -> AccessPath.base

  val replace_base :
    remove_deref_after_base:bool -> AccessPath.base -> access_expression -> access_expression

  val is_base : access_expression -> bool

  val get_typ : access_expression -> Tenv.t -> Typ.t option

  val pp : Format.formatter -> access_expression -> unit

  val equal : access_expression -> access_expression -> bool

  type nonrec t = access_expression = private
    | Base of AccessPath.base
    | FieldOffset of access_expression * Typ.Fieldname.t
    | ArrayOffset of access_expression * Typ.t * t option
    | AddressOf of access_expression
    | Dereference of access_expression
  [@@deriving compare]

  val fold_vars : (t, Var.t, 'accum) Container.fold
end

val pp : F.formatter -> t -> unit

val get_typ : Tenv.t -> t -> Typ.t option
(** Get the type of the expression. Warning: not fully implemented *)

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

val eval : t -> Const.t option

val ignore_cast : t -> t
