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
module L = Logging
module F = Format

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
  | Var of Ident.t  (** Unary operator with type of the result if known *)
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

(** Equality for expressions. *)

val equal : t -> t -> bool

(** Hash function for expressions. *)

val hash : t -> int

(** Set of expressions. *)

module Set : Caml.Set.S with type elt = t

(** Map with expression keys. *)

module Map : Caml.Map.S with type key = t

(** Hashtable with expression keys. *)

module Hash : Caml.Hashtbl.S with type key = t

(** returns true is index is an array index of arr. *)

val is_array_index_of : t -> t -> bool

val is_null_literal : t -> bool

(** return true if [exp] is the special this/self expression *)

val is_this : t -> bool

val is_zero : t -> bool

(** {2 Utility Functions for Expressions} *)

(** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception *)

val texp_to_typ : Typ.t option -> t -> Typ.t

(** Return the root of [lexp]. *)

val root_of_lexp : t -> t

(** Get an expression "undefined", the boolean indicates
    whether the undefined value goest into the footprint *)

val get_undefined : bool -> t

(** Checks whether an expression denotes a location using pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. *)

val pointer_arith : t -> bool

(** returns true if the expression represents a stack-directed address *)

val is_stack_addr : t -> bool

(** returns true if the expression operates on address of local variable *)

val has_local_addr : t -> bool

(** Integer constant 0 *)

val zero : t

(** Null constant *)

val null : t

(** Integer constant 1 *)

val one : t

(** Integer constant -1 *)

val minus_one : t

(** Create integer constant *)

val int : IntLit.t -> t

(** Create float constant *)

val float : float -> t

(** Create integer constant corresponding to the boolean value *)

val bool : bool -> t

(** Create expresstion [e1 == e2] *)

val eq : t -> t -> t

(** Create expresstion [e1 != e2] *)

val ne : t -> t -> t

(** Create expresstion [e1 <= e2] *)

val le : t -> t -> t

(** Create expression [e1 < e2] *)

val lt : t -> t -> t

(** Extract the ids and pvars from an expression *)

val get_vars : t -> Ident.t list * Pvar.t list

val pp_printenv : Pp.env -> (Pp.env -> F.formatter -> Typ.t -> unit) -> F.formatter -> t -> unit

val pp : F.formatter -> t -> unit

val to_string : t -> string
