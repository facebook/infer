(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Expressions *)

open! IStd
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
  | Var of Ident.t  (** Pure variable: it is not an lvalue *)
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

val equal : t -> t -> bool
(** Equality for expressions. *)

(** Set of expressions. *)
module Set : Caml.Set.S with type elt = t

(** Map with expression keys. *)
module Map : Caml.Map.S with type key = t

(** Hashtable with expression keys. *)
module Hash : Caml.Hashtbl.S with type key = t

val is_null_literal : t -> bool

val is_this : t -> bool
(** return true if [exp] is the special this/self expression *)

val is_zero : t -> bool

(** {2 Utility Functions for Expressions} *)

val texp_to_typ : Typ.t option -> t -> Typ.t
(** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception *)

val root_of_lexp : t -> t
(** Return the root of [lexp]. *)

val get_undefined : bool -> t
(** Get an expression "undefined", the boolean indicates
    whether the undefined value goest into the footprint *)

val pointer_arith : t -> bool
(** Checks whether an expression denotes a location using pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. *)

val has_local_addr : t -> bool
(** returns true if the expression operates on address of local variable *)

val zero : t
(** Integer constant 0 *)

val null : t
(** Null constant *)

val one : t
(** Integer constant 1 *)

val minus_one : t
(** Integer constant -1 *)

val int : IntLit.t -> t
(** Create integer constant *)

val float : float -> t
(** Create float constant *)

val bool : bool -> t
(** Create integer constant corresponding to the boolean value *)

val eq : t -> t -> t
(** Create expression [e1 == e2] *)

val ne : t -> t -> t
(** Create expression [e1 != e2] *)

val le : t -> t -> t
(** Create expression [e1 <= e2] *)

val lt : t -> t -> t
(** Create expression [e1 < e2] *)

val free_vars : t -> Ident.t Sequence.t
(** all the idents appearing in the expression *)

val gen_free_vars : t -> (unit, Ident.t) Sequence.Generator.t

val ident_mem : t -> Ident.t -> bool
(** true if the identifier appears in the expression *)

val program_vars : t -> Pvar.t Sequence.t
(** all the program variables appearing in the expression *)

val fold_captured : f:('a -> t -> 'a) -> t -> 'a -> 'a
(** Fold over the expressions captured by this expression. *)

val pp_printenv : print_types:bool -> Pp.env -> F.formatter -> t -> unit

val pp : F.formatter -> t -> unit

val to_string : t -> string

val is_objc_block_closure : t -> bool

val zero_of_type : Typ.t -> t option
(** Returns the zero value of a type, for int, float and ptr types *)

val zero_of_type_exn : Typ.t -> t

val ignore_cast : t -> t

val ignore_integer_cast : t -> t
