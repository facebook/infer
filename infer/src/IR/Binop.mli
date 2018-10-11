(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Binary Operators *)
open! IStd

(** Binary operations *)
type t =
  | PlusA of Typ.ikind option  (** arithmetic + *)
  | PlusPI  (** pointer + integer *)
  | MinusA of Typ.ikind option  (** arithmetic - *)
  | MinusPI  (** pointer - integer *)
  | MinusPP  (** pointer - pointer *)
  | Mult of Typ.ikind option  (** * *)
  | Div  (** / *)
  | Mod  (** % *)
  | Shiftlt  (** shift left *)
  | Shiftrt  (** shift right *)
  | Lt  (** <  (arithmetic comparison) *)
  | Gt  (** >  (arithmetic comparison) *)
  | Le  (** <= (arithmetic comparison) *)
  | Ge  (** >= (arithmetic comparison) *)
  | Eq  (** == (arithmetic comparison) *)
  | Ne  (** != (arithmetic comparison) *)
  | BAnd  (** bitwise and *)
  | BXor  (** exclusive-or *)
  | BOr  (** inclusive-or *)
  | LAnd  (** logical and. Does not always evaluate both operands. *)
  | LOr  (** logical or. Does not always evaluate both operands. *)
[@@deriving compare]

val equal : t -> t -> bool

val injective : t -> bool
(** This function returns true if the operation is injective
    wrt. each argument: op(e,-) and op(-, e) is injective for all e.
    The return value false means "don't know". *)

val is_zero_runit : t -> bool
(** This function returns true if 0 is the right unit of [binop].
    The return value false means "don't know". *)

val str : Pp.env -> t -> string
(** String representation of a binary operator. *)
