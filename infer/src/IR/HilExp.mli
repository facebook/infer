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

type t =
  | AccessExpression of AccessExpression.t  (** access path (e.g., x.f.g or x[i]) *)
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
[@@deriving compare]

val pp : F.formatter -> t -> unit

val get_typ : Tenv.t -> t -> Typ.t option
(** Get the type of the expression. Warning: not fully implemented *)

val of_sil :
  include_array_indexes:bool -> f_resolve_id:(Var.t -> AccessExpression.t option) -> add_deref:bool
  -> Exp.t -> Typ.t -> t
(** Convert SIL expression to HIL expression *)

val get_access_exprs : t -> AccessExpression.t list
(** Get all the access paths used in the given HIL expression, including duplicates if a path is
    used more than once. *)

val is_null_literal : t -> bool

val eval : t -> Const.t option
