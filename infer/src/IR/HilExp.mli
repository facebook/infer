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
  | AccessPath of AccessPath.Raw.t
  (** access path (e.g., x.f.g or x[i]) *)
  | UnaryOperator of Unop.t * t * Typ.t option
  (** Unary operator with type of the result if known *)
  | BinaryOperator of Binop.t * t * t
  (** Binary operator *)
  | Exception of t
  (** Exception *)
  | Closure of Typ.Procname.t * (AccessPath.base * t) list
  (** Name of function + environment *)
  | Constant of Const.t
  (** Constants *)
  | Cast of Typ.t * t
  (** Type cast *)
  | Sizeof of Typ.t * t option
  (** C-style sizeof(), and also used to treate a type as an expression. Refer to [Exp] module for
      canonical documentation *)
[@@deriving compare]

val pp : F.formatter -> t -> unit

(** Convert SIL expression to HIL expression *)
val of_sil : f_resolve_id:(Var.t -> AccessPath.Raw.t option) -> Exp.t -> Typ.t -> t

(** Get all the access paths used in the given HIL expression, including duplicates if a path is
    used more than once. *)
val get_access_paths : t -> AccessPath.Raw.t list

val is_null_literal : t -> bool
