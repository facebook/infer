(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Constants *)

open! IStd
module F = Format

(** Constants *)
type t =
  | Cint of IntLit.t  (** integer constants *)
  | Cfun of Typ.Procname.t  (** function names *)
  | Cstr of string  (** string constants *)
  | Cfloat of float  (** float constants *)
  | Cclass of Ident.name  (** class constant *)
[@@deriving compare]

val equal : t -> t -> bool

val kind_equal : t -> t -> bool
(** Return true if the constants have the same kind (both integers, ...) *)

val pp : Pp.env -> F.formatter -> t -> unit
(** Pretty print a const *)

val iszero_int_float : t -> bool

val isone_int_float : t -> bool

val isminusone_int_float : t -> bool
