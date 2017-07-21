(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Constants *)

open! IStd
module L = Logging
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

val to_string : t -> string

val iszero_int_float : t -> bool

val isone_int_float : t -> bool

val isminusone_int_float : t -> bool
