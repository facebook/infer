(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Decompiled Expressions *)

open! IStd
module L = Logging
module F = Format

(** expression representing the result of decompilation *)
type t =
  | Darray of t * t
  | Dbinop of Binop.t * t * t
  | Dconst of Const.t
  | Dsizeof of Typ.t * t option * Subtype.t
  | Dderef of t
  | Dfcall of t * t list * Location.t * CallFlags.t
  | Darrow of t * Typ.Fieldname.t
  | Ddot of t * Typ.Fieldname.t
  | Dpvar of Pvar.t
  | Dpvaraddr of Pvar.t
  | Dunop of Unop.t * t
  | Dunknown
  | Dretcall of t * t list * Location.t * CallFlags.t

(** Value paths: identify an occurrence of a value in a symbolic heap
    each expression represents a path, with Dpvar being the simplest one *)
type vpath = t option

val to_string : t -> string
(** convert to a string *)

val pp : F.formatter -> t -> unit
(** pretty print *)

val pp_vpath : Pp.env -> F.formatter -> vpath -> unit
(** Pretty print a value path *)

val has_tmp_var : t -> bool
(** return true if [dexp] contains a temporary pvar *)
