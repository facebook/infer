(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Generate unit tests automatically from specs *)

(** type of generated code *)
type code

(** pretty print generated code *)
val pp_code : Format.formatter -> code -> unit

(** generate a unit test form a spec *)
val genunit : string -> Procname.t -> int -> (Mangled.t * Sil.typ) list
  -> Prop.normal Specs.spec -> code

(** generate code for a main calling all the unit test functions passed as argument *)
val genmain : (Procname.t * int) list -> code
