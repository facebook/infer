(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Call Flags *)

open! IStd
module F = Format

(** Flags for a procedure call *)
type t =
  { cf_virtual: bool
  ; cf_interface: bool
  ; cf_noreturn: bool
  ; cf_is_objc_block: bool
  ; cf_targets: Typ.Procname.t list
  ; cf_with_block_parameters: bool }
[@@deriving compare]

val pp : F.formatter -> t -> unit

val default : t
(** Default value where all fields are set to false *)
