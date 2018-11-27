(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Call Flags *)

open! IStd
module F = Format

(** Flags for a procedure call *)
type t =
  { cf_virtual: bool
  ; cf_interface: bool
  ; cf_assign_last_arg: bool
  ; cf_noreturn: bool
  ; cf_is_objc_block: bool
  ; cf_targets: Typ.Procname.t list
  ; cf_with_block_parameters: bool }
[@@deriving compare]

val pp : F.formatter -> t -> unit

val default : t
(** Default value where all fields are set to false *)
