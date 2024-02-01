(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Call Flags *)

open! IStd
module F = Format

(** Flags for a procedure call *)
type t =
  { cf_assign_last_arg: bool
  ; cf_injected_destructor: bool
        (** true if this is an implicit C++ destructor call injected by the clang frontend *)
  ; cf_interface: bool
  ; cf_is_objc_block: bool
  ; cf_is_c_function_ptr: bool
  ; cf_virtual: bool }
[@@deriving compare, equal, hash, normalize]

val pp : F.formatter -> t -> unit

val default : t
(** Default value where all fields are set to false *)
