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

let pp f cf =
  if cf.cf_virtual then F.pp_print_string f " virtual" ;
  if cf.cf_assign_last_arg then F.pp_print_string f " assign_last" ;
  if cf.cf_noreturn then F.pp_print_string f " noreturn" ;
  if cf.cf_with_block_parameters then F.pp_print_string f " block_params"


let default =
  { cf_virtual= false
  ; cf_interface= false
  ; cf_assign_last_arg= false
  ; cf_noreturn= false
  ; cf_is_objc_block= false
  ; cf_with_block_parameters= false
  ; cf_targets= [] }
