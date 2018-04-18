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
module L = Logging
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

let pp f cf =
  if cf.cf_virtual then F.fprintf f " virtual" ;
  if cf.cf_noreturn then F.fprintf f " noreturn" ;
  if cf.cf_with_block_parameters then F.fprintf f " block_params"


let default =
  { cf_virtual= false
  ; cf_interface= false
  ; cf_noreturn= false
  ; cf_is_objc_block= false
  ; cf_with_block_parameters= false
  ; cf_targets= [] }
