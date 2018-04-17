(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val pp_all :
  ?filter:string -> proc_name:bool -> attr_kind:bool -> source_file:bool -> proc_attributes:bool
  -> Format.formatter -> unit -> unit
