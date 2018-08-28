(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_all : filter:Filtering.procedures_filter -> unit -> Typ.Procname.t list

val pp_all :
     filter:Filtering.procedures_filter
  -> proc_name:bool
  -> attr_kind:bool
  -> source_file:bool
  -> proc_attributes:bool
  -> Format.formatter
  -> unit
  -> unit
