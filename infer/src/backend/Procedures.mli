(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_all : filter:Filtering.procedures_filter -> unit -> Procname.t list

val pp_all :
     filter:Filtering.procedures_filter
  -> proc_name:bool
  -> defined:bool
  -> source_file:bool
  -> proc_attributes:bool
  -> proc_cfg:bool
  -> callees:bool
  -> Format.formatter
  -> unit
  -> unit

val select_proc_names_interactive : filter:Filtering.procedures_filter -> Procname.t list option
