(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_custom_init : Tenv.t -> Typ.Procname.t -> bool

val is_logging_method : Typ.Procname.t -> bool

val get_fbthreadsafe_class_annot : Typ.Procname.t -> Tenv.t -> (string * string) option

val message_fbthreadsafe_class : string -> string -> string
