(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_custom_init : Tenv.t -> Procname.t -> bool

val is_logging_method : Procname.t -> bool

val get_fbthreadsafe_class_annot : Procname.t -> Tenv.t -> (string * string) option

val message_fbthreadsafe_class : string -> string -> string
