(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val is_custom_init : 'tenv_t -> 'procname_t -> bool

val is_logging_method : 'procname_t -> bool

val get_fbthreadsafe_class_annot : 'procname_t -> 'tenv_t -> (string * string) option

val message_fbthreadsafe_class : string -> string -> string
