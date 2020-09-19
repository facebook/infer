(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Configuration options from config file *)

val find : string -> string option
val find_exn : string -> string
val find_list : string -> string list
