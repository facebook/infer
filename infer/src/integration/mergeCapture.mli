(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val merge_global_tenv : normalize:bool -> DB.filename list -> unit
(** Merge tenvs from the given paths and store the result as a global tenv *)

val merge_captured_targets : root:string -> unit
