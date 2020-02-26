(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_registered_dir_names : unit -> string list
(** Directory names responsible for storing checker-specific issues generated at file-level analysis
    phase. (Those are additional issues on top of ones stored in summary after procedure analysis
    phase). *)

val register_dir_name : string -> unit
(** Add directory name. No-op if was already added *)
