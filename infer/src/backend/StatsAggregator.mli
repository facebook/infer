(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Generate files containing statistics aggregated from individual statistics files under
    Config.results_dir *)

val generate_files : unit -> unit
