(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val to_dotty : string -> unit
(** construct the file-call-graph and store it in [<results-dir>/<filename>] *)

val partition_source_file_call_graph : n_workers:int -> unit
