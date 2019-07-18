(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* Suppress unused value warning until this is used for incremental diff analysis *)
val build : CallGraph.t -> unit
  [@@warning "-32"]
(** Build the graph from the summaries in the .specs files *)
