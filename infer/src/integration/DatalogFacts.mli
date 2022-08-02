(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val create_from_json : datalog_dir:string -> report_json:string -> unit
(** Produce .facts files in a subdirectory. Facts can be used as input for Soufflé. *)
