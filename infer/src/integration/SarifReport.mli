(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val create_from_json : report_sarif:string -> report_json:string -> unit
(** Read [report_json] and produce a textual output in [report_sarif]. *)
