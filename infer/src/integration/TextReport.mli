(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val create_from_json :
  quiet:bool -> console_limit:int option -> report_txt:string -> report_json:string -> unit
(** Read [report_json] and produce a textual output in [report_txt]. If [not quiet] then display at
    most [console_limit] issues on stdout. If [console_limit] is [None] then display all the issues. *)
