(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val pp_custom_of_report :
  Format.formatter -> Jsonbug_t.jsonbug list -> IssuesTestField.t list -> unit

val write_from_json : json_path:string -> out_path:string -> IssuesTestField.t list -> unit

val pp_trace : Format.formatter -> Jsonbug_t.json_trace_item list -> string -> unit
