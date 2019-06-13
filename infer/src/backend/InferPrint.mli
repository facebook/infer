(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val loc_trace_to_jsonbug_record :
  Errlog.loc_trace_elem list -> Exceptions.severity -> Jsonbug_t.json_trace_item list

val censored_reason : IssueType.t -> SourceFile.t -> string option

val main : report_json:string option -> unit
