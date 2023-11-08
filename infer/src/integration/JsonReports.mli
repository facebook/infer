(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val potential_exception_message : string

val loc_trace_to_jsonbug_record : Errlog.loc_trace_elem list -> Jsonbug_t.json_trace_item list

val censored_reason : IssueType.t -> SourceFile.t -> string option

val sanitize_qualifier : string -> string [@@warning "-unused-value-declaration"]

val compute_hash :
     severity:string
  -> bug_type:string
  -> proc_name:Procname.t
  -> file:string
  -> qualifier:string
  -> string
  [@@warning "-unused-value-declaration"]

val write_reports : issues_json:string -> costs_json:string -> config_impact_json:string -> unit
