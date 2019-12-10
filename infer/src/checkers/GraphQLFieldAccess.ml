(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = LithoDomain

(* return true if this is a graphql getter *)
let is_graphql_function procname summary =
  Option.is_none summary
  (* we skip analysis of all GraphQL procs *)
  &&
  match procname with
  | Typ.Procname.Java java_procname -> (
      PatternMatch.is_getter java_procname
      &&
      match Typ.Procname.Java.get_package java_procname with
      | Some package ->
          String.is_prefix ~prefix:"com.facebook.graphql.model" package
      | None ->
          false )
  | _ ->
      false


module LithoContext = struct
  type t = Domain.t

  let check_callee ~callee_pname ~tenv:_ = is_graphql_function callee_pname

  let satisfies_heuristic ~callee_pname:_ ~callee_summary_opt:_ _ = true

  let field = Payloads.Fields.litho_graphql_field_access

  let should_report proc_desc _tenv =
    LithoFramework.is_on_create_layout (Procdesc.get_proc_name proc_desc)


  let report astate _tenv summary =
    let report_graphql_getter access_path call_chain =
      let call_strings =
        List.map
          ~f:(fun Domain.MethodCall.{procname} ->
            Typ.Procname.to_simplified_string ~withclass:false procname )
          call_chain
      in
      let call_string = String.concat ~sep:"." call_strings in
      let message = F.asprintf "%a.%s" AccessPath.pp access_path call_string in
      let loc = Summary.get_loc summary in
      let ltr = [Errlog.make_trace_element 0 loc message []] in
      Reporting.log_error summary ~loc ~ltr IssueType.graphql_field_access message
    in
    Domain.iter_call_chains ~f:report_graphql_getter astate


  let session_name = "litho graphql field access"
end

module Analyzer = LithoFramework.MakeAnalyzer (LithoContext)

let checker callback = Analyzer.checker callback
