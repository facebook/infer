(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | AccessToInvalidAddress of
      { invalidated_by: PulseInvalidation.t PulseTrace.action
      ; accessed_by: HilExp.AccessExpression.t PulseTrace.action
      ; trace: PulseTrace.t }
  | StackVariableAddressEscape of {variable: Var.t; location: Location.t}

let describe_access = PulseTrace.pp_action HilExp.AccessExpression.pp

let describe_invalidation = PulseTrace.pp_action PulseInvalidation.describe

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseTrace.outer_location_of_action accessed_by
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; _} ->
      F.asprintf "%a accesses address invalidated by %a past its lifetime" describe_access
        accessed_by describe_invalidation invalidated_by
  | StackVariableAddressEscape {variable} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let add_errlog_header ~title location errlog =
  let depth = 0 in
  let tags = [] in
  Errlog.make_trace_element depth location title tags :: errlog


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; trace} ->
      let add_header_if_some ~title location_opt errlog =
        match location_opt with
        | None ->
            errlog
        | Some location ->
            add_errlog_header ~title location errlog
      in
      add_errlog_header ~title:"start of end of lifetime trace"
        (PulseTrace.location_of_action_start invalidated_by)
      @@ PulseTrace.add_errlog_of_action ~nesting:1 ~action_name:"invalidated by"
           PulseInvalidation.describe invalidated_by
      @@ add_errlog_header ~title:"start of use after lifetime trace"
           (PulseTrace.location_of_action_start accessed_by)
      @@ PulseTrace.add_errlog_of_action ~nesting:1 ~action_name:"accessed"
           (Pp.in_backticks HilExp.AccessExpression.pp)
           accessed_by
      @@ add_header_if_some ~title:"start of value trace" (PulseTrace.get_start_location trace)
      @@ PulseTrace.add_errlog_of_trace ~nesting:1 trace
      @@ []
  | StackVariableAddressEscape _ ->
      []


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseTrace.immediate_of_action invalidated_by |> PulseInvalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
