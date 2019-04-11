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

let pp_access = PulseTrace.pp_action HilExp.AccessExpression.pp

let pp_invalidation = PulseTrace.pp_action PulseInvalidation.pp

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseTrace.outer_location_of_action accessed_by
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; trace} ->
      F.asprintf "%a accesses address %a%a past its lifetime" pp_access accessed_by
        PulseTrace.pp_interesting_events trace pp_invalidation invalidated_by
  | StackVariableAddressEscape {variable} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; trace} ->
      PulseTrace.make_errlog_trace ~depth:0 trace
      @ PulseTrace.trace_of_action ~action_name:"invalidated" PulseInvalidation.pp invalidated_by
      @ PulseTrace.trace_of_action ~action_name:"accessed"
          (Pp.in_backticks HilExp.AccessExpression.pp)
          accessed_by
  | StackVariableAddressEscape _ ->
      []


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseTrace.immediate_of_action invalidated_by |> PulseInvalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
