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
      { access: HilExp.AccessExpression.t
      ; invalidated_by: PulseInvalidation.t PulseTrace.t
      ; accessed_by: HilExp.AccessExpression.t PulseTrace.t }
  | StackVariableAddressEscape of
      { variable: Var.t
      ; trace: PulseTrace.breadcrumbs
      ; location: Location.t }

let describe_access f trace =
  PulseTrace.pp_action (Pp.in_backticks HilExp.AccessExpression.pp) f trace.PulseTrace.action


let describe_invalidation f trace =
  PulseTrace.pp_action PulseInvalidation.describe f trace.PulseTrace.action


let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseTrace.outer_location_of_action accessed_by.action
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {access; accessed_by; invalidated_by; _} ->
      (* TODO: [access] might be something irrelevant to the user, shouldn't print it in that case
         *)
      let line_of_trace trace =
        let {Location.line; _} = PulseTrace.outer_location_of_action trace.PulseTrace.action in
        line
      in
      let invalidation_line = line_of_trace invalidated_by in
      let access_line = line_of_trace accessed_by in
      let pp_indirect_access f =
        let erroneous_access = PulseTrace.immediate_of_action accessed_by.action in
        if not (HilExp.AccessExpression.equal erroneous_access access) then
          F.fprintf f " via %a" describe_access accessed_by
      in
      F.asprintf "access to `%a`%t at line %d is to %a on line %d" HilExp.AccessExpression.pp
        access pp_indirect_access access_line describe_invalidation invalidated_by
        invalidation_line
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by} ->
      let pp_invalid_access f access =
        F.fprintf f "invalid access to `%a`" HilExp.AccessExpression.pp access
      in
      PulseTrace.add_to_errlog ~header:"invalidation part of the trace starts here"
        PulseInvalidation.describe invalidated_by
      @@ PulseTrace.add_to_errlog ~header:"use-after-lifetime part of the trace starts here"
           pp_invalid_access accessed_by
      @@ []
  | StackVariableAddressEscape {trace; location; _} ->
      PulseTrace.add_errlog_of_breadcrumbs ~nesting:0 trace
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseTrace.immediate_of_action invalidated_by.action |> PulseInvalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
