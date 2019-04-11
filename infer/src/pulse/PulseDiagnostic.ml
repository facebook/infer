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
      ; invalidated_by: PulseInvalidation.t PulseTrace.action
      ; accessed_by: HilExp.AccessExpression.t PulseTrace.action
      ; trace: PulseTrace.t }
  | StackVariableAddressEscape of {variable: Var.t; trace: PulseTrace.t; location: Location.t}

let describe_access = PulseTrace.pp_action (Pp.in_backticks HilExp.AccessExpression.pp)

let describe_invalidation = PulseTrace.pp_action PulseInvalidation.describe

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseTrace.outer_location_of_action accessed_by
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {access; accessed_by; invalidated_by; _} ->
      (* TODO: [access] might be something irrelevant to the user, shouldn't print it in that case
         *)
      let line_of_action action =
        let {Location.line; _} = PulseTrace.outer_location_of_action action in
        line
      in
      let invalidation_line = line_of_action invalidated_by in
      let access_line = line_of_action accessed_by in
      let pp_indirect_access f =
        let erroneous_access = PulseTrace.immediate_of_action accessed_by in
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
      let pp_invalid_access f access =
        F.fprintf f "invalid access to `%a`" HilExp.AccessExpression.pp access
      in
      add_errlog_header ~title:"invalidation part of the trace starts here"
        (PulseTrace.outer_location_of_action invalidated_by)
      @@ PulseTrace.add_errlog_of_action ~nesting:1 PulseInvalidation.describe invalidated_by
      @@ add_errlog_header ~title:"use-after-lifetime part of the trace starts here"
           (PulseTrace.outer_location_of_action accessed_by)
      @@ PulseTrace.add_errlog_of_action ~nesting:1 pp_invalid_access accessed_by
      @@ add_header_if_some ~title:"trace of how the access expression was constructed starts here"
           (PulseTrace.get_start_location trace)
      @@ PulseTrace.add_errlog_of_trace ~nesting:1 trace
      @@ []
  | StackVariableAddressEscape {trace; location; _} ->
      PulseTrace.add_errlog_of_trace ~nesting:0 trace
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseTrace.immediate_of_action invalidated_by |> PulseInvalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
