(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractAddress = PulseDomain.AbstractAddress

type actor = {access_expr: HilExp.AccessExpression.t; location: Location.t} [@@deriving compare]

type t =
  | AccessToInvalidAddress of
      { invalidated_by: PulseInvalidation.t
      ; accessed_by: actor
      ; trace: PulseTrace.t
      ; address: AbstractAddress.t }
  | StackVariableAddressEscape of {variable: Var.t; location: Location.t}

let get_location = function
  | AccessToInvalidAddress {accessed_by= {location}} | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; address; trace} ->
      let pp_debug_address f =
        if Config.debug_mode then F.fprintf f " (debug: %a)" AbstractAddress.pp address
      in
      F.asprintf "`%a` accesses address %a%a past its lifetime%t" HilExp.AccessExpression.pp
        accessed_by.access_expr PulseTrace.pp_interesting_events trace PulseInvalidation.pp
        invalidated_by pp_debug_address
  | StackVariableAddressEscape {variable} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; trace} ->
      let invalidated_by_trace =
        PulseInvalidation.get_location invalidated_by
        |> Option.map ~f:(fun location ->
               Errlog.make_trace_element 0 location
                 (F.asprintf "%a here" PulseInvalidation.pp invalidated_by)
                 [] )
        |> Option.to_list
      in
      PulseTrace.make_errlog_trace ~depth:0 trace
      @ invalidated_by_trace
      @ [ Errlog.make_trace_element 0 accessed_by.location
            (F.asprintf "accessed `%a` here" HilExp.AccessExpression.pp accessed_by.access_expr)
            [] ]
  | StackVariableAddressEscape _ ->
      []


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseInvalidation.issue_type_of_cause invalidated_by
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
