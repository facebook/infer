(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | AccessToInvalidAddress of
      { invalidated_by: PulseDomain.Invalidation.t PulseDomain.Trace.t
      ; accessed_by: unit PulseDomain.Trace.t }
  | StackVariableAddressEscape of
      { variable: Var.t
      ; history: PulseDomain.ValueHistory.t
      ; location: Location.t }

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      PulseDomain.InterprocAction.to_outer_location accessed_by.action
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {accessed_by; invalidated_by; _} ->
      (* The goal is to get one of the following messages depending on the scenario:

         42: delete x; return x->f
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete` on `x`"

         42: bar(x); return x->f
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete` on `x` in call to `bar`"

         42: bar(x); foo(x);
         "call to `foo` eventually accesses `x->f` but `x` was invalidated at line 42 by `delete` on `x` in call to `bar`"

         If we don't have "x->f" but instead some non-user-visible expression, then
         "access to `x`, which was invalidated at line 42 by `delete` on `x`"

         Likewise if we don't have "x" in the second part but instead some non-user-visible expression, then
         "`x->f` accesses `x`, which was invalidated at line 42 by `delete`"
      *)
      let pp_access_trace fmt (trace : _ PulseDomain.Trace.t) =
        match trace.action with
        | Immediate {imm= _; _} ->
            F.fprintf fmt "accessing memory that "
        | ViaCall {f; _} ->
            F.fprintf fmt "call to %a eventually accesses memory that "
              PulseDomain.CallEvent.describe f
      in
      let pp_invalidation_trace line fmt trace =
        match trace.PulseDomain.Trace.action with
        | Immediate {imm= invalidation; _} ->
            F.fprintf fmt "%a on line %d" PulseDomain.Invalidation.describe invalidation line
        | ViaCall {action; f; _} ->
            F.fprintf fmt "%a on line %d indirectly during the call to %a"
              PulseDomain.Invalidation.describe
              (PulseDomain.InterprocAction.get_immediate action)
              line PulseDomain.CallEvent.describe f
      in
      let line_of_trace trace =
        let {Location.line; _} =
          PulseDomain.InterprocAction.to_outer_location trace.PulseDomain.Trace.action
        in
        line
      in
      let invalidation_line = line_of_trace invalidated_by in
      F.asprintf "%a%a" pp_access_trace accessed_by
        (pp_invalidation_trace invalidation_line)
        invalidated_by
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable


let get_trace = function
  | AccessToInvalidAddress {accessed_by; invalidated_by} ->
      PulseDomain.Trace.add_to_errlog ~header:"invalidation part of the trace starts here"
        (fun f invalidation ->
          F.fprintf f "memory %a here" PulseDomain.Invalidation.describe invalidation )
        invalidated_by
      @@ PulseDomain.Trace.add_to_errlog ~header:"use-after-lifetime part of the trace starts here"
           (fun f () -> F.pp_print_string f "invalid access occurs here")
           accessed_by
      @@ []
  | StackVariableAddressEscape {history; location; _} ->
      PulseDomain.ValueHistory.add_to_errlog ~nesting:0 history
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      PulseDomain.InterprocAction.get_immediate invalidated_by.action
      |> PulseDomain.Invalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
