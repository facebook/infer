(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Invalidation = PulseDomain.Invalidation
module Trace = PulseDomain.Trace
module ValueHistory = PulseDomain.ValueHistory
open PulseBasicInterface

type t =
  | AccessToInvalidAddress of {invalidated_by: Invalidation.t Trace.t; accessed_by: unit Trace.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}

let get_location = function
  | AccessToInvalidAddress {accessed_by} ->
      Trace.get_outer_location accessed_by
  | StackVariableAddressEscape {location} ->
      location


let get_message = function
  | AccessToInvalidAddress {accessed_by; invalidated_by} ->
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
      let pp_access_trace fmt (trace : unit Trace.t) =
        match trace with
        | Immediate {imm= (); _} ->
            F.fprintf fmt "accessing memory that "
        | ViaCall {f; _} ->
            F.fprintf fmt "call to %a eventually accesses memory that " CallEvent.describe f
      in
      let pp_invalidation_trace line fmt (trace : Invalidation.t Trace.t) =
        let pp_line fmt line = F.fprintf fmt " on line %d" line in
        match trace with
        | ViaCall {f; in_call} ->
            let invalid = Trace.get_immediate in_call in
            F.fprintf fmt "%a%a indirectly during the call to %a" Invalidation.describe invalid
              pp_line line CallEvent.describe f
        | Immediate {imm= invalid} ->
            F.fprintf fmt "%a%a" Invalidation.describe invalid pp_line line
      in
      let invalidation_line =
        let {Location.line; _} = Trace.get_outer_location invalidated_by in
        line
      in
      F.asprintf "%a%a" pp_access_trace accessed_by
        (pp_invalidation_trace invalidation_line)
        invalidated_by
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
  | AccessToInvalidAddress {accessed_by; invalidated_by} ->
      let start_location = Trace.get_start_location invalidated_by in
      add_errlog_header ~title:"invalidation part of the trace starts here" start_location
      @@ Trace.add_to_errlog ~nesting:1
           (fun fmt invalid -> F.fprintf fmt "%a" Invalidation.describe invalid)
           invalidated_by
      @@
      let access_start_location = Trace.get_start_location accessed_by in
      add_errlog_header ~title:"use-after-lifetime part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           (fun fmt () -> F.pp_print_string fmt "invalid access occurs here")
           accessed_by
      @@ []
  | StackVariableAddressEscape {history; location; _} ->
      ValueHistory.add_to_errlog ~nesting:0 history
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidated_by} ->
      Trace.get_immediate invalidated_by |> Invalidation.issue_type_of_cause
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
