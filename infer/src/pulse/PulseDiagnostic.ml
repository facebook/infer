(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type t =
  | AccessToInvalidAddress of
      {invalidation: Invalidation.t; invalidation_trace: Trace.t; access_trace: Trace.t}
  | MemoryLeak of {procname: Procname.t; allocation_trace: Trace.t; location: Location.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | OrError of (t list * Location.t)
[@@deriving compare]

let equal = [%compare.equal: t]

let get_location = function
  | AccessToInvalidAddress {access_trace} ->
      Trace.get_outer_location access_trace
  | MemoryLeak {location} | StackVariableAddressEscape {location} ->
     location
  | OrError (_, location) -> location


let rec get_message ?print_loc:(pr=false) = function
  | AccessToInvalidAddress {invalidation; invalidation_trace; access_trace} ->
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
      let pp_line fmt line = if line < 0 then F.fprintf fmt " in a caller" else F.fprintf fmt " on line %d" line in
      let pp_access_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate {location} ->
             if pr then
                 F.fprintf fmt "accessing memory (%a) that " pp_line location.Location.line
             else
                 F.fprintf fmt "accessing memory that "
        | ViaCall {f; location} ->
             if pr then
                 F.fprintf fmt ("call to %a eventually accesses memory (%a) that ") CallEvent.describe f pp_line location.Location.line
             else
                 F.fprintf fmt ("call to %a eventually accesses memory that ") CallEvent.describe f
      in
      let pp_invalidation_trace line invalidation fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt "%a%a" Invalidation.describe invalidation pp_line line
        | ViaCall {f; _} ->
            F.fprintf fmt "%a%a indirectly during the call to %a" Invalidation.describe invalidation
              pp_line line CallEvent.describe f
      in
      let invalidation_line =
        let {Location.line; _} = Trace.get_outer_location invalidation_trace in
        line
      in
      F.asprintf "%a%a" pp_access_trace access_trace
        (pp_invalidation_trace invalidation_line invalidation)
        invalidation_trace
  | MemoryLeak {procname; location; allocation_trace} ->
      let allocation_line =
        let {Location.line; _} = Trace.get_outer_location allocation_trace in
        line
      in
      let pp_allocation_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt "by call to `%a`" Procname.pp procname
        | ViaCall {f; _} ->
            F.fprintf fmt "by call to %a" CallEvent.describe f
      in
      F.asprintf "memory dynamically allocated at line %d %a, is not reachable after %a"
        allocation_line pp_allocation_trace allocation_trace Location.pp location
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "address of %a is returned by the function" pp_var variable
  | OrError (ers, _) ->
     F.asprintf "If this function is called, one of the following errors will happen:\n%s" (List.fold ers ~init:"" ~f:(fun acc er -> acc ^ ("   - " ^ get_message ~print_loc:true er) ^"\n"))

let add_errlog_header ~title location errlog =
  let depth = 0 in
  let tags = [] in
  Errlog.make_trace_element depth location title tags :: errlog


let get_trace = function
  | AccessToInvalidAddress {invalidation; invalidation_trace; access_trace} ->
      let start_location = Trace.get_start_location invalidation_trace in
      add_errlog_header ~title:"invalidation part of the trace starts here" start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt -> F.fprintf fmt "%a" Invalidation.describe invalidation)
           invalidation_trace
      @@
      let access_start_location = Trace.get_start_location access_trace in
      add_errlog_header ~title:"use-after-lifetime part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt -> F.pp_print_string fmt "invalid access occurs here")
           access_trace
      @@ []
  | MemoryLeak {location; allocation_trace} ->
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~title:"allocation part of the trace starts here" access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt -> F.pp_print_string fmt "allocation part of the trace ends here")
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | StackVariableAddressEscape {history; location; _} ->
      ValueHistory.add_to_errlog ~nesting:0 history
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]
  | OrError _ ->
     []


let get_issue_type = function
  | AccessToInvalidAddress {invalidation; _} ->
      Invalidation.issue_type_of_cause invalidation
  | MemoryLeak _ ->
      IssueType.pulse_memory_leak
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
  | OrError _ ->
     IssueType.combined_pointer_errors
