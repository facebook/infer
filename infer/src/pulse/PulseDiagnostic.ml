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

type access_to_invalid_address =
  { calling_context: (CallEvent.t * Location.t) list
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
  ; access_trace: Trace.t }
[@@deriving compare, equal]

type read_uninitialized_value = {calling_context: (CallEvent.t * Location.t) list; trace: Trace.t}
[@@deriving compare, equal]

let yojson_of_access_to_invalid_address = [%yojson_of: _]

let yojson_of_read_uninitialized_value = [%yojson_of: _]

type t =
  | AccessToInvalidAddress of access_to_invalid_address
  | MemoryLeak of {procname: Procname.t; allocation_trace: Trace.t; location: Location.t}
  | ReadUninitializedValue of read_uninitialized_value
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
[@@deriving equal]

let get_location = function
  | AccessToInvalidAddress {calling_context= []; access_trace}
  | ReadUninitializedValue {calling_context= []; trace= access_trace} ->
      Trace.get_outer_location access_trace
  | AccessToInvalidAddress {calling_context= (_, location) :: _}
  | ReadUninitializedValue {calling_context= (_, location) :: _} ->
      (* report at the call site that triggers the bug *) location
  | MemoryLeak {location} | StackVariableAddressEscape {location} ->
      location


(* whether the [calling_context + trace] starts with a call or contains only an immediate event *)
let immediate_or_first_call calling_context (trace : Trace.t) =
  match (calling_context, trace) with
  | [], Immediate _ ->
      `Immediate
  | (f, _) :: _, _ | [], ViaCall {f; _} ->
      `Call f


let get_message = function
  | AccessToInvalidAddress {calling_context; invalidation; invalidation_trace; access_trace} ->
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
      let pp_access_trace fmt (trace : Trace.t) =
        match immediate_or_first_call calling_context trace with
        | `Immediate ->
            F.fprintf fmt "accessing memory that "
        | `Call f ->
            F.fprintf fmt "call to %a eventually accesses memory that " CallEvent.describe f
      in
      let pp_invalidation_trace line invalidation fmt (trace : Trace.t) =
        let pp_line fmt line = F.fprintf fmt " on line %d" line in
        match immediate_or_first_call calling_context trace with
        | `Immediate ->
            F.fprintf fmt "%a%a" Invalidation.describe invalidation pp_line line
        | `Call f ->
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
      F.asprintf
        "memory dynamically allocated at line %d %a, is not freed after the last access at %a"
        allocation_line pp_allocation_trace allocation_trace Location.pp location
  | ReadUninitializedValue {calling_context; trace} ->
      let root_var =
        PulseTrace.find_map trace ~f:(function VariableDeclared (pvar, _) -> Some pvar | _ -> None)
        |> IOption.if_none_evalopt ~f:(fun () ->
               PulseTrace.find_map trace ~f:(function
                 | FormalDeclared (pvar, _) ->
                     Some pvar
                 | _ ->
                     None ) )
        |> Option.map ~f:(F.asprintf "%a" Pvar.pp_value_non_verbose)
      in
      let declared_fields =
        PulseTrace.find_map trace ~f:(function
          | StructFieldAddressCreated (fields, _) ->
              Some fields
          | _ ->
              None )
        |> Option.map ~f:(F.asprintf "%a" ValueHistory.pp_fields)
      in
      let access_path =
        match (root_var, declared_fields) with
        | None, None ->
            None
        | Some root_var, None ->
            Some root_var
        | None, Some declared_fields ->
            Some (F.sprintf "_.%s" declared_fields)
        | Some root_var, Some declared_fields ->
            Some (F.sprintf "%s.%s" root_var declared_fields)
      in
      let pp_access_path fmt = Option.iter access_path ~f:(F.fprintf fmt " `%s`") in
      let pp_location fmt =
        let {Location.line} = Trace.get_outer_location trace in
        match immediate_or_first_call calling_context trace with
        | `Immediate ->
            F.fprintf fmt "on line %d" line
        | `Call f ->
            F.fprintf fmt "during the call to %a on line %d" CallEvent.describe f line
      in
      F.asprintf "uninitialized value%t is read %t" pp_access_path pp_location
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


let get_trace_calling_context calling_context errlog =
  match calling_context with
  | [] ->
      errlog
  | (_, first_call_loc) :: _ ->
      add_errlog_header ~title:"calling context starts here" first_call_loc
      @@ ( List.fold calling_context ~init:(errlog, 0) ~f:(fun (errlog, depth) (call, loc) ->
               ( Errlog.make_trace_element depth loc
                   (F.asprintf "in call to %a" CallEvent.pp call)
                   []
                 :: errlog
               , depth + 1 ) )
         |> fst )


let get_trace = function
  | AccessToInvalidAddress {calling_context; invalidation; invalidation_trace; access_trace} ->
      get_trace_calling_context calling_context
      @@
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
  | ReadUninitializedValue {calling_context; trace} ->
      get_trace_calling_context calling_context
      @@ Trace.add_to_errlog ~nesting:0
           ~pp_immediate:(fun fmt -> F.pp_print_string fmt "read to uninitialized value occurs here")
           trace
      @@ []
  | StackVariableAddressEscape {history; location; _} ->
      ValueHistory.add_to_errlog ~nesting:0 history
      @@
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "returned here" []]


let get_issue_type = function
  | AccessToInvalidAddress {invalidation; _} ->
      Invalidation.issue_type_of_cause invalidation
  | MemoryLeak _ ->
      IssueType.pulse_memory_leak
  | ReadUninitializedValue _ ->
      IssueType.uninitialized_value_pulse
  | StackVariableAddressEscape _ ->
      IssueType.stack_variable_address_escape
