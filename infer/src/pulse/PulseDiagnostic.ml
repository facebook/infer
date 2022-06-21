(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Attribute = PulseAttribute
module CallEvent = PulseCallEvent
module Decompiler = PulseDecompiler
module Invalidation = PulseInvalidation
module Taint = PulseTaint
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type calling_context = (CallEvent.t * Location.t) list [@@deriving compare, equal]

type access_to_invalid_address =
  { calling_context: calling_context
  ; invalid_address: Decompiler.expr
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
  ; access_trace: Trace.t
  ; must_be_valid_reason: Invalidation.must_be_valid_reason option }
[@@deriving compare, equal]

type erlang_error =
  | Badkey of {calling_context: calling_context; location: Location.t}
  | Badmap of {calling_context: calling_context; location: Location.t}
  | Badmatch of {calling_context: calling_context; location: Location.t}
  | Badrecord of {calling_context: calling_context; location: Location.t}
  | Case_clause of {calling_context: calling_context; location: Location.t}
  | Function_clause of {calling_context: calling_context; location: Location.t}
  | If_clause of {calling_context: calling_context; location: Location.t}
  | Try_clause of {calling_context: calling_context; location: Location.t}
[@@deriving compare, equal]

type read_uninitialized_value = {calling_context: calling_context; trace: Trace.t}
[@@deriving compare, equal]

let yojson_of_access_to_invalid_address = [%yojson_of: _]

let yojson_of_erlang_error = [%yojson_of: _]

let yojson_of_read_uninitialized_value = [%yojson_of: _]

type t =
  | AccessToInvalidAddress of access_to_invalid_address
  | MemoryLeak of {allocator: Attribute.allocator; allocation_trace: Trace.t; location: Location.t}
  | RetainCycle of
      { assignment_traces: Trace.t list
      ; value: Decompiler.expr
      ; path: Decompiler.expr
      ; location: Location.t }
  | ErlangError of erlang_error
  | ReadUninitializedValue of read_uninitialized_value
  | ResourceLeak of {class_name: JavaClassName.t; allocation_trace: Trace.t; location: Location.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | TaintFlow of
      { tainted: Decompiler.expr
      ; source: Taint.t * ValueHistory.t
      ; sink: Taint.t * Trace.t
      ; location: Location.t }
  | FlowFromTaintSource of
      { tainted: Decompiler.expr
      ; source: Taint.t * ValueHistory.t
      ; destination: Procname.t
      ; location: Location.t }
  | FlowToTaintSink of
      { source: Decompiler.expr * Trace.t
      ; sanitizers: Attribute.TaintSanitizedSet.t
      ; sink: Taint.t * Trace.t
      ; location: Location.t }
  | UnnecessaryCopy of
      { copied_into: PulseAttribute.CopiedInto.t
      ; typ: Typ.t
      ; location: Location.t
      ; from: PulseNonDisjunctiveDomain.CopyOrigin.t }
[@@deriving equal]

let get_location = function
  | AccessToInvalidAddress {calling_context= []; access_trace}
  | ReadUninitializedValue {calling_context= []; trace= access_trace} ->
      Trace.get_outer_location access_trace
  | AccessToInvalidAddress {calling_context= (_, location) :: _}
  | ReadUninitializedValue {calling_context= (_, location) :: _} ->
      (* report at the call site that triggers the bug *) location
  | MemoryLeak {location}
  | ResourceLeak {location}
  | RetainCycle {location}
  | ErlangError (Badkey {location})
  | ErlangError (Badmap {location})
  | ErlangError (Badmatch {location})
  | ErlangError (Badrecord {location})
  | ErlangError (Case_clause {location})
  | ErlangError (Function_clause {location})
  | ErlangError (If_clause {location})
  | ErlangError (Try_clause {location})
  | StackVariableAddressEscape {location}
  | TaintFlow {location}
  | FlowFromTaintSource {location}
  | FlowToTaintSink {location}
  | UnnecessaryCopy {location} ->
      location


let get_copy_type = function UnnecessaryCopy {typ} -> Some typ | _ -> None

let aborts_execution = function
  | AccessToInvalidAddress _
  | ErlangError
      ( Badkey _
      | Badmap _
      | Badmatch _
      | Badrecord _
      | Case_clause _
      | Function_clause _
      | If_clause _
      | Try_clause _ )
  | ReadUninitializedValue _ ->
      (* these errors either abort the whole program or, if they are false positives, mean that
         pulse is confused and the current abstract state has stopped making sense; either way,
         abort! *)
      true
  | MemoryLeak _
  | ResourceLeak _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | FlowFromTaintSource _
  | FlowToTaintSink _
  | UnnecessaryCopy _ ->
      false


(* whether the [calling_context + trace] starts with a call or contains only an immediate event *)
let immediate_or_first_call calling_context (trace : Trace.t) =
  match (calling_context, trace) with
  | [], Immediate _ ->
      `Immediate
  | (f, _) :: _, _ | [], ViaCall {f; _} ->
      `Call f


let pp_calling_context_prefix fmt calling_context =
  match calling_context with
  | [] ->
      ()
  | [(call_event, _)] ->
      F.fprintf fmt "The call to %a may trigger the following issue: " CallEvent.pp call_event
  | [(call_event1, _); (call_event2, _)] ->
      F.fprintf fmt "The call to %a in turn calls %a and may trigger the following issue: "
        CallEvent.pp call_event1 CallEvent.pp call_event2
  | (call_event, _) :: _ :: _ :: _ ->
      let in_between_calls = List.length calling_context - 2 in
      F.fprintf fmt
        "The call to %a ends up calling %a (after %d more call%s) and may trigger the following \
         issue: "
        CallEvent.pp call_event CallEvent.pp
        (List.last_exn calling_context |> fst)
        in_between_calls
        (if in_between_calls > 1 then "s" else "")


let get_message diagnostic =
  match diagnostic with
  | AccessToInvalidAddress
      { calling_context
      ; invalid_address
      ; invalidation
      ; invalidation_trace
      ; access_trace
      ; must_be_valid_reason } -> (
    match invalidation with
    | ConstantDereference i when IntLit.equal i IntLit.zero ->
        let pp_access_trace fmt (trace : Trace.t) =
          match immediate_or_first_call calling_context trace with
          | `Immediate ->
              ()
          | `Call f ->
              F.fprintf fmt " in call to %a" CallEvent.describe f
        in
        let pp_invalidation_trace line fmt (trace : Trace.t) =
          match immediate_or_first_call calling_context trace with
          | `Immediate ->
              F.fprintf fmt "(last assigned on line %d)" line
          | `Call f ->
              F.fprintf fmt "(from the call to %a on line %d)" CallEvent.describe f line
        in
        let invalidation_line =
          let {Location.line; _} = Trace.get_outer_location invalidation_trace in
          line
        in
        let pp_must_be_valid_reason fmt expr =
          let pp_prefix fmt null_nil_block =
            if Decompiler.is_unknown expr then
              F.fprintf fmt "%s %a" null_nil_block
                (pp_invalidation_trace invalidation_line)
                invalidation_trace
            else
              F.fprintf fmt "`%a` could be %s %a and" Decompiler.pp_expr expr null_nil_block
                (pp_invalidation_trace invalidation_line)
                invalidation_trace
          in
          match must_be_valid_reason with
          | Some (SelfOfNonPODReturnMethod non_pod_typ) ->
              F.fprintf fmt
                "%a is used to call a C++ method with a non-POD return type `%a`%a; nil messaging \
                 such methods is undefined behaviour"
                pp_prefix "nil" (Typ.pp_full Pp.text) non_pod_typ pp_access_trace access_trace
          | Some (InsertionIntoCollectionKey | InsertionIntoCollectionValue) ->
              F.fprintf fmt
                "%a is used as a %s when inserting into a collection%a, potentially causing a crash"
                pp_prefix "nil"
                ( match[@warning "-8"] must_be_valid_reason with
                | Some InsertionIntoCollectionKey ->
                    "key"
                | Some InsertionIntoCollectionValue ->
                    "value" )
                pp_access_trace access_trace
          | Some BlockCall ->
              F.fprintf fmt "%a is called%a, causing a crash" pp_prefix "nil block" pp_access_trace
                access_trace
          | None ->
              F.fprintf fmt "%a is dereferenced%a" pp_prefix "null" pp_access_trace access_trace
        in
        F.asprintf "%a%a" pp_calling_context_prefix calling_context pp_must_be_valid_reason
          invalid_address
    | _ ->
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
              F.fprintf fmt "%a during the call to %a%a" Invalidation.describe invalidation
                CallEvent.describe f pp_line line
        in
        let invalidation_line =
          let {Location.line; _} = Trace.get_outer_location invalidation_trace in
          line
        in
        F.asprintf "%a%a%a" pp_calling_context_prefix calling_context pp_access_trace access_trace
          (pp_invalidation_trace invalidation_line invalidation)
          invalidation_trace )
  | MemoryLeak {allocator; location; allocation_trace} ->
      let allocation_line =
        let {Location.line; _} = Trace.get_outer_location allocation_trace in
        line
      in
      let pp_allocation_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt "by `%a` on line %d" Attribute.pp_allocator allocator allocation_line
        | ViaCall {f; _} ->
            F.fprintf fmt "by `%a`, indirectly via call to %a on line %d" Attribute.pp_allocator
              allocator CallEvent.describe f allocation_line
      in
      F.asprintf "Memory dynamically allocated %a is not freed after the last access at %a"
        pp_allocation_trace allocation_trace Location.pp location
  | ResourceLeak {class_name; location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let allocation_line =
        let {Location.line; _} = Trace.get_outer_location allocation_trace in
        line
      in
      let pp_allocation_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt "by constructor %a() on line %d" JavaClassName.pp class_name
              allocation_line
        | ViaCall {f; _} ->
            F.fprintf fmt "by constructor %a(), indirectly via call to %a on line %d"
              JavaClassName.pp class_name CallEvent.describe f allocation_line
      in
      F.asprintf "Resource dynamically allocated %a is not closed after the last access at %a"
        pp_allocation_trace allocation_trace Location.pp location
  | RetainCycle {location; value; path} ->
      F.asprintf
        "Memory managed via reference counting is locked in a retain cycle at %a: `%a` retains \
         itself via `%a`"
        Location.pp location Decompiler.pp_expr value Decompiler.pp_expr path
  | ErlangError (Badkey {calling_context= _; location}) ->
      F.asprintf "bad key at %a" Location.pp location
  | ErlangError (Badmap {calling_context= _; location}) ->
      F.asprintf "bad map at %a" Location.pp location
  | ErlangError (Badmatch {calling_context= _; location}) ->
      F.asprintf "no match of RHS at %a" Location.pp location
  | ErlangError (Badrecord {calling_context= _; location}) ->
      F.asprintf "bad record at %a" Location.pp location
  | ErlangError (Case_clause {calling_context= _; location}) ->
      F.asprintf "no matching case clause at %a" Location.pp location
  | ErlangError (Function_clause {calling_context= _; location}) ->
      F.asprintf "no matching function clause at %a" Location.pp location
  | ErlangError (If_clause {calling_context= _; location}) ->
      F.asprintf "no true branch in if expression at %a" Location.pp location
  | ErlangError (Try_clause {calling_context= _; location}) ->
      F.asprintf "no matching branch in try at %a" Location.pp location
  | ReadUninitializedValue {calling_context; trace} ->
      let root_var =
        Trace.find_map trace ~f:(function VariableDeclared (pvar, _, _) -> Some pvar | _ -> None)
        |> IOption.if_none_evalopt ~f:(fun () ->
               Trace.find_map trace ~f:(function
                 | FormalDeclared (pvar, _, _) ->
                     Some pvar
                 | _ ->
                     None ) )
        |> Option.map ~f:(F.asprintf "%a" Pvar.pp_value_non_verbose)
      in
      let declared_fields =
        Trace.find_map trace ~f:(function
          | StructFieldAddressCreated (fields, _, _) ->
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
        match immediate_or_first_call calling_context trace with
        | `Immediate ->
            ()
        | `Call f ->
            F.fprintf fmt " during the call to %a" CallEvent.describe f
      in
      F.asprintf "%t is read without initialization%t" pp_access_path pp_location
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "Address of %a is returned by the function" pp_var variable
  | TaintFlow {tainted; source= source, _; sink= sink, _} ->
      (* TODO: say what line the source happened in the current function *)
      F.asprintf "`%a` is tainted by %a and flows to %a" Decompiler.pp_expr tainted Taint.pp source
        Taint.pp sink
  | FlowFromTaintSource {tainted; source= source, _; destination} ->
      F.asprintf "`%a` is tainted by %a and flows to %a" Decompiler.pp_expr tainted Taint.pp source
        Procname.pp destination
  | FlowToTaintSink {source= expr, _; sanitizers; sink= sink, _} ->
      let pp_sanitizers fmt sanitizers =
        if Attribute.TaintSanitizedSet.is_empty sanitizers then ()
        else
          F.fprintf fmt ", and might be sanitized by %a" Attribute.TaintSanitizedSet.pp sanitizers
      in
      F.asprintf "`%a` flows to taint sink %a%a" Decompiler.pp_expr expr Taint.pp sink pp_sanitizers
        sanitizers
  | UnnecessaryCopy {copied_into; typ; location; from} -> (
      let open PulseNonDisjunctiveDomain in
      let suppression_msg =
        "If this copy was intentional, consider adding the word `copy` into the variable name to \
         suppress this warning"
      in
      let suggestion_msg =
        match from with
        | CopyOrigin.CopyCtor ->
            "try using a reference `&`"
        | CopyOrigin.CopyAssignment ->
            "try getting a reference to it or move it if possible"
      in
      match copied_into with
      | IntoVar _ ->
          F.asprintf
            "%a variable `%a` with type `%a` is not modified after it is copied on %a. To avoid \
             the copy, %s. %s."
            CopyOrigin.pp from PulseAttribute.CopiedInto.pp copied_into (Typ.pp_full Pp.text) typ
            Location.pp_line location suggestion_msg suppression_msg
      | IntoField fname ->
          F.asprintf
            "Field `%a` with type `%a` is copied into from an rvalue-ref here but is not modified \
             afterwards. Rather than copying into it, try moving into it instead."
            Fieldname.pp fname (Typ.pp_full Pp.text) typ )


let add_errlog_header ~nesting ~title location errlog =
  let tags = [] in
  Errlog.make_trace_element nesting location title tags :: errlog


let get_trace_calling_context calling_context errlog =
  match calling_context with
  | [] ->
      errlog
  | (_, first_call_loc) :: _ ->
      add_errlog_header ~nesting:0 ~title:"calling context starts here" first_call_loc
      @@ ( (* errlog is built in the reverse order so reverse everything first *)
           List.fold (List.rev calling_context)
             ~init:(errlog, List.length calling_context - 1)
             ~f:(fun (errlog, depth) (call, loc) ->
               ( Errlog.make_trace_element depth loc
                   (F.asprintf "in call to %a" CallEvent.pp call)
                   []
                 :: errlog
               , depth - 1 ) )
         |> fst )


let invalidation_titles (invalidation : Invalidation.t) =
  match invalidation with
  | ConstantDereference i when IntLit.equal i IntLit.zero ->
      ( "source of the null value part of the trace starts here"
      , "null pointer dereference part of the trace starts here" )
  | ConstantDereference _ ->
      ( "source of the constant value part of the trace starts here"
      , "constant value dereference part of the trace starts here" )
  | CFree
  | CustomFree _
  | CppDelete
  | CppDeleteArray
  | EndIterator
  | GoneOutOfScope _
  | OptionalEmpty
  | StdVector _
  | JavaIterator _ ->
      ( "invalidation part of the trace starts here"
      , "use-after-lifetime part of the trace starts here" )


let add_invalidation_trace ~nesting invalidation invalidation_trace errlog =
  let start_title = invalidation_titles invalidation |> fst in
  let start_location = Trace.get_start_location invalidation_trace in
  add_errlog_header ~nesting ~title:start_title start_location
  @@ Trace.add_to_errlog ~nesting:(nesting + 1)
       ~pp_immediate:(fun fmt -> F.fprintf fmt "%a" Invalidation.describe invalidation)
       invalidation_trace
  @@ errlog


let add_access_trace ~include_title ~nesting invalidation access_trace errlog =
  let access_start_location = Trace.get_start_location access_trace in
  let access_title = invalidation_titles invalidation |> snd in
  ( if include_title then add_errlog_header ~nesting ~title:access_title access_start_location
  else Fn.id )
  @@ Trace.add_to_errlog ~nesting:(nesting + 1)
       ~pp_immediate:(fun fmt -> F.pp_print_string fmt "invalid access occurs here")
       access_trace
  @@ errlog


let get_trace = function
  | AccessToInvalidAddress {calling_context; invalidation; invalidation_trace; access_trace} ->
      let in_context_nesting = List.length calling_context in
      let should_print_invalidation_trace = not (Trace.has_invalidation access_trace) in
      get_trace_calling_context calling_context
      @@ ( if should_print_invalidation_trace then
           add_invalidation_trace ~nesting:in_context_nesting invalidation invalidation_trace
         else Fn.id )
      @@ add_access_trace
           ~include_title:(should_print_invalidation_trace || not (List.is_empty calling_context))
           ~nesting:in_context_nesting invalidation access_trace
      @@ []
  | ResourceLeak {class_name; location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt ->
             F.fprintf fmt "allocated by constructor %a() here" JavaClassName.pp class_name )
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | MemoryLeak {allocator; location; allocation_trace} ->
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt ->
             F.fprintf fmt "allocated by `%a` here" Attribute.pp_allocator allocator )
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | RetainCycle {assignment_traces; location} ->
      List.fold_right assignment_traces
        ~init:[Errlog.make_trace_element 0 location "retain cycle here" []]
        ~f:(fun assignment_trace errlog ->
          Trace.add_to_errlog ~nesting:1
            ~pp_immediate:(fun fmt -> F.fprintf fmt "assigned")
            assignment_trace errlog )
  | ErlangError (Badkey {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "bad key here" []]
  | ErlangError (Badmap {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "bad map here" []]
  | ErlangError (Badmatch {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "no match of RHS here" []]
  | ErlangError (Badrecord {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "bad record here" []]
  | ErlangError (Case_clause {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "no matching case clause here" []]
  | ErlangError (Function_clause {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "no matching function clause here" []]
  | ErlangError (If_clause {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "no true branch in if expression here" []]
  | ErlangError (Try_clause {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "no matching branch in try here" []]
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
  | TaintFlow {source= _, source_history; sink= sink, sink_trace} ->
      (* TODO: the sink trace includes the history for the source in its own value history,
         creating duplicate information in the trace if we don't pass
         [include_value_history:false]. The history in the sink can also go further into source
         code than we want if the source is a function that we analyze. Ideally we would cut just
         the overlapping histories from [sink_trace] instead of not including value histories
         altogether. *)
      ValueHistory.add_to_errlog ~nesting:0 source_history
      @@ Trace.add_to_errlog ~include_value_history:false ~nesting:0
           ~pp_immediate:(fun fmt -> Taint.pp fmt sink)
           sink_trace
      @@ []
  | FlowFromTaintSource {source= _, source_history} ->
      ValueHistory.add_to_errlog ~nesting:0 source_history @@ []
  | FlowToTaintSink {source= _, history; sanitizers; sink= sink, sink_trace} ->
      let add_to_errlog = Trace.add_to_errlog ~include_value_history:false ~nesting:0 in
      add_to_errlog history ~pp_immediate:(fun fmt -> F.pp_print_string fmt "allocated here")
      @@ add_to_errlog sink_trace ~pp_immediate:(fun fmt -> Taint.pp fmt sink)
      @@
      let prepend_sanitizer_to_errlog Attribute.TaintSanitized.{sanitizer; trace} errlog =
        let trace_start_location = Trace.get_start_location trace in
        [Errlog.make_trace_element 0 trace_start_location "But potentially sanitized:" []]
        @ add_to_errlog trace ~pp_immediate:(fun fmt -> Taint.pp fmt sanitizer) errlog
      in
      Attribute.TaintSanitizedSet.fold prepend_sanitizer_to_errlog sanitizers []
  | UnnecessaryCopy {location; from} ->
      let nesting = 0 in
      [ Errlog.make_trace_element nesting location
          (F.asprintf "%a here" PulseNonDisjunctiveDomain.CopyOrigin.pp from)
          [] ]


let get_issue_type ~latent issue_type =
  match (issue_type, latent) with
  | MemoryLeak {allocator}, false -> (
    match allocator with
    | CMalloc | CustomMalloc _ | CRealloc | CustomRealloc _ ->
        IssueType.pulse_memory_leak_c
    | CppNew | CppNewArray ->
        IssueType.pulse_memory_leak_cpp
    | JavaResource _ | ObjCAlloc ->
        L.die InternalError
          "Memory leaks should not have a Java resource or Objective-C alloc as allocator" )
  | ResourceLeak _, false ->
      IssueType.pulse_resource_leak
  | RetainCycle _, false ->
      IssueType.retain_cycle
  | StackVariableAddressEscape _, false ->
      IssueType.stack_variable_address_escape
  | UnnecessaryCopy {copied_into= PulseAttribute.CopiedInto.IntoField _}, false ->
      IssueType.unnecessary_copy_movable_pulse
  | UnnecessaryCopy {from= PulseNonDisjunctiveDomain.CopyOrigin.CopyCtor}, false ->
      IssueType.unnecessary_copy_pulse
  | UnnecessaryCopy {from= PulseNonDisjunctiveDomain.CopyOrigin.CopyAssignment}, false ->
      IssueType.unnecessary_copy_assignment_pulse
  | ( ( MemoryLeak _
      | ResourceLeak _
      | RetainCycle _
      | StackVariableAddressEscape _
      | UnnecessaryCopy _ )
    , true ) ->
      L.die InternalError "Issue type cannot be latent"
  | AccessToInvalidAddress {invalidation; must_be_valid_reason}, _ ->
      Invalidation.issue_type_of_cause ~latent invalidation must_be_valid_reason
  | ErlangError (Badkey _), _ ->
      IssueType.bad_key ~latent
  | ErlangError (Badmap _), _ ->
      IssueType.bad_map ~latent
  | ErlangError (Badmatch _), _ ->
      IssueType.no_match_of_rhs ~latent
  | ErlangError (Badrecord _), _ ->
      IssueType.bad_record ~latent
  | ErlangError (Case_clause _), _ ->
      IssueType.no_matching_case_clause ~latent
  | ErlangError (Function_clause _), _ ->
      IssueType.no_matching_function_clause ~latent
  | ErlangError (If_clause _), _ ->
      IssueType.no_true_branch_in_if ~latent
  | ErlangError (Try_clause _), _ ->
      IssueType.no_matching_branch_in_try ~latent
  | ReadUninitializedValue _, _ ->
      IssueType.uninitialized_value_pulse ~latent
  | TaintFlow _, _ ->
      IssueType.taint_error
  | FlowFromTaintSource _, _ ->
      IssueType.sensitive_data_flow
  | FlowToTaintSink _, _ ->
      IssueType.data_flow_to_sink
