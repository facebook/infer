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
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type calling_context = (CallEvent.t * Location.t) list [@@deriving compare, equal]

type access_to_invalid_address =
  { calling_context: calling_context
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
  | RetainCycle of {assignment_trace: Trace.t; location: Location.t}
  | ErlangError of erlang_error
  | ReadUninitializedValue of read_uninitialized_value
  | ResourceLeak of {class_name: JavaClassName.t; allocation_trace: Trace.t; location: Location.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | UnnecessaryCopy of {variable: Var.t; location: Location.t}
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
  | UnnecessaryCopy {location} ->
      location


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
      | Try_clause _ ) ->
      (* these errors either abort the whole program or, if they are false positives, mean that
         pulse is confused and the current abstract state has stopped making sense; either way,
         abort! *)
      true
  | ReadUninitializedValue _
  | StackVariableAddressEscape _
  | UnnecessaryCopy _
  | RetainCycle _
  | MemoryLeak _
  | ResourceLeak _ ->
      false


(* whether the [calling_context + trace] starts with a call or contains only an immediate event *)
let immediate_or_first_call calling_context (trace : Trace.t) =
  match (calling_context, trace) with
  | [], Immediate _ ->
      `Immediate
  | (f, _) :: _, _ | [], ViaCall {f; _} ->
      `Call f


let pulse_start_msg = "Pulse found a potential"

let pp_context_message pp_if_no_context issue_kind_str calling_context fmt =
  match calling_context with
  | [] ->
      pp_if_no_context fmt
  | (call_event, _) :: _ ->
      F.fprintf fmt "The call to %a may indirectly trigger %s" CallEvent.pp call_event
        issue_kind_str


let get_message diagnostic =
  match diagnostic with
  | AccessToInvalidAddress
      {calling_context; invalidation; invalidation_trace; access_trace; must_be_valid_reason} -> (
    match invalidation with
    (* Special error message for nullptr dereference *)
    | ConstantDereference i when IntLit.equal i IntLit.zero ->
        let issue_kind_str, null_str =
          match must_be_valid_reason with
          | Some (SelfOfNonPODReturnMethod non_pod_typ) ->
              ( F.asprintf
                  "undefined behaviour caused by nil messaging of a C++ method with a non-POD \
                   return type `%a`"
                  (Typ.pp_full Pp.text) non_pod_typ
              , "nil receiver" )
          | Some InsertionIntoCollectionKey ->
              ("nil key insertion into collection", "nil key")
          | Some InsertionIntoCollectionValue ->
              ("nil object insertion into collection", "nil value")
          | Some BlockCall ->
              ("nil block call", "nil block")
          | None ->
              ("null pointer dereference", "null")
        in
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
              F.fprintf fmt "%s (%s assigned on line %d)" issue_kind_str null_str line
          | `Call f ->
              F.fprintf fmt "%s (%s value coming from the call to %a on line %d)" issue_kind_str
                null_str CallEvent.describe f line
        in
        let invalidation_line =
          let {Location.line; _} = Trace.get_outer_location invalidation_trace in
          line
        in
        F.asprintf "%t"
          (pp_context_message
             (fun fmt ->
               F.fprintf fmt "%s %a%a." pulse_start_msg
                 (pp_invalidation_trace invalidation_line)
                 invalidation_trace pp_access_trace access_trace )
             ("a " ^ issue_kind_str) calling_context )
    | _ ->
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
              F.fprintf fmt "%a during the call to %a%a" Invalidation.describe invalidation
                CallEvent.describe f pp_line line
        in
        let invalidation_line =
          let {Location.line; _} = Trace.get_outer_location invalidation_trace in
          line
        in
        F.asprintf "%t"
          (pp_context_message
             (fun fmt ->
               F.fprintf fmt "%a%a" pp_access_trace access_trace
                 (pp_invalidation_trace invalidation_line invalidation)
                 invalidation_trace )
             "an invalid access" calling_context ) )
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
      F.asprintf
        "%s memory leak. Memory dynamically allocated %a is not freed after the last access at %a"
        pulse_start_msg pp_allocation_trace allocation_trace Location.pp location
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
      F.asprintf
        "%s resource leak. Resource dynamically allocated %a is not closed after the last access \
         at %a"
        pulse_start_msg pp_allocation_trace allocation_trace Location.pp location
  | RetainCycle {location} ->
      F.asprintf
        "%s retain cycle. Memory managed via reference counting is locked in a retain cycle at %a"
        pulse_start_msg Location.pp location
  | ErlangError (Badkey {calling_context= _; location}) ->
      F.asprintf "%s bad key at %a" pulse_start_msg Location.pp location
  | ErlangError (Badmap {calling_context= _; location}) ->
      F.asprintf "%s bad map at %a" pulse_start_msg Location.pp location
  | ErlangError (Badmatch {calling_context= _; location}) ->
      F.asprintf "%s no match of RHS at %a" pulse_start_msg Location.pp location
  | ErlangError (Badrecord {calling_context= _; location}) ->
      F.asprintf "%s bad record at %a" pulse_start_msg Location.pp location
  | ErlangError (Case_clause {calling_context= _; location}) ->
      F.asprintf "%s no matching case clause at %a" pulse_start_msg Location.pp location
  | ErlangError (Function_clause {calling_context= _; location}) ->
      F.asprintf "%s no matching function clause at %a" pulse_start_msg Location.pp location
  | ErlangError (If_clause {calling_context= _; location}) ->
      F.asprintf "%s no true branch in if expression at %a" pulse_start_msg Location.pp location
  | ErlangError (Try_clause {calling_context= _; location}) ->
      F.asprintf "%s no matching branch in try at %a" pulse_start_msg Location.pp location
  | ReadUninitializedValue {calling_context; trace} ->
      let root_var =
        PulseTrace.find_map trace ~f:(function
          | VariableDeclared (pvar, _, _) ->
              Some pvar
          | _ ->
              None )
        |> IOption.if_none_evalopt ~f:(fun () ->
               PulseTrace.find_map trace ~f:(function
                 | FormalDeclared (pvar, _, _) ->
                     Some pvar
                 | _ ->
                     None ) )
        |> Option.map ~f:(F.asprintf "%a" Pvar.pp_value_non_verbose)
      in
      let declared_fields =
        PulseTrace.find_map trace ~f:(function
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
        let {Location.line} = Trace.get_outer_location trace in
        match immediate_or_first_call calling_context trace with
        | `Immediate ->
            F.fprintf fmt "on line %d" line
        | `Call f ->
            F.fprintf fmt "during the call to %a on line %d" CallEvent.describe f line
      in
      F.asprintf "%s uninitialized value%t being read on %t" pulse_start_msg pp_access_path
        pp_location
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "%s stack variable address escape. Address of %a is returned by the function"
        pulse_start_msg pp_var variable
  | UnnecessaryCopy {variable; location} ->
      F.asprintf
        "%s unnecessary copy: copied variable `%a` is not modified since it is copied in %a. \
         Consider using a reference to it in order to avoid unnecessary copy"
        pulse_start_msg Var.pp variable Location.pp location


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
  | RetainCycle {assignment_trace; location} ->
      Trace.add_to_errlog ~nesting:1
        ~pp_immediate:(fun fmt -> F.fprintf fmt "assigned")
        assignment_trace
      @@ [Errlog.make_trace_element 0 location "retain cycle here" []]
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
  | UnnecessaryCopy {location; _} ->
      let nesting = 0 in
      [Errlog.make_trace_element nesting location "copied here" []]


let get_issue_type ~latent issue_type =
  match (issue_type, latent) with
  | MemoryLeak _, false ->
      IssueType.pulse_memory_leak
  | ResourceLeak _, false ->
      IssueType.pulse_resource_leak
  | RetainCycle _, false ->
      IssueType.retain_cycle
  | StackVariableAddressEscape _, false ->
      IssueType.stack_variable_address_escape
  | UnnecessaryCopy _, false ->
      IssueType.unnecessary_copy_pulse
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
