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
module ConfigName = FbPulseConfigName
module DecompilerExpr = PulseDecompilerExpr
module Invalidation = PulseInvalidation
module TaintItem = PulseTaintItem
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type calling_context = (CallEvent.t * Location.t) list [@@deriving compare, equal]

let pp_calling_context fmt calling_context =
  F.fprintf fmt "[@[<v1>%a@]]"
    (Pp.seq ~sep:";@;" (Pp.pair ~fst:CallEvent.pp ~snd:Location.pp))
    calling_context


type access_to_invalid_address =
  { calling_context: calling_context
  ; invalid_address: DecompilerExpr.t
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
  ; access_trace: Trace.t
  ; must_be_valid_reason: Invalidation.must_be_valid_reason option }
[@@deriving compare, equal]

let yojson_of_access_to_invalid_address = [%yojson_of: _]

let pp_access_to_invalid_address fmt
    ({ calling_context
     ; invalid_address
     ; invalidation
     ; invalidation_trace
     ; access_trace
     ; must_be_valid_reason } [@warning "+missing-record-field-pattern"] ) =
  let pp_immediate fmt = F.pp_print_string fmt "immediate" in
  F.fprintf fmt
    "{@[calling_context=%a;@;\
     invalid_address=%a;@;\
     invalidation=%a;@;\
     invalidation_trace=%a;@;\
     access_trace=%a;@;\
     must_be_valid_reason=%a;@;\
     @]}"
    pp_calling_context calling_context DecompilerExpr.pp_with_abstract_value invalid_address
    Invalidation.pp invalidation (Trace.pp ~pp_immediate) invalidation_trace
    (Trace.pp ~pp_immediate) access_trace Invalidation.pp_must_be_valid_reason must_be_valid_reason


module ErlangError = struct
  type t =
    | Badarg of {calling_context: calling_context; location: Location.t}
    | Badkey of {calling_context: calling_context; location: Location.t}
    | Badmap of {calling_context: calling_context; location: Location.t}
    | Badmatch of {calling_context: calling_context; location: Location.t}
    | Badrecord of {calling_context: calling_context; location: Location.t}
    | Badreturn of {calling_context: calling_context; location: Location.t}
    | Case_clause of {calling_context: calling_context; location: Location.t}
    | Function_clause of {calling_context: calling_context; location: Location.t}
    | If_clause of {calling_context: calling_context; location: Location.t}
    | Try_clause of {calling_context: calling_context; location: Location.t}
  [@@deriving compare, equal, variants]

  let yojson_of_t = [%yojson_of: _]

  let pp fmt erlang_error =
    (* this is for debug purposes so if you add another field please remove this warning but make sure
       to pretty print it too *)
    let[@warning "+missing-record-field-pattern"] ( Badarg {calling_context; location}
                                                  | Badkey {calling_context; location}
                                                  | Badmap {calling_context; location}
                                                  | Badmatch {calling_context; location}
                                                  | Badrecord {calling_context; location}
                                                  | Badreturn {calling_context; location}
                                                  | Case_clause {calling_context; location}
                                                  | Function_clause {calling_context; location}
                                                  | If_clause {calling_context; location}
                                                  | Try_clause {calling_context; location} ) =
      erlang_error
    in
    F.fprintf fmt "%s{@[location=%a; calling_context=%a@]}" (Variants.to_name erlang_error)
      Location.pp location pp_calling_context calling_context
end

module ReadUninitialized = struct
  type t = {typ: Attribute.UninitializedTyp.t; calling_context: calling_context; trace: Trace.t}
  [@@deriving compare, equal]

  let yojson_of_t = [%yojson_of: _]

  let pp fmt {typ; calling_context; trace} =
    F.fprintf fmt "{@[typ=%a;@;calling_context=%a;@;trace=%a@]}" Attribute.UninitializedTyp.pp typ
      pp_calling_context calling_context
      (Trace.pp ~pp_immediate:(fun fmt -> F.pp_print_string fmt "immediate"))
      trace
end

type flow_kind = TaintedFlow | FlowToSink | FlowFromSource [@@deriving equal]

let pp_flow_kind fmt flow_kind =
  match flow_kind with
  | TaintedFlow ->
      F.fprintf fmt "tainted flow"
  | FlowToSink ->
      F.fprintf fmt "flow to a taint sink"
  | FlowFromSource ->
      F.fprintf fmt "flow from a taint source"


type t =
  | AccessToInvalidAddress of access_to_invalid_address
  | ConfigUsage of
      { pname: Procname.t
      ; config: ConfigName.t
      ; branch_location: Location.t
      ; location: Location.t
      ; trace: Trace.t }
  | ConstRefableParameter of {param: Var.t; typ: Typ.t; location: Location.t}
  | CSharpResourceLeak of
      {class_name: CSharpClassName.t; allocation_trace: Trace.t; location: Location.t}
  | ErlangError of ErlangError.t
  | JavaResourceLeak of
      {class_name: JavaClassName.t; allocation_trace: Trace.t; location: Location.t}
    (* TODO: add more data to HackUnawaitedAwaitable tracking the parameter type *)
  | HackUnawaitedAwaitable of {allocation_trace: Trace.t; location: Location.t}
  | MemoryLeak of {allocator: Attribute.allocator; allocation_trace: Trace.t; location: Location.t}
  | ReadonlySharedPtrParameter of
      {param: Var.t; typ: Typ.t; location: Location.t; used_locations: Location.t list}
  | ReadUninitialized of ReadUninitialized.t
  | RetainCycle of
      { assignment_traces: Trace.t list
      ; value: DecompilerExpr.t
      ; path: DecompilerExpr.t
      ; location: Location.t }
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | TaintFlow of
      { expr: DecompilerExpr.t
      ; source: TaintItem.t * ValueHistory.t
      ; sink: TaintItem.t * Trace.t
      ; location: Location.t
      ; flow_kind: flow_kind
      ; policy_description: string
      ; policy_id: int
      ; policy_privacy_effect: string option }
  | UnnecessaryCopy of
      { copied_into: PulseAttribute.CopiedInto.t
      ; source_typ: Typ.t option
      ; source_opt: DecompilerExpr.source_expr option
      ; location: Location.t
      ; copied_location: (Procname.t * Location.t) option
      ; location_instantiated: Location.t option
      ; from: PulseAttribute.CopyOrigin.t }
[@@deriving equal]

let pp fmt diagnostic =
  let pp_immediate fmt = F.pp_print_string fmt "immediate" in
  match[@warning "+missing-record-field-pattern"] diagnostic with
  | AccessToInvalidAddress access_to_invalid_address ->
      F.fprintf fmt "AccessToInvalidAddress %a" pp_access_to_invalid_address
        access_to_invalid_address
  | ConfigUsage {pname; config; branch_location; location; trace} ->
      F.fprintf fmt
        "ConfigUsage {@[pname=%a;@;config=%a;@;branch_location=%a;@;location=%a;@;trace=%a@]}"
        Procname.pp pname ConfigName.pp config Location.pp branch_location Location.pp location
        (Trace.pp ~pp_immediate) trace
  | ConstRefableParameter {param; typ; location} ->
      F.fprintf fmt "ConstRefableParameter {@[param=%a;@;typ=%a;@;location=%a@]}" Var.pp param
        (Typ.pp_full Pp.text) typ Location.pp location
  | CSharpResourceLeak {class_name; allocation_trace; location} ->
      F.fprintf fmt "ResourceLeak {@[class_name=%a;@;allocation_trace:%a;@;location:%a@]}"
        CSharpClassName.pp class_name (Trace.pp ~pp_immediate) allocation_trace Location.pp location
  | ErlangError erlang_error ->
      ErlangError.pp fmt erlang_error
  | JavaResourceLeak {class_name; allocation_trace; location} ->
      F.fprintf fmt "ResourceLeak {@[class_name=%a;@;allocation_trace:%a;@;location:%a@]}"
        JavaClassName.pp class_name (Trace.pp ~pp_immediate) allocation_trace Location.pp location
  | HackUnawaitedAwaitable {allocation_trace; location} ->
      F.fprintf fmt "UnawaitedAwaitable {@[allocation_trace:%a;@;location:%a@]}"
        (Trace.pp ~pp_immediate) allocation_trace Location.pp location
  | MemoryLeak {allocator; allocation_trace; location} ->
      F.fprintf fmt "MemoryLeak {@[allocator=%a;@;allocation_trace=%a;@;location=%a@]}"
        Attribute.pp_allocator allocator (Trace.pp ~pp_immediate) allocation_trace Location.pp
        location
  | ReadonlySharedPtrParameter {param; typ; location; used_locations} ->
      F.fprintf fmt
        "ReadonlySharedPtrParameter {@[param=%a;@;typ=%a;@;location=%a;@;used_locations=%a@]}"
        Var.pp param (Typ.pp_full Pp.text) typ Location.pp location
        (IList.pp_print_list ~max:10 ~pp_sep:(fun f () -> F.pp_print_string f ",") Location.pp)
        used_locations
  | ReadUninitialized read_uninitialized ->
      F.fprintf fmt "ReadUninitialized %a" ReadUninitialized.pp read_uninitialized
  | RetainCycle {assignment_traces; value; path; location} ->
      F.fprintf fmt
        "RetainCycle {@[assignment_traces=[@[<v>%a@]];@;value=%a;@;path=%a;@;location=%a@]}"
        (Pp.seq ~sep:";@;" (Trace.pp ~pp_immediate))
        assignment_traces DecompilerExpr.pp_with_abstract_value value DecompilerExpr.pp path
        Location.pp location
  | StackVariableAddressEscape {variable; history; location} ->
      F.fprintf fmt "StackVariableAddressEscape {@[variable=%a;@;history=%a;@;location:%a@]}" Var.pp
        variable ValueHistory.pp history Location.pp location
  | TaintFlow {expr; source; sink; location; flow_kind; _} ->
      F.fprintf fmt "TaintFlow {@[expr=%a;@;source=%a;@;sink=%a;@;location:%a;@;flow_kind=%a@]}"
        DecompilerExpr.pp_with_abstract_value expr
        (Pp.pair ~fst:TaintItem.pp ~snd:ValueHistory.pp)
        source
        (Pp.pair ~fst:TaintItem.pp ~snd:(Trace.pp ~pp_immediate))
        sink Location.pp location pp_flow_kind flow_kind
  | UnnecessaryCopy
      {copied_into; source_typ; source_opt; location; copied_location; from; location_instantiated}
    ->
      F.fprintf fmt
        "UnnecessaryCopy {@[copied_into=%a;@;\
         typ=%a;@;\
         source_opt=%a;@;\
         location:%a;@;\
         copied_location:%a@;\
         from=%a;loc_instantiated=%a@]}"
        PulseAttribute.CopiedInto.pp copied_into
        (Pp.option (Typ.pp_full Pp.text))
        source_typ
        (Pp.option DecompilerExpr.pp_source_expr)
        source_opt Location.pp location
        (fun fmt -> function
          | None ->
              F.pp_print_string fmt "none"
          | Some (callee, location) ->
              F.fprintf fmt "%a,%a" Procname.pp callee Location.pp location )
        copied_location PulseAttribute.CopyOrigin.pp from (Pp.option Location.pp)
        location_instantiated


let get_location = function
  | AccessToInvalidAddress {calling_context= []; access_trace}
  | ReadUninitialized {calling_context= []; trace= access_trace} ->
      Trace.get_outer_location access_trace
  | ErlangError (Badarg {location; calling_context= []})
  | ErlangError (Badkey {location; calling_context= []})
  | ErlangError (Badmap {location; calling_context= []})
  | ErlangError (Badmatch {location; calling_context= []})
  | ErlangError (Badrecord {location; calling_context= []})
  | ErlangError (Badreturn {location; calling_context= []})
  | ErlangError (Case_clause {location; calling_context= []})
  | ErlangError (Function_clause {location; calling_context= []})
  | ErlangError (If_clause {location; calling_context= []})
  | ErlangError (Try_clause {location; calling_context= []}) ->
      location
  | AccessToInvalidAddress {calling_context= (_, location) :: _}
  | ErlangError (Badarg {calling_context= (_, location) :: _})
  | ErlangError (Badkey {calling_context= (_, location) :: _})
  | ErlangError (Badmap {calling_context= (_, location) :: _})
  | ErlangError (Badmatch {calling_context= (_, location) :: _})
  | ErlangError (Badrecord {calling_context= (_, location) :: _})
  | ErlangError (Badreturn {calling_context= (_, location) :: _})
  | ErlangError (Case_clause {calling_context= (_, location) :: _})
  | ErlangError (Function_clause {calling_context= (_, location) :: _})
  | ErlangError (If_clause {calling_context= (_, location) :: _})
  | ErlangError (Try_clause {calling_context= (_, location) :: _})
  | ReadUninitialized {calling_context= (_, location) :: _} ->
      (* report at the call site that triggers the bug *) location
  | ConfigUsage {location}
  | ConstRefableParameter {location}
  | CSharpResourceLeak {location}
  | JavaResourceLeak {location}
  | HackUnawaitedAwaitable {location}
  | MemoryLeak {location}
  | ReadonlySharedPtrParameter {location}
  | RetainCycle {location}
  | StackVariableAddressEscape {location}
  | TaintFlow {location}
  | UnnecessaryCopy {location} ->
      location


let get_location_instantiated = function
  | UnnecessaryCopy {location_instantiated} ->
      location_instantiated
  | _ ->
      None


let get_copy_type = function
  | UnnecessaryCopy {source_typ} ->
      source_typ
  | ConstRefableParameter {typ} ->
      Some typ
  | _ ->
      None


let aborts_execution = function
  | AccessToInvalidAddress _
  | ErlangError
      ( Badarg _
      | Badkey _
      | Badmap _
      | Badmatch _
      | Badrecord _
      | Badreturn _
      | Case_clause _
      | Function_clause _
      | If_clause _
      | Try_clause _ )
  | ReadUninitialized _ ->
      (* these errors either abort the whole program or, if they are false positives, mean that
         pulse is confused and the current abstract state has stopped making sense; either way,
         abort! *)
      true
  | ConfigUsage _
  | ConstRefableParameter _
  | CSharpResourceLeak _
  | JavaResourceLeak _
  | HackUnawaitedAwaitable _
  | MemoryLeak _
  | ReadonlySharedPtrParameter _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
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


let is_from_std_move (base, _) =
  match (base : DecompilerExpr.base) with
  | ReturnValue (SkippedKnownCall pname) | ReturnValue (Call pname) ->
      String.is_prefix (Procname.to_string pname) ~prefix:"std::move"
  | ReturnValue (Model pname) ->
      String.is_prefix pname ~prefix:"std::move"
  | ReturnValue (SkippedUnknownCall _) | PVar _ ->
      false


let no_suggestion msg = (msg, None)

let is_from_const = Option.exists ~f:Typ.is_pointer_to_const

let get_message_and_suggestion diagnostic =
  match diagnostic with
  | AccessToInvalidAddress
      { calling_context
      ; invalid_address
      ; invalidation
      ; invalidation_trace
      ; access_trace
      ; must_be_valid_reason } ->
      ((* [invalidation_trace] comes from the [Invalid] attribute and may not be about the exact
          thing we are looking at but about some other value that happens to be equal to it (as we'll
          keep only one such attribute per value). [access_trace] has the most accurate info and if
          all goes well it should itself contain a sub-trace leading to how the value became invalid
          prior to being accessed (see also [PulseReport]. *)
       let invalidation_trace =
         Trace.get_trace_until access_trace ~f:(function Invalidated _ -> true | _ -> false)
         |> Option.value ~default:invalidation_trace
       in
       match invalidation with
       | ConstantDereference i when IntLit.equal i IntLit.zero ->
           let pp_access_trace fmt (trace : Trace.t) =
             match immediate_or_first_call calling_context trace with
             | `Immediate ->
                 ()
             | `Call f ->
                 F.fprintf fmt " in the call to %a" CallEvent.describe f
           in
           let pp_invalidation_trace line fmt (trace : Trace.t) =
             match immediate_or_first_call calling_context trace with
             | `Immediate ->
                 F.fprintf fmt "(null value originating from line %d)" line
             | `Call f ->
                 F.fprintf fmt "(from the call to %a on line %d)" CallEvent.describe f line
           in
           let invalidation_line =
             let {Location.line; _} = Trace.get_outer_location invalidation_trace in
             line
           in
           let pp_must_be_valid_reason fmt expr =
             let pp_prefix fmt null_nil_block =
               if DecompilerExpr.is_unknown expr then
                 F.fprintf fmt "%s %a" null_nil_block
                   (pp_invalidation_trace invalidation_line)
                   invalidation_trace
               else
                 F.fprintf fmt "`%a` could be %s %a and" DecompilerExpr.pp expr null_nil_block
                   (pp_invalidation_trace invalidation_line)
                   invalidation_trace
             in
             match must_be_valid_reason with
             | Some (SelfOfNonPODReturnMethod non_pod_typ) ->
                 F.fprintf fmt
                   "%a is used to call a C++ method with a non-POD return type `%a`%a; nil \
                    messaging such methods is undefined behaviour"
                   pp_prefix "nil" (Typ.pp_full Pp.text) non_pod_typ pp_access_trace access_trace
             | Some (InsertionIntoCollectionKey | InsertionIntoCollectionValue) ->
                 F.fprintf fmt
                   "%a is used as a %s when inserting into a collection%a, potentially causing a \
                    crash"
                   pp_prefix "nil"
                   ( match[@warning "-partial-match"] must_be_valid_reason with
                   | Some InsertionIntoCollectionKey ->
                       "key"
                   | Some InsertionIntoCollectionValue ->
                       "value" )
                   pp_access_trace access_trace
             | Some BlockCall ->
                 F.fprintf fmt "%a is called%a, causing a crash" pp_prefix "nil block"
                   pp_access_trace access_trace
             | Some (NullArgumentWhereNonNullExpected procname) ->
                 F.fprintf fmt
                   "%a is passed as argument to %s; this function requires a non-nil argument"
                   pp_prefix "nil" procname
             | None ->
                 F.fprintf fmt "%a is dereferenced%a" pp_prefix "null" pp_access_trace access_trace
           in
           F.asprintf "%a%a" pp_calling_context_prefix calling_context pp_must_be_valid_reason
             invalid_address
       | _ ->
           let pp_invalid_address fmt =
             match invalid_address with
             | SourceExpr (source_expr, _) ->
                 F.fprintf fmt "`%a`" DecompilerExpr.pp_source_expr source_expr
             | Unknown _ ->
                 F.pp_print_string fmt "memory"
           in
           let pp_access_trace fmt (trace : Trace.t) =
             match immediate_or_first_call calling_context trace with
             | `Immediate ->
                 F.fprintf fmt "accessing %t that " pp_invalid_address
             | `Call f ->
                 F.fprintf fmt "call to %a eventually accesses %t that " CallEvent.describe f
                   pp_invalid_address
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
           F.asprintf "%a%a%a" pp_calling_context_prefix calling_context pp_access_trace
             access_trace
             (pp_invalidation_trace invalidation_line invalidation)
             invalidation_trace )
      |> no_suggestion
  | ConfigUsage {pname; config; branch_location} ->
      F.asprintf "Function %a used config %a at %a." Procname.pp pname ConfigName.pp config
        Location.pp branch_location
      |> no_suggestion
  | ConstRefableParameter {param; location} ->
      ( F.asprintf
          "Function parameter `%a` is passed by-value but not modified inside the function on %a, \
           potentially resulting in an unnecessary copy at the function's callsite."
          Var.pp param Location.pp_line location
      , Some "Change the type of the parameter to `const &`." )
  | CSharpResourceLeak {class_name; location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let allocation_line =
        let {Location.line; _} = Trace.get_outer_location allocation_trace in
        line
      in
      let pp_allocation_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt "by constructor %a() on line %d" CSharpClassName.pp class_name
              allocation_line
        | ViaCall {f; _} ->
            F.fprintf fmt "by constructor %a(), indirectly via call to %a on line %d"
              CSharpClassName.pp class_name CallEvent.describe f allocation_line
      in
      F.asprintf "Resource dynamically allocated %a is not closed after the last access at %a"
        pp_allocation_trace allocation_trace Location.pp location
      |> no_suggestion
  | ErlangError (Badarg {calling_context= _; location}) ->
      F.asprintf "bad arg at %a" Location.pp location |> no_suggestion
  | ErlangError (Badkey {calling_context= _; location}) ->
      F.asprintf "bad key at %a" Location.pp location |> no_suggestion
  | ErlangError (Badmap {calling_context= _; location}) ->
      F.asprintf "bad map at %a" Location.pp location |> no_suggestion
  | ErlangError (Badmatch {calling_context= _; location}) ->
      F.asprintf "no match of RHS at %a" Location.pp location |> no_suggestion
  | ErlangError (Badrecord {calling_context= _; location}) ->
      F.asprintf "bad record at %a" Location.pp location |> no_suggestion
  | ErlangError (Badreturn {calling_context= _; location}) ->
      F.asprintf "dynamic type of returned value disagrees with spec at %a" Location.pp location
      |> no_suggestion
  | ErlangError (Case_clause {calling_context= _; location}) ->
      F.asprintf "no matching case clause at %a" Location.pp location |> no_suggestion
  | ErlangError (Function_clause {calling_context= _; location}) ->
      F.asprintf "no matching function clause at %a" Location.pp location |> no_suggestion
  | ErlangError (If_clause {calling_context= _; location}) ->
      F.asprintf "no true branch in if expression at %a" Location.pp location |> no_suggestion
  | ErlangError (Try_clause {calling_context= _; location}) ->
      F.asprintf "no matching branch in try at %a" Location.pp location |> no_suggestion
  | JavaResourceLeak {class_name; location; allocation_trace} ->
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
      |> no_suggestion
  | HackUnawaitedAwaitable {location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let allocation_line =
        let {Location.line; _} = Trace.get_outer_location allocation_trace in
        line
      in
      let pp_allocation_trace fmt (trace : Trace.t) =
        match trace with
        | Immediate _ ->
            F.fprintf fmt " on line %d" allocation_line
        | ViaCall {f; _} ->
            F.fprintf fmt "indirectly via call to %a on line %d" CallEvent.describe f
              allocation_line
      in
      F.asprintf "Awaitable dynamically allocated %a is not awaited after the last access at %a"
        pp_allocation_trace allocation_trace Location.pp location
      |> no_suggestion
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
      |> no_suggestion
  | ReadonlySharedPtrParameter {param; location; used_locations} ->
      let pp_used_locations f =
        match used_locations with
        | [] ->
            ()
        | _ :: _ ->
            F.fprintf f " at %a"
              (IList.pp_print_list ~max:3
                 ~pp_sep:(fun f () -> F.pp_print_string f ", ")
                 Location.pp_line )
              used_locations
      in
      ( F.asprintf
          "Function parameter `%a` is passed as a shared pointer but its lifetime is not extended \
           inside the function on %a. At the callsite, this might cause a potentially expensive \
           unnecessary copy of the shared pointer, especially when many number of threads are \
           sharing it."
          Var.pp param Location.pp_line location
      , Some
          (F.asprintf "Pass the raw pointer instead and change its usages if necessary%t."
             pp_used_locations ) )
  | ReadUninitialized {calling_context; trace} ->
      let root_var =
        Trace.find_map_last_main trace ~f:(function
          | VariableDeclared (pvar, _, _) ->
              Some pvar
          | _ ->
              None )
        |> IOption.if_none_evalopt ~f:(fun () ->
               Trace.find_map_last_main trace ~f:(function
                 | FormalDeclared (pvar, _, _) ->
                     Some pvar
                 | _ ->
                     None ) )
        |> Option.map ~f:(F.asprintf "%a" Pvar.pp_value_non_verbose)
      in
      let declared_fields =
        Trace.find_map_last_main trace ~f:(function
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
      F.asprintf "%t is read without initialization%t" pp_access_path pp_location |> no_suggestion
  | RetainCycle {location; value; path} ->
      F.asprintf
        "Memory managed via reference counting is locked in a retain cycle at %a: `%a` retains \
         itself via `%a`"
        Location.pp location DecompilerExpr.pp value DecompilerExpr.pp path
      |> no_suggestion
  | StackVariableAddressEscape {variable; _} ->
      let pp_var f var =
        if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
        else F.fprintf f "stack variable `%a`" Var.pp var
      in
      F.asprintf "Address of %a is returned by the function" pp_var variable |> no_suggestion
  | TaintFlow {expr; source= source, _; sink= sink, _; policy_description} ->
      (* TODO: say what line the source happened in the current function *)
      ( if TaintItem.equal source sink then
          F.asprintf "%s. Value is tainted by %a" policy_description TaintItem.pp source
        else
          let flows_to = if TaintItem.is_set_field_origin sink then "" else " flows to" in
          if DecompilerExpr.is_unknown expr then
            F.asprintf "%s. Value is tainted by %a and%s %a" policy_description TaintItem.pp source
              flows_to TaintItem.pp sink
          else
            F.asprintf "%s. `%a` is tainted by %a and%s %a" policy_description DecompilerExpr.pp
              expr TaintItem.pp source flows_to TaintItem.pp sink )
      |> no_suggestion
  | UnnecessaryCopy {copied_into; copied_location= Some (callee, {file; line})} ->
      let open PulseAttribute in
      ( F.asprintf
          "the return value `%a` is not modified after it is copied in the callee `%a` at `%a:%d`. "
          CopiedInto.pp copied_into Procname.pp callee SourceFile.pp file line
      , Some
          (F.asprintf "Either change the return type of `%a` or revise the function body."
             Procname.pp callee ) )
  | UnnecessaryCopy {copied_into; source_typ; source_opt; location; copied_location= None; from}
    -> (
      let open PulseAttribute in
      let is_from_const = is_from_const source_typ in
      let get_suggestion_msg_move copied_into source_opt =
        let call_move = "call `std::move` instead" in
        let is_move_called = Option.exists ~f:is_from_std_move source_opt in
        let move_suggestion = "To avoid the copy, " ^ call_move in
        if is_move_called then
          let reason =
            if is_from_const then
              "it is being called on a `const` which resolves to the copy operation"
            else "the type doesn't have a move operation"
          in
          F.asprintf
            "Even though `std::move` is called, nothing is actually getting moved (e.g. %s) so \
             make sure the copy is expected"
            reason
        else
          match (copied_into : PulseAttribute.CopiedInto.t) with
          | IntoIntermediate _ ->
              let const_ref_callee = "change the callee's parameter type to `const &`" in
              if is_from_const then
                "To avoid the copy, either 1) remove the `const` from the source and " ^ call_move
                ^ " or 2) " ^ const_ref_callee
              else move_suggestion ^ " or " ^ const_ref_callee
          | IntoField _ ->
              if is_from_const then
                "Rather than copying into the field, first remove `const` qualifier from the \
                 source and then move into it instead"
              else "Rather than copying into the field, move into it instead"
          | IntoVar _ ->
              move_suggestion
      in
      let suppression_msg =
        "If this copy was intentional, call `folly::copy` to make it explicit and hence suppress \
         the warning"
      in
      let get_suggestion_msg source_opt =
        match (from, copied_into) with
        | CopyInGetDefault, _ ->
            F.asprintf
              "To avoid the copy, use either `folly::get_ref_default` or `folly::get_ptr` instead%t"
              (fun f ->
                Option.iter FbInternalLinks.bad_pattern_folly_get_default ~f:(fun link ->
                    F.fprintf f " ([[%s | bad patterns]])" link ) )
        | CopyCtor, IntoVar _ ->
            "To avoid the copy, use reference `&`"
        | _, _ ->
            get_suggestion_msg_move copied_into source_opt
      in
      match (copied_into, source_opt) with
      | IntoIntermediate _, None ->
          ( F.asprintf "An intermediate is %a on %a." CopyOrigin.pp from Location.pp_line location
          , Some (get_suggestion_msg source_opt) )
      | IntoIntermediate _, Some ((PVar _, _) as source_expr) ->
          ( F.asprintf "variable `%a` is %a unnecessarily into an intermediate on %a."
              DecompilerExpr.pp_source_expr source_expr CopyOrigin.pp from Location.pp_line location
          , Some (get_suggestion_msg source_opt) )
      | IntoIntermediate _, Some ((ReturnValue _, _) as source_expr) ->
          ( F.asprintf "The return value from %a is %a unnecessarily  on %a."
              DecompilerExpr.pp_source_expr source_expr CopyOrigin.pp from Location.pp_line location
          , Some (get_suggestion_msg source_opt) )
      | IntoVar _, None ->
          ( F.asprintf "%a variable `%a` is not modified after it is copied from a source on %a."
              CopyOrigin.pp from CopiedInto.pp copied_into Location.pp_line location
          , Some (F.asprintf "%s. %s." (get_suggestion_msg source_opt) suppression_msg) )
      | IntoVar _, Some source_expr ->
          ( F.asprintf "%a variable `%a` is not modified after it is copied from `%a` on %a."
              CopyOrigin.pp from CopiedInto.pp copied_into DecompilerExpr.pp_source_expr source_expr
              Location.pp_line location
          , Some (F.asprintf "%s. %s." (get_suggestion_msg source_opt) suppression_msg) )
      | IntoField {field}, None ->
          ( F.asprintf "Field `%a` is %a into from an rvalue-ref but is not modified afterwards."
              Fieldname.pp field CopyOrigin.pp from
          , Some (get_suggestion_msg source_opt) )
      | IntoField {field}, Some source_expr ->
          ( F.asprintf "`%a` is %a into field `%a` but is not modified afterwards."
              DecompilerExpr.pp_source_expr source_expr CopyOrigin.pp from Fieldname.pp field
          , Some (get_suggestion_msg source_opt) ) )


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
  | CppDelete
  | CppDeleteArray
  | EndIterator
  | GoneOutOfScope _
  | OptionalEmpty
  | StdVector _
  | CppMap _ ->
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


let get_param_typ param typ =
  F.asprintf "Parameter %a with type `%a`" Var.pp param (Typ.pp_full Pp.text) typ


let pp_copy_typ fmt =
  Option.iter ~f:(fun typ -> F.fprintf fmt " (with type `%a`)" (Typ.pp_full Pp.text) typ)


let get_trace = function
  | AccessToInvalidAddress {calling_context; invalidation; invalidation_trace; access_trace} ->
      let in_context_nesting = List.length calling_context in
      let should_print_invalidation_trace =
        not (Trace.exists_main access_trace ~f:(function Invalidated _ -> true | _ -> false))
      in
      get_trace_calling_context calling_context
      @@ ( if should_print_invalidation_trace then
             add_invalidation_trace ~nesting:in_context_nesting invalidation invalidation_trace
           else Fn.id )
      @@ add_access_trace
           ~include_title:(should_print_invalidation_trace || not (List.is_empty calling_context))
           ~nesting:in_context_nesting invalidation access_trace
      @@ []
  | ConfigUsage {config; trace} ->
      Trace.add_to_errlog ~nesting:0
        ~pp_immediate:(fun fmt ->
          F.fprintf fmt "config %a is used as branch condition here" ConfigName.pp config )
        trace
      @@ []
  | ConstRefableParameter {param; typ; location} ->
      let nesting = 0 in
      [Errlog.make_trace_element nesting location (get_param_typ param typ) []]
  | CSharpResourceLeak {class_name; location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt ->
             F.fprintf fmt "allocated by constructor %a() here" CSharpClassName.pp class_name )
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | ErlangError (Badarg {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "bad arg here" []]
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
  | ErlangError (Badreturn {calling_context; location}) ->
      get_trace_calling_context calling_context
      @@ [Errlog.make_trace_element 0 location "bad return here" []]
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
  | JavaResourceLeak {class_name; location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt ->
             F.fprintf fmt "allocated by constructor %a() here" JavaClassName.pp class_name )
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | HackUnawaitedAwaitable {location; allocation_trace} ->
      (* NOTE: this is very similar to the MemoryLeak case *)
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt -> F.fprintf fmt "allocated here")
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "awaitable becomes unreachable here" []]
  | MemoryLeak {allocator; location; allocation_trace} ->
      let access_start_location = Trace.get_start_location allocation_trace in
      add_errlog_header ~nesting:0 ~title:"allocation part of the trace starts here"
        access_start_location
      @@ Trace.add_to_errlog ~nesting:1
           ~pp_immediate:(fun fmt ->
             F.fprintf fmt "allocated by `%a` here" Attribute.pp_allocator allocator )
           allocation_trace
      @@ [Errlog.make_trace_element 0 location "memory becomes unreachable here" []]
  | ReadonlySharedPtrParameter {param; typ; location; used_locations} ->
      let nesting = 0 in
      Errlog.make_trace_element nesting location (get_param_typ param typ) []
      :: List.map used_locations ~f:(fun used_location ->
             Errlog.make_trace_element nesting used_location "used" [] )
  | ReadUninitialized {calling_context; trace} ->
      get_trace_calling_context calling_context
      @@ Trace.add_to_errlog ~nesting:0
           ~pp_immediate:(fun fmt -> F.pp_print_string fmt "read to uninitialized value occurs here")
           trace
      @@ []
  | RetainCycle {assignment_traces; location} ->
      let errlog = [Errlog.make_trace_element 0 location "retain cycle here" []] in
      Trace.synchronous_add_to_errlog ~nesting:1
        ~pp_immediate:(fun fmt -> F.fprintf fmt "assigned")
        assignment_traces errlog
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
      ValueHistory.add_to_errlog ~include_taint_events:true ~nesting:0 source_history
      @@ Trace.add_to_errlog ~include_value_history:false ~include_taint_events:true ~nesting:0
           ~pp_immediate:(fun fmt -> TaintItem.pp fmt sink)
           sink_trace
      @@ []
  | UnnecessaryCopy {location; source_typ; copied_location= None; from} ->
      let nesting = 0 in
      [ Errlog.make_trace_element nesting location
          (F.asprintf "%a here%a" PulseAttribute.CopyOrigin.pp from pp_copy_typ source_typ)
          [] ]
  | UnnecessaryCopy {location; source_typ; copied_location= Some (_, copied_location); from} ->
      let nesting = 0 in
      [ Errlog.make_trace_element nesting location (F.asprintf "returned here") []
      ; Errlog.make_trace_element nesting copied_location
          (F.asprintf "%a here%a" PulseAttribute.CopyOrigin.pp from pp_copy_typ source_typ)
          [] ]


let get_issue_type ~latent issue_type =
  match (issue_type, latent) with
  | AccessToInvalidAddress {invalidation; must_be_valid_reason}, _ ->
      Invalidation.issue_type_of_cause ~latent invalidation must_be_valid_reason
  | ConfigUsage _, false ->
      IssueType.pulse_config_usage
  | ConstRefableParameter _, false ->
      IssueType.pulse_const_refable
  | CSharpResourceLeak _, false | JavaResourceLeak _, false | HackUnawaitedAwaitable _, false ->
      IssueType.pulse_resource_leak
  | ErlangError (Badarg _), _ ->
      IssueType.bad_arg ~latent
  | ErlangError (Badkey _), _ ->
      IssueType.bad_key ~latent
  | ErlangError (Badmap _), _ ->
      IssueType.bad_map ~latent
  | ErlangError (Badmatch _), _ ->
      IssueType.no_match_of_rhs ~latent
  | ErlangError (Badrecord _), _ ->
      IssueType.bad_record ~latent
  | ErlangError (Badreturn _), _ ->
      IssueType.bad_return ~latent
  | ErlangError (Case_clause _), _ ->
      IssueType.no_matching_case_clause ~latent
  | ErlangError (Function_clause _), _ ->
      IssueType.no_matching_function_clause ~latent
  | ErlangError (If_clause _), _ ->
      IssueType.no_true_branch_in_if ~latent
  | ErlangError (Try_clause _), _ ->
      IssueType.no_matching_branch_in_try ~latent
  | MemoryLeak {allocator}, false -> (
    match allocator with
    | CMalloc | CustomMalloc _ | CRealloc | CustomRealloc _ ->
        IssueType.pulse_memory_leak_c
    | CppNew | CppNewArray ->
        IssueType.pulse_memory_leak_cpp
    | JavaResource _ | CSharpResource _ | ObjCAlloc | HackAsync ->
        L.die InternalError
          "Memory leaks should not have a Java resource, Hack async, C sharp, or Objective-C alloc \
           as allocator" )
  | ReadonlySharedPtrParameter _, false ->
      IssueType.readonly_shared_ptr_param
  | ReadUninitialized {typ= Value}, _ ->
      IssueType.uninitialized_value_pulse ~latent
  | ReadUninitialized {typ= Const}, _ ->
      IssueType.pulse_uninitialized_const
  | RetainCycle _, false ->
      IssueType.retain_cycle
  | StackVariableAddressEscape _, false ->
      IssueType.stack_variable_address_escape
  | TaintFlow {flow_kind= TaintedFlow}, _ ->
      IssueType.taint_error
  | TaintFlow {flow_kind= FlowToSink}, _ ->
      IssueType.data_flow_to_sink
  | TaintFlow {flow_kind= FlowFromSource}, _ ->
      IssueType.sensitive_data_flow
  | UnnecessaryCopy {copied_location= Some _}, false ->
      IssueType.unnecessary_copy_return_pulse
  | UnnecessaryCopy {copied_into= IntoField _; source_typ; from= CopyAssignment}, false
    when Option.exists ~f:Typ.is_rvalue_reference source_typ ->
      IssueType.unnecessary_copy_assignment_movable_pulse
  | UnnecessaryCopy {copied_into= IntoField _; source_typ; from= CopyCtor}, false
    when Option.exists ~f:Typ.is_rvalue_reference source_typ ->
      IssueType.unnecessary_copy_movable_pulse
  | ( UnnecessaryCopy {copied_into= IntoField _ | IntoIntermediate _; source_typ; from= CopyCtor}
    , false )
    when is_from_const source_typ ->
      IssueType.unnecessary_copy_intermediate_const_pulse
  | ( UnnecessaryCopy
        {copied_into= IntoField _ | IntoIntermediate _; from= CopyCtor | CopyInGetDefault}
    , false ) ->
      IssueType.unnecessary_copy_intermediate_pulse
  | UnnecessaryCopy {copied_into= IntoVar _; from= CopyCtor | CopyInGetDefault}, false ->
      IssueType.unnecessary_copy_pulse
  | UnnecessaryCopy {from= CopyAssignment; source_typ}, false ->
      if is_from_const source_typ then IssueType.unnecessary_copy_assignment_const_pulse
      else IssueType.unnecessary_copy_assignment_pulse
  | UnnecessaryCopy {source_typ; from= CopyToOptional}, false ->
      if is_from_const source_typ then IssueType.unnecessary_copy_optional_const_pulse
      else IssueType.unnecessary_copy_optional_pulse
  | ( ( ConfigUsage _
      | ConstRefableParameter _
      | CSharpResourceLeak _
      | JavaResourceLeak _
      | HackUnawaitedAwaitable _
      | MemoryLeak _
      | ReadonlySharedPtrParameter _
      | RetainCycle _
      | StackVariableAddressEscape _
      | UnnecessaryCopy _ )
    , true ) ->
      L.die InternalError "Issue type cannot be latent"
