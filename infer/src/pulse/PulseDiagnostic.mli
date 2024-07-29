(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Attribute = PulseAttribute
module CallEvent = PulseCallEvent
module ConfigName = FbPulseConfigName
module DecompilerExpr = PulseDecompilerExpr
module Invalidation = PulseInvalidation
module TaintItem = PulseTaintItem
module Trace = PulseTrace
module TransitiveInfo = PulseTransitiveInfo
module ValueHistory = PulseValueHistory

type calling_context = (CallEvent.t * Location.t) list [@@deriving compare, equal]

type access_to_invalid_address =
  { calling_context: calling_context
        (** the list of function calls leading to the issue being realised, in
            outermost-to-innermost order, which is an additional common prefix to the traces in the
            record *)
  ; invalid_address: DecompilerExpr.t
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to [invalidation] without
            further assumptions *)
  ; access_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to an access to the value
            invalidated in [invalidation_trace] without further assumptions *)
  ; must_be_valid_reason: Invalidation.must_be_valid_reason option }
[@@deriving compare, equal, yojson_of]

module ErlangError : sig
  type t =
    | Badarg of {calling_context: calling_context; location: Location.t}
    | Badgenerator of {calling_context: calling_context; location: Location.t}
    | Badkey of {calling_context: calling_context; location: Location.t}
    | Badmap of {calling_context: calling_context; location: Location.t}
    | Badmatch of {calling_context: calling_context; location: Location.t}
    | Badrecord of {calling_context: calling_context; location: Location.t}
    | Badreturn of {calling_context: calling_context; location: Location.t}
    | Case_clause of {calling_context: calling_context; location: Location.t}
    | Else_clause of {calling_context: calling_context; location: Location.t}
    | Function_clause of {calling_context: calling_context; location: Location.t}
    | If_clause of {calling_context: calling_context; location: Location.t}
    | Try_clause of {calling_context: calling_context; location: Location.t}
  [@@deriving compare, equal, yojson_of]
end

module ReadUninitialized : sig
  type t =
    { typ: Attribute.UninitializedTyp.t
    ; calling_context: calling_context
          (** the list of function calls leading to the issue being realised, which is an additional
              common prefix to the traces in the record *)
    ; trace: Trace.t
          (** assuming we are in the calling context, the trace leads to read of the uninitialized
              value *) }
  [@@deriving compare, equal, yojson_of]
end

type flow_kind = TaintedFlow | FlowToSink | FlowFromSource [@@deriving equal]

type retain_cycle_data = {expr: DecompilerExpr.t; location: Location.t option; trace: Trace.t option}
[@@deriving equal]

(** an error to report to the user *)
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
  | DynamicTypeMismatch of {location: Location.t}
  | ErlangError of ErlangError.t
  | TransitiveAccess of
      { tag: string
      ; description: string
      ; call_trace: Trace.t
      ; transitive_callees: TransitiveInfo.Callees.t
      ; transitive_missed_captures: Typ.Name.Set.t }
  | JavaResourceLeak of
      {class_name: JavaClassName.t; allocation_trace: Trace.t; location: Location.t}
  | HackCannotInstantiateAbstractClass of {type_name: Typ.Name.t; trace: Trace.t}
  | HackUnawaitedAwaitable of {allocation_trace: Trace.t; location: Location.t}
  | HackUnfinishedBuilder of
      {builder_type: HackClassName.t; allocation_trace: Trace.t; location: Location.t}
  | MemoryLeak of {allocator: Attribute.allocator; allocation_trace: Trace.t; location: Location.t}
  | MutualRecursionCycle of {cycle: PulseMutualRecursion.t; location: Location.t}
  | ReadonlySharedPtrParameter of
      {param: Var.t; typ: Typ.t; location: Location.t; used_locations: Location.t list}
  | ReadUninitialized of ReadUninitialized.t
  | RetainCycle of {values: retain_cycle_data list; location: Location.t; unknown_access_type: bool}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | TaintFlow of
      { expr: DecompilerExpr.t
      ; source: TaintItem.t * ValueHistory.t
      ; sink: TaintItem.t * Trace.t
      ; location: Location.t
      ; flow_kind: flow_kind
      ; policy_description: string
      ; policy_id: int
      ; policy_privacy_effect: string option
      ; report_as_issue_type: string option
      ; report_as_category: string option }
  | UnnecessaryCopy of
      { copied_into: PulseAttribute.CopiedInto.t
      ; source_typ: Typ.t option
      ; source_opt: DecompilerExpr.source_expr option
      ; location: Location.t (* the location to report the issue *)
      ; copied_location: (Procname.t * Location.t) option
            (* [copied_location] has a value when the copied location is different to where to
               report: e.g. this is the case for returning copied values. *)
      ; location_instantiated: Location.t option
      ; from: PulseAttribute.CopyOrigin.t }
[@@deriving equal]

val pp : F.formatter -> t -> unit

val aborts_execution : t -> bool
(** whether the presence of an error should abort the execution *)

val get_message_and_suggestion : t -> string * string option

val get_location : t -> Location.t

val get_location_instantiated : t -> Location.t option

val get_copy_type : t -> Typ.t option

val get_issue_type : latent:bool -> t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
