(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
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
        (** the list of function calls leading to the issue being realised, in
            outermost-to-innermost order, which is an additional common prefix to the traces in the
            record *)
  ; invalid_address: Decompiler.expr
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to [invalidation] without
            further assumptions *)
  ; access_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to an access to the value
            invalidated in [invalidation_trace] without further assumptions *)
  ; must_be_valid_reason: Invalidation.must_be_valid_reason option }
[@@deriving compare, equal, yojson_of]

type erlang_error =
  | Badkey of {calling_context: calling_context; location: Location.t}
  | Badmap of {calling_context: calling_context; location: Location.t}
  | Badmatch of {calling_context: calling_context; location: Location.t}
  | Badrecord of {calling_context: calling_context; location: Location.t}
  | Case_clause of {calling_context: calling_context; location: Location.t}
  | Function_clause of {calling_context: calling_context; location: Location.t}
  | If_clause of {calling_context: calling_context; location: Location.t}
  | Try_clause of {calling_context: calling_context; location: Location.t}
[@@deriving compare, equal, yojson_of]

type read_uninitialized_value =
  { calling_context: calling_context
        (** the list of function calls leading to the issue being realised, which is an additional
            common prefix to the traces in the record *)
  ; trace: Trace.t
        (** assuming we are in the calling context, the trace leads to read of the uninitialized
            value *) }
[@@deriving compare, equal, yojson_of]

(** an error to report to the user *)
type t =
  | AccessToInvalidAddress of access_to_invalid_address
  | MemoryLeak of {allocator: Attribute.allocator; allocation_trace: Trace.t; location: Location.t}
  | RetainCycle of {assignment_trace: Trace.t; location: Location.t}
  | ErlangError of erlang_error
  | ReadUninitializedValue of read_uninitialized_value
  | ResourceLeak of {class_name: JavaClassName.t; allocation_trace: Trace.t; location: Location.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
  | TaintFlow of
      { tainted: Decompiler.expr
      ; source: Taint.t * ValueHistory.t
      ; sink: Taint.t * Trace.t
      ; location: Location.t }
  | UnnecessaryCopy of {variable: Var.t; location: Location.t}
[@@deriving equal]

val aborts_execution : t -> bool
(** whether the presence of an error should abort the execution *)

val get_message : t -> string

val get_location : t -> Location.t

val get_issue_type : latent:bool -> t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
