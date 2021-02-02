(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type access_to_invalid_address =
  { calling_context: (CallEvent.t * Location.t) list
        (** the list of function calls leading to the issue being realised, which is an additional
            common prefix to the traces in the record *)
  ; invalidation: Invalidation.t
  ; invalidation_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to [invalidation] without
            further assumptions *)
  ; access_trace: Trace.t
        (** assuming we are in the calling context, the trace leads to an access to the value
            invalidated in [invalidation_trace] without further assumptions *) }
[@@deriving compare, equal, yojson_of]

type read_uninitialized_value =
  { calling_context: (CallEvent.t * Location.t) list
        (** the list of function calls leading to the issue being realised, which is an additional
            common prefix to the traces in the record *)
  ; trace: Trace.t
        (** assuming we are in the calling context, the trace leads to read of the uninitialized
            value *) }
[@@deriving compare, equal, yojson_of]

(** an error to report to the user *)
type t =
  | AccessToInvalidAddress of access_to_invalid_address
  | MemoryLeak of {procname: Procname.t; allocation_trace: Trace.t; location: Location.t}
  | ReadUninitializedValue of read_uninitialized_value
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}
[@@deriving equal]

val get_message : t -> string

val get_location : t -> Location.t

val get_issue_type : t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
