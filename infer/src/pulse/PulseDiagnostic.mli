(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

(** an error to report to the user *)
type t =
  | AccessToInvalidAddress of
      {invalidation: Invalidation.t; invalidation_trace: Trace.t; access_trace: Trace.t}
  | StackVariableAddressEscape of {variable: Var.t; history: ValueHistory.t; location: Location.t}

val get_message : t -> string

val get_location : t -> Location.t

val get_issue_type : t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
