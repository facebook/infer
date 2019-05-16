(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** an error to report to the user *)
type t =
  | AccessToInvalidAddress of
      { access: HilExp.AccessExpression.t
      ; invalidated_by: PulseInvalidation.t PulseTrace.action
      ; accessed_by: HilExp.AccessExpression.t PulseTrace.action
      ; trace: PulseTrace.breadcrumbs }
  | StackVariableAddressEscape of
      { variable: Var.t
      ; trace: PulseTrace.breadcrumbs
      ; location: Location.t }

val get_message : t -> string

val get_location : t -> Location.t

val get_issue_type : t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
