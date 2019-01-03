(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type actor = {access_expr: HilExp.AccessExpression.t; location: Location.t} [@@deriving compare]

type t =
  | AccessToInvalidAddress of
      { invalidated_by: PulseInvalidation.t
      ; accessed_by: actor
      ; trace: PulseTrace.t
      ; address: PulseDomain.AbstractAddress.t }
  | StackVariableAddressEscape of {variable: Var.t; location: Location.t}

val get_message : t -> string

val get_location : t -> Location.t

val get_issue_type : t -> IssueType.t

val get_trace : t -> Errlog.loc_trace
