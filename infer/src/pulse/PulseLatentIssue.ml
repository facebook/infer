(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Arithmetic = PulseArithmetic

type t = AccessToInvalidAddress of Diagnostic.access_to_invalid_address [@@deriving equal]

let to_diagnostic = function
  | AccessToInvalidAddress access_to_invalid_address ->
      Diagnostic.AccessToInvalidAddress access_to_invalid_address


let add_call call_and_loc = function
  | AccessToInvalidAddress access ->
      AccessToInvalidAddress {access with calling_context= call_and_loc :: access.calling_context}


let should_report (astate : AbductiveDomain.summary) =
  Arithmetic.has_no_assumptions (astate :> AbductiveDomain.t)


(* require a summary because we don't want to stop reporting because some non-abducible condition is
   not true as calling context cannot possibly influence such conditions *)
let should_report_diagnostic (astate : AbductiveDomain.summary) (diagnostic : Diagnostic.t) =
  match diagnostic with
  | MemoryLeak _ | StackVariableAddressEscape _ | OrError _ ->
      (* these issues are reported regardless of the calling context, not sure if that's the right
         decision yet *)
      `ReportNow
  | AccessToInvalidAddress diag ->
      if should_report astate then `ReportNow else `DelayReport (AccessToInvalidAddress diag)
