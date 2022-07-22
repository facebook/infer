(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module Arithmetic = PulseArithmetic
module Diagnostic = PulseDiagnostic

type t =
  | AccessToInvalidAddress of Diagnostic.access_to_invalid_address
  | ErlangError of Diagnostic.erlang_error
  | ReadUninitializedValue of Diagnostic.read_uninitialized_value
[@@deriving compare, equal, yojson_of]

let to_diagnostic = function
  | AccessToInvalidAddress access_to_invalid_address ->
      Diagnostic.AccessToInvalidAddress access_to_invalid_address
  | ErlangError erlang_error ->
      Diagnostic.ErlangError erlang_error
  | ReadUninitializedValue read_uninitialized_value ->
      Diagnostic.ReadUninitializedValue read_uninitialized_value


let add_call call_and_loc = function
  | AccessToInvalidAddress access ->
      AccessToInvalidAddress {access with calling_context= call_and_loc :: access.calling_context}
  | ErlangError (Badkey {calling_context; location}) ->
      ErlangError (Badkey {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmap {calling_context; location}) ->
      ErlangError (Badmap {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badmatch {calling_context; location}) ->
      ErlangError (Badmatch {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Badrecord {calling_context; location}) ->
      ErlangError (Badrecord {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Case_clause {calling_context; location}) ->
      ErlangError (Case_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Function_clause {calling_context; location}) ->
      ErlangError (Function_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (If_clause {calling_context; location}) ->
      ErlangError (If_clause {calling_context= call_and_loc :: calling_context; location})
  | ErlangError (Try_clause {calling_context; location}) ->
      ErlangError (Try_clause {calling_context= call_and_loc :: calling_context; location})
  | ReadUninitializedValue read ->
      ReadUninitializedValue {read with calling_context= call_and_loc :: read.calling_context}


let is_manifest (astate : AbductiveDomain.summary) =
  Arithmetic.has_no_assumptions (astate :> AbductiveDomain.t)
  && ( (not Config.pulse_isl)
     || AbductiveDomain.is_isl_without_allocation (astate :> AbductiveDomain.t)
        && ( (not Config.pulse_manifest_emp)
           || AbductiveDomain.is_pre_without_isl_abduced (astate :> AbductiveDomain.t) ) )


(* require a summary because we don't want to stop reporting because some non-abducible condition is
   not true as calling context cannot possibly influence such conditions *)
let should_report (astate : AbductiveDomain.summary) (diagnostic : Diagnostic.t) =
  match diagnostic with
  | ConstRefableParameter _
  | MemoryLeak _
  | ResourceLeak _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | FlowFromTaintSource _
  | UnnecessaryCopy _ ->
      (* these issues are reported regardless of the calling context, not sure if that's the right
         decision yet *)
      `ReportNow
  | AccessToInvalidAddress latent ->
      if is_manifest astate then `ReportNow else `DelayReport (AccessToInvalidAddress latent)
  | ErlangError latent ->
      if is_manifest astate then `ReportNow else `DelayReport (ErlangError latent)
  | ReadUninitializedValue latent ->
      if is_manifest astate then `ReportNow else `DelayReport (ReadUninitializedValue latent)
