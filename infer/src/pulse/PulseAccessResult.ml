(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module DecompilerExpr = PulseDecompilerExpr
module Decompiler = PulseAbductiveDecompiler
module Diagnostic = PulseDiagnostic

type summary_error =
  | PotentialInvalidAccessSummary of
      { astate: AbductiveDomain.summary
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableErrorSummary of {astate: AbductiveDomain.summary; diagnostic: Diagnostic.t}
  | ISLErrorSummary of {astate: AbductiveDomain.summary}

type error =
  | PotentialInvalidAccess of
      { astate: AbductiveDomain.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
  | ISLError of {astate: AbductiveDomain.t}
  | Summary of summary_error

let is_fatal_summary = function
  | PotentialInvalidAccessSummary _ | ISLErrorSummary _ ->
      true
  | ReportableErrorSummary {diagnostic} ->
      Diagnostic.aborts_execution diagnostic


let is_fatal = function
  | PotentialInvalidAccess _ | ISLError _ ->
      true
  | ReportableError {diagnostic} ->
      Diagnostic.aborts_execution diagnostic
  | Summary summary_error ->
      is_fatal_summary summary_error


let summary_of_error = function
  | PotentialInvalidAccessSummary {astate}
  | ReportableErrorSummary {astate}
  | ISLErrorSummary {astate} ->
      astate


let astate_of_error = function
  | PotentialInvalidAccess {astate} | ReportableError {astate} | ISLError {astate} ->
      astate
  | Summary summary_error ->
      (summary_of_error summary_error :> AbductiveDomain.t)


type 'a t = ('a, error) PulseResult.t

type 'a summary = ('a, summary_error) PulseResult.t

type abductive_error =
  [ `ISLError of AbductiveDomain.t
  | `PotentialInvalidAccess of
    AbductiveDomain.t * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option) ]

type abductive_summary_error =
  [ `PotentialInvalidAccessSummary of
    AbductiveDomain.summary * DecompilerExpr.t * (Trace.t * Invalidation.must_be_valid_reason option)
  ]

let ignore_leaks = function
  | Ok astate
  | Error (`MemoryLeak (astate, _, _, _))
  | Error (`ResourceLeak (astate, _, _, _))
  | Error (`RetainCycle (astate, _, _, _, _)) ->
      Ok astate
  | Error #abductive_summary_error as result ->
      result


let of_abductive_error = function
  | `ISLError astate ->
      ISLError {astate}
  | `PotentialInvalidAccess (astate, address, must_be_valid) ->
      PotentialInvalidAccess {astate; address= Decompiler.find address astate; must_be_valid}


let of_abductive_result abductive_result =
  (* note: all errors here are fatal *)
  Result.map_error abductive_result ~f:of_abductive_error |> PulseResult.fatal_of_result


let of_abductive_summary_error = function
  | `PotentialInvalidAccessSummary (astate, address, must_be_valid) ->
      PotentialInvalidAccessSummary {astate; address; must_be_valid}


let of_abductive_summary_result abductive_summary_result =
  (* note: all errors here are fatal *)
  Result.map_error abductive_summary_result ~f:of_abductive_summary_error
  |> PulseResult.fatal_of_result


let of_invalid_access access_trace = function
  | `InvalidAccess (invalid_address, invalidation, invalidation_trace, astate) ->
      ReportableError
        { astate
        ; diagnostic=
            AccessToInvalidAddress
              { calling_context= []
              ; invalid_address= Decompiler.find invalid_address astate
              ; invalidation
              ; invalidation_trace
              ; access_trace
              ; must_be_valid_reason= None } }


let of_abductive_access_result access_trace abductive_result =
  Result.map_error abductive_result ~f:(function
    | `InvalidAccess _ as invalid_access ->
        of_invalid_access access_trace invalid_access
    | (`ISLError _ | `PotentialInvalidAccess _) as error ->
        of_abductive_error error )
  (* note: all errors here are fatal *)
  |> PulseResult.fatal_of_result


let of_error_f error ~f : _ t =
  if is_fatal error then FatalError (error, []) else Recoverable (f error, [error])


let of_result_f (result : _ result) ~f : _ t =
  match result with Ok x -> Ok x | Error error -> of_error_f ~f error


let of_result result = of_result_f ~f:astate_of_error result

let of_summary summary_result =
  PulseResult.map_error summary_result ~f:(fun summary_error -> Summary summary_error)
