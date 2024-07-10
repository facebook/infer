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

type error =
  | PotentialInvalidAccess of
      { astate: AbductiveDomain.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | PotentialInvalidSpecializedCall of
      {astate: AbductiveDomain.t; specialized_type: Typ.Name.t; trace: Trace.t}
  | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
  | WithSummary of error * AbductiveDomain.Summary.t

let with_summary result =
  PulseResult.map_error result ~f:(fun (error, summary) -> WithSummary (error, summary))


let rec is_fatal = function
  | PotentialInvalidAccess _ | PotentialInvalidSpecializedCall _ ->
      true
  | ReportableError {diagnostic} ->
      Diagnostic.aborts_execution diagnostic
  | WithSummary (error, _) ->
      is_fatal error


let rec astate_of_error = function
  | PotentialInvalidAccess {astate}
  | PotentialInvalidSpecializedCall {astate}
  | ReportableError {astate} ->
      astate
  | WithSummary (error, _) ->
      astate_of_error error


type 'a t = ('a, error) PulseResult.t

type abductive_error =
  [ `PotentialInvalidAccess of
    AbductiveDomain.t * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option) ]

type abductive_summary_error =
  [ `PotentialInvalidAccessSummary of
    AbductiveDomain.Summary.t
    * AbductiveDomain.t
    * DecompilerExpr.t
    * (Trace.t * Invalidation.must_be_valid_reason option) ]

let ignore_leaks = function
  | Ok astate
  | Error (`MemoryLeak (astate, _, _, _, _))
  | Error (`JavaResourceLeak (astate, _, _, _, _))
  | Error (`HackUnawaitedAwaitable (astate, _, _, _))
  | Error (`HackUnfinishedBuilder (astate, _, _, _, _))
  | Error (`CSharpResourceLeak (astate, _, _, _, _)) ->
      Ok astate
  | Error #abductive_summary_error as result ->
      result


let of_abductive_error = function
  | `PotentialInvalidAccess (astate, address, must_be_valid) ->
      PotentialInvalidAccess {astate; address= Decompiler.find address astate; must_be_valid}


let of_abductive_result abductive_result =
  (* note: all errors here are fatal *)
  Result.map_error abductive_result ~f:of_abductive_error |> PulseResult.fatal_of_result


let of_abductive_summary_error = function
  | `PotentialInvalidAccessSummary (summary, astate, address, must_be_valid) ->
      (PotentialInvalidAccess {astate; address; must_be_valid}, summary)


let of_abductive_summary_result abductive_summary_result =
  (* note: all errors here are fatal *)
  Result.map_error abductive_summary_result ~f:of_abductive_summary_error
  |> PulseResult.fatal_of_result


let of_error_f error ~f : _ t =
  if is_fatal error then FatalError (error, []) else Recoverable (f error, [error])


let of_result_f (result : _ result) ~f : _ t =
  match result with Ok x -> Ok x | Error error -> of_error_f ~f error


let of_result result = of_result_f ~f:astate_of_error result
