(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module DecompilerExpr = PulseDecompilerExpr
module Diagnostic = PulseDiagnostic
module LatentIssue = PulseLatentIssue

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | ExceptionRaised of 'abductive_domain_t
  | ExitProgram of AbductiveDomain.Summary.t
  | AbortProgram of AbductiveDomain.Summary.t
  | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
  | LatentInvalidAccess of
      { astate: AbductiveDomain.Summary.t
      ; address: DecompilerExpr.t
      ; must_be_valid: (Trace.t * Invalidation.must_be_valid_reason option[@yojson.opaque])
      ; calling_context: ((CallEvent.t * Location.t) list[@yojson.opaque]) }
[@@deriving equal, compare, yojson_of, variants]

type t = AbductiveDomain.t base_t

let continue astate = ContinueProgram astate

let leq ~lhs ~rhs =
  phys_equal lhs rhs
  ||
  match (lhs, rhs) with
  | AbortProgram astate1, AbortProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | ExceptionRaised astate1, ExceptionRaised astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      AbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentAbortProgram {astate= astate1; latent_issue= issue1}
    , LatentAbortProgram {astate= astate2; latent_issue= issue2} ) ->
      LatentIssue.equal issue1 issue2 && AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentInvalidAccess {astate= astate1; address= v1; must_be_valid= _}
    , LatentInvalidAccess {astate= astate2; address= v2; must_be_valid= _} ) ->
      DecompilerExpr.equal v1 v2 && AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | _ ->
      false


let pp_ pp_abductive_domain_t fmt = function
  | AbortProgram astate ->
      F.fprintf fmt "{AbortProgram %a}" AbductiveDomain.Summary.pp astate
  | ContinueProgram astate ->
      pp_abductive_domain_t fmt astate
  | ExceptionRaised astate ->
      F.fprintf fmt "{ExceptionRaised %a}" pp_abductive_domain_t astate
  | ExitProgram astate ->
      F.fprintf fmt "{ExitProgram %a}" AbductiveDomain.Summary.pp astate
  | LatentAbortProgram {astate; latent_issue} ->
      let diagnostic = LatentIssue.to_diagnostic latent_issue in
      let message, _suggestion = Diagnostic.get_message_and_suggestion diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "{LatentAbortProgram(%a: %s)@ %a@ %a}" Location.pp location message
        LatentIssue.pp latent_issue AbductiveDomain.Summary.pp astate
  | LatentInvalidAccess {astate; address; must_be_valid= _} ->
      F.fprintf fmt "{LatentInvalidAccess(%a) %a}" DecompilerExpr.pp address
        AbductiveDomain.Summary.pp astate


let pp fmt exec_state = pp_ AbductiveDomain.pp fmt exec_state

type summary = AbductiveDomain.Summary.t base_t [@@deriving compare, equal, yojson_of]

let pp_summary fmt exec_summary = pp_ AbductiveDomain.Summary.pp fmt exec_summary

let equal_fast exec_state1 exec_state2 =
  phys_equal exec_state1 exec_state2
  ||
  match (exec_state1, exec_state2) with
  | AbortProgram astate1, AbortProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      phys_equal astate1 astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      phys_equal astate1 astate2
  | _ ->
      false


let is_normal (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> false | _ -> true


let is_exceptional (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> true | _ -> false


let is_executable (exec_state : t) : bool =
  match exec_state with ContinueProgram _ | ExceptionRaised _ -> true | _ -> false


let exceptional_to_normal : t -> t = function
  | ExceptionRaised astate ->
      ContinueProgram astate
  | x ->
      x


let to_name = Variants_of_base_t.to_name
