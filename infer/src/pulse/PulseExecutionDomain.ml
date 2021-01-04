(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module LatentIssue = PulseLatentIssue

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | ExitProgram of AbductiveDomain.summary
  | AbortProgram of AbductiveDomain.summary
  | LatentAbortProgram of {astate: AbductiveDomain.summary; latent_issue: LatentIssue.t}
  | ISLLatentMemoryError of 'abductive_domain_t
[@@deriving yojson_of]

type t = AbductiveDomain.t base_t

let continue astate = ContinueProgram astate

let mk_initial pdesc = ContinueProgram (AbductiveDomain.mk_initial pdesc)

let leq ~lhs ~rhs =
  match (lhs, rhs) with
  | AbortProgram astate1, AbortProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      AbductiveDomain.leq ~lhs:(astate1 :> AbductiveDomain.t) ~rhs:(astate2 :> AbductiveDomain.t)
  | ContinueProgram astate1, ContinueProgram astate2
  | ISLLatentMemoryError astate1, ISLLatentMemoryError astate2 ->
      AbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentAbortProgram {astate= astate1; latent_issue= issue1}
    , LatentAbortProgram {astate= astate2; latent_issue= issue2} ) ->
      LatentIssue.equal issue1 issue2
      && AbductiveDomain.leq ~lhs:(astate1 :> AbductiveDomain.t) ~rhs:(astate2 :> AbductiveDomain.t)
  | _ ->
      false


let pp fmt = function
  | ContinueProgram astate ->
      AbductiveDomain.pp fmt astate
  | ISLLatentMemoryError astate ->
      F.fprintf fmt "{ISLLatentMemoryError %a}" AbductiveDomain.pp astate
  | ExitProgram astate ->
      F.fprintf fmt "{ExitProgram %a}" AbductiveDomain.pp (astate :> AbductiveDomain.t)
  | AbortProgram astate ->
      F.fprintf fmt "{AbortProgram %a}" AbductiveDomain.pp (astate :> AbductiveDomain.t)
  | LatentAbortProgram {astate; latent_issue} ->
      let diagnostic = LatentIssue.to_diagnostic latent_issue in
      let message = Diagnostic.get_message diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "{LatentAbortProgram(%a: %s) %a}" Location.pp location message
        AbductiveDomain.pp
        (astate :> AbductiveDomain.t)


(* do not export this function as there lies wickedness: clients should generally care about what
   kind of state they are manipulating so let's not encourage them not to *)
let get_astate : t -> AbductiveDomain.t = function
  | ContinueProgram astate | ISLLatentMemoryError astate ->
      astate
  | ExitProgram astate | AbortProgram astate | LatentAbortProgram {astate} ->
      (astate :> AbductiveDomain.t)


let is_unsat_cheap exec_state = PathCondition.is_unsat_cheap (get_astate exec_state).path_condition

type summary = AbductiveDomain.summary base_t [@@deriving yojson_of]

let summary_of_posts_common ~continue_program pdesc posts =
  List.filter_mapi posts ~f:(fun i exec_state ->
      L.d_printfln "Creating spec out of state #%d:@\n%a" i pp exec_state ;
      match exec_state with
      | ContinueProgram astate | ISLLatentMemoryError astate -> (
        match AbductiveDomain.summary_of_post pdesc astate with
        | Unsat ->
            None
        | Sat astate ->
            Some (continue_program astate) )
      (* already a summary but need to reconstruct the variants to make the type system happy *)
      | AbortProgram astate ->
          Some (AbortProgram astate)
      | ExitProgram astate ->
          Some (ExitProgram astate)
      | LatentAbortProgram {astate; latent_issue} ->
          Some (LatentAbortProgram {astate; latent_issue}) )


let summary_of_posts =
  summary_of_posts_common ~continue_program:(fun astate ->
      match (astate :> AbductiveDomain.t).isl_status with
      | ISLOk ->
          ContinueProgram astate
      | ISLError ->
          ISLLatentMemoryError astate )


let force_exit_program =
  summary_of_posts_common ~continue_program:(fun astate -> ExitProgram astate)
