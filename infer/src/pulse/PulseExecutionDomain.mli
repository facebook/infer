(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module LatentIssue = PulseLatentIssue

type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t  (** represents the state at the program point *)
  | ExceptionRaised of 'abductive_domain_t  (** state after an exception has been thrown *)
  | ExitProgram of AbductiveDomain.summary
      (** represents the state originating at exit/divergence. *)
  | AbortProgram of AbductiveDomain.summary
      (** represents the state at the program point that caused an error *)
  | LatentAbortProgram of {astate: AbductiveDomain.summary; latent_issue: LatentIssue.t}
      (** this path leads to an error but we don't have conclusive enough data to report it yet *)
  | LatentInvalidAccess of
      { astate: AbductiveDomain.summary
      ; address: AbstractValue.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
      ; calling_context: (CallEvent.t * Location.t) list }
      (** if [address] is ever observed to be invalid then there is an invalid access because it
          [must_be_valid] *)
  | ISLLatentMemoryError of AbductiveDomain.summary
      (** represents the state at the program point that might cause an error; used for
          {!Config.pulse_isl} *)

type t = AbductiveDomain.t base_t

include AbstractDomain.Disjunct with type t := t

val continue : AbductiveDomain.t -> t

val is_unsat_cheap : t -> bool
(** see {!PulsePathCondition.is_unsat_cheap} *)

type summary = AbductiveDomain.summary base_t [@@deriving compare, equal, yojson_of]
