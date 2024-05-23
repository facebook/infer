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
module LatentIssue = PulseLatentIssue

type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t  (** represents the state at the program point *)
  | ExceptionRaised of 'abductive_domain_t  (** state after an exception has been thrown *)
  | ExitProgram of AbductiveDomain.Summary.t
      (** represents the state originating at exit/divergence. *)
  | AbortProgram of AbductiveDomain.Summary.t
      (** represents the state at the program point that caused an error *)
  | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
      (** this path leads to an error but we don't have conclusive enough data to report it yet *)
  | LatentInvalidAccess of
      { astate: AbductiveDomain.Summary.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
      ; calling_context: (CallEvent.t * Location.t) list }
      (** if [address] is ever observed to be invalid then there is an invalid access because it
          [must_be_valid] *)
  | LatentSpecializedTypeIssue of
      {astate: AbductiveDomain.Summary.t; specialized_type: Typ.Name.t; trace: Trace.t}
      (** this path leads to an error but we need to know where type specialization happened to
          report it *)

type t = AbductiveDomain.t base_t

include AbstractDomain.Disjunct with type t := t

val pp_with_kind : Pp.print_kind -> PulsePathContext.t option -> F.formatter -> t -> unit

val pp : F.formatter -> t -> unit

val continue : AbductiveDomain.t -> t

type summary = AbductiveDomain.Summary.t base_t [@@deriving compare, equal, yojson_of]

val pp_summary : F.formatter -> summary -> unit

val to_name : 'a base_t -> string
