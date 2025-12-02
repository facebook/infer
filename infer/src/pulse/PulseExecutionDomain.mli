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

type stopped_execution =
  | ExitProgram of AbductiveDomain.Summary.t
  | AbortProgram of
      {astate: AbductiveDomain.Summary.t; diagnostic: Diagnostic.t; trace_to_issue: Trace.t}
  | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
  | LatentInvalidAccess of
      { astate: AbductiveDomain.Summary.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
      ; calling_context: (CallEvent.t * Location.t) list }
  | LatentSpecializedTypeIssue of
      {astate: AbductiveDomain.Summary.t; specialized_type: Typ.Name.t; trace: Trace.t}

type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | InfiniteLoop of 'abductive_domain_t
  | ExceptionRaised of 'abductive_domain_t
  | Stopped of stopped_execution

type t = AbductiveDomain.t base_t

include AbstractDomain.Disjunct with type t := t

val pp_with_kind : Pp.print_kind -> PulsePathContext.t option -> F.formatter -> t -> unit

val pp : F.formatter -> t -> unit

val continue : AbductiveDomain.t -> t

val summary_of_stopped_execution : stopped_execution -> AbductiveDomain.Summary.t

type summary = AbductiveDomain.Summary.t base_t [@@deriving compare, equal, yojson_of]

val pp_summary : Pp.print_kind -> F.formatter -> summary -> unit

val to_name : 'a base_t -> string

val back_edge : t list -> t list -> int -> int option
