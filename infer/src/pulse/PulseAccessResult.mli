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
module Diagnostic = PulseDiagnostic

type error =
  | PotentialInvalidAccess of
      { astate: AbductiveDomain.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
  | ISLError of {astate: AbductiveDomain.t}
  | WithSummary of error * AbductiveDomain.Summary.t
      (** An error for which the summary corresponding to the abstract state has already been
          computed, to avoid having to compute the summary for a given abstract state multiple
          times. For convenience this is part of the [error] datatype but the invariant that an
          [error] within a [WithSummary (error, summary)] cannot itself be a [WithSummary _]. *)

type 'a t = ('a, error) PulseResult.t

val with_summary : ('a, error * AbductiveDomain.Summary.t) PulseResult.t -> 'a t

(** Intermediate datatype since {!AbductiveDomain} cannot refer to this module without creating a
    circular dependency.

    Purposefully omits [`MemoryLeak] errors as it's a good idea to double-check if you really want
    to report a leak. *)
type abductive_error =
  [ `ISLError of AbductiveDomain.t
  | `PotentialInvalidAccess of
    AbductiveDomain.t * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option) ]

type abductive_summary_error =
  [ `PotentialInvalidAccessSummary of
    AbductiveDomain.Summary.t
    * AbductiveDomain.t
    * DecompilerExpr.t
    * (Trace.t * Invalidation.must_be_valid_reason option) ]

val of_result_f : ('a, error) result -> f:(error -> 'a) -> 'a t

val of_result : (AbductiveDomain.t, error) result -> AbductiveDomain.t t

val of_error_f : error -> f:(error -> 'a) -> 'a t

val of_abductive_summary_error : [< abductive_summary_error] -> error * AbductiveDomain.Summary.t

val of_abductive_result : ('a, [< abductive_error]) result -> 'a t

val of_abductive_summary_result :
  ('a, [< abductive_summary_error]) result -> ('a, error * AbductiveDomain.Summary.t) PulseResult.t

val of_abductive_access_result :
     Trace.t
  -> ( 'a
     , [< `InvalidAccess of AbstractValue.t * Invalidation.t * Trace.t * AbductiveDomain.t
       | abductive_error ] )
     result
  -> 'a t

val ignore_leaks :
     ( AbductiveDomain.Summary.t
     , [< `MemoryLeak of
          AbductiveDomain.Summary.t * AbductiveDomain.t * Attribute.allocator * Trace.t * Location.t
       | `JavaResourceLeak of
         AbductiveDomain.Summary.t * AbductiveDomain.t * JavaClassName.t * Trace.t * Location.t
       | `CSharpResourceLeak of
         AbductiveDomain.Summary.t * AbductiveDomain.t * CSharpClassName.t * Trace.t * Location.t
       | `RetainCycle of
         AbductiveDomain.Summary.t
         * AbductiveDomain.t
         * Trace.t list
         * DecompilerExpr.t
         * DecompilerExpr.t
         * Location.t
       | abductive_summary_error ] )
     result
  -> (AbductiveDomain.Summary.t, [> abductive_summary_error]) result
