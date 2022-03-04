(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Decompiler = PulseAbductiveDecompiler
module Diagnostic = PulseDiagnostic

type summary_error =
  | PotentialInvalidAccessSummary of
      { astate: AbductiveDomain.summary
      ; address: Decompiler.expr
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableErrorSummary of {astate: AbductiveDomain.summary; diagnostic: Diagnostic.t}
  | ISLErrorSummary of {astate: AbductiveDomain.summary}

type error =
  | PotentialInvalidAccess of
      { astate: AbductiveDomain.t
      ; address: Decompiler.expr
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
  | ISLError of {astate: AbductiveDomain.t}
  | Summary of summary_error

type 'a t = ('a, error) PulseResult.t

type 'a summary = ('a, summary_error) PulseResult.t

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
    AbductiveDomain.summary * Decompiler.expr * (Trace.t * Invalidation.must_be_valid_reason option)
  ]

val of_result_f : ('a, error) result -> f:(error -> 'a) -> 'a t

val of_result : (AbductiveDomain.t, error) result -> AbductiveDomain.t t

val of_error_f : error -> f:(error -> 'a) -> 'a t

val of_abductive_summary_error : [< abductive_summary_error] -> summary_error

val of_abductive_result : ('a, [< abductive_error]) result -> 'a t

val of_abductive_summary_result : ('a, [< abductive_summary_error]) result -> 'a summary

val of_abductive_access_result :
     Trace.t
  -> ( 'a
     , [< `InvalidAccess of AbstractValue.t * Invalidation.t * Trace.t * AbductiveDomain.t
       | abductive_error ] )
     result
  -> 'a t

val of_summary : 'a summary -> 'a t

val ignore_leaks :
     ( AbductiveDomain.summary
     , [< `MemoryLeak of AbductiveDomain.summary * Attribute.allocator * Trace.t * Location.t
       | `ResourceLeak of AbductiveDomain.summary * JavaClassName.t * Trace.t * Location.t
       | `RetainCycle of AbductiveDomain.summary * Trace.t * Location.t
       | abductive_summary_error ] )
     result
  -> (AbductiveDomain.summary, [> abductive_summary_error]) result
