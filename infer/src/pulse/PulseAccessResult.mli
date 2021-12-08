(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain

type 'astate error =
  | PotentialInvalidAccess of
      { astate: 'astate
      ; address: AbstractValue.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | PotentialInvalidAccessSummary of
      { astate: AbductiveDomain.summary
      ; address: AbstractValue.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
  | ReportableError of {astate: 'astate; diagnostic: Diagnostic.t}
  | ReportableErrorSummary of {astate: AbductiveDomain.summary; diagnostic: Diagnostic.t}
  | ISLError of 'astate

type ('a, 'astate) base_t = ('a, 'astate error) result

type 'a t = ('a, AbductiveDomain.t) base_t

(** Intermediate datatype since {!AbductiveDomain} cannot refer to this module without creating a
    circular dependency.

    Purposefully omits [`MemoryLeak] errors as it's a good idea to double-check if you really want
    to report a leak. *)
type 'astate abductive_error =
  [ `ISLError of 'astate
  | `PotentialInvalidAccess of
    'astate * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option)
  | `PotentialInvalidAccessSummary of
    AbductiveDomain.summary * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option)
  ]

val of_abductive_error : [< 'astate abductive_error] -> 'astate error

val of_abductive_result : ('a, [< 'astate abductive_error]) result -> ('a, 'astate) base_t

val of_abductive_access_result :
     Trace.t
  -> ('a, [< `InvalidAccess of Invalidation.t * Trace.t * 'astate | 'astate abductive_error]) result
  -> ('a, 'astate) base_t

val ignore_leaks :
     ( AbductiveDomain.summary
     , [< `MemoryLeak of AbductiveDomain.summary * Attribute.allocator * Trace.t * Location.t
       | `ResourceLeak of AbductiveDomain.summary * JavaClassName.t * Trace.t * Location.t
       | AbductiveDomain.summary abductive_error ] )
     result
  -> (AbductiveDomain.summary, [> AbductiveDomain.summary abductive_error]) result
