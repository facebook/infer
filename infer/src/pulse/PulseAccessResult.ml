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
  | PotentialInvalidAccess of {astate: 'astate; address: AbstractValue.t; must_be_valid: Trace.t}
  | PotentialInvalidAccessSummary of
      {astate: AbductiveDomain.summary; address: AbstractValue.t; must_be_valid: Trace.t}
  | ReportableError of {astate: 'astate; diagnostic: Diagnostic.t}
  | ISLError of 'astate

type ('a, 'astate) base_t = ('a, 'astate error) result

type 'a t = ('a, AbductiveDomain.t) base_t

type 'astate abductive_error =
  [ `ISLError of 'astate
  | `PotentialInvalidAccess of 'astate * AbstractValue.t * Trace.t
  | `PotentialInvalidAccessSummary of AbductiveDomain.summary * AbstractValue.t * Trace.t ]

let of_abductive_error = function
  | `ISLError astate ->
      ISLError astate
  | `PotentialInvalidAccess (astate, address, must_be_valid) ->
      PotentialInvalidAccess {astate; address; must_be_valid}
  | `PotentialInvalidAccessSummary (astate, address, must_be_valid) ->
      PotentialInvalidAccessSummary {astate; address; must_be_valid}


let of_abductive_result abductive_result = Result.map_error abductive_result ~f:of_abductive_error

let of_abductive_access_result access_trace abductive_result =
  Result.map_error abductive_result ~f:(function
    | `InvalidAccess (invalidation, invalidation_trace, astate) ->
        ReportableError
          { astate
          ; diagnostic=
              AccessToInvalidAddress
                {calling_context= []; invalidation; invalidation_trace; access_trace} }
    | (`ISLError _ | `PotentialInvalidAccess _ | `PotentialInvalidAccessSummary _) as error ->
        of_abductive_error error )
