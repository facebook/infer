(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain

type 'astate error = ReportableError of {astate: 'astate; diagnostic: Diagnostic.t}

type ('a, 'astate) base_t = ('a, 'astate error) result

type 'a t = ('a, AbductiveDomain.t) base_t

val to_summary :
  Tenv.t -> Procdesc.t -> AbductiveDomain.t error -> AbductiveDomain.summary error SatUnsat.t
