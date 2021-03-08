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
  | ReportableError of {astate: 'astate; diagnostic: Diagnostic.t}
  | ISLError of 'astate

type ('a, 'astate) base_t = ('a, 'astate error) result

type 'a t = ('a, AbductiveDomain.t) base_t

let to_summary tenv proc_desc error =
  let open SatUnsat.Import in
  match error with
  | ReportableError {astate; diagnostic} ->
      let+ astate = AbductiveDomain.summary_of_post tenv proc_desc astate in
      ReportableError {astate; diagnostic}
  | ISLError astate ->
      let+ astate = AbductiveDomain.summary_of_post tenv proc_desc astate in
      ISLError astate
