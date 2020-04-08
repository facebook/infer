(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type exec_state =
  | ContinueProgram of PulseAbductiveDomain.t
  | ExitProgram of PulseAbductiveDomain.t

type t = exec_state

let continue astate = ContinueProgram astate

let mk_initial pdesc = ContinueProgram (PulseAbductiveDomain.mk_initial pdesc)

let leq ~lhs ~rhs =
  match (lhs, rhs) with
  | ContinueProgram astate1, ContinueProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      PulseAbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | ExitProgram _, ContinueProgram _ | ContinueProgram _, ExitProgram _ ->
      false


let pp fmt = function
  | ContinueProgram astate ->
      PulseAbductiveDomain.pp fmt astate
  | ExitProgram astate ->
      F.fprintf fmt "{ExitProgram %a}" PulseAbductiveDomain.pp astate


let map ~f exec_state =
  match exec_state with
  | ContinueProgram astate ->
      ContinueProgram (f astate)
  | ExitProgram astate ->
      ExitProgram (f astate)


let of_post pdesc = map ~f:(PulseAbductiveDomain.of_post pdesc)
