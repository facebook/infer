(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type exec_state =
  | AbortProgram of PulseAbductiveDomain.t
  | ContinueProgram of PulseAbductiveDomain.t
  | ExitProgram of PulseAbductiveDomain.t

type t = exec_state

let continue astate = ContinueProgram astate

let mk_initial pdesc = ContinueProgram (PulseAbductiveDomain.mk_initial pdesc)

let leq ~lhs ~rhs =
  match (lhs, rhs) with
  | AbortProgram astate1, AbortProgram astate2
  | ContinueProgram astate1, ContinueProgram astate2
  | ExitProgram astate1, ExitProgram astate2 ->
      PulseAbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | _ ->
      false


let pp fmt = function
  | ContinueProgram astate ->
      PulseAbductiveDomain.pp fmt astate
  | ExitProgram astate ->
      F.fprintf fmt "{ExitProgram %a}" PulseAbductiveDomain.pp astate
  | AbortProgram astate ->
      F.fprintf fmt "{AbortProgram %a}" PulseAbductiveDomain.pp astate


let map ~f exec_state =
  match exec_state with
  | AbortProgram astate ->
      AbortProgram (f astate)
  | ContinueProgram astate ->
      ContinueProgram (f astate)
  | ExitProgram astate ->
      ExitProgram (f astate)


let of_post pdesc = map ~f:(PulseAbductiveDomain.of_post pdesc)
