(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue
module Access = PulseAccess
module ValueHistory = PulseValueHistory

type 'value t_ =
  | InMemory of
      {src: 'value * ValueHistory.t; access: 'value Access.access; dest: 'value * ValueHistory.t}
  | OnStack of {var: Var.t; addr_hist: 'value * ValueHistory.t}
  | Unknown of 'value * ValueHistory.t

type t = AbstractValue.t t_

let unknown (addr, hist) = Unknown (addr, hist)

let addr_hist = function
  | InMemory {dest= addr_hist} | OnStack {addr_hist} ->
      addr_hist
  | Unknown (addr, hist) ->
      (addr, hist)


let value t = addr_hist t |> fst

let hist t = addr_hist t |> snd

let addr_hist_arg = ProcnameDispatcher.Call.FuncArg.map_payload ~f:addr_hist

let addr_hist_args = List.map ~f:addr_hist_arg
