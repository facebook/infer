(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue
module ValueHistory = PulseValueHistory
module Access = PulseAccess

type t =
  | InMemory of
      { src: AbstractValue.t * ValueHistory.t
      ; access: Access.t
      ; dest: AbstractValue.t * ValueHistory.t }
  | OnStack of {var: Var.t; addr_hist: AbstractValue.t * ValueHistory.t}
  | Unknown of (AbstractValue.t * ValueHistory.t)

let unknown addr_hist = Unknown addr_hist

let addr_hist = function
  | InMemory {dest= addr_hist} | OnStack {addr_hist} | Unknown addr_hist ->
      addr_hist


let value t =
  let value, _ = addr_hist t in
  value


let addr_hist_arg = ProcnameDispatcher.Call.FuncArg.map_payload ~f:addr_hist

let addr_hist_args = List.map ~f:addr_hist_arg
