(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module Access = PulseAccess
module ValueHistory = PulseValueHistory

type 'value t_ =
  | InMemory of
      {src: 'value * ValueHistory.t; access: 'value Access.access; dest: 'value * ValueHistory.t}
  | OnStack of {var: Var.t; addr_hist: 'value * ValueHistory.t}
  | Unknown of 'value * ValueHistory.t
[@@deriving compare, equal, yojson_of]

type t = AbstractValue.t t_ [@@deriving compare, equal, yojson_of]

let unknown (addr, hist) = Unknown (addr, hist)

let map_value vo ~f =
  match vo with
  | InMemory {src= v_src, hist_src; access; dest= v_dest, hist_dest} ->
      InMemory
        { src= (f v_src, hist_src)
        ; access= Access.map_array_index ~f access
        ; dest= (f v_dest, hist_dest) }
  | OnStack {var; addr_hist= v, hist} ->
      OnStack {var; addr_hist= (f v, hist)}
  | Unknown (v, hist) ->
      Unknown (f v, hist)


let with_value v = function
  | InMemory {src; access; dest= _, hist} ->
      InMemory {src; access; dest= (v, hist)}
  | OnStack {var; addr_hist= _, hist} ->
      OnStack {var; addr_hist= (v, hist)}
  | Unknown (_, hist) ->
      Unknown (v, hist)


let with_hist hist = function
  | InMemory {src; access; dest= v, _} ->
      InMemory {src; access; dest= (v, hist)}
  | OnStack {var; addr_hist= v, _} ->
      OnStack {var; addr_hist= (v, hist)}
  | Unknown (v, _) ->
      Unknown (v, hist)


let addr_hist = function
  | InMemory {dest= addr_hist} | OnStack {addr_hist} ->
      addr_hist
  | Unknown (addr, hist) ->
      (addr, hist)


let value t = addr_hist t |> fst

let hist t = addr_hist t |> snd

let addr_hist_arg = ProcnameDispatcher.Call.FuncArg.map_payload ~f:addr_hist

let addr_hist_args = List.map ~f:addr_hist_arg

let pp_addr_hist fmt addr_hist = Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp fmt addr_hist

let detailed_pp fmt = function
  | InMemory {src; access; dest} ->
      F.fprintf fmt "InMemory {@[src=%a; access=%a; dest= %a@]}" pp_addr_hist src Access.pp access
        pp_addr_hist dest
  | OnStack {var; addr_hist} ->
      F.fprintf fmt "OnStack {@[var=%a; addr_hist= %a@]}" Var.pp var pp_addr_hist addr_hist
  | Unknown (addr, hist) ->
      F.fprintf fmt "Unknown @[%a@]" pp_addr_hist (addr, hist)


let pp fmt vo =
  if Config.debug_level_analysis >= 3 then detailed_pp fmt vo else AbstractValue.pp fmt (value vo)
