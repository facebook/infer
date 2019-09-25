(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module ClassLoad = struct
  include String

  let describe = pp
end

module Event =
  ExplicitTrace.MakeTraceElemModuloLocation (ClassLoad) (ExplicitTrace.DefaultCallPrinter)
include Event.FiniteSet

let add ({Event.trace} as x) astate =
  match find_opt x astate with
  | None ->
      add x astate
  | Some ({Event.trace= trace'} as x') ->
      if Int.( <= ) (List.length trace') (List.length trace) then astate
      else remove x' astate |> add x


let union xs ys = fold add xs ys

let add_string loc astate clazz =
  L.debug Analysis Verbose "CL: LOADING class %s@." clazz ;
  let new_event = Event.make clazz loc in
  add new_event astate


let mem_typename name astate =
  let str_name = Typ.Name.name name in
  (* comparison of elements is only over the string component so fake the rest *)
  let fake_event = Event.make str_name Location.dummy in
  mem fake_event astate


let add_typename loc astate name = Typ.Name.name name |> add_string loc astate

let integrate_summary callee_pname loc astate callee_summary =
  L.debug Analysis Verbose "CL: ADDING SUMMARY OF %a@." Typ.Procname.pp callee_pname ;
  let callsite = CallSite.make callee_pname loc in
  let summary = with_callsite callee_summary callsite in
  union astate summary


type summary = t

let pp_summary = pp
