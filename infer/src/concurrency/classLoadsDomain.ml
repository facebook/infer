(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module ClassLoad = struct
  include String

  let pp_human = pp
end

module Event = ExplicitTrace.MakeTraceElem (ClassLoad)
include Event.FiniteSet

let add ({Event.trace} as x) astate =
  match find_opt x astate with
  | None ->
      add x astate
  | Some ({Event.trace= trace'} as x') ->
      if Int.( <= ) (List.length trace') (List.length trace) then astate
      else remove x' astate |> add x


let union xs ys = fold add xs ys

let get_java_class = function
  | Typ.Procname.Java java_pname ->
      Some (Typ.Procname.Java.get_class_name java_pname)
  | _ ->
      None


let integrate_summary callee_pname loc astate callee_summary =
  get_java_class callee_pname
  |> Option.value_map ~default:astate ~f:(fun clazz ->
         let new_event = Event.make clazz loc in
         let callsite = CallSite.make callee_pname loc in
         let summary = with_callsite callee_summary callsite in
         add new_event astate |> union summary )


type summary = t

let pp_summary = pp
