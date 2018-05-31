(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module F = Format

module BoTrace = struct
  type elem =
    | ArrAccess of Location.t
    | ArrDecl of Location.t
    | Assign of Location.t
    | Call of Location.t
    | Return of Location.t
    | SymAssign of Loc.t * Location.t
    | UnknownFrom of Typ.Procname.t * Location.t
  [@@deriving compare]

  type t = {length: int; trace: elem list} [@@deriving compare]

  let singleton elem = {length= 1; trace= [elem]}

  let add_elem elem t = {length= t.length + 1; trace= elem :: t.trace}

  let add_elem_last elem t = {length= t.length + 1; trace= t.trace @ [elem]}

  let append x y = {length= x.length + y.length; trace= x.trace @ y.trace}

  let pp_elem : F.formatter -> elem -> unit =
   fun fmt elem ->
    match elem with
    | ArrAccess location ->
        F.fprintf fmt "ArrAccess (%a)" Location.pp_file_pos location
    | ArrDecl location ->
        F.fprintf fmt "ArrDecl (%a)" Location.pp_file_pos location
    | Assign location ->
        F.fprintf fmt "Assign (%a)" Location.pp_file_pos location
    | Call location ->
        F.fprintf fmt "Call (%a)" Location.pp_file_pos location
    | Return location ->
        F.fprintf fmt "Return (%a)" Location.pp_file_pos location
    | SymAssign (loc, location) ->
        F.fprintf fmt "SymAssign (%a, %a)" Loc.pp loc Location.pp_file_pos location
    | UnknownFrom (pname, location) ->
        F.fprintf fmt "UnknownFrom (%a, %a)" Typ.Procname.pp pname Location.pp_file_pos location


  let pp : F.formatter -> t -> unit =
   fun fmt t ->
    let pp_sep fmt () = F.pp_print_string fmt " :: " in
    F.pp_print_list ~pp_sep pp_elem fmt t.trace


  let is_unknown_elem = function UnknownFrom _ -> true | _ -> false

  let has_unknown x = List.exists x.trace ~f:is_unknown_elem
end

module Set = struct
  include AbstractDomain.FiniteSet (BoTrace)

  (* currently, we keep only one trace for efficiency *)
  let join x y =
    if is_empty x then y
    else if is_empty y then x
    else
      let tx, ty = (min_elt x, min_elt y) in
      if Int.( <= ) tx.length ty.length then x else y


  let choose_shortest set = min_elt set

  let add_elem elem t =
    if is_empty t then singleton (BoTrace.singleton elem) else map (BoTrace.add_elem elem) t


  let instantiate ~traces_caller ~traces_callee location =
    if is_empty traces_caller then
      map
        (fun trace_callee -> BoTrace.add_elem_last (BoTrace.Call location) trace_callee)
        traces_callee
    else
      fold
        (fun trace_callee traces ->
          fold
            (fun trace_caller traces ->
              let new_trace_caller = BoTrace.add_elem (BoTrace.Call location) trace_caller in
              let new_trace = BoTrace.append trace_callee new_trace_caller in
              add new_trace traces )
            traces_caller traces )
        traces_callee empty


  let merge ~arr_traces ~idx_traces location =
    if is_empty idx_traces then
      map (fun arr_traces -> BoTrace.add_elem (BoTrace.ArrAccess location) arr_traces) arr_traces
    else
      fold
        (fun idx_traces traces ->
          fold
            (fun arr_traces traces ->
              let new_trace_idx = BoTrace.add_elem (BoTrace.ArrAccess location) idx_traces in
              let new_trace = BoTrace.append new_trace_idx arr_traces in
              add new_trace traces )
            arr_traces traces )
        idx_traces empty


  let has_unknown t = exists BoTrace.has_unknown t
end

include BoTrace
