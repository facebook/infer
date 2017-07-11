(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

module BoTrace = struct
  type elem =
    | ArrAccess of Location.t
    | ArrDecl of Location.t
    | Assign of Location.t
    | Call of Location.t
    | Return of Location.t
    | SymAssign of Location.t
    [@@deriving compare]

  type t = {length: int; trace: elem list} [@@deriving compare]

  let empty = {length= 0; trace= []}

  let singleton elem = {length= 1; trace= [elem]}

  let add_elem elem t = {length= t.length + 1; trace= elem :: t.trace}

  let add_elem_last elem t = {length= t.length + 1; trace= t.trace @ [elem]}

  let append x y = {length= x.length + y.length; trace= x.trace @ y.trace}

  let pp_elem : F.formatter -> elem -> unit =
    fun fmt elem ->
      match elem with
      | Assign loc
       -> F.fprintf fmt "Assign (%a)" Location.pp_file_pos loc
      | ArrDecl loc
       -> F.fprintf fmt "ArrDecl (%a)" Location.pp_file_pos loc
      | Call loc
       -> F.fprintf fmt "Call (%a)" Location.pp_file_pos loc
      | Return loc
       -> F.fprintf fmt "Return (%a)" Location.pp_file_pos loc
      | SymAssign loc
       -> F.fprintf fmt "SymAssign (%a)" Location.pp_file_pos loc
      | ArrAccess loc
       -> F.fprintf fmt "ArrAccess (%a)" Location.pp_file_pos loc

  let pp : F.formatter -> t -> unit =
    fun fmt t ->
      let pp_sep fmt () = F.fprintf fmt " :: " in
      F.pp_print_list ~pp_sep pp_elem fmt t.trace
end

module Set = struct
  include AbstractDomain.FiniteSet (BoTrace)

  (* currently, we keep only one trace for efficiency *)
  let join x y =
    if is_empty x then y
    else if is_empty y then x
    else
      let tx, ty = (min_elt x, min_elt y) in
      if Pervasives.( <= ) tx.length ty.length then x else y

  let choose_shortest set = min_elt set

  let add_elem elem t =
    if is_empty t then singleton (BoTrace.singleton elem) else map (BoTrace.add_elem elem) t

  let add_elem_last elem t =
    if is_empty t then singleton (BoTrace.singleton elem) else map (BoTrace.add_elem_last elem) t

  let instantiate ~traces_caller ~traces_callee loc =
    if is_empty traces_caller then
      map (fun trace_callee -> BoTrace.add_elem_last (BoTrace.Call loc) trace_callee) traces_callee
    else
      fold
        (fun trace_callee traces ->
          fold
            (fun trace_caller traces ->
              let new_trace_caller = BoTrace.add_elem (BoTrace.Call loc) trace_caller in
              let new_trace = BoTrace.append trace_callee new_trace_caller in
              add new_trace traces)
            traces_caller traces)
        traces_callee empty

  let merge ~traces_arr ~traces_idx loc =
    if is_empty traces_idx then
      map (fun trace_arr -> BoTrace.add_elem (BoTrace.ArrAccess loc) trace_arr) traces_arr
    else
      fold
        (fun trace_idx traces ->
          fold
            (fun trace_arr traces ->
              let new_trace_idx = BoTrace.add_elem (BoTrace.ArrAccess loc) trace_idx in
              let new_trace = BoTrace.append new_trace_idx trace_arr in
              add new_trace traces)
            traces_arr traces)
        traces_idx empty
end

include BoTrace
