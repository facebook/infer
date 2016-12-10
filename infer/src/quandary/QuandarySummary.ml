(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** summary type for Quandary taint analysis *)

open! IStd

module F = Format
module L = Logging

type input =
  | In_formal of int * AccessPath.t
  (** input flows via parameter at given index at offset in given access path *)
  | In_global of AccessPath.t
  (** input flows from global at offset in given access path *)
  | In_empty
  (** no input value *)

type output =
  | Out_formal of int * AccessPath.t
  (** output flows into parameter at given index at offset in given access path *)
  | Out_global of AccessPath.t
  (** output flows into global at offset in given access path *)
  | Out_return of AccessPath.t
  (** output flows into return value at offset in given access path *)

(** enumeration of all the different trace types that are possible (just Java, for now). needed
    because we can't functorize Specs.summary's *)
type summary_trace =
  | Java of JavaTrace.t
  | Cpp of CppTrace.t
  | Unknown

(** input-output summary for a pair of values. intuitively, read it as
    "if [input] is associated with trace T before the call, then [output] is
    associated with trace (T + [output_trace]) after the call" (where `+` denotes trace
    concatenation). *)
type in_out_summary =
  {
    input : input;
    output : output;
    output_trace : summary_trace;
  }

type t = in_out_summary list

let make_formal_input index access_path =
  In_formal (index, access_path)

let make_global_input access_path =
  In_global access_path

let empty_input = In_empty

let make_formal_output index access_path =
  Out_formal (index, access_path)

let make_global_output access_path =
  Out_global access_path

let make_return_output access_path =
  Out_return access_path

let make_in_out_summary input output output_trace =
  { input; output; output_trace; }

let pp_trace fmt = function
  | Java trace -> JavaTrace.pp fmt trace
  | Cpp trace -> CppTrace.pp fmt trace
  | Unknown -> assert false

let pp_input fmt = function
  | In_formal (index, access_path) -> F.fprintf fmt "%a (formal %d)" AccessPath.pp access_path index
  | In_global access_path -> F.fprintf fmt "%a (global)" AccessPath.pp access_path
  | In_empty -> F.fprintf fmt "_"

let pp_output fmt = function
  | Out_formal (index, access_path) ->
      F.fprintf fmt "%a (formal %d)" AccessPath.pp access_path index
  | Out_global access_path ->
      F.fprintf fmt "%a (global)" AccessPath.pp access_path
  | Out_return access_path ->
      F.fprintf fmt "%a (return)" AccessPath.pp access_path

let pp_in_out_summary fmt summary =
  match summary.input with
  | In_empty ->
      F.fprintf fmt
        "%a => (%a, %a)"
        pp_input summary.input
        pp_output summary.output
        pp_trace summary.output_trace
  | In_formal _ | In_global _ ->
      F.fprintf fmt
        "(%a, T) => (%a, T :: %a)"
        pp_input summary.input
        pp_output summary.output
        pp_trace summary.output_trace

let pp fmt t =
  PrettyPrintable.pp_collection ~pp_item:pp_in_out_summary fmt t
