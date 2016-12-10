(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(** summary type for Quandary taint analysis *)

module F = Format

type input =
  private
  | In_formal of int * AccessPath.t
  (** input flows via parameter at given index at offset in given access path *)
  | In_global of AccessPath.t
  (** input flows from global at offset in given access path *)
  | In_empty
  (** no input value *)

type output =
  private
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
  private
  {
    input : input;
    output : output;
    output_trace : summary_trace;
  }

type t = in_out_summary list

val make_formal_input : int -> AccessPath.t -> input

val make_global_input : AccessPath.t -> input

val empty_input : input

val make_formal_output : int -> AccessPath.t -> output

val make_global_output : AccessPath.t -> output

val make_return_output : AccessPath.t -> output

val make_in_out_summary : input -> output -> summary_trace -> in_out_summary

val pp_trace : F.formatter -> summary_trace -> unit

val pp_input : F.formatter -> input -> unit

val pp_output : F.formatter -> output -> unit

val pp_in_out_summary : F.formatter -> in_out_summary -> unit

val pp : F.formatter -> t -> unit
