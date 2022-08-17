(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int [@@deriving compare, equal]

let t0 = 0

let pp = F.pp_print_int

let incr t = t + 1

(** A trace of timestamps: a way of recording evolution of a timestamp over the course of analysis.
    In particular, this can permit lightweight comparison of histories across interprocedural calls. *)
type trace = t list [@@deriving compare, equal]

let trace0 time = [time]

let pp_trace = Pp.seq ~sep:"," pp

let add_to_trace trace time = time :: trace
