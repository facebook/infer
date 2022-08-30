(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = private int [@@deriving compare, equal]

val t0 : t

val pp : F.formatter -> t -> unit

val incr : t -> t

(** A trace of timestamps: a way of recording evolution of a timestamp over the course of analysis.
    In particular, this can permit lightweight comparison of histories across interprocedural calls. *)
type trace = private t list [@@deriving compare, equal]

val trace0 : t -> trace

val pp_trace : F.formatter -> trace -> unit

val add_to_trace : trace -> t -> trace
