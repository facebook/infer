(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Given a sparse goal trace to follow through a program, compute at each
    basic block an upper bound on the distance to completion of that goal
    trace, and decorate those basic blocks by their respective distances.

    This distance computation is performed using a "tabulation" algorithm,
    building summaries of each procedure that describe the lengths of paths
    through that procedure as well as their effect on progress through the
    goal trace.

    Currently, we only provide a "top down" analysis, which begins from the
    given entrypoint and recursively computes all summaries needed to fully
    analyze the program.

    A bottom-up approach is also possible, by analyzing the procedures
    separately in reverse topological order. This has the advantage that
    summaries are always computed before they need to be applied, but may
    incur additional work by computing unneeded summaries. *)

val top_down :
     Llair.program
  -> entry:Llair.FuncName.t
  -> [`Call of Llair.FuncName.t | `Retn of Llair.FuncName.t] iarray
  -> (unit, Format.formatter -> unit) result
(** Returns [Ok ()] if a path was found and written to block metadata, and
    [Error dp_path] otherwise, with [dp_path] a delayed printer describing
    the specific trace segment that could not be followed. *)

val max_disjuncts : int ref
(** An upper bound on the number of constant-return summary disjuncts *)
