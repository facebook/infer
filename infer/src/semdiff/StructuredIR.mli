(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Structured control-flow IR following Ramsey's Wasm-style design (ICFP 2022).

    The type [t] mirrors WebAssembly's control-flow constructs: [Block], [Loop], [If] create nesting
    levels targeted by [Branch] (multilevel break/continue). Sequential composition is expressed via
    [Seq]. *)

open! IStd

type label = Textual.NodeName.t

type t =
  | Instrs of {label: label; instrs: Textual.Instr.t list}
  | Seq of t * t
  | Block of t
  | Loop of {label: label; body: t}
  | If of {label: label; bexp: Textual.BoolExp.t; then_: t; else_: t}
  | Branch of int
  | Return of {label: label; exp: Textual.Exp.t}
  | Throw of {label: label; exp: Textual.Exp.t}

val pp : Format.formatter -> t -> unit

val to_cfg : t -> Textual.Node.t list * Textual.NodeName.t
(** Convert structured IR back to a Textual CFG (list of basic blocks + start label). Used for
    round-trip testing. *)

val successor_labels : Textual.Terminator.t -> Textual.NodeName.t list

val reverse_postorder : Textual.Node.t list -> Textual.NodeName.t -> int Textual.NodeName.Map.t
(** [reverse_postorder nodes start] computes a reverse postorder numbering of the CFG. The entry
    node [start] gets number 0; larger numbers are further from the entry. *)

val compute_idom :
  Textual.Node.t list -> Textual.NodeName.t -> Textual.NodeName.t Textual.NodeName.Map.t
(** [compute_idom nodes start] computes immediate dominators using Cooper, Harvey & Kennedy's
    iterative algorithm. Returns a map from each node to its immediate dominator. The entry node
    maps to itself. *)

val dominator_children :
  Textual.NodeName.t Textual.NodeName.Map.t -> Textual.NodeName.t list Textual.NodeName.Map.t
(** [dominator_children idom] inverts the immediate-dominator map to produce a map from each node to
    its children in the dominator tree. *)

val of_cfg : Textual.Node.t list -> Textual.NodeName.t -> t
(** [of_cfg nodes start] translates a Textual CFG into structured IR using Ramsey's doTree
    algorithm. Produces Wasm-style [t] with [Block], [Loop], [If], and [Branch] depth counting. *)
