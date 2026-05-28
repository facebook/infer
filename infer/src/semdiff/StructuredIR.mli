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
