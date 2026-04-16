(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Program Equivalence Graph (PEG) built from Textual IR.

    A PEG is a pure functional representation of an imperative program where control flow is encoded
    as data: branches become [@phi(cond, then, else)] nodes, side effects are ordered by state
    tokens [@seq(state, effect)], and the procedure result is [@ret(final_state, return_value)]. Two
    programs are semantically equivalent when their PEG roots belong to the same e-class after
    equality saturation. See Tate et al., "Equality Saturation: a New Approach to Optimization",
    POPL 2009.

    This module converts a Textual procedure into PEG terms inside a {!CongruenceClosureSolver}
    e-graph. Python-specific: interprets [py_store_fast] / [py_load_fast] to build proper SSA with
    [@phi] nodes at control-flow join points rather than treating the mutable [*PyLocals] frame as
    opaque.

    Phase 1 handles loop-free procedures only (no back-edges). *)

open! IStd
module CC = CongruenceClosureSolver

(** {2 Equations trace}

    Records every binding produced during PEG construction. Useful for debugging and for
    understanding the translation. *)

module Equations : sig
  type t

  val pp : CC.t -> Format.formatter -> t -> unit
end

(** {2 Construction} *)

val convert_proc :
  ?theta_counter:int ref -> CC.t -> Textual.ProcDesc.t -> (CC.Atom.t * Equations.t, string) result
(** [convert_proc cc proc] converts a loop-free Textual procedure into e-graph terms in [cc].
    Returns the root atom and the full equation trace, or [Error msg] for back-edges / unsupported
    constructs. *)

(** {2 Pretty-printing} *)

val pp_tree : ?depth:int -> CC.t -> Format.formatter -> CC.Atom.t -> unit
(** [pp_tree cc fmt atom] prints the PEG rooted at [atom] as an ASCII tree. *)
