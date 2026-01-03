(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseDomainInterface

val implies : AbductiveDomain.t -> AbductiveDomain.t -> bool
(** [implies lhs rhs] is [true] if there is a renaming [σ] of abstract values in [rhs.current_state]
    such that [lhs.current_state ⊢ σ(rhs.current_state) ∧ σ(lhs.path_condition)]. Assuming [lhs] is
    the analysis state after going through a loop body and [rhs] is the state before, this tests
    whether it is possible to satisfy the loop conditions again. If so, following the UNTER theory,
    an infinite loop is detected. *)
