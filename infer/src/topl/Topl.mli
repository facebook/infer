(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val automaton : unit -> ToplAutomaton.t
(** Return the automaton representing all Topl properties. *)

val is_active : unit -> bool
(** Return whether PulseTopl is active. *)
