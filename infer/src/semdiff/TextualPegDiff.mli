(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Semantic equivalence checking of Textual procedures via PEG equality saturation.

    Builds PEGs for two procedures in a shared e-graph, applies rewrite rules to saturation, and
    checks whether the two root atoms are equivalent. *)

open! IStd

val check_equivalence : ?debug:bool -> Textual.ProcDesc.t -> Textual.ProcDesc.t -> bool

val check_b007_migration : ?debug:bool -> Textual.ProcDesc.t -> Textual.ProcDesc.t -> bool
(** Directional migration check: verify that [proc_new] is a valid B007 simplification of
    [proc_old]. Uses accept rules instead of bidirectional rewrites. *)

val convert_and_print : ?debug:bool -> string -> unit
(** Parse a Textual source string, convert each procedure to PEG, print the equations and nested
    term. For expect tests. *)
