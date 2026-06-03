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
[@@warning "-unused-value-declaration"]
(** used only by unit tests *)

val check_b007_migration : ?debug:bool -> Textual.ProcDesc.t -> Textual.ProcDesc.t -> bool
(** Directional migration check: verify that [proc_new] is a valid B007 simplification of
    [proc_old]. Uses accept rules instead of bidirectional rewrites. *)

val b006_named_rules : (string * string) list
(** The B006 rewrite rules as (label, rule text) pairs. Exposed for per-rule unit testing. *)

val check_b006_migration :
     ?debug:bool
  -> defaults_old:Textual.Exp.t IString.Map.t
  -> defaults_new:Textual.Exp.t IString.Map.t
  -> Textual.ProcDesc.t
  -> Textual.ProcDesc.t
  -> bool
(** Directional migration check for B006 (mutable default argument): verify that [proc_new] is a
    valid simplification of [proc_old], where a mutable default has been replaced by [None] plus an
    "if p is None: p = <literal>" guard. [defaults_old]/[defaults_new] are the parameter default
    maps (from {!StructuredPeg.extract_defaults}). Equivalence holds under the correct-execution
    hypothesis that explicitly-passed arguments are not [None]. *)

val convert_and_print : ?debug:bool -> string -> unit
[@@warning "-unused-value-declaration"]
(** Parse a Textual source string, convert each procedure to PEG, print the equations and nested
    term. For expect tests. *)
