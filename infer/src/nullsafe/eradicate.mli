(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The main entry point for Nullsafe typechecker. *)

val analyze_procedure : IntraproceduralAnalysis.t -> NullsafeSummary.t option
(** Proc-level callback for nullsafe. *)

val analyze_for_immutable_cast_checker :
  TypeCheck.check_return_type -> IntraproceduralAnalysis.t -> NullsafeSummary.t option
(** For checkers that explore eradicate/nullsafe infra, but not part of nullsafe.Annot Call the
    given check_return_type at the end of every procedure. *)
