(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Main module for the analysis after the capture phase *)

val main : changed_files:SourceFile.Set.t option -> unit
(** Given a name of the Makefile to use for multicore analysis, analyze the captured code *)

val invalidate_changed_procedures : SourceFile.Set.t option -> unit
(** Invalidate specs files for procedures that have changed. Used for incremental analysis. *)
