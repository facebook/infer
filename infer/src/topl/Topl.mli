(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_active : unit -> bool
(** Returns whether the TOPL analysis is active. *)

val get_proc_attr : Procname.t -> ProcAttributes.t option
(** [get_proc_attr proc_name] returns the attributes of [get_proc_desc proc_name] *)

val get_proc_desc : Procname.t -> Procdesc.t option
(** Returns a synthesized Procdesc.t, when it corresponds to instrumentation for a TOPL property. *)

val sourcefile : unit -> SourceFile.t
(** The (fake) sourcefile in which synthesized code resides. This function has a side-effect,
    because that's how [SourceFile] works, so do NOT call when Topl is inactive. *)

val cfg : unit -> Cfg.t
(** The CFG of the synthesized code. This function has a side-effect, because that's how [Cfg]
    works, so do NOT call when Topl is inactive.*)

val instrument_callback :
     (BiabductionSummary.t InterproceduralAnalysis.t -> BiabductionSummary.t option)
  -> BiabductionSummary.t InterproceduralAnalysis.t
  -> BiabductionSummary.t option
(** Run biabduction with Topl instrumentation if active. Inserts calls to the TOPL automaton.
    Mutates the arguments: it is the caller's responsibility to instrument procedures at most once. *)
