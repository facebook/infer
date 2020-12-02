(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val automaton : unit -> ToplAutomaton.t
(** Return the automaton representing all Topl properties. *)

val is_shallow_active : unit -> bool
(** Return whether the Topl based on Sil instrumentation is active. *)

val is_deep_active : unit -> bool
(** Return whether PulseTopl is active. *)

val get_proc_attr : Procname.t -> ProcAttributes.t option
(** [get_proc_attr proc_name] returns the attributes of [get_proc_desc proc_name] *)

val get_proc_desc : Procname.t -> Procdesc.t option
(** Return a synthesized Procdesc.t, when it corresponds to instrumentation for a TOPL property. *)

val sourcefile : unit -> SourceFile.t
(** The (fake) sourcefile in which synthesized code resides. This function has a side-effect,
    because that's how [SourceFile] works, so do NOT call when Topl is inactive. *)

val cfg : unit -> Cfg.t
(** The CFG of the synthesized code. This function has a side-effect, because that's how [Cfg]
    works, so do NOT call when Topl is inactive.*)

type 'summary analysis = 'summary InterproceduralAnalysis.t -> 'summary option

type 'summary postprocess = 'summary InterproceduralAnalysis.t -> 'summary -> unit

type 'summary analysis_transformer = 'summary analysis -> 'summary analysis

val analyze_with : 'summary postprocess -> 'summary analysis_transformer
(** [analyze_with analyze postprocess analysis_data] calls [analyze] and then [postprocess] on
    [analysis_data] *)

val analyze_with_biabduction : BiabductionSummary.t analysis_transformer
(** Run biabduction with Topl instrumentation if active. Inserts calls to the TOPL automaton.
    Mutates the arguments: it is the caller's responsibility to instrument procedures at most once. *)
