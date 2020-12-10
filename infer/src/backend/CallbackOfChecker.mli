(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Conversions from checkers taking "functional" {!Interprocedural.t} et al. payloads to
    {!Callbacks.proc_callback_t} and friends. *)

val mk_interprocedural_field_t :
     (Payloads.t, 'payload option) Field.t
  -> Exe_env.t
  -> Summary.t
  -> ?tenv:Tenv.t
  -> unit
  -> 'payload InterproceduralAnalysis.t * Summary.Stats.t ref

val interprocedural :
     f_analyze_dep:(Procdesc.t -> 'payloads_orig -> (Procdesc.t * 'payloads) option)
  -> get_payload:(Payloads.t -> 'payloads_orig)
  -> set_payload:(Payloads.t -> 'payload_checker -> Payloads.t)
  -> ('payloads InterproceduralAnalysis.t -> 'payload_checker)
  -> Callbacks.proc_callback_t
(** the general form of interprocedural checkers: can read and update several payloads, and massage
    analysis results (mostly used to join option types) *)

val interprocedural_with_field :
     (Payloads.t, 'payload option) Field.t
  -> ('payload InterproceduralAnalysis.t -> 'payload option)
  -> Callbacks.proc_callback_t
(** [interprocedural field checker] expects [checker] to compute a payload (option) suitable for
    [field], given an inter-procedural analysis of callees that computes the same payload type *)

val interprocedural_file :
     (Payloads.t, 'payload option) Field.t
  -> ('payload InterproceduralAnalysis.file_t -> IssueLog.t)
  -> Callbacks.file_callback_t
(** [interprocedural_file field checker] expects [checker] to compute an {!IssueLog.t} from the
    file-level analysis, given an inter-procedural analysis of dependencies that computes the
    payload type corresponding to [field] *)

val intraprocedural : (IntraproceduralAnalysis.t -> unit) -> Callbacks.proc_callback_t
(** runs a simple intra-procedural analysis (one that doesn't need the results of the analysis on
    any transitive dependencies to analyze a given procedure) *)

val intraprocedural_with_field_dependency :
     (Payloads.t, 'payload) Field.t
  -> (IntraproceduralAnalysis.t -> 'payload -> unit)
  -> Callbacks.proc_callback_t
(** an intra-procedural analysis that depends on the summary payload found by another *)

val intraprocedural_with_field :
     (Payloads.t, 'payload option) Field.t
  -> (IntraproceduralAnalysis.t -> 'payload option)
  -> Callbacks.proc_callback_t
(** runs an intra-procedural analysis that nonetheless produces a payload *)
