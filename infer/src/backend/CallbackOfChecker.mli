(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Conversions from checkers taking "functional" {!Absint.InterproceduralAnalysis.t} et al.
    payloads to {!Callbacks.proc_callback_t} and friends. *)

val mk_interprocedural_field_t :
     (Payloads.t, 'payload option Lazy.t) Field.t
  -> Callbacks.proc_callback_args
  -> ?tenv:Tenv.t
  -> unit
  -> 'payload InterproceduralAnalysis.t * Summary.Stats.t ref

val interprocedural :
     f_analyze_dep:('payloads_orig -> 'payloads option)
  -> get_payload:(Payloads.t -> 'payloads_orig)
  -> set_payload:(Payloads.t -> 'payload_checker Lazy.t -> Payloads.t)
  -> ('payloads InterproceduralAnalysis.t -> 'payload_checker)
  -> Callbacks.proc_callback_t
(** the general form of interprocedural checkers: can read and update several payloads, and massage
    analysis results (mostly used to join option types) *)

val interprocedural_with_field :
     (Payloads.t, 'payload option Lazy.t) Field.t
  -> ('payload InterproceduralAnalysis.t -> 'payload option)
  -> Callbacks.proc_callback_t
(** [interprocedural_with_field field checker] expects [checker] to compute a payload (option)
    suitable for [field], given an inter-procedural analysis of callees that computes the same
    payload type *)

val interprocedural_with_field_and_specialization :
     (Payloads.t, 'payload option Lazy.t) Field.t
  -> (   ?specialization:'payload * Specialization.t
      -> 'payload InterproceduralAnalysis.t
      -> 'payload option )
  -> Callbacks.proc_callback_with_specialization_t
(** same as [interprocedural_with_field] but allowing specialization *)

val make_is_already_specialized_test :
     (Payloads.t, 'payload option Lazy.t) Field.t
  -> (Specialization.t -> 'payload -> bool)
  -> Specialization.t
  -> Summary.t
  -> bool

val interprocedural_with_field_dependency :
     dep_field:(Payloads.t, 'payload_dep Lazy.t) Field.t
  -> (Payloads.t, 'payload Lazy.t) Field.t
  -> (('payload * 'payload_dep) InterproceduralAnalysis.t -> 'payload_dep -> 'payload)
  -> Callbacks.proc_callback_args
  -> Summary.t
(** An inter-procedural analysis that depends on the summary payload found by another one for a
    procedure to analyse the same procedure. The checker will be passed that payload dependency as
    an argument for the procedure being analysed. The [InterproceduralAnalysis] argument allows
    retrieving both the dependency payload and the "current" one on other procedures. *)

val interprocedural_file :
     (Payloads.t, 'payload option Lazy.t) Field.t
  -> ('payload InterproceduralAnalysis.file_t -> IssueLog.t)
  -> Callbacks.file_callback_t
(** [interprocedural_file field checker] expects [checker] to compute an {!Absint.IssueLog.t} from
    the file-level analysis, given an inter-procedural analysis of dependencies that computes the
    payload type corresponding to [field] *)

val intraprocedural : (IntraproceduralAnalysis.t -> unit) -> Callbacks.proc_callback_t
(** runs a simple intra-procedural analysis (one that doesn't need the results of the analysis on
    any transitive dependencies to analyze a given procedure) *)

val intraprocedural_with_field_dependency :
     (Payloads.t, 'payload Lazy.t) Field.t
  -> (IntraproceduralAnalysis.t -> 'payload -> unit)
  -> Callbacks.proc_callback_t
(** an intra-procedural analysis that depends on the summary payload found by another *)
