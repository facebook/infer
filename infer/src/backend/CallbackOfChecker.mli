(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Conversions from checkers taking "functional" {!Interprocedural.t} et al. payloads to
    {!Callbacks.proc_callback_t} and friends. *)

val interprocedural :
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

val intraprocedural_with_payload :
     (Payloads.t, 'payload option) Field.t
  -> (IntraproceduralAnalysis.t -> 'payload option)
  -> Callbacks.proc_callback_t
(** runs an intra-procedural analysis that nonetheless produces a payload *)
