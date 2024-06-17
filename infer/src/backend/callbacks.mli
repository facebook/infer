(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to register and invoke checkers' callbacks. *)

(* There are two types of callbacks:
   1) Procedure level callback.
      The checker is responsible for updating checker-specific summary artifacts for that procedure, including
      writing errors that were detected during per-procedure analysis.
   2) File level callback. Guaranteed to be invoked after all procedure-level callbacks for this files are invoked,
      and generated summaries are serialized.
      The checker should return an issue log containing list of additional errors to be added
      to ones produced on procedure-level stage.
      NOTE: The checker SHALL NOT modify summaries at this point, including updating
      summaries' error log.
*)

(** Type of a procedure callback:

    - List of all the procedures the callback will be called on.
    - get_proc_desc to get a proc desc from a proc name
    - Type environment.
    - Procedure for the callback to act on. *)
type proc_callback_args = {summary: Summary.t; exe_env: Exe_env.t; proc_desc: Procdesc.t}

(* Result is updated summary with all information relevant for the checker (including list of errors found by the checker for this procedure *)
type proc_callback_t = proc_callback_args -> Summary.t

type proc_callback_with_specialization_t = ?specialization:Specialization.t -> proc_callback_t

type file_callback_args =
  {procedures: Procname.t list; source_file: SourceFile.t; exe_env: Exe_env.t}

(** Result is a list of additional issues found at this stage (complementary to issues generated on
    per-procedure analysis stage) *)
type file_callback_t = file_callback_args -> IssueLog.t

val register_procedure_callback :
  Checker.t -> ?dynamic_dispatch:bool -> Language.t -> proc_callback_t -> unit
(** Register a procedure callback (see details above) *)

val register_procedure_callback_with_specialization :
     Checker.t
  -> ?dynamic_dispatch:bool
  -> Language.t
  -> proc_callback_with_specialization_t
  -> is_already_specialized:(Specialization.t -> Summary.t -> bool)
  -> unit
(** Same as [register_procedure_callback] with specialization *)

val register_file_callback : Checker.t -> Language.t -> file_callback_t -> unit
(** Register a file callback (see details above). [issues_dir] must be unique for this type of
    checker. *)

val iterate_procedure_callbacks :
     Exe_env.t
  -> AnalysisRequest.t
  -> ?specialization:Specialization.t
  -> Summary.t
  -> Procdesc.t
  -> Summary.t
(** Invoke all registered procedure callbacks on the given procedure. *)

val is_specialized_for : Specialization.t -> Summary.t -> bool
(** Check if all callbacks are specialized wrt the given specialization *)

val iterate_file_callbacks_and_store_issues : Procname.t list -> Exe_env.t -> SourceFile.t -> unit
(** Invoke all registered file callbacks on a file, and store produced errors in a corresponding
    directory. Guaranteed to be called after all procedure-level callbacks are invoked *)
