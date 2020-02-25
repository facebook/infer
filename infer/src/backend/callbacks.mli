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
      The checker is responsible for doing any additional work, but SHALL NOT modify summaries at this point, including updating
      summaries' error log.
      Additional errors can be issued at this stage using capabilities of [IssueLog].
 *)

type proc_callback_args =
  {get_procs_in_file: Procname.t -> Procname.t list; summary: Summary.t; exe_env: Exe_env.t}

(** Type of a procedure callback:

    - List of all the procedures the callback will be called on.
    - get_proc_desc to get a proc desc from a proc name
    - Type environment.
    - Procedure for the callback to act on. *)
type proc_callback_t = proc_callback_args -> Summary.t

type file_callback_args =
  {procedures: Procname.t list; source_file: SourceFile.t; exe_env: Exe_env.t}

type file_callback_t = file_callback_args -> unit

val register_procedure_callback :
  checker_name:string -> ?dynamic_dispatch:bool -> Language.t -> proc_callback_t -> unit
(** Register a procedure callback (see details above) *)

val register_file_callback : checker_name:string -> Language.t -> file_callback_t -> unit
(** Register a file callback (see details above) *)

val iterate_procedure_callbacks : Exe_env.t -> Summary.t -> Summary.t
(** Invoke all registered procedure callbacks on the given procedure. *)

val iterate_file_callbacks : Procname.t list -> Exe_env.t -> SourceFile.t -> unit
(** Invoke all registered file callbacks on a file. Guaranteed to be called after all
    procedure-level callbacks are invoked *)
