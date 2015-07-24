(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Interprocedural Analysis *)

(** Analyze [proc_name] and return the updated summary. Use module
    {!Timeout } to call {!perform_analysis_phase } with a time limit, and
    then return the updated summary. Executed as a child process. *)
val analyze_proc : Exe_env.t -> Procname.t -> Specs.summary

(** Process the result of the analysis of [proc_name]: update the
    returned summary and add it to the spec table. Executed in the
    parent process as soon as a child process returns a result. *)
val process_result : Exe_env.t -> (Procname.t * Cg.in_out_calls) -> Specs.summary -> unit

(** Return true if the analysis of [proc_name] should be
    skipped. Called by the parent process before attempting to analyze a
    proc. *)
val filter_out : Cg.t -> Procname.t -> bool

(** Perform the analysis of an exe_env *)
val do_analysis : Exe_env.t -> unit

(** Print the stats for all the files in the exe_env *)
val print_stats : Exe_env.t -> unit
