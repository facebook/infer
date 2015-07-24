(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Implementation of the Parallel Interprocedural Footprint Analysis Algorithm *)

(** Handle timeout events *)
module Timeout : sig
  val exe_timeout : int -> ('a -> 'b) -> 'a -> 'b option (* execute the function up to a given timeout given by the iterations parameter *)
end

val this_cluster_files : int ref (** Number of files in the current cluster *)
val tot_files : int ref (** Total number of files in all the clusters *)
val tot_files_done : int ref (** Total number of files done so far *)

(** {2 Algorithm} *)

val procs_become_done : Cg.t -> Procname.t -> Procname.t list

val post_process_procs : Exe_env.t -> Procname.t list -> unit

(** Return the list of procedures which should perform a phase
    transition from [FOOTPRINT] to [RE_EXECUTION] *)
val should_perform_transition : Cg.t -> Procname.t -> Procname.t list

(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
val transition_footprint_re_exe : Procname.t -> Prop.normal Specs.Jprop.t list -> unit

(** Update the specs of the current proc after the execution of one phase *)
val update_specs : Procname.t -> Specs.NormSpec.t list -> Specs.NormSpec.t list * bool

(** [parallel_iter_nodes tenv cfg call_graph analyze_proc process_result filter_out]
    executes [analyze_proc] in parallel as much as possible as allowed
    by the call graph, and applies [process_result] to the result as
    soon as it is returned by a child process. If [filter_out] returns
    true, no execution. *)
val parallel_iter_nodes : Exe_env.t -> (Exe_env.t -> Procname.t -> Specs.summary) -> (Exe_env.t -> (Procname.t * Cg.in_out_calls) -> Specs.summary -> unit) -> (Cg.t -> Procname.t -> bool) -> unit

(** print the timing stats, and generate timing.dot if in dotty mode *)
val print_timing : unit -> unit
