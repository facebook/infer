(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Interprocedural Analysis *)

(** Run the biabduction analysis on the given procedure *)
val analyze_procedure : Callbacks.proc_callback_t

(** Create closures to perform the analysis of an exe_env *)
val do_analysis_closures : Exe_env.t -> Tasks.closure list

(** Print the stats for all the files in the cluster *)
val print_stats : Cluster.t -> unit
