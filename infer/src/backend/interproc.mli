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

(** Perform the analysis of an exe_env *)
val do_analysis : Exe_env.t -> unit

(** Print the stats for all the files in the exe_env *)
val print_stats : Exe_env.t -> unit
