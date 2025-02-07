(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Config = sig
  val loop_bound : int
  (** Bound on number of loop iterations *)

  val switch_bound : int
  (** Bound on number of context switches *)

  val function_summaries : bool
  (** Use function summarization *)

  val entry_points : string list
  (** C linkage names of entry points *)

  val globals : Domain_used_globals.used_globals
  (** results of used globals pre-analysis *)
end
