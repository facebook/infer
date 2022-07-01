(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for on-demand analysis. *)

val analyze_proc_name : Exe_env.t -> caller_summary:Summary.t -> Procname.t -> Summary.t option
(** [analyze_proc_name exe_env ~caller_summary callee_pname] performs an on-demand analysis of
    [callee_pname] triggered during the analysis of [caller_summary] *)

val analyze_proc_name_no_caller : Exe_env.t -> Procname.t -> Summary.t option
(** [analyze_proc_name_no_caller exe_env callee_pname] performs an on-demand analysis of
    [callee_pname] triggered by the top-level of a file-level checker. This must not be used in any
    other context, as this will break incremental analysis. *)

module LocalCache : sig
  val clear : unit -> unit
  (** Empty the cache of ondemand results *)

  val remove : Procname.t -> unit
  (** Remove an element from the cache of ondemand results *)
end

val analyze_file : Exe_env.t -> SourceFile.t -> unit
(** Invoke all the callbacks registered in {!Callbacks} on the given file. *)

val analyze_proc_name_toplevel : Exe_env.t -> Procname.t -> unit
(** Invoke all the callbacks registered in {!Callbacks} on the given procedure. *)
