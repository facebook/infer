(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Stateful store of procedure and sourcefile dependencies found to be missing from the capture
    database during analysis. *)

module ProcUidSet : HashSet.S with type elt = string

type t = {procedures: ProcUidSet.t; sources: SourceFile.HashSet.t}

val record_procname : Procname.t -> unit

val record_sourcefile : SourceFile.t -> unit

val get : unit -> t
(** get recorded missing dependencies *)

val save : t list -> unit
(** destructively merge missing dependencies and store into the results directory *)

val load : unit -> t
(** load from results directory *)
