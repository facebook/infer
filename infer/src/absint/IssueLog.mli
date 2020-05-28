(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for storing issues detected outside of per-procedure analysis (and hence not serialized
    as a part of procedure summary). *)
type t

val empty : t

val iter : f:(Procname.t -> Errlog.t -> unit) -> t -> unit
(** iterate a function on map contents *)

val get_or_add : proc:Procname.t -> t -> t * Errlog.t
(** Get the error log for a given procname. If there is none, add an empty one to the map. Return
    the resulting map together with the errlog. *)

val store : entry:ResultsDirEntryName.id -> file:SourceFile.t -> t -> unit
(** If there are any issues in the log, [store ~entry ~file] stores map to [infer-out/entry/file].
    Otherwise, no file is written. *)

val load : ResultsDirEntryName.id -> t
(** [load entry] walks [infer-out/entry], merging maps stored in files into one map. *)
