(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to store a map from procnames to error logs.  Starts with an empty map. *)

val iter : (Typ.Procname.t -> Errlog.t -> unit) -> unit
(** iterate a function on map contents *)

val get_errlog : Typ.Procname.t -> Errlog.t
(** Get the error log for a given procname.  If not present, then add the association from
    procname to an empty error log and return the latter. *)

val store : string -> SourceFile.t -> unit
(** If there are any issues in the log, [store dirname filename] stores map to [infer-out/dirname/filename].
    Otherwise, no file is written. *)

val load : string -> unit
(** [load directory] resets the issue map first, then walks [infer-out/directory], merging all
    maps stored in the found files into the current map.  *)
