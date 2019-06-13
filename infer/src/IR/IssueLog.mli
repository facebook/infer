(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for maps from procnames to error logs. *)
type t

val empty : t

val iter : f:(Typ.Procname.t -> Errlog.t -> unit) -> t -> unit
(** iterate a function on map contents *)

val get_or_add : proc:Typ.Procname.t -> t -> t * Errlog.t
(** Get the error log for a given procname. If there is none, add an empty one to the map. 
    Return the resulting map together with the errlog. *)

val store : dir:string -> file:SourceFile.t -> t -> unit
(** If there are any issues in the log, [store ~dir ~file] stores map to [infer-out/dir/file].
    Otherwise, no file is written. *)

val load : string -> t
(** [load directory] walks [infer-out/directory], merging maps stored in files into one map.  *)
