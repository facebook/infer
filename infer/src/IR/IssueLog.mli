(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module to store a map from procnames to error logs.  Starts with an empty map. *)

val get_map : unit -> Errlog.t Typ.Procname.Map.t

val exist_issues : unit -> bool

val get_err_log : Typ.Procname.t -> Errlog.t
(** Get the error log for a given procname.  If not present, then add the association from 
    procname to an empty error log and return the latter. *)

val store : DB.filename -> unit
(** Store map to a file *)

val load : string -> unit
(** Reset the issue map first, then walk the directory given as argument and merge all
    maps stored in the found files into the current map.  *)
