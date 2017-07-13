(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** In-memory representation of multilink files. *)
type t

val add : t -> string -> unit
(** Add a link. *)

val create : unit -> t
(** Create a new multilink. *)

val multilink_file_name : string
(** Name of the multilink file.
    A multilink file is recognized by its file name. *)

val read : dir:string -> t option
(** Read a multilink file from disk. *)

val resolve : DB.filename -> DB.filename
(** Resolve a filename following multilinks.
    The cache is updated if a new multilinks file is read. *)

val reset_cache : unit -> unit
(** Reset the cache of multilink files *)

val write : t -> dir:string -> unit
(** Write a multilink file in the given directory *)
