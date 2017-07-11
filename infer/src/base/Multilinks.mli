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

(** Add a link. *)

val add : t -> string -> unit

(** Create a new multilink. *)

val create : unit -> t

(** Name of the multilink file.
    A multilink file is recognized by its file name. *)

val multilink_file_name : string

(** Read a multilink file from disk. *)

val read : dir:string -> t option

(** Resolve a filename following multilinks.
    The cache is updated if a new multilinks file is read. *)

val resolve : DB.filename -> DB.filename

(** Reset the cache of multilink files *)

val reset_cache : unit -> unit

(** Write a multilink file in the given directory *)

val write : t -> dir:string -> unit
