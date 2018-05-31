(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
