(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to read specific lines from files. The data from any file will stay in memory until the
    handle is collected by the gc *)

type t

val create : unit -> t

val from_loc : t -> Location.t -> string option
(** get the line from a location looking for the copy of the file in the results dir *)

val iteri : t -> SourceFile.t -> f:(int -> string -> unit) -> unit
(** iterate on the lines of the file, with line numbers *)
