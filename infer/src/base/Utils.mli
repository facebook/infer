(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val initial_times : Unix.process_times
(** initial process times *)

val initial_timeofday : float
(** precise time of day at the start of the analysis *)

val string_crc_hex32 : string -> string
(** Compute a 32-character hexadecimal crc using the Digest module  *)

val copy_file : string -> string -> int option
(** copy a source file, return the number of lines, or None in case of error *)

val read_file : string -> (string list, string) Result.t
(** read a source file and return a list of lines *)

val filename_to_absolute : root:string -> string -> string
(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)

val filename_to_relative : root:string -> string -> string option
(** Convert an absolute filename to one relative to a root directory.  Returns [None] if filename is
    not under root. *)

(** type for files used for printing *)
type outfile =
  { fname: string  (** name of the file *)
  ; out_c: Out_channel.t  (** output channel *)
  ; fmt: Format.formatter  (** formatter for printing *) }

val create_outfile : string -> outfile option
(** create an outfile for the command line, the boolean indicates whether to do demangling when closing the file *)

val do_outf : outfile option -> (outfile -> unit) -> unit
(** operate on an outfile reference if it is not None *)

val close_outf : outfile -> unit
(** close an outfile *)

val directory_fold : ('a -> string -> 'a) -> 'a -> string -> 'a
(** Functional fold function over all the file of a directory *)

val directory_iter : (string -> unit) -> string -> unit
(** Functional iter function over all the file of a directory *)

val directory_is_empty : string -> bool
(** Returns true if a given directory is empty. The directory is assumed to exist. *)

val read_json_file : string -> (Yojson.Basic.json, string) Result.t

val with_file_in : string -> f:(In_channel.t -> 'a) -> 'a

val with_file_out : string -> f:(Out_channel.t -> 'a) -> 'a

val write_json_to_file : string -> Yojson.Basic.json -> unit

val consume_in : In_channel.t -> unit

val with_process_in : string -> (In_channel.t -> 'a) -> 'a * Unix.Exit_or_signal.t

val shell_escape_command : string list -> string

val create_dir : string -> unit
(** create a directory if it does not exist already *)

val realpath : ?warn_on_error:bool -> string -> string
(** [realpath warn_on_error path] returns path with all symbolic links resolved.
    It caches results of previous calls to avoid expensive system calls.
    WARNING: If warn_on_error is false, no warning will be shown whenever an error occurs for
    the given path (e.g. if it does not exist). *)

val suppress_stderr2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(** wraps a function expecting 2 arguments in another that temporarily redirects stderr to /dev/null
    for the duration of the function call *)

val compare_versions : string -> string -> int
(** [compare_versions v1 v2] returns 1 if  v1 is newer than v2,
    -1 if v1 is older than v2 and 0 if they are the same version.
    The versions are strings of the shape "n.m.t", the order is lexicographic. *)

val write_file_with_locking : ?delete:bool -> f:(Out_channel.t -> unit) -> string -> unit
(** Lock file passed as argument and write into it using [f]. If [delete] then the file is unlinked
    once this is done. *)

val rmtree : string -> unit
(** [rmtree path] removes [path] and, if [path] is a directory, recursively removes its contents *)
