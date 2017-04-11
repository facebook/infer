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

(** initial process times *)
val initial_times : Unix.process_times

(** precise time of day at the start of the analysis *)
val initial_timeofday : float

(** Compute a 32-character hexadecimal crc using the Digest module  *)
val string_crc_hex32 : string -> string

(** copy a source file, return the number of lines, or None in case of error *)
val copy_file : string -> string -> int option

(** read a source file and return a list of lines, or None in case of error *)
val read_file : string -> string list option

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
val filename_to_absolute : root:string -> string -> string

(** Convert an absolute filename to one relative to a root directory.  Returns [None] if filename is
    not under root. *)
val filename_to_relative : root:string -> string -> string option

(** type for files used for printing *)
type outfile =
  { fname : string; (** name of the file *)
    out_c : out_channel; (** output channel *)
    fmt : Format.formatter (** formatter for printing *) }

(** create an outfile for the command line, the boolean indicates whether to do demangling when closing the file *)
val create_outfile : string -> outfile option

(** operate on an outfile reference if it is not None *)
val do_outf : outfile option -> (outfile -> unit) -> unit

(** close an outfile *)
val close_outf : outfile -> unit

(** Functional fold function over all the file of a directory *)
val directory_fold : ('a -> string -> 'a) -> 'a -> string -> 'a

(** Functional iter function over all the file of a directory *)
val directory_iter : (string -> unit) -> string -> unit

(** Returns true if a given directory is empty. The directory is assumed to exist. *)
val dir_is_empty : string -> bool

val read_json_file : string -> (Yojson.Basic.json, string) Result.t

val with_file_in : string -> f:(In_channel.t -> 'a) -> 'a
val with_file_out : string -> f:(Out_channel.t -> 'a) -> 'a

val write_json_to_file : string -> Yojson.Basic.json -> unit

val consume_in : in_channel -> unit

val with_process_in : string -> (in_channel -> 'a) -> ('a * Unix.Exit_or_signal.t)

val shell_escape_command : string list -> string

(** create a directory if it does not exist already *)
val create_dir : string -> unit

(** [realpath path] returns path with all symbolic links resolved. It caches results of previous
    calls to avoid expensive system calls *)
val realpath : string -> string

(** wraps a function expecting 2 arguments in another that temporarily redirects stderr to /dev/null
    for the duration of the function call *)
val suppress_stderr2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

(** [compare_versions v1 v2] returns 1 if  v1 is newer than v2,
    -1 if v1 is older than v2 and 0 if they are the same version.
    The versions are strings of the shape "n.m.t", the order is lexicographic. *)
val compare_versions : string -> string -> int

(** Like List.iter but operates in parallel up to a number of jobs *)
val iter_parallel : f:('a -> unit) -> ?jobs:int -> 'a list -> unit

(** Like List.iteri but operates in parallel up to a number of jobs *)
val iteri_parallel : f:(int -> 'a -> unit) -> ?jobs:int -> 'a list -> unit

(** Pool of processes to execute in parallel up to a number of jobs. *)
module ProcessPool : sig
  type t

  (** Create a new pool of processes *)
  val create : jobs:int -> t

  (** Start a new child process in the pool.
      If all the jobs are taken, wait until one is free. *)
  val start_child : f:('a -> unit) -> pool:t -> 'a -> unit

  (** Wait until all the currently executing processes terminate *)
  val wait_all : t -> unit
end

(** Register a function to run when the program exits or is interrupted. Registered functions are
    run in the reverse order in which they were registered. *)
val register_epilogue : (unit -> unit) -> string -> unit
