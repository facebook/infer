(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val find_files : path:string -> extension:string -> string list
(** recursively traverse a path for files ending with a given extension *)

val fold_folders : init:'acc -> f:('acc -> string -> 'acc) -> path:string -> 'acc
(** recursively traverse a path for folders, returning resuls by a given fold function *)

val string_crc_hex32 : string -> string
(** Compute a 32-character hexadecimal crc using the Digest module *)

val read_file : string -> (string list, string) Result.t
(** read a source file and return a list of lines *)

val normalize_path_from : root:string -> string -> string * string
(** [normalize_path_from ~root path] removes ".." and "." parts of [root/path] when possible and
    returns the new [root] and [path], eg if [root = "r"] and [path = "a/../../../foo/./bar"] then
    the result is [("../foo/bar", ".")] (in particular "r/a/../../../foo/./bar" and "./../foo/bar"
    represent the same file) *)

val normalize_path : string -> string
(** Normalize a path without a root *)

val filename_to_absolute : root:string -> string -> string
(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)

val filename_to_relative :
  ?force_full_backtrack:bool -> ?backtrack:int -> root:string -> string -> string option
(** Convert an absolute filename to one relative to a root directory. Returns [None] if filename is
    not under root. The backtrack level sets the maximum level of steps in the parent directories to
    search for a common prefix *)

(** type for files used for printing *)
type outfile =
  { fname: string  (** name of the file *)
  ; out_c: Out_channel.t  (** output channel *)
  ; fmt: Format.formatter  (** formatter for printing *) }

val create_outfile : string -> outfile option
(** create an outfile for the command line, the boolean indicates whether to do demangling when
    closing the file *)

val close_outf : outfile -> unit
(** close an outfile *)

val directory_fold : ('a -> string -> 'a) -> 'a -> string -> 'a
(** Functional fold function over all the file of a directory *)

val directory_iter : (string -> unit) -> string -> unit
(** Functional iter function over all the file of a directory *)

val directory_is_empty : string -> bool
(** Returns true if a given directory is empty. The directory is assumed to exist. *)

val read_json_file : string -> (Yojson.Basic.t, string) Result.t

val with_file_in : string -> f:(In_channel.t -> 'a) -> 'a

val with_file_out : string -> f:(Out_channel.t -> 'a) -> 'a

val with_intermediate_temp_file_out : string -> f:(Out_channel.t -> 'a) -> 'a
(** like [with_file_out] but uses a fresh intermediate temporary file and rename to avoid
    write-write races *)

val write_json_to_file : string -> Yojson.Basic.t -> unit

val consume_in : In_channel.t -> unit
(** consume and ignore all the lines from the channel until End_of_file is reached *)

val echo_in : In_channel.t -> unit
(** echo the lines we get to stdout until End_of_file is reached *)

val with_channel_in : f:(string -> unit) -> In_channel.t -> unit

val with_process_in : string -> (In_channel.t -> 'a) -> 'a * Unix.Exit_or_signal.t

val create_dir : string -> unit
(** recursively create a directory if it does not exist already *)

val out_channel_create_with_dir : Filename.t -> Out_channel.t
(** create an out channel with creating missing directories *)

val realpath : ?warn_on_error:bool -> string -> string
(** [realpath warn_on_error path] returns path with all symbolic links resolved. It caches results
    of previous calls to avoid expensive system calls. WARNING: If warn_on_error is false, no
    warning will be shown whenever an error occurs for the given path (e.g. if it does not exist). *)

val suppress_stderr2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(** wraps a function expecting 2 arguments in another that temporarily redirects stderr to /dev/null
    for the duration of the function call *)

val rmtree : string -> unit
(** [rmtree path] removes [path] and, if [path] is a directory, recursively removes its contents *)

val try_finally_swallow_timeout : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
(** Calls [f] then [finally] even if [f] raised an exception. The original exception is reraised
    afterwards. Where possible use [SymOp.try_finally] to avoid swallowing timeouts. *)

val better_hash : 'a -> Caml.Digest.t
(** Hashtbl.hash only hashes the first 10 meaningful values, [better_hash] uses everything. *)

val unlink_file_on_exit : string -> unit
(** delete [temporary] file on exit *)

val strip_balanced_once : drop:(char -> bool) -> string -> string
(** drop at most one layer of well-balanced first and last characters satisfying [drop] from the
    string; for instance,
    [strip_balanced ~drop:(function | 'a' | 'x' -> true | _ -> false) "xaabax"] returns "aaba" *)

val assoc_of_yojson : Yojson.Basic.t -> src:string -> (string, Yojson.Basic.t) List.Assoc.t
(** Verify we have a json object (or empty list) and return the corresponding assoc list. Otherwise
    die with a message including src. *)

val string_of_yojson : Yojson.Basic.t -> src:string -> string
(** Verify we have a json string and return the corresponding ocaml string. Otherwise die with a
    message including src. *)

val string_list_of_yojson : Yojson.Basic.t -> src:string -> string list
(** Verify we have a json list of strings and return the corresponding ocaml string list. Otherwise
    die with a message including src. *)

val yojson_lookup :
     (string, Yojson.Basic.t) List.Assoc.t
  -> string
  -> src:string
  -> f:(Yojson.Basic.t -> src:string -> 'a)
  -> default:'a
  -> 'a
(** Lookup a json value on an assoc list. If not present, returns default. Otherwise returns (f
    json_value ~src) where src has element name appended. f is typically one of the above _of_yojson
    functions. *)

val timeit : f:(unit -> 'a) -> 'a * int
(** Returns the execution time of [f] in milliseconds together with its result *)

val do_in_dir : dir:string -> f:(unit -> 'a) -> 'a
(** executes [f] after cding into [dir] and then restores original cwd *)

val get_available_memory_MB : unit -> int option
(** On Linux systems, return [Some x] where [MemAvailable x] is in [/proc/meminfo]. Returns [None]
    in all other cases. *)

val iter_infer_deps : project_root:string -> f:(string -> unit) -> string -> unit
(** Parse each line of the given infer_deps.txt file (split on tabs, assume 3 elements per line) and
    run [f] on the third element. [project_root] is an argument to avoid dependency cycles. *)

val numcores : int
(** - On Linux return the number of physical cores (sockets * cores per socket).
    - On Darwin and Windows returns half of the number of CPUs since most processors have 2 hardware
      threads per core. *)

val set_best_cpu_for : int -> unit
(** Pins processes to CPUs aiming to saturate physical cores evenly *)

val zip_fold_filenames : init:'a -> f:('a -> string -> 'a) -> zip_filename:string -> 'a
(** fold over each filename in the given [zip_filename]. *)
