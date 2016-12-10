(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** General utility functions *)

module Arg = Core.Std.Arg
module Array = Core.Std.Array
module Bool = Core.Std.Bool
module Bytes = Core.Std.Bytes
module Caml = Core.Std.Caml
module Char = Core.Std.Char
module Exn = Core.Std.Exn
module Filename = Core.Std.Filename
module Fn = Core.Std.Fn
module Gc = Core.Std.Gc
module In_channel = Core.Std.In_channel
module Int = Core.Std.Int
module Int32 = Core.Std.Int32
module Int63 = Core.Std.Int63
module Int64 = Core.Std.Int64
module Lazy = Core.Std.Lazy
module Nativeint = Core.Std.Nativeint
module Option = Core.Std.Option
module Pid = Core.Std.Pid
module Printexc = Core.Std.Printexc
module Signal = Core.Std.Signal
module String = Core.Std.String
module Sys : module type of Core.Std.Sys
module Unix = Core.Std.Unix

(** {2 Generic Utility Functions} *)

(** List police: don't use the list module to avoid non-tail recursive
    functions and builtin equality. Use IList instead. *)
module List : sig end

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(** initial process times *)
val initial_times : Unix.process_times

(** precise time of day at the start of the analysis *)
val initial_timeofday : float

(** Compare police: generic compare disabled. *)
val compare : unit

(** Return the first component of a triple. *)
val fst3 : 'a * 'b * 'c -> 'a

(** Return the second component of a triple. *)
val snd3 : 'a * 'b * 'c -> 'b

(** Return the third component of a triple. *)
val trd3 : 'a * 'b * 'c -> 'c

(** Convert a bool into an int *)
val int_of_bool : bool -> int

(** {2 Useful Modules} *)

(** Set of integers *)
module IntSet : Set.S with type elt = int

(** Hash table over strings *)
module StringHash : Hashtbl.S with type key = string

(** Set of strings *)
module StringSet : Set.S with type elt = string

(** Pretty Printable Set of strings *)
module StringPPSet : PrettyPrintable.PPSet with type elt = string

(** Pretty print a set of strings *)
val pp_stringset : Format.formatter -> StringSet.t -> unit

(** list of strings -> set of strings *)
val string_set_of_list : string list -> StringSet.t

(** List intersection *)
val string_list_intersection : string list -> string list -> StringSet.t

(** Maps from integers *)
module IntMap : Map.S with type key = int

(** Maps from strings *)
module StringMap : Map.S with type key = string

(** {2 Printing} *)

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Kind of simple printing: default or with full types *)
type pp_simple_kind = PP_SIM_DEFAULT | PP_SIM_WITH_TYP

(** Kind of printing *)
type printkind = PP_TEXT | PP_LATEX | PP_HTML

(** Print environment threaded through all the printing functions *)
type printenv = {
  pe_opt : pp_simple_kind; (** Current option for simple printing *)
  pe_kind : printkind; (** Current kind of printing *)
  pe_cmap_norm : colormap; (** Current colormap for the normal part *)
  pe_cmap_foot : colormap; (** Current colormap for the footprint part *)
  pe_color : color; (** Current color *)
  pe_obj_sub : (Obj.t -> Obj.t) option (** generic object substitution *)
}

(** Reset the object substitution, so that no substitution takes place *)
val pe_reset_obj_sub : printenv -> printenv

(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
val pe_set_obj_sub : printenv -> ('a -> 'a) -> printenv

(** standard colormap: black *)
val colormap_black : colormap

(** red colormap *)
val colormap_red : colormap

(** Extend the normal colormap for the given object with the given color *)
val pe_extend_colormap : printenv -> Obj.t -> color -> printenv

(** Default text print environment *)
val pe_text : printenv

(** Default html print environment *)
val pe_html : color -> printenv

(** Default latex print environment *)
val pe_latex : color -> printenv

(** string representation of colors *)
val color_string : color -> string

(** Pretty print a space-separated sequence *)
val pp_seq : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a comma-separated sequence *)
val pp_comma_seq : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a ;-separated sequence *)
val pp_semicolon_seq : printenv -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a ;-separated sequence on one line *)
val pp_semicolon_seq_oneline : printenv -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Pretty print a or-separated sequence *)
val pp_or_seq : printenv -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** Produce a string from a 1-argument pretty printer function *)
val pp_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

(** Print the current time and date in a format similar to the "date" command *)
val pp_current_time : Format.formatter -> unit -> unit

(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
val pp_elapsed_time : Format.formatter -> unit -> unit

(** Compute a 32-character hexadecimal crc using the Digest module  *)
val string_crc_hex32 : string -> string

(** Append a crc to the string, using string_crc_hex32.
    Cut the string if it exceeds the cutoff limit.
    Use an optional key to compute the crc.  *)
val string_append_crc_cutoff : ?cutoff:int -> ?key:string -> string -> string

(** copy a source file, return the number of lines, or None in case of error *)
val copy_file : string -> string -> int option

(** read a source file and return a list of lines, or None in case of error *)
val read_file : string -> string list option

(** Filename.concat *)
val ( // ) : string -> string -> string

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
val filename_to_absolute : string -> string

(** Convert an absolute filename to one relative to a root directory *)
val filename_to_relative : string -> string -> string

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

(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t [@@deriving compare]

(** keys for proc_flags *)

val proc_flag_skip : string (** key to specify that a function should be treated as a skip function *)

val proc_flag_ignore_return : string (** key to specify that it is OK to ignore the return value *)

(** empty proc flags *)
val proc_flags_empty : unit -> proc_flags

(** add a key value pair to a proc flags *)
val proc_flags_add : proc_flags -> string -> string -> unit

(** find a value for a key in the proc flags *)
val proc_flags_find : proc_flags -> string -> string

(** Functional fold function over all the file of a directory *)
val directory_fold : ('a -> string -> 'a) -> 'a -> string -> 'a

(** Functional iter function over all the file of a directory *)
val directory_iter : (string -> unit) -> string -> unit

(** Remove a directory and its contents *)
val remove_directory_tree : string -> unit

val read_optional_json_file : string -> (Yojson.Basic.json, string) result

val with_file : string -> f:(out_channel -> 'a) -> 'a

val write_json_to_file : string -> Yojson.Basic.json -> unit

val consume_in : in_channel -> unit

val with_process_in: string -> (in_channel -> 'a) -> ('a * Unix.Exit_or_signal.t)

val failwithf : ('a, Format.formatter, unit, 'b) format4 -> 'a

val invalid_argf : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** create a directory if it does not exist already *)
val create_dir : string -> unit

(** [realpath path] returns path with all symbolic links resolved. It caches results of previous
    calls to avoid expensive system calls *)
val realpath : string -> string
