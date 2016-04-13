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

(** {2 Generic Utility Functions} *)

(** List police: don't use the list module to avoid non-tail recursive
    functions and builtin equality. Use IList instead. *)
module List : sig end

(** initial process times *)
val initial_times : Unix.process_times

(** precise time of day at the start of the analysis *)
val initial_timeofday : float

(** Compare police: generic compare disabled. *)
val compare : unit

(** Comparison for booleans *)
val bool_compare : bool -> bool -> int

(** Equality for booleans *)
val bool_equal : bool -> bool -> bool

(** Efficient comparison for integers *)
val int_compare : int -> int -> int

(** Equality for integers *)
val int_equal : int -> int -> bool

(** Extend and equality function to an option type. *)
val opt_equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

(** Generic comparison of pairs given a compare function for each element of the pair. *)
val pair_compare : ('a -> 'b -> int) -> ('c -> 'd -> int) -> ('a * 'c) -> ('b * 'd) -> int

(** Generic comparison of pairs given a compare function for each element of the triple. *)
val triple_compare : ('a -> 'b -> int) -> ('c -> 'd -> int) -> ('e -> 'f -> int) -> ('a * 'c * 'e) -> ('b * 'd * 'f) -> int

(** Comparison for strings *)
val string_compare : string -> string -> int

(** Equality for strings *)
val string_equal : string -> string -> bool

(** Comparison for floats *)
val float_compare : float -> float -> int

(** {2 Useful Modules} *)

(** Set of integers *)
module IntSet : Set.S with type elt = int

(** Set of strings *)
module StringSet : Set.S with type elt = string

(** Pretty print a set of strings *)
val pp_stringset : Format.formatter -> StringSet.t -> unit

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


module Arg : sig
  include module type of Arg with type spec = Arg.spec

  (** type of aligned commend-line options *)
  type aligned = private (key * spec * doc)

  val align : (key * spec * doc) list -> aligned list

  (** [parse env_var arg_desc anon_fun usage_msg] prepends the decoded value of environment variable
      [env_var] to [Sys.argv] and then parses the result using the standard [Arg.parse].  Therefore
      arguments passed on the command line supercede those specified in the environment variable.
      WARNING: If an argument appears both in the environment variable and on the command line, it
      will be interpreted twice. *)
  val parse : string -> aligned list -> anon_fun -> usage_msg -> unit

  val usage : aligned list -> usage_msg -> unit

  (** [create_options_desc double_minus unsorted_desc title] creates a group of sorted command-line arguments.
      [double_minus] is a booleand indicating whether the [-- option = nn] format or [- option n] format is to be used.
      [title] is the title of this group of options.
      It expects a list [opname, desc, param_opt, text] where
      [opname] is the name of the option
      [desc] is the Arg.spec
      [param_opt] is the optional parameter to [opname]
      [text] is the description of the option *)
  val create_options_desc : bool -> string -> (string * Arg.spec * string option * string) list -> aligned list

end

(** Compute a 32-character hexadecimal crc using the Digest module  *)
val string_crc_hex32 : string -> string

(** Append a crc to the string, using string_crc_hex32.
    Cut the string if it exceeds the cutoff limit.
    Use an optional key to compute the crc.  *)
val string_append_crc_cutoff : ?cutoff:int -> ?key:string -> string -> string

(** Check if the lhs is a substring of the rhs. *)
val string_is_prefix : string -> string -> bool

(** Check if the lhs is a suffix of the rhs. *)
val string_is_suffix : string -> string -> bool

(** Check if the lhs is contained in the rhs. *)
val string_contains : string -> string -> bool

(** Split a string across the given character, if given. (e.g. split first.second with '.').*)
val string_split_character : string -> char -> string option * string

(** The value of a string option or the empty string.: *)
val string_value_or_empty_string : string option -> string

(** copy a source file, return the number of lines, or None in case of error *)
val copy_file : string -> string -> int option

(** read a source file and return a list of lines, or None in case of error *)
val read_file : string -> string list option

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
val filename_to_absolute : string -> string

(** Convert an absolute filename to one relative to a root directory *)
val filename_to_relative : string -> string -> string

module FileLOC : (** count lines of code of files and keep processed results in a cache *)
sig
  val reset: unit -> unit (** reset the cache *)
  val file_get_loc : string -> int (** get the LOC of the file *)
end

(** type for files used for printing *)
type outfile =
  { fname : string; (** name of the file *)
    out_c : out_channel; (** output channel *)
    fmt : Format.formatter (** formatter for printing *) }

(** create an outfile for the command line, the boolean indicates whether to do demangling when closing the file *)
val create_outfile : string -> outfile option

(** operate on an outfile reference if it is not None *)
val do_outf : outfile option ref -> (outfile -> unit) -> unit

(** close an outfile *)
val close_outf : outfile -> unit

(* Type of command-line arguments before processing *)
type arg_list = (string * Arg.spec * string option * string) list

(* Filter arguments by name, i.e. keep those whose name appears in the name list *)
val arg_desc_filter : string list -> arg_list -> arg_list

(** Basic command-line arguments *)
val base_arg_desc : arg_list

(** Reserved command-line arguments *)
val reserved_arg_desc : arg_list

(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t

(** keys for proc_flags *)
val proc_flag_skip : string (** key to specify that a function should be treated as a skip function *)
val proc_flag_ignore_return : string (** key to specify that it is OK to ignore the return value *)

(** empty proc flags *)
val proc_flags_empty : unit -> proc_flags

(** add a key value pair to a proc flags *)
val proc_flags_add : proc_flags -> string -> string -> unit

(** find a value for a key in the proc flags *)
val proc_flags_find : proc_flags -> string -> string

(** [join_strings sep parts] contatenates the elements of [parts] using [sep] as separator *)
val join_strings : string -> string list -> string

(** [next compare] transforms the comparison function [compare] to another function taking
    the outcome of another comparison as last parameter and only performs this comparison if this value
    is different from 0. Useful to combine comparison functions using the operator |>. The outcome of
    the expression [Int.compare x y |> next Set.compare s t] is: [Int.compare x y] if this value is
    not [0], skipping the evaluation of [Set.compare s t] in such case; or [Set.compare s t] in case
    [Int.compare x y] is [0] *)
val next : ('a -> 'a -> int) -> ('a -> 'a -> int -> int)

(** Functional fold function over all the file of a directory *)
val directory_fold : ('a -> string -> 'a) -> 'a -> string -> 'a

(** Functional iter function over all the file of a directory *)
val directory_iter : (string -> unit) -> string -> unit

(** Various kind of analyzers *)
type analyzer = Infer | Eradicate | Checkers | Tracing

(** List of analyzers *)
val analyzers: analyzer list

val string_of_analyzer: analyzer -> string

val analyzer_of_string: string -> analyzer

(** Call f x with Config.abs_val set to zero.
    Restore the initial value also in case of exception. *)
val run_with_abs_val_equal_zero : ('a -> 'b) -> 'a -> 'b

(** Call f x with Config.footprint set to true.
    Restore the initial value of footprint also in case of exception. *)
val run_in_footprint_mode : ('a -> 'b) -> 'a -> 'b

(** Call f x with Config.footprint set to false.
    Restore the initial value of footprint also in case of exception. *)
val run_in_re_execution_mode : ('a -> 'b) -> 'a -> 'b

(** [set_reference_and_call_function ref val f x] calls f x with ref set to val.
    Restore the initial value also in case of exception. *)
val set_reference_and_call_function : 'a ref -> 'a -> ('b -> 'c) -> 'b -> 'c
