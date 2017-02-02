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

(** log messages at different levels of verbosity *)

(** type of printable elements *)
type print_type =
  | PTatom
  | PTattribute
  | PTdecrease_indent
  | PTexp
  | PTexp_list
  | PThpred
  | PTincrease_indent
  | PTinstr
  | PTinstr_list
  | PTjprop_list
  | PTjprop_short
  | PTloc
  | PTnode_instrs
  | PToff
  | PToff_list
  | PTpath
  | PTprop
  | PTproplist
  | PTprop_list_with_typ
  | PTprop_with_typ
  | PTpvar
  | PTspec
  | PTstr
  | PTstr_color
  | PTstrln
  | PTstrln_color
  | PTpathset
  | PTpi
  | PTsexp
  | PTsexp_list
  | PTsigma
  | PTtexp_full
  | PTsub
  | PTtyp_full
  | PTtyp_list
  | PTwarning
  | PTerror
  | PTinfo

(** delayable print action *)
type print_action =
  print_type * Obj.t (** data to be printed *)

(** hook for the current printer of delayed print actions *)
val printer_hook : (Format.formatter -> print_action -> unit) ref

(** extend he current print log *)
val add_print_action : print_action -> unit

(** return the delayed print actions *)
val get_delayed_prints : unit -> print_action list

(** set the delayed print actions *)
val set_delayed_prints : print_action list -> unit

(** reset the delayed print actions *)
val reset_delayed_prints : unit -> unit

(** Set a custom identifier to be part of the filename of the current logfiles. *)
val set_log_file_identifier : CommandLineOption.parse_action -> string option -> unit

(** print to the current out stream, as specified in set_log_file_identifier
    (note: only prints in debug or in stats mode) *)
val out : ('a, Format.formatter, unit) format -> 'a

(** print to the current out stream, as specified in set_log_file_identifier
    (note: only prints in debug mode) *)
val out_debug : ('a, Format.formatter, unit) format -> 'a

(** print to the current error stream, as specified in set_log_file_identifier
    (note: only prints in debug or stats mode) *)
val err : ('a, Format.formatter, unit) format -> 'a

(** print to the current error stream, as specified in set_log_file_identifier
    (note: only prints in debug mode) *)
val err_debug : ('a, Format.formatter, unit) format -> 'a

(** print to the current out stream, as specified in set_log_file_identifier  *)
val do_out : ('a, Format.formatter, unit) format -> 'a

(** print to the current err stream, as specified in set_log_file_identifier *)
val do_err : ('a, Format.formatter, unit) format -> 'a

(** print immediately to standard error *)
val stderr : ('a, Format.formatter, unit) format -> 'a

(** print immediately to standard output *)
val stdout : ('a, Format.formatter, unit) format -> 'a

(** Type of location in ml source: __POS__ *)
type ml_loc = string * int * int * int

(** Convert a ml location to a string *)
val ml_loc_to_string : ml_loc -> string

(** Pretty print a location of ml source *)
val pp_ml_loc_opt : Format.formatter -> ml_loc option -> unit

(** Print stack trace and throw assert false *)
val assert_false : ml_loc -> 'a

(** print a warning with information of the position in the ml source where it oririnated.
    use as: warning_position "description" (try assert false with Assert_failure x -> x); *)
val warning_position: string -> ml_loc -> unit

(** dump a string *)
val d_str : string -> unit

(** dump a string with the given color *)
val d_str_color : Pp.color -> string -> unit

(** dump a string plus newline *)
val d_strln : string -> unit

(** dump a string plus newline with the given color *)
val d_strln_color : Pp.color -> string -> unit

(** dump a newline *)
val d_ln : unit -> unit

(** dump an error string *)
val d_error : string -> unit

(** dump a warning string *)
val d_warning : string -> unit

(** dump an info string *)
val d_info : string -> unit

(** dump an indentation *)
val d_indent : int -> unit

(** dump command to increase the indentation level *)
val d_increase_indent : int -> unit

(** dump command to decrease the indentation level *)
val d_decrease_indent : int -> unit

(** Progress bar: start of the analysis of a file. *)
val log_progress_file : unit -> unit

(** Progress bar: start of the analysis of a procedure. *)
val log_progress_procedure : unit -> unit

(** Progress bar: log a timeout event if in developer mode. *)
val log_progress_timeout_event : SymOp.failure_kind -> unit

(** Names of current temporary files for logging the output in the current executable *)
val log_file_names : unit -> string * string
