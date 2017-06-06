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

(** log information about the environment *)
val environment_info : ('a, Format.formatter, unit) format -> 'a

(** print immediately to standard error unless --quiet is specified *)
val progress : ('a, Format.formatter, unit) format -> 'a

(** Progress bar: start of the analysis of a file. *)
val progressbar_file : unit -> unit

(** Progress bar: start of the analysis of a procedure. *)
val progressbar_procedure : unit -> unit

(** Progress bar: log a timeout event if in developer mode. *)
val progressbar_timeout_event : SymOp.failure_kind -> unit

(** Emit a result to stdout. Use only if the output format is stable and useful enough that it may
    conceivably get piped to another program, ie, almost never (use [progress] instead otherwise).
*)
val result : ('a, Format.formatter, unit) format -> 'a

(** bad input, etc. detected *)
val user_error : ('a, Format.formatter, unit) format -> 'a
val user_warning : ('a, Format.formatter, unit) format -> 'a

(** huho, infer has a bug *)
val internal_error : ('a, Format.formatter, unit) format -> 'a

(** some other tool has a bug or is called wrongly *)
val external_error : ('a, Format.formatter, unit) format -> 'a
val external_warning : ('a, Format.formatter, unit) format -> 'a

type debug_kind = Analysis | BufferOverrun | Capture | Linters | MergeCapture

(** Level of verbosity for debug output. Each level enables all the levels before it. *)
type debug_level =
  | Quiet (** innocuous, eg emitted once per toplevel execution *)
  | Medium (** still fairly lightweight, eg emitted O(<number of infer processes>) *)
  | Verbose (** go crazy *)

(** log debug info *)
val debug : debug_kind -> debug_level -> ('a, Format.formatter, unit) format -> 'a

(** Type of location in ml source: __POS__ *)
type ml_loc = string * int * int * int

(** Convert a ml location to a string *)
val ml_loc_to_string : ml_loc -> string

(** Pretty print a location of ml source *)
val pp_ml_loc_opt : Format.formatter -> ml_loc option -> unit


(** log management *)

(** Set up logging to go to the log file. Call this once the results directory has been set up. *)
val setup_log_file : unit -> unit

(** Reset the formatters used for logging. Call this when you fork(2). *)
val reset_formatters : unit -> unit


(** Delayed printing (HTML debug, ...) *)

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
