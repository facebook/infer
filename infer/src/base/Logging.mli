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

module F = Format

(* If Logging has not been set up yet, Die can be used instead. Prefer to use the
   functions here, as they can do more logging. These functions are documented in Die. *)

include module type of Die

val environment_info : ('a, F.formatter, unit) format -> 'a
(** log information about the environment *)

val progress : ('a, F.formatter, unit) format -> 'a
(** print immediately to standard error unless --quiet is specified *)

val progressbar_file : unit -> unit
(** Progress bar: start of the analysis of a file. *)

val progressbar_procedure : unit -> unit
(** Progress bar: start of the analysis of a procedure. *)

val progressbar_timeout_event : SymOp.failure_kind -> unit
(** Progress bar: log a timeout event if in developer mode. *)

val result : ('a, F.formatter, unit) format -> 'a
(** Emit a result to stdout. Use only if the output format is stable and useful enough that it may
    conceivably get piped to another program, ie, almost never (use [progress] instead otherwise).
*)

val user_error : ('a, F.formatter, unit) format -> 'a
(** bad input, etc. detected *)

val user_warning : ('a, F.formatter, unit) format -> 'a

val internal_error : ('a, F.formatter, unit) format -> 'a
(** huho, infer has a bug *)

val external_error : ('a, F.formatter, unit) format -> 'a
(** some other tool has a bug or is called wrongly *)

val external_warning : ('a, F.formatter, unit) format -> 'a

type debug_kind = Analysis | BufferOverrun | Capture | Linters | MergeCapture

(** Level of verbosity for debug output. Each level enables all the levels before it. *)
type debug_level =
  | Quiet  (** innocuous, eg emitted once per toplevel execution *)
  | Medium  (** still fairly lightweight, eg emitted O(<number of infer processes>) *)
  | Verbose  (** go crazy *)

val debug : debug_kind -> debug_level -> ('a, F.formatter, unit) format -> 'a
(** log debug info *)

val debug_dev : ('a, Format.formatter, unit) format -> 'a
  [@@deprecated
    "Only use to debug during development. If you want more permanent logging, use \
     [Logging.debug] instead."]
[@@warning "-32"]
(** For debugging during development. *)

(** Type of location in ml source: __POS__ *)
type ocaml_pos = string * int * int * int

val ocaml_pos_to_string : ocaml_pos -> string
(** Convert an ocaml position to a string *)

val pp_ocaml_pos_opt : F.formatter -> ocaml_pos option -> unit
(** Pretty print a position in ocaml source *)

(** log management *)

val setup_log_file : unit -> unit
(** Set up logging to go to the log file. Call this once the results directory has been set up. *)

val reset_formatters : unit -> unit
(** Reset the formatters used for logging. Call this when you fork(2). *)

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
type print_action = print_type * Obj.t  (** data to be printed *)

val printer_hook : (F.formatter -> print_action -> unit) ref
(** hook for the current printer of delayed print actions *)

val add_print_action : print_action -> unit
(** extend he current print log *)

val get_delayed_prints : unit -> print_action list
(** return the delayed print actions *)

val set_delayed_prints : print_action list -> unit
(** set the delayed print actions *)

val reset_delayed_prints : unit -> unit
(** reset the delayed print actions *)

val d_str : string -> unit
(** dump a string *)

val d_str_color : Pp.color -> string -> unit
(** dump a string with the given color *)

val d_strln : string -> unit
(** dump a string plus newline *)

val d_strln_color : Pp.color -> string -> unit
(** dump a string plus newline with the given color *)

val d_ln : unit -> unit
(** dump a newline *)

val d_error : string -> unit
(** dump an error string *)

val d_warning : string -> unit
(** dump a warning string *)

val d_info : string -> unit
(** dump an info string *)

val d_indent : int -> unit
(** dump an indentation *)

val d_increase_indent : int -> unit
(** dump command to increase the indentation level *)

val d_decrease_indent : int -> unit
(** dump command to decrease the indentation level *)
