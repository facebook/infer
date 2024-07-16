(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

val log_task : ('a, F.formatter, unit) format -> 'a
(** log progress in the log file and on the console unless there is an active task bar *)

val task_progress : f:(unit -> 'a) -> (F.formatter -> 'b -> unit) -> 'b -> 'a
(** [task_progress ~f pp x] executes [f] and log progress [pp x] in the log file and also on the
    console unless there is an active task bar *)

val result : ?style:ANSITerminal.style list -> ('a, F.formatter, unit) format -> 'a
(** Emit a result to stdout. Use only if the output format is stable and useful enough that it may
    conceivably get piped to another program, ie, almost never (use [progress] instead otherwise). *)

val user_error : ('a, F.formatter, unit) format -> 'a
(** bad input, etc. detected *)

val user_warning : ('a, F.formatter, unit) format -> 'a
(** the user may have done something wrong *)

val internal_error : ('a, F.formatter, unit) format -> 'a
(** huho, infer has a bug *)

val external_error : ('a, F.formatter, unit) format -> 'a
(** some other tool has a bug or is called wrongly *)

val external_warning : ('a, F.formatter, unit) format -> 'a

type debug_kind = Analysis | BufferOverrun | Capture | MergeCapture | Report

(** Level of verbosity for debug output. Each level enables all the levels before it. *)
type debug_level =
  | Quiet  (** innocuous, eg emitted once per toplevel execution *)
  | Medium  (** still fairly lightweight, eg emitted O(<number of infer processes>) *)
  | Verbose  (** go crazy *)

val debug : debug_kind -> debug_level -> ('a, F.formatter, unit) format -> 'a
(** log debug info *)

val debug_dev : ('a, Format.formatter, unit) format -> 'a
[@@deprecated
  "Only use to debug during development. If you want more permanent logging, use [Logging.debug] \
   instead."]
[@@warning "-unused-value-declaration"]
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

val flush_formatters : unit -> unit
(** Flushes the formatters used for logging. Call this in the parent before you fork(2). *)

val reset_formatters : unit -> unit
(** Reset the formatters used for logging. Call this in the child after you fork(2). *)

(** Delayed printing (HTML debug, ...) *)

type delayed_prints

val d_pp : (F.formatter -> 'a -> unit) -> 'a -> unit

val d_pp_with_pe : ?color:Pp.color -> (Pp.env -> F.formatter -> 'a -> unit) -> 'a -> unit

val force_and_reset_delayed_prints : F.formatter -> unit

val get_and_reset_delayed_prints : unit -> delayed_prints
(** return the delayed print actions and reset them *)

val set_delayed_prints : delayed_prints -> unit
(** set the delayed print actions *)

val reset_delayed_prints : unit -> unit
(** reset the delayed print actions *)

val d_str : ?color:Pp.color -> string -> unit
(** dump a string *)

val d_strln : ?color:Pp.color -> string -> unit
(** dump a string plus newline *)

val d_ln : unit -> unit
(** dump a newline *)

val d_printf : ?color:Pp.color -> ('a, F.formatter, unit) format -> 'a

val d_printfln : ?color:Pp.color -> ('a, F.formatter, unit) format -> 'a

val d_printfln_escaped : ?color:Pp.color -> ('a, F.formatter, unit) format -> 'a

val d_error : string -> unit
(** dump an error string *)

val d_warning : string -> unit
(** dump a warning string *)

val d_info : string -> unit
(** dump an info string *)

val d_indent : int -> unit
(** dump an indentation *)

val d_increase_indent : unit -> unit
(** dump command to increase the indentation level. NOTE: most likely, you need [d_with_indent]
    instead *)

val d_decrease_indent : unit -> unit
(** dump command to decrease the indentation level NOTE: most likely, you need [d_with_indent]
    instead. *)

val with_indent :
     ?name_color:Pp.color
  -> ?collapsible:bool
  -> ?escape_result:bool
  -> ?pp_result:(F.formatter -> 'a -> unit)
  -> f:(unit -> 'a)
  -> ('b, F.formatter, unit, 'a) format4
  -> 'b
(** Unconditionally execute [f] so that all logs written inside (if any) are written with
    indentation, using the title format provided.

    [pp_result], if provided, will make the result of a call to be printed as well (useful for cases
    when there are several places when the function returns).

    NOTE: If you want to use it NOT at the very top level of a function, it is a code smell, and you
    probably want to split your function into smaller ones. *)
