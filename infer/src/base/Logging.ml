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
module CLOpt = CommandLineOption

(* log files *)

let dup_formatter fmt1 fmt2 =
  let (out_string1, flush1) =
    Format.pp_get_formatter_output_functions fmt1 () in
  let (out_string2, flush2) =
    Format.pp_get_formatter_output_functions fmt2 () in
  (* crude multiplexing; may cause garbled output if a formatter is shared between several
     processes *)
  let out_string s p n = out_string1 s p n; out_string2 s p n in
  let flush () = flush1 (); flush2 () in
  Format.pp_set_formatter_output_functions fmt1 out_string flush

(** Name of dir for logging the output in the specific executable *)
let log_dir_of_command (command : CLOpt.command) = match command with
  | Analyze -> "analyze"
  | Capture | Clang | Compile -> "capture"
  | Report -> "report"
  | ReportDiff -> "reportdiff"
  | Run -> "driver"

let log_file = (
  lazy F.std_formatter,
  lazy Pervasives.stderr,
  lazy "out log file not initialized, stderr used instead"
)

let close_log_file fmt chan file =
  (* evaluating any of the three values will evaluate the rest *)
  if Lazy.is_val fmt || Lazy.is_val chan || Lazy.is_val file then (
    F.pp_print_flush (Lazy.force fmt) () ;
    let c = Lazy.force chan in
    if c <> stdout && c <> stderr then
      Out_channel.close c
  )

let create_log_file command name_prefix =
  let log_dir = Config.results_dir ^/ Config.log_dir_name ^/ log_dir_of_command command in
  let file = match Config.log_file with
    | Some file ->
        (* the command-line option takes precedence if specified *)
        file
    | None ->
        log_dir ^/ name_prefix ^ ".log" in
  Unix.mkdir_p log_dir ;
  let chan = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 file in
  let file_fmt = F.formatter_of_out_channel chan in
  F.fprintf file_fmt
    "---- start logging from %d -------------------------------------------@."
    (Pid.to_int (Unix.getpid ()));
  if Config.print_logs then (
    dup_formatter file_fmt Format.err_formatter
  );
  Utils.register_epilogue
    (fun () -> close_log_file (lazy file_fmt) (lazy chan) (lazy file))
    "log files flushing";
  (file_fmt, chan, file)

let should_setup_log_files (command : CLOpt.command) = match command with
  | Analyze | Capture | Clang | Compile ->
      Config.debug_mode || Config.stats_mode
  | Run ->
      true
  | Report | ReportDiff ->
      false

let setup_log_file command prefix_opt =
  let lazy3 x = (lazy (fst3 (Lazy.force x)),
                 lazy (snd3 (Lazy.force x)),
                 lazy (trd3 (Lazy.force x))) in
  if should_setup_log_files command then
    let name_prefix = match prefix_opt with
      | Some name -> name ^ "-"
      | None -> "" in
    lazy (create_log_file command name_prefix) |> lazy3
  else
    log_file

let (out_formatter, out_chan, out_file) =
  let (o_fmt, o_c, o_f) = setup_log_file Config.command None in
  (ref o_fmt, ref o_c, ref o_f)

let set_log_file_identifier command prefix_opt =
  let (o_fmt, o_c, o_f) = setup_log_file command prefix_opt in
  (* close previous log files *)
  close_log_file !out_formatter !out_chan !out_file;
  out_formatter := o_fmt; out_chan := o_c; out_file := o_f

let log_file_name () = Lazy.force !out_file


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

let delayed_actions = ref []

(** hook for the current printer of delayed print actions *)
let printer_hook = ref (fun _ -> failwith "uninitialized printer hook")

(** extend the current print log *)
let add_print_action pact =
  if Config.write_html then delayed_actions := pact :: !delayed_actions
  else if not Config.test then !printer_hook (Lazy.force !out_formatter) pact

(** reset the delayed print actions *)
let reset_delayed_prints () =
  delayed_actions := []

(** return the delayed print actions *)
let get_delayed_prints () =
  !delayed_actions

(** set the delayed print actions *)
let set_delayed_prints new_delayed_actions =
  delayed_actions := new_delayed_actions

let do_print (lazy fmt) = F.fprintf fmt

let do_print_in_debug_or_stats_mode (lazy fmt) =
  if Config.debug_mode || Config.stats_mode then
    F.fprintf fmt
  else
    F.ifprintf fmt

let do_print_in_debug_mode (lazy fmt) =
  if Config.debug_mode then
    F.fprintf fmt
  else
    F.ifprintf fmt

let out fmt_string =
  do_print_in_debug_or_stats_mode !out_formatter fmt_string

let out_debug fmt_string =
  do_print_in_debug_mode !out_formatter ("PROUT: " ^^ fmt_string)

let do_out fmt_string =
  do_print !out_formatter ("CACA" ^^ fmt_string)

let stderr = F.eprintf

let progress fmt_string =
  if Config.quiet then F.ifprintf F.err_formatter fmt_string
  else F.fprintf F.err_formatter fmt_string

let stdout fmt_string = F.printf ("PIPI" ^^ fmt_string)

(** Type of location in ml source: __POS__ *)
type ml_loc = string * int * int * int

(** Convert a ml location to a string *)
let ml_loc_to_string (file, lnum, cnum, enum) =
  Printf.sprintf "%s:%d:%d-%d:" file lnum cnum enum

(** Pretty print a location of ml source *)
let pp_ml_loc fmt ml_loc =
  F.fprintf fmt "%s" (ml_loc_to_string ml_loc)

let pp_ml_loc_opt fmt ml_loc_opt =
  if Config.developer_mode then match ml_loc_opt with
    | None -> ()
    | Some ml_loc -> F.fprintf fmt "(%a)" pp_ml_loc ml_loc

let assert_false ((file, lnum, cnum, _) as ml_loc) =
  Printf.eprintf "\nASSERT FALSE %s\nCALL STACK\n%s\n%!"
    (ml_loc_to_string ml_loc)
    (Printexc.get_backtrace ());
  raise (Assert_failure (file, lnum, cnum))

(** print a warning with information of the position in the ml source where it oririnated.
    use as: warning_position "description" __POS__; *)
let warning_position (s: string) (ml_loc: ml_loc) =
  out "WARNING: %s in %a@." s pp_ml_loc_opt (Some ml_loc)

(** dump a string *)
let d_str (s: string) = add_print_action (PTstr, Obj.repr s)

(** dump a string with the given color *)
let d_str_color (c: Pp.color) (s: string) = add_print_action (PTstr_color, Obj.repr (s, c))

(** dump an error string *)
let d_error (s: string) = add_print_action (PTerror, Obj.repr s)

(** dump a warning string *)
let d_warning (s: string) = add_print_action (PTwarning, Obj.repr s)

(** dump an info string *)
let d_info (s: string) = add_print_action (PTinfo, Obj.repr s)

(** dump a string plus newline *)
let d_strln (s: string) = add_print_action (PTstrln, Obj.repr s)

(** dump a string plus newline with the given color *)
let d_strln_color (c: Pp.color) (s: string) = add_print_action (PTstrln_color, Obj.repr (s, c))

(** dump a newline *)
let d_ln () = add_print_action (PTstrln, Obj.repr "")

(** dump an indentation *)
let d_indent indent =
  let s = ref "" in
  for _ = 1 to indent do s := "  " ^ !s done;
  if indent <> 0 then add_print_action (PTstr, Obj.repr !s)

(** dump command to increase the indentation level *)
let d_increase_indent (indent: int) =
  add_print_action (PTincrease_indent, Obj.repr indent)

(** dump command to decrease the indentation level *)
let d_decrease_indent (indent: int) =
  add_print_action (PTdecrease_indent, Obj.repr indent)

let log_progress_simple text =
  if Config.show_progress_bar && not Config.quiet then
    F.fprintf F.err_formatter "%s@?" text

let log_progress_file () =
  log_progress_simple Config.log_analysis_file

let log_progress_procedure () =
  log_progress_simple Config.log_analysis_procedure

let log_progress_timeout_event failure_kind =
  if Config.stats_mode || Config.debug_mode then
    begin
      match failure_kind with
      | SymOp.FKtimeout ->
          log_progress_simple Config.log_analysis_wallclock_timeout
      | SymOp.FKsymops_timeout _ ->
          log_progress_simple Config.log_analysis_symops_timeout
      | SymOp.FKrecursion_timeout _ ->
          log_progress_simple Config.log_analysis_recursion_timeout
      | SymOp.FKcrash msg ->
          log_progress_simple (Printf.sprintf "%s(%s)" Config.log_analysis_crash msg)
    end
