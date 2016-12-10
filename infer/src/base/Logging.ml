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


(** Name of dir for logging the output in the specific executable *)
let log_dir_of_current_exe (current_exe : CLOpt.exe) =
  match current_exe with
  | Analyze -> "analyze"
  | Clang -> "clang"
  | Interactive -> "interactive"
  | Java -> "java"
  | Print -> "print"
  | Toplevel -> "top_level"


let out_file = ref (lazy "out log file not initialized, stdout used instead")
let err_file = ref (lazy "err log file not initialized, stderr used instead")
let out_chan = ref (lazy Pervasives.stdout)
let err_chan = ref (lazy Pervasives.stderr)
let out_formatter = ref (lazy F.std_formatter)
let err_formatter = ref (lazy F.err_formatter)

let set_log_file_identifier (current_exe : CLOpt.exe) string_opt =
  let should_setup_log_files =
    match current_exe with
    | Analyze
    | Clang -> Config.debug_mode || Config.stats_mode
    | Toplevel -> true
    | _ -> false in
  if should_setup_log_files then (
    let name_prefix =
      (match string_opt with
       | Some name -> name ^ "_"
       | None -> ""
      ) ^ Pid.to_string (Unix.getpid ()) ^ "_" in
    let exe_log_dir =
      let log_dir = Config.results_dir ^/ Config.log_dir_name in
      log_dir ^/ (log_dir_of_current_exe current_exe) in
    let fmt_chan_file name suffix = lazy (
      try
        Unix.mkdir_p exe_log_dir ;
        let file =
          (* the command-line option takes precedence if specified *)
          if name <> "" then name
          else Filename.temp_file ~in_dir:exe_log_dir name_prefix suffix in
        let chan = Pervasives.open_out file in
        let fmt = F.formatter_of_out_channel chan in
        (fmt, chan, file)
      with Sys_error _ ->
        failwithf "ERROR: cannot open log file %s@\n" name
    ) in
    let out_fmt_chan_file = fmt_chan_file Config.out_file_cmdline "-out.log" in
    let err_fmt_chan_file = fmt_chan_file Config.err_file_cmdline "-err.log" in
    Pervasives.at_exit (fun () ->
        let close fmt_chan_file =
          if Lazy.is_val fmt_chan_file then (
            let (fmt, chan, _) = Lazy.force fmt_chan_file in
            F.pp_print_flush fmt () ;
            Out_channel.close chan
          ) in
        close out_fmt_chan_file ;
        close err_fmt_chan_file
      );
    out_formatter := lazy (fst3 (Lazy.force out_fmt_chan_file)) ;
    out_chan := lazy (snd3 (Lazy.force out_fmt_chan_file)) ;
    out_file := lazy (trd3 (Lazy.force out_fmt_chan_file)) ;
    err_formatter := lazy (fst3 (Lazy.force err_fmt_chan_file)) ;
    err_chan := lazy (snd3 (Lazy.force err_fmt_chan_file)) ;
    err_file := lazy (trd3 (Lazy.force err_fmt_chan_file))
  )

(* set up log files on startup if needed *)
let () = set_log_file_identifier Config.current_exe (Some (CLOpt.exe_name Config.current_exe))

let log_file_names () = (Lazy.force !out_file, Lazy.force !err_file)


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
  do_print_in_debug_mode !out_formatter fmt_string

let do_out fmt_string =
  do_print !out_formatter fmt_string

let err fmt_string =
  do_print_in_debug_or_stats_mode !err_formatter fmt_string

let do_err fmt_string =
  do_print !err_formatter fmt_string

let err_debug fmt_string =
  do_print_in_debug_mode !err_formatter fmt_string

let stderr fmt_string =
  do_print (Lazy.from_val F.err_formatter) fmt_string

let stdout fmt_string =
  do_print (Lazy.from_val F.std_formatter) fmt_string

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
    use as: warning_position "description" (try assert false with Assert_failure x -> x); *)
let warning_position (s: string) (ml_loc: ml_loc) =
  err "WARNING: %s in %a@." s pp_ml_loc_opt (Some ml_loc)

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
  if Config.show_progress_bar then
    F.fprintf Format.err_formatter "%s@?" text

let log_progress_file () =
  log_progress_simple Config.log_analysis_file

let log_progress_procedure () =
  log_progress_simple Config.log_analysis_procedure

let log_progress_timeout_event failure_kind =
  if Config.stats_mode then
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
