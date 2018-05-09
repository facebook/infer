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
include Die

(* log files *)
(* make a copy of [f] *)
let copy_formatter f =
  let out_string, flush = F.pp_get_formatter_output_functions f () in
  let out_funs = F.pp_get_formatter_out_functions f () in
  let new_f = F.make_formatter out_string flush in
  F.pp_set_formatter_out_functions new_f out_funs ;
  new_f


(* Return a formatter that multiplexes to [fmt1] and [fmt2]. *)
let dup_formatter fmt1 fmt2 =
  let out_funs1 = F.pp_get_formatter_out_functions fmt1 () in
  let out_funs2 = F.pp_get_formatter_out_functions fmt2 () in
  let f = copy_formatter fmt1 in
  F.pp_set_formatter_out_functions f
    { F.out_string= (fun s p n -> out_funs1.out_string s p n ; out_funs2.out_string s p n)
    ; out_indent= (fun n -> out_funs1.out_indent n ; out_funs2.out_indent n)
    ; out_flush= (fun () -> out_funs1.out_flush () ; out_funs2.out_flush ())
    ; out_newline= (fun () -> out_funs1.out_newline () ; out_funs2.out_newline ())
    ; out_spaces= (fun n -> out_funs1.out_spaces n ; out_funs2.out_spaces n) } ;
  f


(* can be set up to emit to a file later on *)
let log_file = ref None

type formatters =
  { file: F.formatter option  (** send to log file *)
  ; console_file: F.formatter  (** send both to console and log file *) }

let logging_formatters = ref []

(* shared ref is less punishing to sloppy accounting of newlines *)
let is_newline = ref true

let prev_category = ref ""

let mk_file_formatter file_fmt category0 =
  let f = copy_formatter file_fmt in
  let out_functions_orig = F.pp_get_formatter_out_functions f () in
  let prefix = Printf.sprintf "[%d][%s] " (Pid.to_int (Unix.getpid ())) category0 in
  let print_prefix_if_newline () =
    let category_has_changed =
      (* take category + PID into account *)
      not (phys_equal !prev_category prefix)
    in
    if !is_newline || category_has_changed then (
      if not !is_newline && category_has_changed then
        (* category change but previous line has not ended: print newline *)
        out_functions_orig.out_newline () ;
      is_newline := false ;
      prev_category := prefix ;
      out_functions_orig.out_string prefix 0 (String.length prefix) )
  in
  let out_string s p n =
    print_prefix_if_newline () ;
    out_functions_orig.out_string s p n
  in
  let out_indent n = print_prefix_if_newline () ; out_functions_orig.out_indent n in
  let out_newline () =
    print_prefix_if_newline () ;
    out_functions_orig.out_newline () ;
    is_newline := true
  in
  let out_spaces n = print_prefix_if_newline () ; out_functions_orig.out_spaces n in
  F.pp_set_formatter_out_functions f
    {F.out_string; out_flush= out_functions_orig.out_flush; out_indent; out_newline; out_spaces} ;
  f


let color_console ?(use_stdout= false) scheme =
  let scheme = Option.value scheme ~default:Normal in
  let formatter = if use_stdout then F.std_formatter else F.err_formatter in
  let can_colorize = Unix.(isatty (if use_stdout then stdout else stderr)) in
  if can_colorize then (
    let styles = term_styles_of_style scheme in
    let out_string s p n =
      let print = if use_stdout then ANSITerminal.print_string else ANSITerminal.prerr_string in
      print styles (String.slice s p n)
    in
    F.pp_set_formatter_out_functions formatter
      {(F.pp_get_formatter_out_functions formatter ()) with F.out_string} ;
    formatter )
  else formatter


let register_formatter =
  let all_prefixes = ref [] in
  fun ?use_stdout ?color_scheme prefix ->
    all_prefixes := prefix :: !all_prefixes ;
    (* lazy so that we get a chance to register all prefixes before computing their max length for
       alignment purposes *)
    lazy
      (let max_prefix = List.map ~f:String.length !all_prefixes |> List.fold_left ~f:max ~init:0 in
       let fill =
         let n = max_prefix - String.length prefix in
         String.make n ' '
       in
       let justified_prefix = fill ^ prefix in
       let mk_formatters () =
         let console = color_console ?use_stdout color_scheme in
         match !log_file with
         | Some (file_fmt, _) ->
             let file = mk_file_formatter file_fmt justified_prefix in
             let console_file = dup_formatter console file in
             {file= Some file; console_file}
         | None ->
             {file= None; console_file= console}
       in
       let formatters = mk_formatters () in
       let formatters_ref = ref formatters in
       logging_formatters := ((formatters_ref, mk_formatters), formatters) :: !logging_formatters ;
       formatters_ref)


let flush_formatters {file; console_file} =
  Option.iter file ~f:(fun file -> F.pp_print_flush file ()) ;
  F.pp_print_flush console_file ()


let reset_formatters () =
  let refresh_formatter ((formatters_ref, mk_formatters), formatters) =
    (* flush to be nice *)
    flush_formatters formatters ;
    (* recreate formatters, in particular update PID info *)
    formatters_ref := mk_formatters ()
  in
  let previous_formatters = !logging_formatters in
  (* delete previous formatters *)
  logging_formatters := [] ;
  (* create new formatters *)
  List.iter ~f:refresh_formatter previous_formatters ;
  if not !is_newline then
    Option.iter !log_file ~f:(function log_file, _ -> F.pp_print_newline log_file ()) ;
  is_newline := true


let close_logs () =
  let close_fmt (_, formatters) = flush_formatters formatters in
  List.iter ~f:close_fmt !logging_formatters ;
  Option.iter !log_file ~f:(function file_fmt, chan ->
      F.pp_print_flush file_fmt () ; Out_channel.close chan )


let () = Epilogues.register ~f:close_logs "flushing logs and closing log file"

let log ~to_console ?(to_file= true) (lazy formatters) =
  match (to_console, to_file) with
  | false, false ->
      F.ifprintf F.std_formatter
  | true, _ when not Config.print_logs ->
      F.fprintf !formatters.console_file
  | _ ->
      (* to_console might be true, but in that case so is Config.print_logs so do not print to
         stderr because it will get logs from the log file already *)
      Option.value_map !formatters.file
        ~f:(fun file_fmt -> F.fprintf file_fmt)
        ~default:(F.fprintf F.err_formatter)


let debug_file_fmts = register_formatter "debug"

let debug_dev_file_fmts = register_formatter "local debug"

let environment_info_file_fmts = register_formatter "environment"

let external_warning_file_fmts = register_formatter ~color_scheme:Warning "extern warn"

let external_error_file_fmts = register_formatter ~color_scheme:Error "extern err"

let internal_error_file_fmts = register_formatter ~color_scheme:Error "intern err"

let phase_file_fmts = register_formatter "phase"

let progress_file_fmts = register_formatter "progress"

let result_file_fmts = register_formatter ~use_stdout:true "result"

let user_warning_file_fmts = register_formatter ~color_scheme:Warning "user warn"

let user_error_file_fmts = register_formatter ~color_scheme:Fatal "user err"

let phase fmt = log ~to_console:false phase_file_fmts fmt

let progress fmt = log ~to_console:(not Config.quiet) progress_file_fmts fmt

let progress_bar text =
  log
    ~to_console:(Config.show_progress_bar && not Config.quiet)
    ~to_file:true progress_file_fmts "%s@?" text


let progressbar_file () = progress_bar Config.log_analysis_file

let progressbar_procedure () = progress_bar Config.log_analysis_procedure

let progressbar_timeout_event failure_kind =
  if Config.debug_mode then
    match failure_kind with
    | SymOp.FKtimeout ->
        progress_bar Config.log_analysis_wallclock_timeout
    | SymOp.FKsymops_timeout _ ->
        progress_bar Config.log_analysis_symops_timeout
    | SymOp.FKrecursion_timeout _ ->
        progress_bar Config.log_analysis_recursion_timeout
    | SymOp.FKcrash msg ->
        progress_bar (Printf.sprintf "%s(%s)" Config.log_analysis_crash msg)


let user_warning fmt = log ~to_console:(not Config.quiet) user_warning_file_fmts fmt

let user_error fmt = log ~to_console:true user_error_file_fmts fmt

type debug_level = Quiet | Medium | Verbose [@@deriving compare]

let debug_level_of_int n =
  if n <= 0 then Quiet else if Int.equal n 1 then Medium else (* >= 2 *) Verbose


let debug_dev fmt = log ~to_console:true debug_dev_file_fmts fmt

let analysis_debug_level = debug_level_of_int Config.debug_level_analysis

let bufferoverrun_debug_level = debug_level_of_int Config.bo_debug

let capture_debug_level = debug_level_of_int Config.debug_level_capture

let linters_debug_level = debug_level_of_int Config.debug_level_linters

let mergecapture_debug_level = Quiet

type debug_kind = Analysis | BufferOverrun | Capture | Linters | MergeCapture

let debug kind level fmt =
  let base_level =
    match kind with
    | Analysis ->
        analysis_debug_level
    | BufferOverrun ->
        bufferoverrun_debug_level
    | Capture ->
        capture_debug_level
    | Linters ->
        linters_debug_level
    | MergeCapture ->
        mergecapture_debug_level
  in
  let to_file = compare_debug_level level base_level <= 0 in
  log ~to_console:false ~to_file debug_file_fmts fmt


let result fmt = log ~to_console:true result_file_fmts fmt

let environment_info fmt = log ~to_console:false environment_info_file_fmts fmt

let external_warning fmt = log ~to_console:(not Config.quiet) external_warning_file_fmts fmt

let external_error fmt = log ~to_console:true external_error_file_fmts fmt

let internal_error fmt = log ~to_console:true internal_error_file_fmts fmt

(** Type of location in ml source: __POS__ *)
type ocaml_pos = string * int * int * int

(** Convert a ml location to a string *)
let ocaml_pos_to_string (file, lnum, cnum, enum) =
  Printf.sprintf "%s:%d:%d-%d:" file lnum cnum enum


(** Pretty print a location of ml source *)
let pp_ocaml_pos fmt ocaml_pos = F.fprintf fmt "%s" (ocaml_pos_to_string ocaml_pos)

let pp_ocaml_pos_opt fmt ocaml_pos_opt =
  if Config.developer_mode then
    match ocaml_pos_opt with
    | None ->
        ()
    | Some ocaml_pos ->
        F.fprintf fmt "(%a)" pp_ocaml_pos ocaml_pos


let log_of_kind error fmt =
  match error with
  | UserError ->
      log ~to_console:false user_error_file_fmts fmt
  | ExternalError ->
      log ~to_console:false external_error_file_fmts fmt
  | InternalError ->
      log ~to_console:false internal_error_file_fmts fmt


let die error msg =
  F.kasprintf (fun msg -> log_of_kind error "%s@\n" msg ; raise_error error ~msg) msg


(* create new channel from the log file, and dumps the contents of the temporary log buffer there *)
let setup_log_file () =
  match !log_file with
  | Some _ ->
      (* already set up *)
      ()
  | None ->
      let fmt, chan, preexisting_logfile =
        let results_dir =
          (* if invoked in a sub-dir (e.g., in Buck integrations), log inside the original log
              file *)
          Sys.getenv Config.infer_top_results_dir_env_var
          |> Option.value ~default:Config.results_dir
        in
        (* assumes the results dir exists already *)
        let logfile_path = results_dir ^/ Config.log_file in
        let preexisting_logfile = PolyVariantEqual.( = ) (Sys.file_exists logfile_path) `Yes in
        let chan = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 logfile_path in
        let file_fmt =
          let f = F.formatter_of_out_channel chan in
          if Config.print_logs then dup_formatter f F.err_formatter else f
        in
        (file_fmt, chan, preexisting_logfile)
      in
      log_file := Some (fmt, chan) ;
      if preexisting_logfile then is_newline := false ;
      reset_formatters () ;
      if CLOpt.is_originator && preexisting_logfile then
        phase
          "============================================================@\n\
           = New infer execution begins@\n\
           ============================================================"


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

let delayed_actions = ref []

(** hook for the current printer of delayed print actions *)
let printer_hook = ref (fun _ -> Die.(die InternalError) "uninitialized printer hook")

(** extend the current print log *)
let add_print_action pact =
  if Config.write_html then delayed_actions := pact :: !delayed_actions
  else if not Config.only_cheap_debug then
    Option.iter !log_file ~f:(function file_fmt, _ -> !printer_hook file_fmt pact)


(** reset the delayed print actions *)
let reset_delayed_prints () = delayed_actions := []

(** return the delayed print actions *)
let get_delayed_prints () = !delayed_actions

(** set the delayed print actions *)
let set_delayed_prints new_delayed_actions = delayed_actions := new_delayed_actions

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
  for _ = 1 to indent do s := "  " ^ !s done ;
  if indent <> 0 then add_print_action (PTstr, Obj.repr !s)


(** dump command to increase the indentation level *)
let d_increase_indent (indent: int) = add_print_action (PTincrease_indent, Obj.repr indent)

(** dump command to decrease the indentation level *)
let d_decrease_indent (indent: int) = add_print_action (PTdecrease_indent, Obj.repr indent)
