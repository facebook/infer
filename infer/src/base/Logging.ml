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
include Die

(* log files *)

(* Return a formatter that multiplexes to [fmt1] and [fmt2]. *)
let dup_formatter fmt1 fmt2 =
  let out_funs1 = F.pp_get_formatter_out_functions fmt1 () in
  let out_funs2 = F.pp_get_formatter_out_functions fmt2 () in
  F.formatter_of_out_functions
    { F.out_string=
        (fun s p n ->
          out_funs1.out_string s p n ;
          out_funs2.out_string s p n )
    ; out_indent=
        (fun n ->
          out_funs1.out_indent n ;
          out_funs2.out_indent n )
    ; out_flush=
        (fun () ->
          out_funs1.out_flush () ;
          out_funs2.out_flush () )
    ; out_newline=
        (fun () ->
          out_funs1.out_newline () ;
          out_funs2.out_newline () )
    ; out_spaces=
        (fun n ->
          out_funs1.out_spaces n ;
          out_funs2.out_spaces n ) }


(* can be set up to emit to a file later on *)
let log_file = ref None

type formatters =
  { file: F.formatter option  (** send to log file *)
  ; console_file: F.formatter  (** send both to console and log file *) }

let logging_formatters : (formatters ref * (unit -> formatters)) list ref = ref []

(* shared ref is less punishing to sloppy accounting of newlines *)
let is_newline = ref true

let prev_category = ref ""

let mk_file_formatter file_fmt category0 =
  let out_functions_orig = F.pp_get_formatter_out_functions file_fmt () in
  let prefix = Printf.sprintf "[%d][%s] " (Pid.to_int (Unix.getpid ())) category0 in
  let print_prefix_if_newline () =
    let category_has_changed =
      (* take category + PID into account *)
      not (phys_equal !prev_category prefix)
    in
    if !is_newline || category_has_changed then (
      if (not !is_newline) && category_has_changed then
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
  let out_indent n =
    print_prefix_if_newline () ;
    out_functions_orig.out_indent n
  in
  let out_newline () =
    print_prefix_if_newline () ;
    out_functions_orig.out_newline () ;
    is_newline := true
  in
  let out_spaces n =
    print_prefix_if_newline () ;
    out_functions_orig.out_spaces n
  in
  F.formatter_of_out_functions
    {F.out_string; out_flush= out_functions_orig.out_flush; out_indent; out_newline; out_spaces}


let color_console ?(use_stdout = false) scheme =
  let scheme = Option.value scheme ~default:Normal in
  let formatter = if use_stdout then F.std_formatter else F.err_formatter in
  let can_colorize = Unix.(isatty (if use_stdout then stdout else stderr)) in
  if can_colorize then
    let styles = term_styles_of_style scheme in
    let orig_out_functions = F.pp_get_formatter_out_functions formatter () in
    let out_string s p n =
      let s = ANSITerminal.sprintf styles "%s" (String.slice s p n) in
      orig_out_functions.F.out_string s 0 (String.length s)
    in
    let out_newline () =
      (* erase to end-of-line to avoid garbage, in particular when writing over the taskbar *)
      let erase_eol = "\027[0K" in
      orig_out_functions.F.out_string erase_eol 0 (String.length erase_eol) ;
      orig_out_functions.F.out_newline ()
    in
    F.formatter_of_out_functions {orig_out_functions with F.out_string; out_newline}
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
       logging_formatters := (formatters_ref, mk_formatters) :: !logging_formatters ;
       formatters_ref )


let flush_formatter {file; console_file} =
  Option.iter file ~f:(fun file -> F.pp_print_flush file ()) ;
  F.pp_print_flush console_file ()


let flush_formatters () =
  let flush (formatters, _) = flush_formatter !formatters in
  List.iter ~f:flush !logging_formatters


let close_logs () =
  flush_formatters () ;
  Option.iter !log_file ~f:(function file_fmt, chan ->
      F.pp_print_flush file_fmt () ;
      Out_channel.close chan )


let register_epilogue () =
  Epilogues.register_late ~f:close_logs ~description:"flushing logs and closing log file"


let reset_formatters () =
  let refresh_formatter (formatters, mk_formatters) =
    (* flush to be nice *)
    flush_formatter !formatters ;
    (* recreate formatters, in particular update PID info *)
    formatters := mk_formatters ()
  in
  List.iter ~f:refresh_formatter !logging_formatters ;
  if not !is_newline then
    Option.iter !log_file ~f:(function log_file, _ -> F.pp_print_newline log_file ()) ;
  is_newline := true ;
  register_epilogue ()


let () = register_epilogue ()

let log ~to_console ?(to_file = true) (lazy formatters) =
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

let log_task fmt =
  let to_console = match Config.progress_bar with `Plain -> true | `Quiet | `MultiLine -> false in
  log ~to_console progress_file_fmts fmt


let task_progress ~f pp x =
  log_task "%a starting@." pp x ;
  let result = f () in
  log_task "%a DONE@." pp x ;
  result


let user_warning fmt = log ~to_console:(not Config.quiet) user_warning_file_fmts fmt

let user_error fmt = log ~to_console:true user_error_file_fmts fmt

type debug_level = Quiet | Medium | Verbose [@@deriving compare]

let debug_level_of_int n =
  if n <= 0 then Quiet else if Int.equal n 1 then Medium else (* >= 2 *) Verbose


let debug_dev fmt = log ~to_console:true debug_dev_file_fmts fmt

let analysis_debug_level = debug_level_of_int Config.debug_level_analysis

let bufferoverrun_debug_level = debug_level_of_int Config.bo_debug

let capture_debug_level = debug_level_of_int Config.debug_level_capture

let report_debug_level = debug_level_of_int Config.debug_level_report

let mergecapture_debug_level = Quiet

type debug_kind = Analysis | BufferOverrun | Capture | MergeCapture | Report

let debug kind level fmt =
  let base_level =
    match kind with
    | Analysis ->
        analysis_debug_level
    | BufferOverrun ->
        bufferoverrun_debug_level
    | Capture ->
        capture_debug_level
    | MergeCapture ->
        mergecapture_debug_level
    | Report ->
        report_debug_level
  in
  let to_file = compare_debug_level level base_level <= 0 in
  log ~to_console:false ~to_file debug_file_fmts fmt


(** log to scuba as well as in the original logger *)
let wrap_in_scuba_log ~label ~log fmt =
  let wrapper message =
    ScubaLogging.log_message ~label ~message ;
    (* [format_of_string] is there to satisfy the type checker *)
    log (format_of_string "%s") message
  in
  F.kasprintf wrapper fmt


let result_string ?(style = []) s =
  log ~to_console:false result_file_fmts "%s" s ;
  ANSITerminal.print_string style s


let result ?style fmt = F.kasprintf (fun s -> result_string ?style s) fmt

let environment_info fmt = log ~to_console:false environment_info_file_fmts fmt

let external_warning fmt = log ~to_console:(not Config.quiet) external_warning_file_fmts fmt

let external_error fmt = log ~to_console:true external_error_file_fmts fmt

let internal_error fmt = log ~to_console:true internal_error_file_fmts fmt

(* mask original function and replicate log in scuba *)
let internal_error fmt = wrap_in_scuba_log ~label:"internal_error" ~log:internal_error fmt

(** Type of location in ml source: __POS__ *)
type ocaml_pos = string * int * int * int

(** Convert a ml location to a string *)
let ocaml_pos_to_string (file, lnum, cnum, enum) = Printf.sprintf "%s:%d:%d-%d:" file lnum cnum enum

(** Pretty print a location of ml source *)
let pp_ocaml_pos fmt ocaml_pos = F.pp_print_string fmt (ocaml_pos_to_string ocaml_pos)

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
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  F.kasprintf
    (fun msg ->
      log_of_kind error "%s@\n%s@." msg (Caml.Printexc.raw_backtrace_to_string backtrace) ;
      raise_error ~backtrace error ~msg )
    msg


(* create new channel from the log file, and dumps the contents of the temporary log buffer there *)
let setup_log_file () =
  match !log_file with
  | Some _ ->
      (* already set up *)
      ()
  | None ->
      (* TODO T114149430 *)
      let fmt, chan, preexisting_logfile =
        (* if invoked in a sub-dir (e.g., in Buck integrations), log inside the original log file *)
        (* assumes the results dir exists already *)
        let logfile_path =
          ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir Logs
        in
        let preexisting_logfile = ISys.file_exists logfile_path in
        let chan = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 logfile_path in
        let file_fmt =
          let f = F.formatter_of_out_channel chan in
          if Config.print_logs then dup_formatter f F.err_formatter else f
        in
        (file_fmt, chan, preexisting_logfile)
      in
      log_file := Some (fmt, chan) ;
      if preexisting_logfile then is_newline := false ;
      reset_formatters () ;
      EarlyScubaLogging.finish () |> ScubaLogging.log_many ;
      if Config.is_originator && preexisting_logfile then
        phase
          "============================================================@\n\
           = New infer execution begins@\n\
           ============================================================"


let add_init_printer fmt = format_of_string "%t" ^^ fmt

let set_geometry f =
  F.pp_set_geometry f ~max_indent:(Config.margin_html - 10) ~margin:Config.margin_html


type delayed_prints = Buffer.t * F.formatter

let new_delayed_prints () =
  let b = Buffer.create 16 in
  let f = F.formatter_of_buffer b in
  set_geometry f ;
  (b, f)


let delayed_prints = ref (new_delayed_prints ())

(** reset the delayed prints *)
let reset_delayed_prints () = delayed_prints := new_delayed_prints ()

(** return the delayed prints *)
let get_and_reset_delayed_prints () =
  let res = !delayed_prints in
  reset_delayed_prints () ;
  res


let force_and_reset_delayed_prints f =
  let delayed_prints_buffer, delayed_prints_formatter = get_and_reset_delayed_prints () in
  F.pp_print_flush delayed_prints_formatter () ;
  F.pp_print_string f (Buffer.contents delayed_prints_buffer)


(** set the delayed prints *)
let set_delayed_prints new_delayed_prints = delayed_prints := new_delayed_prints

let get_f () =
  if Config.write_html then Some (snd !delayed_prints)
  else if not Config.only_cheap_debug then Option.map ~f:fst !log_file
  else None


let d_kfprintf ?color k f fmt =
  match color with
  | Some color when Config.write_html ->
      F.fprintf f "<span class='%s'>" (Pp.color_string color) ;
      F.kfprintf
        (fun f ->
          F.pp_print_string f "</span>" ;
          k f )
        f fmt
  | _ ->
      F.kfprintf k f fmt


let d_iprintf fmt = Format.ikfprintf ignore Format.err_formatter fmt

let d_kprintf ?color k fmt =
  match get_f () with Some f -> d_kfprintf ?color k f fmt | None -> d_iprintf fmt


let d_kasprintf k fmt =
  match get_f () with
  | Some f ->
      F.kasprintf (fun s -> k f s) (add_init_printer fmt) set_geometry
  | _ ->
      d_iprintf fmt


let d_printf ?color fmt = d_kprintf ?color ignore fmt

let k_force_newline f = F.pp_force_newline f ()

let d_printfln ?color fmt = d_kprintf ?color k_force_newline fmt

let d_pp pp x = d_printf "%a" pp x

let d_pp_with_pe ?color pp x =
  let pe = if Config.write_html then Pp.html (Option.value ~default:Pp.Black color) else Pp.text in
  d_printf ?color "%a" (pp pe) x


(** dump a string *)
let d_str ?color s = d_printf ?color "%s" s

(** dump an error string *)
let d_error s = d_printf ~color:Pp.Red "ERROR: %s" s

(** dump a warning string *)
let d_warning s = d_printf ~color:Pp.Orange "WARNING: %s" s

(** dump an info string *)
let d_info s = d_printf ~color:Pp.Blue "INFO: %s" s

(** dump a newline *)
let d_ln () = d_printf "@\n"

(** dump a string plus newline *)
let d_strln ?color s = d_kprintf ?color k_force_newline "%s" s

let d_printf_escaped ?color fmt =
  d_kasprintf (fun f s -> d_kfprintf ?color ignore f "%s" (Escape.escape_xml s)) fmt


let d_printfln_escaped ?color fmt =
  d_kasprintf (fun f s -> d_kfprintf ?color k_force_newline f "%s" (Escape.escape_xml s)) fmt


(** dump an indentation *)
let d_indent indent =
  if indent <> 0 then
    let s = String.make (2 * indent) ' ' in
    d_str s


let d_increase_indent () = d_printf "  @["

let d_decrease_indent () = d_printf "@]"

let with_indent ?name_color ?(collapsible = false) ?(escape_result = true) ?pp_result ~f name_fmt =
  if not Config.write_html then F.ikfprintf (fun _ -> f ()) Format.std_formatter name_fmt
  else
    let print_block name =
      let block_tag, name_tag = if collapsible then ("details", "summary") else ("div", "div") in
      (* Open details block that has a summary + collapsible execution trace *)
      d_printf "<%s class='d_with_indent'>" block_tag ;
      (* Write a summary that also acts as a toggle for details  *)
      d_printf "<%s class='d_with_indent_name'>" name_tag ;
      d_printf_escaped ?color:name_color "%s" name ;
      d_printf "</%s>" name_tag ;
      (* Open a paragraph for the log of [f] *)
      d_printf "<DIV class='details_child'>@\n" ;
      let result = f () in
      d_printf "</DIV>" ;
      (* Print result if needed *)
      Option.iter pp_result ~f:(fun pp_result ->
          d_printfln "<DIV class='details_result'>" ;
          d_printfln ~color:Green "Result of %s" name ;
          let ppf = if escape_result then d_printf_escaped else d_printf in
          ppf "%a" pp_result result ;
          d_printfln "</DIV>" ) ;
      (* Close details *)
      d_printf "</%s>" block_tag ;
      result
    in
    F.kasprintf print_block name_fmt
