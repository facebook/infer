(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format
module Worker = ProcessPool.Worker

let textual_subcommand = "compile-infer"

(** Utility functions to work with hackc output. *)
module OutputLine = struct
  type t =
    | UnitStart of string  (** Start of a unit with a given filename *)
    | UnitEnd of string  (** End of a unit with a given filename *)
    | UnitCount of int  (** Expected number of units in the output *)
    | Regular of string  (** Regular line of output *)

  let start_marker = "// TEXTUAL UNIT START"

  let end_marker = "// TEXTUAL UNIT END"

  let count_marker = "// TEXTUAL UNIT COUNT"

  let detect line =
    match String.chop_prefix line ~prefix:start_marker with
    | Some filename ->
        UnitStart (String.strip filename)
    | None -> (
      match String.chop_prefix line ~prefix:end_marker with
      | Some filename ->
          UnitEnd (String.strip filename)
      | None -> (
        match String.chop_prefix line ~prefix:count_marker with
        | Some count_str -> (
          match int_of_string_opt (String.strip count_str) with
          | Some cnt ->
              UnitCount cnt
          | None ->
              Regular line )
        | None ->
            Regular line ) )
end

(** Utility wrapper around [In_channel.t] that provides one line of look-ahead. *)
module Peekable_in_channel = struct
  type t =
    { mutable cur_line: string option
          (** [cur_line] is the last line read from [ic], None only on EOF. *)
    ; ic: In_channel.t }

  let mk ic =
    let cur_line = In_channel.input_line ic in
    {cur_line; ic}


  let input_line t =
    let line = t.cur_line in
    if Option.is_some line then t.cur_line <- In_channel.input_line t.ic ;
    line


  let rec input_line_until_nonempty t =
    match input_line t with
    | Some line when not (String.is_empty line) ->
        Some line
    | Some _ ->
        input_line_until_nonempty t
    | None ->
        None


  let discard_line t = input_line t |> ignore

  let peek_line {cur_line} = cur_line
end

(** Utility functions to consume (potentially) multi-file hackc output. *)
module Unit : sig
  type t = private {source_path: string; content: string}

  val extract_units : In_channel.t -> int option * t Seq.t
  (** Returns the expected number of units and a lazy sequence of units extracted from the channel. *)

  val capture_unit : t -> (Tenv.t, unit) Result.t
end = struct
  type t = {source_path: string; content: string}

  let extract_unit pic =
    let buf = Buffer.create 4096 in
    let rec find_start line_opt =
      match line_opt with
      | None ->
          None
      | Some line -> (
        match OutputLine.detect line with
        | UnitStart filename ->
            Buffer.clear buf ;
            acc_unit filename (Peekable_in_channel.input_line pic)
        | _ ->
            L.user_warning "Unexpected line outside of a textual unit: %s@\n" line ;
            find_start (Peekable_in_channel.input_line_until_nonempty pic) )
    and acc_unit source_path line_opt =
      match line_opt with
      | None ->
          L.user_warning "Unfinished unit: %s@\n" source_path ;
          None
      | Some line -> (
        match OutputLine.detect line with
        | UnitEnd end_filename when String.equal source_path end_filename ->
            let content = Buffer.contents buf in
            Buffer.clear buf ;
            Some {source_path; content}
        | UnitEnd end_filename ->
            L.user_warning "Unexpected end of another unit: expected=%s, actual=%s@\n" source_path
              end_filename ;
            find_start (Peekable_in_channel.input_line_until_nonempty pic)
        | UnitStart _ ->
            L.user_warning "Unexpected start of another unit: %s@\n" line ;
            find_start (Some line)
        | UnitCount _ ->
            L.user_warning "Unexpected unit count marker inside a unit: %s@\n" line ;
            acc_unit source_path (Peekable_in_channel.input_line pic)
        | Regular line ->
            (* Accumulate lines in the state *)
            Buffer.add_string buf line ;
            Buffer.add_char buf '\n' ;
            acc_unit source_path (Peekable_in_channel.input_line pic) )
    in
    find_start (Peekable_in_channel.input_line_until_nonempty pic)


  let extract_units ic =
    let pic = Peekable_in_channel.mk ic in
    let line = Peekable_in_channel.peek_line pic in
    match line with
    | None ->
        (Some 0, Seq.empty)
    | Some line ->
        let count_opt =
          match OutputLine.detect line with
          | UnitCount cnt ->
              Peekable_in_channel.discard_line pic ;
              Some cnt
          | _ ->
              None
        in
        (count_opt, Seq.of_dispenser (fun () -> extract_unit pic))


  let dump_textual_to_tmp_file source_path content =
    let textual_filename = TextualSil.to_filename source_path in
    try
      let out_file =
        Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename "sil"
      in
      Out_channel.write_all out_file ~data:content
    with Sys_error err ->
      L.debug Capture Quiet "Error occurred during textual dump of %s: %s" source_path err


  (** Translate and capture a textual unit. Returns [Ok] on success and [Error] if there were errors
      during capture. *)
  let capture_unit {source_path; content} =
    L.debug Capture Quiet "Capturing %s@\n" source_path ;
    let open TextualParser in
    let line_map = LineMap.create content in
    let trans = TextualFile.translate (TranslatedFile {source_path; content; line_map}) in
    let log_error sourcefile error =
      if Config.keep_going then (
        L.debug Capture Quiet "%a@\n" (pp_error sourcefile) error ;
        ScubaLogging.log_message_with_location ~label:"hack_capture_failure"
          ~loc:(SourceFile.to_rel_path (Textual.SourceFile.file sourcefile))
          ~message:(error_to_string sourcefile error) )
      else L.external_error "%a@\n" (pp_error sourcefile) error
    in
    let res =
      match trans with
      | Ok sil ->
          if not Config.hack_verify_capture_only then TextualFile.capture ~use_global_tenv:true sil ;
          Ok sil.tenv
      | Error (sourcefile, errs) ->
          List.iter errs ~f:(log_error sourcefile) ;
          Error ()
    in
    if Config.debug_mode || Result.is_error trans then dump_textual_to_tmp_file source_path content ;
    res
end

(** Bridge between ephemeral [Seq.t] and the interface required by [ProcessPool.TaskGenerator]. *)
module IterSeq = struct
  type 'a t = {estimated_size: int option; mutable seq: 'a Seq.t; mutable n_processed: int}

  let create ?estimated_size seq =
    (* We need to memoize the sequence because when the sequence is ephemeral poking at it with
       e.g. [is_empty] consumes the head of the sequence and we need the head to be persistent. Note
       that the sequence will still be processed in constant memory because we discard the head as
       soon it's no longer needed and process the tail on demand. *)
    let seq = Seq.memoize seq in
    {estimated_size; seq; n_processed= 0}


  let next t =
    match Seq.uncons t.seq with
    | Some (hd, tl) ->
        t.n_processed <- t.n_processed + 1 ;
        t.seq <- tl ;
        Some hd
    | None ->
        None


  let is_empty t = Seq.is_empty t.seq

  let estimated_remaining t =
    match t.estimated_size with
    | Some estimated_size when estimated_size > t.n_processed ->
        estimated_size - t.n_processed
    | _ ->
        0
end

(** Setup for capture workers where each worker writes into its own [capture.db] and [.global.tenv].
    These are then merged in the main process. *)
module CaptureWorker = struct
  type t =
    { action: Unit.t -> unit option
          (** Process individual Hack compilation units. Returns None on success and Some on error
              (due to ProcessPool contract). *)
    ; prologue: Worker.id -> unit
    ; epilogue: Worker.id -> string
          (** Returns the absolute path to infer-out of a worker containing a [capture.db] and
              [.global.tenv] *) }

  let worker_out_dir_name id = F.asprintf "worker-%a-out" Worker.pp_id id

  let is_file_block_listed file =
    Option.exists ~f:(fun re -> Str.string_match re file 0) Config.skip_analysis_in_path
    || Inferconfig.capture_block_list_file_matcher (SourceFile.create file)


  let mk_blueprint () =
    (* Each worker accumulates its own global tenv. In epilogue the tenv is stored to disk and its
       filepath returned to the main process. *)
    let child_tenv = Tenv.create () in
    let action (unit : Unit.t) =
      let t0 = Mtime_clock.now () in
      if is_file_block_listed unit.source_path then None
      else (
        !ProcessPoolState.update_status t0 unit.Unit.source_path ;
        match Unit.capture_unit unit with
        | Ok file_tenv ->
            Tenv.merge ~src:file_tenv ~dst:child_tenv ;
            None
        | Error () ->
            Some () )
    in
    (* Create worker's [infer-out] and connect to a secondary capture DB inside it *)
    let prologue id =
      L.debug Capture Quiet "Running worker %a prologue@\n" Worker.pp_id id ;
      let worker_out_dir_abspath = ResultsDir.get_path Temporary ^/ worker_out_dir_name id in
      Utils.create_dir worker_out_dir_abspath ;
      let capture_db_abspath =
        ResultsDirEntryName.get_path ~results_dir:worker_out_dir_abspath CaptureDB
      in
      let capture_db = Database.Secondary capture_db_abspath in
      DBWriter.override_use_daemon false ;
      Database.create_db capture_db CaptureDatabase ;
      Database.new_database_connection capture_db CaptureDatabase
    in
    (* Write out a [.globa.tenv] and return the path to the worker's out folder *)
    let epilogue id =
      let worker_out_dir_abspath = ResultsDir.get_path Temporary ^/ worker_out_dir_name id in
      let tenv_path =
        ResultsDirEntryName.get_path ~results_dir:worker_out_dir_abspath GlobalTypeEnvironment
      in
      L.debug Capture Quiet "Epilogue: writing worker %a tenv to %s@\n" Worker.pp_id id tenv_path ;
      Tenv.write child_tenv (DB.filename_from_string tenv_path) ;
      worker_out_dir_abspath
    in
    {action; prologue; epilogue}


  (** Generate [infer-deps.txt] from paths to workers' output folders *)
  let write_infer_deps worker_outs =
    let infer_deps_content =
      let pp fmt =
        Pp.seq ~sep:"\n"
          (fun fmt (child_num, out_path) -> F.fprintf fmt "%d\t<skip>\t%s" child_num out_path)
          fmt
      in
      F.asprintf "%a" pp (Array.to_list worker_outs)
    in
    let infer_deps_file = ResultsDir.get_path CaptureDependencies in
    Utils.with_file_out infer_deps_file ~f:(fun oc ->
        Out_channel.output_string oc infer_deps_content )
end

(** Process hackc output from [ic] extracting and capturing individual textual units.

    The structure of hackc output is as follows:

    - COUNT MARKER <count>
    - START MARKER <source path>
    - <content>
    - END MARKER <source path>
    - ... repeat

    The function below processes such input from [ic] line by line and does some light-weight error
    detection mainly to detect situations when different compilation units get mixed up in the
    output (this shouldn't happen normally).

    When the whole compilation unit has been accumulated, [Unit.capture_unit] is called. *)
let process_output_in_parallel ic =
  let unit_count, units = Unit.extract_units ic in
  Option.iter unit_count ~f:(L.progress "Expecting to capture %d files@\n") ;
  let n_captured, n_error = (ref 0, ref 0) in
  (* action's output and on_finish's input are connected and consistent with
     ProcessPool.TaskGenerator's contract *)
  let unit_iter = IterSeq.create ?estimated_size:unit_count units in
  let worker_blueprint = CaptureWorker.mk_blueprint () in
  let on_finish = function Some () -> incr n_error | None -> incr n_captured in
  let tasks () =
    ProcessPool.TaskGenerator.
      { remaining_tasks= (fun () -> IterSeq.estimated_remaining unit_iter)
      ; is_empty= (fun () -> IterSeq.is_empty unit_iter)
      ; finished= (fun ~result _ -> on_finish result)
      ; next= (fun () -> IterSeq.next unit_iter) }
  in
  (* Cap the number of capture workers based on the number of textual units. This will make the
       default behavior more reasonable on a high core-count machine. *)
  let jobs =
    match unit_count with
    | Some unit_count ->
        let units_per_worker = 100 in
        min ((units_per_worker + unit_count) / units_per_worker) Config.jobs
    | None ->
        Config.jobs
  in
  L.debug Capture Quiet "Preparing to capture with %d workers@\n" jobs ;
  let runner =
    Tasks.Runner.create ~with_primary_db:false ~jobs ~child_prologue:worker_blueprint.prologue
      ~f:worker_blueprint.action ~child_epilogue:worker_blueprint.epilogue tasks
  in
  let worker_outs =
    Tasks.Runner.run runner
    |> Array.mapi ~f:(fun worker_num out_path ->
           match out_path with
           | Some out_path ->
               (worker_num, out_path)
           | None ->
               L.die ExternalError "Worker %d did't return a path to its output folder" worker_num )
  in
  CaptureWorker.write_infer_deps worker_outs ;
  if not Config.hack_verify_capture_only then (
    MergeCapture.merge_captured_targets ~root:Config.results_dir ;
    let tenv =
      Tenv.load_global ()
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "Global tenv not found after capture merge" )
    in
    L.progress "Finished capture: success %d files, error %d files.@\n" !n_captured !n_error ;
    ( if Config.debug_level_capture > 0 then (
        let types_with_source, types_without_source =
          Tenv.fold tenv ~init:(Typ.Name.Set.empty, Typ.Name.Set.empty)
            ~f:(fun name {Struct.source_file} (with_, without) ->
              match source_file with
              | None ->
                  (with_, Typ.Name.Set.add name without)
              | Some _ ->
                  (Typ.Name.Set.add name with_, without) )
        in
        L.progress "Tenv types with source: %a.@\n" Typ.Name.Set.pp types_with_source ;
        L.progress "Tenv types without source: %a.@\n" Typ.Name.Set.pp types_without_source )
      else
        let nb_types_without_source =
          Tenv.fold tenv ~init:0 ~f:(fun _name {Struct.source_file} counter ->
              match source_file with None -> 1 + counter | Some _ -> counter )
        in
        L.progress "Tenv contains %d types without source file@\n" nb_types_without_source ) ;
    (tenv, !n_captured, !n_error) )
  else (Tenv.create (), !n_captured, !n_error)


let process_output_sequentially hackc_stdout =
  let _, units = Unit.extract_units hackc_stdout in
  let units = Caml.List.of_seq units in
  let acc_tenv = Tenv.create () in
  let n_captured, n_error =
    List.fold units ~init:(0, 0) ~f:(fun (n_captured, n_error) unit ->
        match Unit.capture_unit unit with
        | Ok tenv ->
            Tenv.merge ~src:tenv ~dst:acc_tenv ;
            (n_captured + 1, n_error)
        | Error () ->
            (n_captured, n_error + 1) )
  in
  (acc_tenv, n_captured, n_error)


(** Start hackc [compiler] with [args] in a subprocess returning its pid and stdout. *)
let start_hackc compiler args =
  let stderr_log = Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "hackc" "stderr" in
  let escaped_cmd = List.map ~f:Escape.escape_shell (compiler :: args) |> String.concat ~sep:" " in
  let redirected_cmd = F.sprintf "exec %s 2>%s" escaped_cmd stderr_log in
  let {Unix.Process_info.stdin; stdout; stderr; pid} =
    Unix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  Unix.close stderr ;
  let stdout = Unix.in_channel_of_descr stdout in
  (pid, stdout)


(** Run hackc [compiler] with [args] and consume results of translation from its stdout. We don't do
    any pre-processing of [args] and let hackc deal with multiple files on its own. We also pipe
    stderr into a temp file just in case. *)
let compile compiler args ~process_output =
  let hackc_pid, hackc_stdout = start_hackc compiler args in
  let tenv, _, n_error = process_output hackc_stdout in
  In_channel.close hackc_stdout ;
  ( match Unix.waitpid hackc_pid with
  | Error _ as status ->
      L.die ExternalError "Error executing hackc: %s@\n" (Unix.Exit_or_signal.to_string_hum status)
  | Ok () ->
      ()
  | exception Unix.Unix_error (ECHILD, _, _) ->
      (* ProcessPool has a code path that awaits any children inside and outside of its pool,
         including possibly a hackc process. When this happens a waitpid above will raise, but it's
         fine. *)
      () ) ;
  if (not Config.keep_going) && n_error > 0 then
    L.die ExternalError
      "There were errors during capture. Re-run with --keep-going to ignore the errors."
  else tenv


let load_textual_models filenames =
  let acc_tenv = Tenv.create () in
  List.iter filenames ~f:(fun filename ->
      L.debug Capture Quiet "Loading textual models in %s@\n" filename ;
      match TextualParser.TextualFile.translate (StandaloneFile filename) with
      | Ok sil ->
          TextualParser.TextualFile.capture ~use_global_tenv:true sil ;
          Tenv.merge ~src:sil.tenv ~dst:acc_tenv
      | Error (sourcefile, errs) ->
          List.iter errs ~f:(L.external_error "%a@\n" (TextualParser.pp_error sourcefile)) ) ;
  acc_tenv


let load_hack_models compiler filenames =
  if List.is_empty filenames then (
    L.debug Capture Quiet "No hack models supplied, skipping...@\n" ;
    Tenv.create () )
  else (
    L.debug Capture Quiet "Preparing to capture %d hack model files:@\n  @[%a@]@\n"
      (List.length filenames) (Pp.seq F.pp_print_string) filenames ;
    let args = textual_subcommand :: filenames in
    compile compiler args ~process_output:process_output_sequentially )


let load_models compiler =
  let builtins = Config.hack_builtin_models in
  let textual, hack =
    Config.hack_models |> List.partition_tf ~f:(String.is_suffix ~suffix:TextualSil.textual_ext)
  in
  let textual_tenv = load_textual_models (builtins :: textual) in
  let hack_tenv = load_hack_models compiler hack in
  (textual_tenv, hack_tenv)


let capture ~prog ~args =
  if List.exists args ~f:(fun arg -> String.equal arg textual_subcommand) then (
    (* In force_integration mode we should use whatever program is provided on the command line to
       support cases where hackc is invoked via buck run or similar. *)
    let compiler =
      if Option.is_some Config.force_integration then prog
      else Option.value Config.hackc_binary ~default:"hackc"
    in
    let captured_tenv = compile compiler args ~process_output:process_output_in_parallel in
    let textual_model_tenv, hack_model_tenv = load_models compiler in
    Tenv.merge ~src:hack_model_tenv ~dst:captured_tenv ;
    Tenv.merge ~src:textual_model_tenv ~dst:captured_tenv ;
    (* normalization already happened in the compile call through merging, no point repeating it *)
    Tenv.store_global ~normalize:false captured_tenv )
  else L.die UserError "hackc command line is missing %s subcommand" textual_subcommand


let location_of_class_db db ~class_name =
  (* TODO(vsiles): sanitize class_name *)
  let query =
    Printf.sprintf "SELECT PATH_SUFFIX FROM NAMING_FILE_INFO WHERE CLASSES = \"%s\";" class_name
  in
  let locations = ref [] in
  let cb row =
    Array.iter row ~f:(function None -> () | Some row -> locations := row :: !locations)
  in
  let error = Sqlite3.exec_no_headers db ~cb query in
  (error, !locations)


let location_of_class ~naming_table ~class_name =
  let open Sqlite3 in
  let& db = Sqlite3.db_open ~mode:`READONLY naming_table in
  location_of_class_db db ~class_name
