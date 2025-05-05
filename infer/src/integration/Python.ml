(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type kind = Bytecode of {files: string list} | Files of {prog: string; args: string list}

module Error = struct
  type t =
    | FFI of FFI.Error.t
    | IR of PyIR.Error.t
    | TextualVerification of (Textual.SourceFile.t * TextualVerification.error list)
    | TextualTransformation of (Textual.SourceFile.t * Textual.transform_error list)

  let log level =
    let level = if Config.keep_going then L.InternalError else level in
    match (level : L.error) with
    | InternalError ->
        L.internal_error
    | ExternalError ->
        L.external_error
    | UserError ->
        L.user_error


  let format_error file error =
    match error with
    | FFI (level, err) ->
        log level "[python:%s][-1] %a@\n" file FFI.Error.pp_kind err
    | IR (level, loc, err) ->
        log level "[python:%s][%a] %a@\n" file PyIR.Location.pp loc PyIR.Error.pp_kind err
    | TextualVerification (sourcefile, errs) ->
        List.iter errs
          ~f:(log L.InternalError "%a@\n" (TextualVerification.pp_error_with_sourcefile sourcefile))
    | TextualTransformation (sourcefile, errs) ->
        List.iter errs ~f:(log L.InternalError "%a@\n" (Textual.pp_transform_error sourcefile))


  let ffi x = FFI x

  let ir x = IR x

  let textual_verification sourcefile list = TextualVerification (sourcefile, list)

  let textual_transformation sourcefile list = TextualTransformation (sourcefile, list)
end

module Interpreter = struct
  let process_file file =
    let open IResult.Let_syntax in
    let* code = Result.map_error ~f:Error.ffi @@ FFI.from_file ~is_binary:false file in
    Result.map_error ~f:Error.ir @@ PyIR.mk ~debug:false ~path_prefix:None code


  let process_files files =
    let open IResult.Let_syntax in
    let+ modules =
      List.fold_result (List.rev files) ~init:[] ~f:(fun l file ->
          let+ ir = process_file file |> Result.map_error ~f:(fun err -> (file, err)) in
          ir :: l )
    in
    PyIRExec.run_files modules


  let run files =
    Py.initialize ~interpreter:Version.python_exe () ;
    ( match process_files files with
    | Error (file, err) ->
        Error.format_error file err ;
        ()
    | Ok () ->
        () ) ;
    Py.finalize ()
end

let dump_textual_file ~version pyc module_ =
  let suffix = Format.asprintf ".v%d.sil" version in
  let filename =
    let textual_filename = TextualSil.to_filename pyc in
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename suffix
  in
  TextualSil.dump_module ~show_location:true ~filename module_


type process_file_output = CapturedTenv of Tenv.t | CapturedSkipDb | NotCapturedTooManyImports

let process_file ~is_binary file =
  let open IResult.Let_syntax in
  let project_root_prefix =
    if Config.python_trim_source_paths then
      Utils.filename_to_relative ~root:Config.buck2_root Config.project_root
    else None
  in
  let sourcefile =
    let file' =
      (* if buck2_root and project_root are different, we need to use absolute paths in
         order for Config.project_root to be properly applied in SourceFile.create *)
      if Option.is_some project_root_prefix then
        Utils.filename_to_absolute ~root:Config.buck2_root file
      else file
    in
    Textual.SourceFile.create file'
  in
  let* code = FFI.from_file ~is_binary file |> Result.map_error ~f:Error.ffi in
  let* pyir =
    PyIR.mk ~debug:false ~path_prefix:project_root_prefix code |> Result.map_error ~f:Error.ir
  in
  if
    Option.is_some Config.python_skip_capture_imports_threshold
    && pyir.PyIR.Module.stats.count_imported_modules
       > Option.value_exn Config.python_skip_capture_imports_threshold
  then Ok NotCapturedTooManyImports
  else
    let textual = PyIR2Textual.mk_module pyir in
    if Config.debug_mode then dump_textual_file ~version:0 file textual ;
    let* verified_textual =
      let f = Error.textual_verification sourcefile in
      TextualVerification.verify textual |> Result.map_error ~f
    in
    if Config.debug_mode then dump_textual_file ~version:1 file verified_textual ;
    let transformed_textual, decls = TextualTransform.run Python verified_textual in
    let {PyIR.Module.name= module_name} = pyir in
    let transformed_textual =
      PyIRTypeInference.gen_module_default_type pyir
      |> Option.value_map ~default:transformed_textual ~f:(fun pyir_type ->
             PyIR2Textual.add_pyir_type pyir_type ~module_name transformed_textual )
    in
    if Config.debug_mode then dump_textual_file ~version:2 file transformed_textual ;
    let* cfg, tenv =
      let f = Error.textual_transformation sourcefile in
      TextualSil.module_to_sil Python transformed_textual decls |> Result.map_error ~f
    in
    let sil = {TextualParser.TextualFile.sourcefile; cfg; tenv} in
    if Config.python_skip_db then Ok CapturedSkipDb
    else (
      TextualParser.TextualFile.capture ~use_global_tenv:true sil ;
      Ok (CapturedTenv tenv) )


let is_file_block_listed file =
  Option.exists Config.python_skip_capture_path_regex ~f:(fun regexp ->
      Str.string_match regexp file 0 )


let capture_file ~is_binary file = process_file ~is_binary file

let write_infer_deps out_dirs =
  ResultsDir.get_path CaptureDependencies
  |> Utils.with_file_out ~f:(fun out_channel ->
         Array.iter out_dirs ~f:(fun out_dir_opt ->
             Option.iter out_dir_opt ~f:(fun out_dir ->
                 Out_channel.output_string out_channel (Printf.sprintf "-\t-\t%s\n" out_dir) ) ) )


type not_captured_reason = SkippedBlocked | SkippedTooManyImports | Error

let capture_files ~is_binary files =
  let n_files = List.length files in
  let interpreter = Config.python_exe |> Option.value ~default:Version.python_exe in
  let child_action, child_prologue, child_epilogue =
    let child_tenv = Tenv.create () in
    let child_action file =
      if is_file_block_listed file then Some SkippedBlocked
      else
        let t0 = Mtime_clock.now () in
        !WorkerPoolState.update_status (Some t0) file ;
        match capture_file ~is_binary file with
        | Ok (CapturedTenv file_tenv) ->
            Tenv.merge ~src:file_tenv ~dst:child_tenv ;
            None
        | Ok CapturedSkipDb ->
            None
        | Ok NotCapturedTooManyImports ->
            Some SkippedTooManyImports
        | Error err ->
            Error.format_error file err ;
            Some Error
    in
    let worker_out_dir_name id = Format.asprintf "worker-%a-out" ProcessPool.Worker.pp_id id in
    let child_prologue id =
      let worker_out_dir_abspath = ResultsDir.get_path Temporary ^/ worker_out_dir_name id in
      Utils.create_dir worker_out_dir_abspath ;
      let capture_db_abspath =
        ResultsDirEntryName.get_path ~results_dir:worker_out_dir_abspath CaptureDB
      in
      let capture_db = Database.Secondary capture_db_abspath in
      DBWriterProcess.override_use_daemon false ;
      Database.create_db capture_db CaptureDatabase ;
      Database.new_database_connection capture_db CaptureDatabase ;
      Py.initialize ~interpreter ()
    in
    let child_epilogue worker_id =
      let worker_out_dir_abspath = ResultsDir.get_path Temporary ^/ worker_out_dir_name worker_id in
      let tenv_path =
        ResultsDirEntryName.get_path ~results_dir:worker_out_dir_abspath GlobalTypeEnvironment
      in
      L.debug Capture Quiet "Epilogue: writing child %a tenv to %s@\n" ProcessPool.Worker.pp_id
        worker_id tenv_path ;
      Tenv.write child_tenv (DB.filename_from_string tenv_path) ;
      Py.finalize () ;
      worker_out_dir_abspath
    in
    (child_action, child_prologue, child_epilogue)
  in
  L.progress "Expecting to capture %d files@\n" n_files ;
  (* TODO(vsiles) keep track of the number of success / failures like Hack *)
  let n_captured, n_error, n_skipped_blocked, n_skipped_too_many_imports =
    (ref 0, ref 0, ref 0, ref 0)
  in
  let tasks () =
    TaskGenerator.of_list files ~finish:(fun result _ ->
        match result with
        | Some Error ->
            incr n_error ;
            None
        | Some SkippedBlocked ->
            incr n_skipped_blocked ;
            None
        | Some SkippedTooManyImports ->
            incr n_skipped_too_many_imports ;
            None
        | None ->
            incr n_captured ;
            None )
  in
  let jobs = min n_files Config.jobs in
  L.debug Capture Quiet "Preparing to capture with %d workers@\n" jobs ;
  let runner =
    ProcessPool.create ~with_primary_db:false ~jobs ~child_prologue ~f:child_action ~child_epilogue
      ~tasks ()
  in
  let child_out_dirs = ProcessPool.run runner in
  write_infer_deps child_out_dirs ;
  L.progress "Success: %d files@\n" !n_captured ;
  L.progress "Skipped: %d blocked files@\n" !n_skipped_blocked ;
  L.progress "Skipped: %d files with too many imports@\n" !n_skipped_too_many_imports ;
  L.progress "Failure: %d files@\n" !n_error ;
  L.progress "Merging type environments...@\n%!" ;
  if not Config.python_skip_db then MergeCapture.merge_captured_targets ~root:Config.results_dir


let capture input =
  match input with
  | Bytecode {files} ->
      capture_files ~is_binary:true files ;
      L.progress "Finished capture.@\n"
  | Files {prog; args} ->
      if not (String.equal prog "python3") then
        L.die UserError "python3 should be explicitly used instead of %s." prog ;
      let files =
        match Config.python_files_index with
        | Some f -> (
          match Utils.read_file f with
          | Ok lines ->
              lines @ args
          | Error error ->
              L.die UserError "Error reading the python input files index '%s': %s@." f error )
        | None ->
            args
      in
      if Config.run_python_interpreter then Interpreter.run files
      else capture_files ~is_binary:false files ;
      L.progress "Finished capture.@\n"
