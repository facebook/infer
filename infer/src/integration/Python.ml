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
  type t = FFI of FFI.Error.t | IR of PyIR.Error.t

  let format_error file error =
    let log level =
      match (level : L.error) with
      | InternalError ->
          L.internal_error
      | ExternalError ->
          L.external_error
      | UserError ->
          L.user_error
    in
    match error with
    | FFI (level, err) ->
        log level "[python:%s][-1] %a@\n" file FFI.Error.pp_kind err
    | IR (level, loc, err) ->
        log level "[python:%s][%a] %a@\n" file PyIR.Location.pp loc PyIR.Error.pp_kind err


  let ffi x = FFI x

  let ir x = IR x
end

let process_file ~is_binary file =
  let open IResult.Let_syntax in
  let _sourcefile = Textual.SourceFile.create file in
  let* code = Result.map_error ~f:Error.ffi @@ FFI.from_file ~is_binary file in
  let* _ir = Result.map_error ~f:Error.ir @@ PyIR.mk ~debug:false code in
  let _ = PyTrans.to_module in
  (* let module = PyTrans.to_module ~sourcefile code *)
  Ok ()


let _dump_file ~next_to_source pyc module_ =
  let filename =
    if next_to_source then
      let filename = SourceFile.create pyc in
      Filename.chop_extension (SourceFile.to_abs_path filename) ^ ".sil"
    else
      let textual_filename = TextualSil.to_filename pyc in
      Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename "sil"
  in
  TextualSil.dump_module ~filename module_


let capture_file ~is_binary file =
  let open TextualParser in
  let open IResult.Let_syntax in
  let _sourcefile = Textual.SourceFile.create file in
  (* let* module_ = process_file ~is_binary file in *)
  let* () = process_file ~is_binary file in
  (* if Config.dump_textual then dump_file ~next_to_source:true file module_ ; *)
  (* let trans = TextualFile.translate_module sourcefile module_ in *)
  let _ = TextualFile.translate_module in
  (* let log_error sourcefile error = *)
  (*   if Config.keep_going then L.debug Capture Quiet "%a@\n" (pp_error sourcefile) error *)
  (*   else L.external_error "%a@\n" (pp_error sourcefile) error *)
  (* in *)
  (* let res = *)
  (*   match trans with *)
  (*   | Ok sil -> *)
  (*       TextualFile.capture ~use_global_tenv:true sil ; *)
  (*       Ok sil.tenv *)
  (*   | Error (sourcefile, errs) -> *)
  (*       List.iter errs ~f:(log_error sourcefile) ; *)
  (*       Error (PyTrans.Error.textual_parser sourcefile) *)
  (* in *)
  (* if Config.debug_mode || Result.is_error trans then dump_file ~next_to_source:false file module_ ; *)
  (* res *)
  Ok ()


let load_textual_model filename =
  let acc_tenv = Tenv.create () in
  L.debug Capture Quiet "Loading textual models in %s@\n" filename ;
  ( match TextualParser.TextualFile.translate (StandaloneFile filename) with
  | Ok sil ->
      TextualParser.TextualFile.capture ~use_global_tenv:true sil ;
      Tenv.merge ~src:sil.tenv ~dst:acc_tenv
  | Error (sourcefile, errs) ->
      List.iter errs ~f:(L.external_error "%a@\n" (TextualParser.pp_error sourcefile)) ) ;
  acc_tenv


let capture_files ~is_binary files =
  let builtins = Config.python_builtin_models in
  let n_files = List.length files in
  let child_action, child_prologue, child_epilogue =
    let child_tenv = Tenv.create () in
    (* TODO: is this the best place to do so ? *)
    let builtin_model = load_textual_model builtins in
    Tenv.merge ~src:builtin_model ~dst:child_tenv ;
    let child_action file =
      let t0 = Mtime_clock.now () in
      !ProcessPoolState.update_status t0 file ;
      match capture_file ~is_binary file with
      | Ok () ->
          (* | Ok file_tenv -> *)
          (*     Tenv.merge ~src:file_tenv ~dst:child_tenv ; *)
          None
      | Error err ->
          Error.format_error file err ;
          Some ()
    in
    let child_prologue _ = Py.initialize ~interpreter:Version.python_exe () in
    let child_epilogue worker_id =
      let tenv_path = ResultsDir.get_path Temporary ^/ "child.tenv" |> DB.filename_from_string in
      let tenv_path = DB.filename_add_suffix tenv_path (ProcessPool.Worker.show_id worker_id) in
      L.debug Capture Quiet "Epilogue: writing child %a tenv to %s@\n" ProcessPool.Worker.pp_id
        worker_id (DB.filename_to_string tenv_path) ;
      Tenv.write child_tenv tenv_path ;
      Py.finalize () ;
      tenv_path
    in
    (child_action, child_prologue, child_epilogue)
  in
  L.progress "Expecting to capture %d files@\n" n_files ;
  (* TODO(vsiles) keep try of the number of success / failures like Hack *)
  let tasks () = ProcessPool.TaskGenerator.of_list files in
  let jobs =
    let per_worker = 100 in
    min ((per_worker + n_files) / per_worker) Config.jobs
  in
  L.debug Capture Quiet "Preparing to capture with %d workers@\n" jobs ;
  let runner = Tasks.Runner.create ~jobs ~child_prologue ~f:child_action ~child_epilogue tasks in
  let child_tenv_paths = Tasks.Runner.run runner in
  (* Merge worker tenvs into a global tenv *)
  let child_tenv_paths =
    Array.mapi child_tenv_paths ~f:(fun child_num tenv_path ->
        match tenv_path with
        | Some tenv_path ->
            tenv_path
        | None ->
            L.die ExternalError "Child %d did't return a path to its tenv" child_num )
  in
  L.progress "Merging type environments...@\n%!" ;
  MergeCapture.merge_global_tenv ~normalize:true (Array.to_list child_tenv_paths) ;
  Array.iter child_tenv_paths ~f:(fun filename -> DB.filename_to_string filename |> Unix.unlink)


let capture input =
  match input with
  | Bytecode {files} ->
      capture_files ~is_binary:true files ;
      L.progress "Finished capture.@\n"
  | Files {prog; args} ->
      if not (String.equal prog "python3") then
        L.die UserError "python3 should be explicitly used instead of %s." prog ;
      capture_files ~is_binary:false args ;
      L.progress "Finished capture.@\n"
