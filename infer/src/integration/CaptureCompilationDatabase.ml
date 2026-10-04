(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let create_cmd (source_file, (compilation_data : CompilationDatabase.compilation_data)) =
  let swap_executable cmd =
    if String.is_suffix ~suffix:"++" cmd then Config.wrappers_dir ^/ "clang++"
    else Config.wrappers_dir ^/ "clang"
  in
  let arg_file =
    ClangQuotes.mk_arg_file "cdb_clang_args" ClangQuotes.EscapedNoQuotes
      compilation_data.escaped_arguments
  in
  ( source_file
  , { CompilationDatabase.directory= compilation_data.directory
    ; executable= swap_executable compilation_data.executable
    ; escaped_arguments=
        ["@" ^ arg_file; "-fsyntax-only"; "-fno-builtin"] @ Config.clang_extra_flags } )


let invoke_cmd ~is_includer includer_failures
    (source_file, (cmd : CompilationDatabase.compilation_data)) =
  let argv = cmd.executable :: cmd.escaped_arguments in
  ( InferSubprocess.run ~cwd:cmd.directory ~prog:cmd.executable ~argv ()
  |> function
  | Ok () ->
      ()
  | Error error ->
      if is_includer source_file then incr includer_failures ;
      let log_or_die fmt =
        if Config.keep_going || is_includer source_file then L.debug Capture Quiet fmt
        else L.die ExternalError fmt
      in
      log_or_die "Error running compilation for '%a': %a:@\n%s@." SourceFile.pp source_file
        Pp.cli_args argv error ) ;
  None


let run_compilation_database ?(is_includer = fun _ -> false) compilation_database
    should_capture_file =
  let compilation_data =
    CompilationDatabase.filter_compilation_data compilation_database ~f:should_capture_file
  in
  let number_of_jobs = List.length compilation_data in
  L.(debug Capture Quiet)
    "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  L.progress "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  let compilation_commands = List.map ~f:create_cmd compilation_data in
  let tasks () =
    TaskGenerator.of_list ~finish:TaskGenerator.finish_always_none compilation_commands
  in
  (* failures of each worker to capture the files added by [--capture-includers-of-changed-headers],
     returned by [child_epilogue] *)
  let includer_failures = ref 0 in
  let includer_failures =
    ProcessPool.create ~jobs:Config.jobs ~child_prologue:ignore
      ~f:(invoke_cmd ~is_includer includer_failures)
      ~child_epilogue:(fun _ -> !includer_failures)
      ~tasks ()
    |> ProcessPool.run
    |> Array.sum (module Int) ~f:(Option.value ~default:0)
  in
  L.progress "@." ;
  if includer_failures > 0 then
    L.external_warning
      "Could not capture %d source files that include the changed headers; see the logs for \
       details.@."
      includer_failures ;
  L.(debug Analysis Medium) "Ran %d jobs" number_of_jobs


let get_compilation_database_files_xcodebuild ~prog ~args =
  let tmp_file = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "cdb" ".json" in
  let xcodebuild_prog, xcodebuild_args = (prog, prog :: args) in
  let xcpretty_prog = "xcpretty" in
  let xcpretty_args =
    [xcpretty_prog; "--report"; "json-compilation-database"; "--output"; tmp_file]
  in
  L.(debug Capture Quiet)
    "Running %s | %s@\n@."
    (List.to_string ~f:Fn.id xcodebuild_args)
    (List.to_string ~f:Fn.id xcpretty_args) ;
  let producer_status, consumer_status =
    Process.pipeline ~producer_prog:xcodebuild_prog ~producer_args:xcodebuild_args
      ~consumer_prog:xcpretty_prog ~consumer_args:xcpretty_args
  in
  match (producer_status, consumer_status) with
  | Ok (), Ok () ->
      [`Escaped tmp_file]
  | _ ->
      L.(die ExternalError) "There was an error executing the build command"


let headers_to_scan changed_files compilation_database =
  SourceFile.Set.filter
    (fun file ->
      SourceFile.is_header file
      && (not (CompilationDatabase.mem file compilation_database))
      (* with [--suffix-match-changed-files], the entries can be partial paths *)
      && (Config.suffix_match_changed_files || ISys.file_exists (SourceFile.to_abs_path file)) )
    changed_files


let check_changed_files_in_database changed_files compilation_database =
  let not_in_database =
    SourceFile.Set.filter
      (fun file -> not (CompilationDatabase.mem file compilation_database))
      changed_files
  in
  if not (SourceFile.Set.is_empty not_in_database) then (
    L.debug Capture Quiet "Changed files without an entry in the compilation database: %a@\n"
      (Pp.seq ~sep:", " SourceFile.pp)
      (SourceFile.Set.elements not_in_database) ;
    if SourceFile.Set.equal not_in_database changed_files then
      L.user_warning
        "None of the files in the changed files index has an entry in the compilation database: \
         nothing will be captured.%s@."
        ( if not (SourceFile.Set.exists SourceFile.is_header changed_files) then ""
          else if not Config.capture_includers_of_changed_headers then
            " Headers are captured through the source files that include them: add these source \
             files to the changed files index, or pass --capture-includers-of-changed-headers."
          else if SourceFile.Set.is_empty (headers_to_scan changed_files compilation_database) then
            " The changed headers do not exist, so the source files that include them were not \
             searched."
          else " No entry of the compilation database includes the changed headers." ) )


let includes_one_of ~headers ~header_names
    {CompilationDatabase.directory; executable; escaped_arguments} =
  let arg_file =
    ClangQuotes.mk_arg_file "cdb_deps_args" ClangQuotes.EscapedNoQuotes escaped_arguments
  in
  let included_files =
    try
      Utils.do_in_dir ~dir:directory ~f:(fun () ->
          ClangWrapper.included_files ~prog:executable ~args:["@" ^ arg_file] )
    with Unix.Unix_error (error, f, arg) ->
      Error (Printf.sprintf "%s(%s): %s" f arg (IUnix.Error.message error))
  in
  Unix.unlink arg_file ;
  let is_changed_header file =
    let abs_file = if Filename.is_relative file then directory ^/ file else file in
    SourceFile.from_abs_path ~warn_on_error:false abs_file
    |> SourceFile.is_changed ~changed_files:headers
  in
  (* comparing the names first avoids resolving the path of every included file *)
  Result.map included_files
    ~f:
      (List.exists ~f:(fun file ->
           IString.Set.mem (Filename.basename file) header_names && is_changed_header file ) )


let find_includers ~headers entries =
  L.progress
    "Finding the source files that include the changed headers in %d entries of the compilation \
     database@."
    (List.length entries) ;
  let header_names =
    SourceFile.Set.elements headers
    |> List.map ~f:(fun header -> Filename.basename (SourceFile.to_rel_path header))
    |> IString.Set.of_list
  in
  (* results of each worker, returned by [child_epilogue] *)
  let includers = ref [] in
  let failures = ref 0 in
  let f (source_file, compilation_data) =
    ( match includes_one_of ~headers ~header_names compilation_data with
    | Ok true ->
        includers := source_file :: !includers
    | Ok false ->
        ()
    | Error error ->
        L.debug Capture Quiet "Could not compute the files included by '%a': %s@\n" SourceFile.pp
          source_file error ;
        incr failures ) ;
    None
  in
  let includers, failures =
    ProcessPool.create ~jobs:Config.jobs ~child_prologue:ignore ~f
      ~child_epilogue:(fun _ -> (!includers, !failures))
      ~tasks:(fun () -> TaskGenerator.of_list ~finish:TaskGenerator.finish_always_none entries)
      ()
    |> ProcessPool.run
    |> Array.fold ~init:(SourceFile.Set.empty, 0) ~f:(fun ((includers, failures) as acc) -> function
      | Some (worker_includers, worker_failures) ->
          ( SourceFile.Set.union includers (SourceFile.Set.of_list worker_includers)
          , failures + worker_failures )
      | None ->
          acc )
  in
  if failures > 0 then
    L.external_warning
      "Could not compute the files included by %d entries of the compilation database, so they are \
       not captured; see the logs for details.@."
      failures ;
  L.debug Capture Quiet "Source files that include the changed headers: %a@\n"
    (Pp.seq ~sep:", " SourceFile.pp)
    (SourceFile.Set.elements includers) ;
  includers


(** the entries of the compilation database that are not in [changed_files] and include one of its
    headers *)
let find_includers_of_changed_headers changed_files compilation_database =
  let headers = headers_to_scan changed_files compilation_database in
  if SourceFile.Set.is_empty headers then SourceFile.Set.empty
  else
    CompilationDatabase.filter_compilation_data compilation_database ~f:(fun source_file ->
        not
          ( Inferconfig.skip_analysis_in_path_matcher source_file
          || SourceFile.Set.mem source_file changed_files ) )
    |> find_includers ~headers


let capture_files_in_database ~changed_files compilation_database =
  let is_skipped = Inferconfig.skip_analysis_in_path_matcher in
  match changed_files with
  | None ->
      run_compilation_database compilation_database (fun source_file ->
          not (is_skipped source_file) )
  | Some changed_files ->
      let includers =
        if Config.capture_includers_of_changed_headers then
          find_includers_of_changed_headers changed_files compilation_database
        else SourceFile.Set.empty
      in
      let changed_files = SourceFile.Set.union changed_files includers in
      check_changed_files_in_database changed_files compilation_database ;
      run_compilation_database compilation_database
        ~is_includer:(fun source_file -> SourceFile.Set.mem source_file includers)
        (fun source_file ->
          (not (is_skipped source_file)) && SourceFile.Set.mem source_file changed_files )


let capture ~changed_files ~db_files =
  let root = Config.project_root in
  let clang_compilation_dbs =
    List.map db_files ~f:(function
      | `Escaped fname ->
          `Escaped (Utils.filename_to_absolute ~root fname)
      | `Raw fname ->
          `Raw (Utils.filename_to_absolute ~root fname) )
  in
  let compilation_database = CompilationDatabase.from_json_files clang_compilation_dbs in
  capture_files_in_database ~changed_files compilation_database
