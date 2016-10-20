(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Top-level driver that orchestrates build system integration, frontends, and backend *)

(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json = DB.Results_dir.(path_to_filename Abs_root ["report.json"]) in
  match read_file (DB.filename_to_string issues_json) with
  | Some lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat "" lines in
      if issues <> [] then exit Config.fail_on_issue_exit_code
  | None -> ()

let () =
  let infer_py = Config.lib_dir // "python" // "infer.py" in
  let build_cmd = IList.rev Config.rest in
  let in_buck_mode = match build_cmd with "buck" :: _ -> true | _ -> false in
  let args_py =
    Array.of_list (
      infer_py ::
      Config.anon_args @
      (if not Config.absolute_paths then [] else
         ["--absolute-paths"]) @
      (match Config.analyzer with None -> [] | Some a ->
          ["--analyzer";
           IList.assoc (=) a (IList.map (fun (n,a) -> (a,n)) Config.string_to_analyzer)]) @
      (match Config.blacklist with
       | Some s when in_buck_mode -> ["--blacklist-regex"; s]
       | _ -> []) @
      (if not Config.create_harness then [] else
         ["--android-harness"]) @
      (if not Config.buck then [] else
         ["--buck"]) @
      (match Config.java_jar_compiler with None -> [] | Some p ->
          ["--java-jar-compiler"; p]) @
      (match IList.rev Config.buck_build_args with
       | args when in_buck_mode ->
           IList.map (fun arg -> ["--Xbuck"; "'" ^ arg ^ "'"]) args |> IList.flatten
       | _ -> []) @
      (if not Config.continue_capture then [] else
         ["--continue"]) @
      (if not Config.debug_mode then [] else
         ["--debug"]) @
      (if not Config.debug_exceptions then [] else
         ["--debug-exceptions"]) @
      (if Config.filtering then [] else
         ["--no-filtering"]) @
      (if not Config.flavors || not in_buck_mode then [] else
         ["--use-flavors"]) @
      (if Option.is_none Config.use_compilation_database || not in_buck_mode then [] else
         ["--use-compilation-database"]) @
      "-j" :: (string_of_int Config.jobs) ::
      "-l" :: (string_of_float Config.load_average) ::
      (if not Config.pmd_xml then [] else
         ["--pmd-xml"]) @
      (if not Config.reactive_mode then [] else
         ["--reactive"]) @
      "--out" :: Config.results_dir ::
      (match Config.project_root with None -> [] | Some pr ->
          ["--project_root"; pr]) @
      (match Config.xcode_developer_dir with None -> [] | Some d ->
          ["--xcode-developer-dir"; d]) @
      (if Config.rest = [] then [] else
         ("--" :: build_cmd))
    ) in
  let pid = Unix.create_process args_py.(0) args_py Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  let exit_code = match status with
    | Unix.WEXITED i -> i
    | _ -> 1 in
  if exit_code = Config.infer_py_argparse_error_exit_code then
    (* swallow infer.py argument parsing error *)
    Config.print_usage_exit ();
  if exit_code <> 0 then (
    prerr_endline ("Failed to execute: " ^ (String.concat " " (Array.to_list args_py))) ;
    exit exit_code
  );
  if Config.is_originator then (
    if Config.analyzer = Some Config.Crashcontext then
      Crashcontext.crashcontext_epilogue ~in_buck_mode;
    if Config.fail_on_bug then
      fail_on_issue_epilogue ();
  )
