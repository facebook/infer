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

let set_env_for_clang_wrapper () =
  (match Config.clang_include_to_override with
   | Some dir -> Unix.putenv "FCP_CLANG_INCLUDE_TO_REPLACE" dir
   | None -> ()
  );
  if Config.cxx_experimental then
    Unix.putenv "FCP_INFER_CXX_MODELS" "1" ;
  if Config.debug_mode || Config.frontend_stats then
    Unix.putenv "FCP_DEBUG_MODE" "1" ;
  if not Config.failures_allowed then
    Unix.putenv "FCP_REPORT_FRONTEND_FAILURE" "1" ;
  if Config.llvm then
    Unix.putenv "LLVM_MODE" "1"

let () =
  set_env_for_clang_wrapper () ;
  (* The infer executable in the bin directory is a symbolic link to the real binary in the lib
     directory, so that the python script in the lib directory can be found relative to it. *)
  let real_exe =
    match Unix.readlink Sys.executable_name with
    | link when Filename.is_relative link ->
        (* Sys.executable_name is a relative symbolic link *)
        (Filename.dirname Sys.executable_name) // link
    | link ->
        (* Sys.executable_name is an absolute symbolic link *)
        link
    | exception Unix.Unix_error(Unix.EINVAL, _, _) ->
        (* Sys.executable_name is not a symbolic link *)
        Sys.executable_name
  in
  let infer_py = (Filename.dirname real_exe) // "python" // "infer.py" in
  let build_cmd = IList.rev Config.rest in
  let buck = match build_cmd with "buck" :: _ -> true | _ -> false in
  let args_py =
    Array.of_list (
      infer_py ::
      Config.anon_args @
      (match Config.analyzer with None -> [] | Some a ->
          ["--analyzer";
           IList.assoc (=) a (IList.map (fun (n,a) -> (a,n)) Config.string_to_analyzer)]) @
      (match Config.blacklist with
       | Some s when buck -> ["--blacklist-regex"; s]
       | _ -> []) @
      (if not Config.create_harness then [] else
         ["--android-harness"]) @
      (if not Config.buck then [] else
         ["--buck"]) @
      (match IList.rev Config.buck_build_args with
       | args when buck ->
           IList.map (fun arg -> ["--Xbuck"; "'" ^ arg ^ "'"]) args |> IList.flatten
       | _ -> []) @
      (if not Config.debug_mode then [] else
         ["--debug"]) @
      (if not Config.debug_exceptions then [] else
         ["--debug-exceptions"]) @
      (if Config.filtering then [] else
         ["--no-filtering"]) @
      (if not Config.flavors || not buck then [] else
         ["--use-flavors"]) @
      (match Config.infer_cache with None -> [] | Some s ->
        ["--infer_cache"; s]) @
      "--multicore" :: (string_of_int Config.jobs) ::
      "--out" :: Config.results_dir ::
      (match Config.project_root with None -> [] | Some pr ->
        ["--project_root"; pr]) @
      (if Config.rest = [] then [] else
         ("--" :: build_cmd))
    ) in
  let pid = Unix.create_process args_py.(0) args_py Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  if status <> Unix.WEXITED 0 then (
    prerr_endline ("Failed to execute: " ^ (String.concat " " (Array.to_list args_py))) ;
    exit 1
  );
  ()
