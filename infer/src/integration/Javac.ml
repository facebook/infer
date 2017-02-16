(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format

type compiler = Java | Javac [@@ deriving compare]

let compile compiler build_prog build_args =
  let prog, prog_args =
    match compiler, Config.java_jar_compiler with
    | _, None -> (build_prog, ["-J-Duser.language=en"])
    | Java, Some jar -> (build_prog, ["-jar"; jar])
    | _, Some jar -> (* fall back to java in PATH to avoid passing -jar to javac *)
        ("java", ["-jar"; jar]) in
  let cli_args, file_args =
    let rec has_classes_out = function
      | [] -> false
      | ("-d" | "-classes_out")::_ -> true
      | file_arg::tl when String.is_prefix file_arg ~prefix:"@" -> (
          let fname = String.slice file_arg 1 (String.length file_arg) in
          match In_channel.read_lines fname with
          | lines ->
              (* crude but we only care about simple cases that will not involve trickiness, eg
                 unbalanced or escaped quotes such as "ending in\"" *)
              let lines_without_quotes =
                List.map ~f:(String.strip ~drop:(function '"' | '\'' -> true | _ -> false)) lines in
              has_classes_out lines_without_quotes || has_classes_out tl
          | exception _ ->
              has_classes_out tl)
      | _::tl ->
          has_classes_out tl in
    let args =
      "-verbose" :: "-g" ::
      (* Ensure that some form of "-d ..." is passed to javac. It's unclear whether this is strictly
         needed but the tests break without this for now. See discussion in D4397716. *)
      if has_classes_out build_args then
        build_args
      else
        "-d" :: Config.javac_classes_out :: build_args in
    List.partition_tf args ~f:(fun arg ->
        (* As mandated by javac, argument files must not contain certain arguments. *)
        String.is_prefix ~prefix:"-J" arg || String.is_prefix ~prefix:"@" arg) in
  (* Pass non-special args via a file to avoid exceeding the command line size limit. *)
  let args_file =
    let file = Filename.temp_file "args_" "" in
    let quoted_file_args =
      List.map file_args ~f:(fun arg ->
          if String.contains arg '\'' then arg else F.sprintf "'%s'" arg) in
    Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc quoted_file_args) ;
    file in
  let cli_file_args = cli_args @ ["@" ^ args_file] in
  let args = prog_args @ cli_file_args in
  let verbose_out_file = Filename.temp_file "javac_" ".out" in
  Unix.with_file verbose_out_file ~mode:[Unix.O_WRONLY] ~f:(
    fun verbose_out_fd ->
      L.out "Logging into %s@\n" verbose_out_file;
      L.out "Current working directory: '%s'@." (Sys.getcwd ());
      try
        L.out "Trying to execute: '%s' '%s'@." prog (String.concat ~sep:"' '" args);
        Unix_.fork_redirect_exec_wait ~prog ~args ~stderr:verbose_out_fd ()
      with exn ->
      try
        L.out "*** Failed!@\nTrying to execute javac instead: '%s' '%s'@\nLogging into %s@."
          "javac" (String.concat ~sep:"' '" cli_file_args) verbose_out_file;
        Unix_.fork_redirect_exec_wait ~prog:"javac" ~args:cli_file_args ~stderr:verbose_out_fd ()
      with _ ->
        L.stderr "Failed to execute: %s %s@." prog (String.concat ~sep:" " args);
        raise exn
  );
  verbose_out_file


let capture compiler ~prog ~args =
  let verbose_out_file = compile compiler prog args in
  if Config.analyzer <> Config.Compile then
    JMain.from_verbose_out verbose_out_file;
  if not (Config.debug_mode || Config.stats_mode) then Unix.unlink verbose_out_file
