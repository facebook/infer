(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let arg_start_pattern = " Compiler arguments: "

type javac_data = {files: string list; opts: string list}

(* file_st / opt_st are 'stacks' where parts of opts or filenames accumulate,
   these will then later be concatenated and added to files / opts *)
type fold_state = {files: string list; opts: string list; opt_st: string list; file_st: string list}

(* see GradleTest.ml *)
let parse_gradle_line ~line =
  let concat_st lst st = if List.is_empty st then lst else String.concat ~sep:" " st :: lst in
  let file_exist file = PolyVariantEqual.(Sys.file_exists file = `Yes) in
  let rev_args = line |> String.strip |> String.split ~on:' ' |> List.rev in
  let res =
    List.fold rev_args ~init:{files= []; opts= []; opt_st= []; file_st= []}
      ~f:(fun ({files; opts; opt_st; file_st} as state) arg ->
        if String.is_suffix arg ~suffix:".java" then
          if file_exist arg then {state with files= concat_st files (arg :: file_st); file_st= []}
          else {state with file_st= arg :: file_st}
        else if String.is_prefix arg ~prefix:"-" then
          {state with opts= arg :: concat_st opts opt_st; opt_st= []}
        else if String.is_prefix arg ~prefix:"@" then
          let fname = String.drop_prefix arg 1 in
          if file_exist fname then {state with opts= concat_st opts (arg :: opt_st); opt_st= []}
          else {state with opt_st= arg :: opt_st}
        else {state with opt_st= arg :: opt_st} )
  in
  {files= concat_st res.files res.file_st; opts= res.opts @ res.opt_st}


let normalize path = if String.is_substring path ~substring:" " then "\"" ^ path ^ "\"" else path

let capture ~prog ~args =
  let java_version =
    Process.create_process_and_wait_with_output ~prog:"java" ~args:["-version"] ReadStderr
  in
  let javac_version =
    Process.create_process_and_wait_with_output ~prog:"javac" ~args:["-version"] ReadStderr
  in
  let gradle_version =
    Process.create_process_and_wait_with_output ~prog ~args:["--version"] ReadStdout
  in
  L.environment_info "%s %s %s@." java_version javac_version gradle_version ;
  let process_gradle_line seen line =
    match String.substr_index line ~pattern:arg_start_pattern with
    | Some pos ->
        let content = String.drop_prefix line (pos + String.length arg_start_pattern) in
        L.debug Capture Verbose "Processing: %s@." content ;
        if String.Set.mem seen content then seen
        else
          let javac_data = parse_gradle_line ~line:content in
          let tmpfile, oc =
            Core.Filename.open_temp_file ~in_dir:(ResultsDir.get_path Temporary) "gradle_files" ""
          in
          List.iter javac_data.files ~f:(fun file ->
              Out_channel.output_string oc (normalize file ^ "\n") ) ;
          Out_channel.close oc ;
          Javac.call_infer_javac_capture ~javac_args:(("@" ^ tmpfile) :: javac_data.opts) ;
          String.Set.add seen content
    | None ->
        seen
  in
  let gradle_output_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "gradle_output" ".log"
  in
  let shell_cmd =
    List.map ~f:Escape.escape_shell (prog :: "--debug" :: args)
    |> String.concat ~sep:" "
    |> fun cmd -> Printf.sprintf "%s >'%s'" cmd gradle_output_file
  in
  L.progress "[GRADLE] %s@." shell_cmd ;
  Process.create_process_and_wait ~prog:"sh" ~args:["-c"; shell_cmd] ;
  match Utils.read_file gradle_output_file with
  | Ok lines ->
      let processed = List.fold lines ~init:String.Set.empty ~f:process_gradle_line in
      L.progress "[GRADLE] processed %d lines" @@ String.Set.length processed
  | Error _ ->
      L.die ExternalError "*** failed to read gradle output: %s" gradle_output_file
