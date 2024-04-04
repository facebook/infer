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
let parse_gradle_line ~kotlin ~line =
  let concat_st lst st = if List.is_empty st then lst else String.concat ~sep:" " st :: lst in
  let file_exist file = ISys.file_exists file in
  let rev_args = line |> String.strip |> String.split ~on:' ' |> List.rev in
  let res =
    List.fold rev_args ~init:{files= []; opts= []; opt_st= []; file_st= []}
      ~f:(fun ({files; opts; opt_st; file_st} as state) arg ->
        if String.is_suffix arg ~suffix:".java" || (kotlin && String.is_suffix arg ~suffix:".kt")
        then
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


let process_gradle_output_line =
  let capture_output_template = ResultsDir.get_path Temporary ^/ "capture_target" in
  fun ((seen, target_dirs) as acc) line ->
    String.substr_index line ~pattern:arg_start_pattern
    |> Option.value_map ~default:acc ~f:(fun pos ->
           let content = String.drop_prefix line (pos + String.length arg_start_pattern) in
           L.debug Capture Verbose "Processing: %s@." content ;
           if String.Set.mem seen content then acc
           else
             let javac_data = parse_gradle_line ~kotlin:Config.kotlin_capture ~line:content in
             let out_dir = Unix.mkdtemp capture_output_template in
             (String.Set.add seen content, (out_dir, javac_data) :: target_dirs) )


let run_gradle ~prog ~args =
  let gradle_output_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "gradle_output" ".log"
  in
  let shell_cmd =
    List.map ~f:Escape.escape_shell (prog :: "--debug" :: args)
    |> String.concat ~sep:" "
    |> fun cmd -> Printf.sprintf "%s >'%s'" cmd gradle_output_file
  in
  L.progress "[GRADLE] %s@\n" shell_cmd ;
  let time = Mtime_clock.counter () in
  Process.create_process_and_wait ~prog:"sh" ~args:["-c"; shell_cmd] ;
  L.progress "[GRADLE] running gradle took %a@\n" Mtime.Span.pp (Mtime_clock.count time) ;
  match Utils.read_file gradle_output_file with
  | Ok lines ->
      List.fold lines ~init:(String.Set.empty, []) ~f:process_gradle_output_line
  | Error _ ->
      L.die ExternalError "*** failed to read gradle output: %s@\n" gradle_output_file


let write_args_file prefix args =
  let argfile, oc = Filename.open_temp_file ~in_dir:(ResultsDir.get_path Temporary) prefix "" in
  List.iter args ~f:(fun arg ->
      Out_channel.output_string oc (Escape.escape_shell arg) ;
      Out_channel.newline oc ) ;
  Out_channel.close oc ;
  argfile


let capture_gradle_target (out_dir, (javac_data : javac_data)) =
  let gradle_files = write_args_file "gradle_files" javac_data.files in
  let java_opts =
    List.filter_map javac_data.opts ~f:(fun arg ->
        if String.equal "-Werror" arg then None
        else if String.is_substring arg ~substring:"-g:" then Some "-g"
        else Some arg )
    |> write_args_file "java_opts"
  in
  let prog = Config.bin_dir ^/ "infer" in
  let args =
    ["capture"; "-j"; "1"; "-o"; out_dir; "--"; "javac"; "@" ^ gradle_files; "@" ^ java_opts]
  in
  L.debug Capture Verbose "%s %s@." prog (String.concat ~sep:" " args) ;
  Process.create_process_and_wait ~prog ~args ;
  None


let run_infer_capture target_data =
  Tasks.Runner.create ~jobs:Config.jobs ~child_prologue:ignore ~f:capture_gradle_target
    ~child_epilogue:ignore (fun () -> ProcessPool.TaskGenerator.of_list target_data )
  |> Tasks.Runner.run |> ignore


let write_rev_infer_deps rev_target_data =
  ResultsDir.get_path CaptureDependencies
  |> Utils.with_file_out ~f:(fun out_channel ->
         List.rev_map rev_target_data ~f:(fun (out_dir, _) -> Printf.sprintf "-\t-\t%s" out_dir)
         |> Out_channel.output_lines out_channel )


let log_environment_info ~prog =
  let java_version =
    Process.create_process_and_wait_with_output ~prog:"java" ~args:["-version"] ReadStderr
  in
  let javac_version =
    Process.create_process_and_wait_with_output ~prog:"javac" ~args:["-version"] ReadStderr
  in
  let gradle_version =
    Process.create_process_and_wait_with_output ~prog ~args:["--version"] ReadStdout
  in
  L.environment_info "%s %s %s@\n" java_version javac_version gradle_version


let capture ~prog ~args =
  log_environment_info ~prog ;
  let processed, rev_target_data = run_gradle ~prog ~args in
  let time = Mtime_clock.counter () in
  run_infer_capture rev_target_data ;
  write_rev_infer_deps rev_target_data ;
  L.debug Capture Quiet "[GRADLE] infer processed %d lines in %a@\n" (String.Set.length processed)
    Mtime.Span.pp (Mtime_clock.count time)
