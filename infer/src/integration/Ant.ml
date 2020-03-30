(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let javac_pattern = "[javac]"

let arg_start_pattern = "Compilation arguments"

let extract_javac_args line =
  let is_quoted s =
    String.length s > 2 && String.is_prefix s ~prefix:"'" && String.is_suffix s ~suffix:"'"
  in
  let remove_quotes s = if is_quoted s then String.sub s ~pos:1 ~len:(String.length s - 2) else s in
  let is_interesting s = String.is_suffix s ~suffix:".java" || is_quoted s in
  Option.map (String.substr_index line ~pattern:javac_pattern) ~f:(fun pos ->
      let content = String.drop_prefix line (pos + String.length javac_pattern) |> String.strip in
      if is_interesting content then Some (remove_quotes content) else None )
  |> Option.join


type fold_state = {collecting: bool; rev_javac_args: string list}

let capture ~prog ~args =
  let java_version =
    Process.create_process_and_wait_with_output ~prog:"java" ~args:["-version"] ReadStderr
  in
  let javac_version =
    Process.create_process_and_wait_with_output ~prog:"javac" ~args:["-version"] ReadStderr
  in
  let ant_version =
    Process.create_process_and_wait_with_output ~prog ~args:["-version"] ReadStdout
  in
  L.environment_info "%s %s %s@\n" java_version javac_version ant_version ;
  L.debug Capture Quiet "%s %a@." prog Pp.cli_args args ;
  let ant_out =
    Process.create_process_and_wait_with_output ~prog ~args:("-verbose" :: args) ReadStdout
  in
  L.debug Capture Verbose "%s" ant_out ;
  let res =
    List.fold (String.split_lines ant_out) ~init:{collecting= false; rev_javac_args= []}
      ~f:(fun {collecting; rev_javac_args} line ->
        let is_line_interesting = String.is_substring line ~substring:javac_pattern in
        if is_line_interesting then
          let start_collecting = String.is_substring line ~substring:arg_start_pattern in
          let collecting = collecting || start_collecting in
          let rev_javac_args =
            if start_collecting && not (List.is_empty rev_javac_args) then (
              Javac.call_infer_javac_capture ~javac_args:(List.rev rev_javac_args) ;
              [] )
            else rev_javac_args
          in
          let rev_javac_args =
            if collecting then
              Option.fold (extract_javac_args line) ~init:rev_javac_args ~f:(Fn.flip List.cons)
            else rev_javac_args
          in
          {collecting; rev_javac_args}
        else {collecting; rev_javac_args} )
  in
  if not (List.is_empty res.rev_javac_args) then
    Javac.call_infer_javac_capture ~javac_args:(List.rev res.rev_javac_args)
