(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

let textual_ext = ".sil"

let textual_subcommand = "compile-infer"

(** Flatten a/b/c as a-b-c. Special dirs .. and . are abbreviated. *)
let flatten_path path =
  let normalized_path = Utils.normalize_path path in
  let path_parts = Filename.parts normalized_path in
  let process_part = function ".." -> ["dd"] | "." -> [] | other -> [other] in
  List.bind path_parts ~f:process_part |> String.concat ~sep:"-"


let to_textual_filename path =
  let flat = flatten_path path in
  let noext = Filename.chop_extension flat in
  noext ^ textual_ext


let dump_textual_to_tmp_file source_path content =
  let textual_filename = to_textual_filename source_path in
  let out_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename "sil"
  in
  Out_channel.write_all out_file ~data:content


module Marker = struct
  type t = UnitStart | UnitEnd

  let start_marker = "// TEXTUAL UNIT START"

  let end_marker = "// TEXTUAL UNIT END"

  let detect line =
    match String.chop_prefix line ~prefix:start_marker with
    | Some filename ->
        Some (UnitStart, String.strip filename)
    | _ -> (
      match String.chop_prefix line ~prefix:end_marker with
      | Some filename ->
          Some (UnitEnd, String.strip filename)
      | _ ->
          None )
end

(** The structure of hackc output is as follows:

    - START MARKER <source path>
    - <content>
    - END MARKER <source path>
    - ... repeat

    The function below processes such input from [chan] line by line and does some light-weight
    error detection mainly to detect situations when different compilation units get mixed up in the
    output (this shouldn't happen normally).

    When the whole compilation unit has been accumulated, it calls [action]. *)
let process_output chan ~action =
  let module State = struct
    type t =
      | WaitForStart
      | ReadingUnit of {filename: string; rev_lines: string list}
      | ExtractedUnit of {filename: string; content: string}
  end in
  let open State in
  (* Reading until we find first unit start marker *)
  let rec step state line =
    match state with
    | WaitForStart -> (
        if String.is_empty line then WaitForStart
        else
          match Marker.detect line with
          | Some (UnitStart, filename) ->
              ReadingUnit {filename; rev_lines= [line]}
          | _ ->
              L.user_warning "Unexpected line outside of a textual unit: %s@." line ;
              WaitForStart )
    | ReadingUnit {filename; rev_lines} -> (
      (* We've detected a start marker and now accumulating the contents of the unit *)
      match Marker.detect line with
      | Some (UnitEnd, end_filename) when String.equal filename end_filename ->
          (* Happy path; we've found a matching end marker *)
          let content = List.rev (line :: rev_lines) |> String.concat ~sep:"\n" in
          step (ExtractedUnit {filename; content}) line
      | Some (UnitEnd, end_filename) ->
          L.user_warning "Unexpected end of another unit: expected=%s, actual=%s@." filename
            end_filename ;
          WaitForStart
      | Some (UnitStart, _) ->
          L.user_warning "Unexpected start of another unit: %s@." line ;
          step WaitForStart line
      | _ ->
          (* Accumulate lines in the state *)
          ReadingUnit {filename; rev_lines= line :: rev_lines} )
    | ExtractedUnit {filename; content} ->
        action filename content ;
        WaitForStart
  in
  let final_state = In_channel.fold_lines chan ~init:WaitForStart ~f:step in
  match final_state with
  | ReadingUnit {filename; _} ->
      L.user_warning "Unfinished unit: %s@." filename
  | _ ->
      ()


(** Run hackc [compiler] with [args] and consume results of translation from its stdout. We don't do
    any pre-processing of [args] and let hackc deal with multiple files on its own. We also pipe
    stderr into a temp file just in case. *)
let compile compiler args =
  let stderr_log = Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "hackc" "stderr" in
  let escaped_cmd = List.map ~f:Escape.escape_shell (compiler :: args) |> String.concat ~sep:" " in
  let redirected_cmd = F.sprintf "exec %s 2>%s" escaped_cmd stderr_log in
  let {Unix.Process_info.stdin; stdout; stderr; pid} =
    Unix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  Unix.close stderr ;
  let chan = Unix.in_channel_of_descr stdout in
  let n_captured, n_error = (ref 0, ref 0) in
  process_output chan ~action:(fun source_path content ->
      L.debug Capture Quiet "Capturing %s@." source_path ;
      let open TextualParser in
      let trans = TextualFile.translate (TextualFile.TranslatedFile {source_path; content}) in
      ( match trans with
      | Ok sil ->
          TextualFile.capture sil ;
          incr n_captured
      | Error (sourcefile, errs) ->
          List.iter errs ~f:(log_error sourcefile) ;
          incr n_error ) ;
      if Config.debug_mode || Result.is_error trans then
        dump_textual_to_tmp_file source_path content ) ;
  In_channel.close chan ;
  match Unix.waitpid pid with
  | Ok () ->
      L.progress "Finished capture: success %d files, error %d files.@." !n_captured !n_error ;
      if (not Config.keep_going) && !n_error > 0 then
        L.die ExternalError
          "There were errors during capture. Re-run with --keep-going to ignore the errors."
  | Error _ as status ->
      L.die ExternalError "Error executing: %s@\n%s@\n" escaped_cmd
        (Unix.Exit_or_signal.to_string_hum status)


let capture ~args =
  if List.exists args ~f:(fun arg -> String.equal arg textual_subcommand) then
    compile Config.hackc_binary args
  else L.die UserError "hackc command line is missing %s subcommand" textual_subcommand
