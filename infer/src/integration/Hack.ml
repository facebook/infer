(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let hack_source_ext = [".hack"; ".php"]

let textual_ext = ".sil"

let textual_subcommand = "compile-infer"

let is_source_file str = List.exists hack_source_ext ~f:(Filename.check_suffix str)

(** Split args into options and filenames *)
let process_args args =
  let process_arg (filenames, options, has_textual_subcommand) arg =
    if is_source_file arg then (arg :: filenames, options, has_textual_subcommand)
    else if String.equal arg textual_subcommand then (filenames, arg :: options, true)
    else (filenames, arg :: options, has_textual_subcommand)
  in
  let rev_sources, rev_options, has_textual_subcommand =
    List.fold args ~init:([], [], false) ~f:process_arg
  in
  let rev_options =
    if has_textual_subcommand then rev_options else textual_subcommand :: rev_options
  in
  (List.rev rev_options, List.rev rev_sources)


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


(** Compile a single source file saving intermediate textual in a temp .sil file. *)
let compile_single compiler result_dir options source =
  L.debug Capture Verbose "Running %s on %s@." compiler source ;
  let args = options @ [source] in
  let out_file = Filename.concat result_dir (to_textual_filename source) in
  let textual = Process.create_process_and_wait_with_output ~prog:compiler ~args ReadStdout in
  Out_channel.write_all out_file ~data:textual ;
  (source, out_file)


let compile compiler result_dir args =
  let options, sources = process_args args in
  let compile_source = compile_single compiler result_dir options in
  (* This is a temporary solution until we have hackc handle multiple files in a reasonable way. *)
  List.map sources ~f:compile_source


let capture ~command ~args =
  let in_dir = ResultsDir.get_path Temporary in
  let result_dir = Filename.temp_dir ~in_dir command "sil" in
  let captured_files = compile Config.hackc_binary result_dir args in
  L.debug Capture Verbose "Translated into %d textual files@." (List.length captured_files) ;
  List.iter captured_files ~f:(fun (source_path, textual_path) ->
      TextualParser.capture ~source_path textual_path )
