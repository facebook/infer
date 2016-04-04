(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Take as input an ast file and a C or ObjectiveC file such that the ast file
   corresponds to the compilation of the C file with clang.
   Parse the ast file into a data structure and translates it into a cfg. *)

module L = Logging

open CFrontend_utils

let arg_desc =
  let desc =
    (Utils.arg_desc_filter ["-results_dir"] base_arg_desc) @
    [
      "-c",
      Arg.String (fun cfile -> CFrontend_config.source_file := Some cfile),
      Some "cfile",
      "C File to translate"
      ;
      "-x",
      Arg.String (fun lang -> CFrontend_config.lang_from_string lang),
      Some "cfile",
      "Language (c, objective-c, c++, objc-++)"
      ;
      "-ast",
      Arg.String (fun file -> CFrontend_config.ast_file := Some file),
      Some "file",
      "AST file for the translation"
      ;
      "-dotty_no_cfg_libs",
      Arg.Unit (fun _ -> Config.dotty_cfg_libs := false),
      None,
      "Prints the cfg of the code coming from the libraries"
      ;
      "-no_headers",
      Arg.Unit (fun _ -> CFrontend_config.no_translate_libs := true),
      None,
      "Do not translate code in header files (default)"
      ;
      "-headers",
      Arg.Unit (fun _ -> CFrontend_config.no_translate_libs := false),
      None,
      "Translate code in header files"
      ;
      "-testing_mode",
      Arg.Unit (fun _ -> CFrontend_config.testing_mode := true),
      None,
      "Mode for testing, where no headers are translated, \
       and dot files are created"
      ;
      "-debug",
      Arg.Unit (fun _ -> CFrontend_config.debug_mode := true),
      None,
      "Enables debug mode"
      ;
      "-stats",
      Arg.Unit (fun _ -> CFrontend_config.stats_mode := true),
      None,
      "Enables stats mode"
      ;
      "-project_root",
      Arg.String (fun s ->
          Config.project_root := Some (Utils.filename_to_absolute s)),
      Some "dir",
      "Toot directory of the project"
      ;
      "-fobjc-arc",
      Arg.Unit (fun _ -> Config.arc_mode := true),
      None,
      "Translate with Objective-C Automatic Reference Counting (ARC)"
      ;
      "-models_mode",
      Arg.Unit (fun _ -> CFrontend_config.models_mode := true),
      None,
      "Mode for computing the models"
      ;
      "-cxx-experimental",
      Arg.Unit (fun _ -> CFrontend_config.cxx_experimental := true),
      None,
      "Analyze C++ methods, still experimental"
      ;
    ] in
  Arg.create_options_desc false "Parsing Options" desc

let usage =
  "\nUsage: InferClang -c C Files -ast AST Files -results_dir <output-dir> [options] \n"

let print_usage_exit () =
  Arg.usage arg_desc usage;
  exit(1)

let buffer_len = 16384

(* This function reads the json file in fname, validates it, and encoded in the AST data structure*)
(* defined in Clang_ast_t.  *)
let validate_decl_from_file fname =
  try
    Ag_util.Biniou.from_file ~len:buffer_len Clang_ast_b.read_decl fname
  with (Invalid_argument "Bi_inbuf.refill_from_channel") ->
    Printing.log_stats "WARNING: biniou buffer too short, skipping the file\n";
    assert false

let validate_decl_from_stdin () =
  try
    Ag_util.Biniou.from_channel ~len:buffer_len Clang_ast_b.read_decl stdin
  with (Invalid_argument "Bi_inbuf.refill_from_channel") ->
    Printing.log_stats "WARNING: biniou buffer too short, skipping the file\n";
    assert false


let do_run source_path ast_path =
  let init_time = Unix.gettimeofday () in
  let print_elapsed () =
    let elapsed = Unix.gettimeofday () -. init_time in
    Printf.printf "Elapsed: %07.3f seconds.\n" elapsed in
  try
    let ast_filename, ast_decl =
      match ast_path with
      | Some path ->
          path, validate_decl_from_file path
      | None ->
          "stdin of " ^ source_path, validate_decl_from_stdin () in

    let decl_index, stmt_index, type_index = Clang_ast_main.index_node_pointers ast_decl in
    CFrontend_config.pointer_decl_index := decl_index;
    CFrontend_config.pointer_stmt_index := stmt_index;
    CFrontend_config.pointer_type_index := type_index;
    CFrontend_config.json := ast_filename;
    CLocation.check_source_file source_path;
    let source_file = CLocation.source_file_from_path source_path in
    Printf.printf "Start translation of AST from %s\n" !CFrontend_config.json;
    CFrontend.do_source_file  source_file ast_decl;
    Printf.printf "End translation AST file %s... OK!\n" !CFrontend_config.json;
    print_elapsed ();
  with
    (Yojson.Json_error s) as exc ->
      Printing.log_err "%s\n" s;
      print_elapsed ();
      raise exc

let () =
  Arg.parse "INFERCLANG_ARGS" arg_desc (fun _ -> ()) usage ;
  Config.print_types := true;
  if Option.is_none !CFrontend_config.source_file then
    (Printing.log_err "Incorrect command line arguments\n";
     print_usage_exit ())
  else
    match !CFrontend_config.source_file with
    | Some path ->
        do_run path !CFrontend_config.ast_file
    | None ->
        assert false
