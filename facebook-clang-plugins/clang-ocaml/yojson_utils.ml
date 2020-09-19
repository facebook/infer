(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module U = Utils
module P = Process

(* Needed as a (pointer-stable) default value for atd specs. *)
let empty_string = ""

let ydump ?(compact_json = false) ?(std_json = false) ic oc =
  let cmd = ["ydump"] @ (if compact_json then ["-c"] else []) @ if std_json then ["-std"] else [] in
  P.exec (Array.of_list cmd) ic oc stderr


let read_data_from_file reader fname =
  let input_gunzipped read_data ic =
    let pid, icz = P.fork (P.gunzip ic) in
    let data = read_data icz in
    let r = P.wait pid in
    P.close_in icz ;
    if not r then failwith "read_data_from_file (gunzip)" else () ;
    data
  in
  let ic = open_in fname in
  let data =
    if U.string_ends_with fname ".value.gz" then input_gunzipped Marshal.from_channel ic
    else if U.string_ends_with fname ".gz" then
      input_gunzipped (Atdgen_runtime.Util.Json.from_channel ~fname reader) ic
    else if U.string_ends_with fname ".value" then Marshal.from_channel ic
    else Atdgen_runtime.Util.Json.from_channel ~fname reader ic
  in
  close_in ic ;
  data


let write_data_to_file ?(pretty = false) ?(compact_json = false) ?(std_json = false) writer fname
    data =
  let output_gzipped write_data oc data =
    let pid, icz =
      P.fork (fun ocz ->
          write_data ocz data ;
          true )
    in
    let r1 = P.gzip icz oc and r2 = P.wait pid in
    P.close_in icz ;
    if not (r1 && r2) then failwith "write_data_to_file (gzip)" else ()
  and output_pretty write_data oc data =
    (* TODO(mathieubaudet): find out how to write directly pretty json? *)
    let pid, icp =
      P.fork (fun ocp ->
          write_data ocp data ;
          true )
    in
    let r1 = ydump ~compact_json ~std_json icp oc and r2 = P.wait pid in
    P.close_in icp ;
    if not (r1 && r2) then failwith "write_data_to_file (pretty)" else ()
  in
  let write_json ocx data =
    if pretty then output_pretty (Atdgen_runtime.Util.Json.to_channel writer) ocx data
    else Atdgen_runtime.Util.Json.to_channel writer ocx data
  in
  let oc = open_out fname in
  if U.string_ends_with fname ".value.gz" then
    output_gzipped (fun oc data -> Marshal.to_channel oc data []) oc data
  else if U.string_ends_with fname ".value" then Marshal.to_channel oc data []
  else if U.string_ends_with fname ".gz" then output_gzipped write_json oc data
  else write_json oc data ;
  close_out oc


let convert ?(pretty = false) ?(compact_json = false) ?(std_json = false) reader writer fin fout =
  try
    read_data_from_file reader fin |> write_data_to_file writer ~pretty ~compact_json ~std_json fout
  with Yojson.Json_error s | Atdgen_runtime.Oj_run.Error s ->
    prerr_string s ;
    prerr_newline () ;
    exit 1


let run_converter_tool reader writer =
  let pretty = ref false and std_json = ref false and compact_json = ref false and files = ref [] in
  let add_files x = files := x :: !files
  and usage_msg =
    "Usage: " ^ Sys.argv.(0) ^ "[OPTIONS] INPUT_FILE [OUTPUT_FILE]\n"
    ^ "Parse yojson values and convert them to another format based on the extension"
    ^ " of the output file (default: ${INPUT_FILE}.value.gz).\n"
  in
  let spec =
    Utils.fix_arg_spec
      [ ("--pretty", Arg.Set pretty, " Pretty print outputs.")
      ; ("--std", Arg.Set std_json, " Use standard json for outputs.")
      ; ("--compact", Arg.Set compact_json, " Use compact json for outputs.")
      ; ("--", Arg.Rest add_files, " Mark the end of options.") ]
      usage_msg
  in
  (* Parse the command line. *)
  Arg.parse spec add_files usage_msg ;
  let input, output =
    match List.rev !files with
    | [input] ->
        (input, input ^ ".value.gz")
    | [input; output] ->
        (input, output)
    | _ ->
        prerr_string usage_msg ;
        exit 1
  in
  convert ~pretty:!pretty ~std_json:!std_json ~compact_json:!compact_json reader writer input output
