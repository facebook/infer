(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

let copyright_modified_exit_code = 1
let copyright_malformed_exit_code = 2

type comment_style =
  | Line of string (** line comments, eg "#" for shell *)
  | Block of string * string * string (** block comments, eg ("(*", "*", "*)") for ocaml *)

let comment_style_ocaml = Block ("(*", "*", "*)")
let comment_style_c = Block ("/*", "*", "*/")
let comment_style_shell = Line "#"
let comment_style_llvm = Line ";"

let comment_styles = [
  comment_style_ocaml;
  comment_style_c;
  comment_style_shell;
  comment_style_llvm;
]

let lang_of_com_style style =
  if style = comment_style_ocaml then "ocaml"
  else if style = comment_style_c then "c"
  else if style = comment_style_shell then "shell"
  else if style = comment_style_llvm then "llvm"
  else "??unknown??"

let default_start_line_of_com_style style = match style with
  | Line _ -> 2
  | Block _ -> 0

let prefix_of_comment_style = function
  | Line _ -> ""
  | Block (_, inter, _) -> String.make (String.length inter) ' '

(** If true, update the copyright message of the files. *)
let update_files = ref false

let line_contains_copyright line =
  string_contains "opyright " line

let rec find_copyright_line lines n = match lines with
  | [] -> None
  | line :: lines' ->
      if line_contains_copyright line then Some n
      else find_copyright_line lines' (n + 1)

let find_comment_start_and_style lines_arr n =
  (* are we in a line comment? *)
  let cur_line_comment = try
      Some (IList.find (function
          | Line (s) when string_is_prefix s lines_arr.(n) -> true
          | _ -> false) comment_styles)
    with Not_found -> None in
  let is_start line = match cur_line_comment with
    | Some (Line (s)) -> if string_is_prefix s line then None else Some (Line (s))
    | _ -> try
          Some (IList.find (function
              | Block(s, _, _) -> string_contains s line
              | _ -> false) comment_styles)
        with Not_found -> None in
  let i = ref (n - 1) in
  (* hacky fake line comment to avoid an option type *)
  let found = ref (-1, Line(">>>>>>>>>>>")) in
  while !i >= 0 && fst (!found) = -1 do
    match is_start lines_arr.(!i) with
    | Some style -> found := (!i, style);
    | None -> decr i
  done;
  !found

let find_comment_end lines_arr n com_style =
  let is_end line = match com_style with
    | Line s -> not (string_is_prefix s line)
    | Block (_, _, s) -> string_contains s line in
  let i = ref (n + 1) in
  let len = Array.length lines_arr in
  let found = ref (len - 1) in
  while !i < len && !found = len - 1 do
    if is_end lines_arr.(!i) then found := !i;
    incr i
  done;
  match com_style with
  | Line _ -> !found
  | Block _ -> !found + 1

(** Heuristic to check if this looks like a copyright message. *)
let looks_like_copyright_message cstart cend lines_arr =
  let max_len = 100 in
  let check_len () =
    let ok = ref true in
    for i = cstart to cend do
      if String.length lines_arr.(i) > max_len then ok := false
    done;
    !ok in
  cstart >= 0 && (cend - cstart) <= 10 && check_len ()

let contains_monoidics cstart cend lines_arr =
  let found = ref false in
  for i = cstart to cend do
    if string_contains "Monoidics" lines_arr.(i) then found := true
  done;
  !found

let get_fb_year cstart cend lines_arr =
  let found = ref None in
  let do_line line =
    try
      let fmt_re = Str.regexp "[0-9]+" in
      let _ = Str.search_forward fmt_re line 0 in
      let fmt_match = Str.matched_string line in
      if String.length fmt_match = 4 then
        try
          found := Some (int_of_string fmt_match)
        with _ -> ()
    with Not_found -> () in
  for i = cstart to cend do
    let line = lines_arr.(i) in
    if string_contains "Facebook" line then
      do_line line
  done;
  !found

let pp_copyright mono fb_year com_style fmt _prefix =
  let running_comment = match com_style with | Line s | Block (_, s, _) -> s in
  let prefix = _prefix ^ running_comment in
  let pp_line str = F.fprintf fmt "%s%s@\n" prefix str in
  let pp_start () = match com_style with
    | Line _ -> F.fprintf fmt "@\n";
    | Block (start, _, _) -> F.fprintf fmt "%s@\n" start in
  let pp_end () = match com_style with
    | Line _ -> F.fprintf fmt "@\n";
    | Block (_, _, finish) -> F.fprintf fmt "%s%s@\n@\n" _prefix finish in
  pp_start ();
  if mono then
    pp_line " Copyright (c) 2009 - 2013 Monoidics ltd.";
  pp_line (F.sprintf " Copyright (c) %d - present Facebook, Inc." fb_year);
  pp_line " All rights reserved.";
  pp_line "";
  pp_line " This source code is licensed under the BSD style license found in the";
  pp_line " LICENSE file in the root directory of this source tree. An additional grant";
  pp_line " of patent rights can be found in the PATENTS file in the same directory.";
  pp_end ()

let copyright_has_changed mono fb_year com_style prefix cstart cend lines_arr =
  let old_copyright =
    let r = ref "" in
    for i = cstart to cend do
      r := !r ^ lines_arr.(i) ^ "\n"
    done;
    !r in
  let new_copyright =
    let pp fmt () = pp_copyright mono fb_year com_style fmt prefix in
    pp_to_string pp () in
  old_copyright <> new_copyright

let update_file fname mono fb_year com_style prefix cstart cend lines_arr =
  try
    let cout = open_out fname in
    let fmt = F.formatter_of_out_channel cout in
    for i = 0 to cstart - 1 do
      F.fprintf fmt "%s@." lines_arr.(i)
    done;
    pp_copyright mono fb_year com_style fmt prefix;
    for i = cend + 1 to Array.length lines_arr - 1 do
      F.fprintf fmt "%s@\n" lines_arr.(i)
    done;
    F.fprintf fmt "@?";
    close_out cout
  with _ -> ()

let com_style_of_lang = [
  (".ml", comment_style_ocaml);
  (".mli", comment_style_ocaml);
  (".mly", comment_style_c);
  (".mll", comment_style_ocaml);
  (".c", comment_style_c);
  (".h", comment_style_c);
  (".cpp", comment_style_c);
  (".m", comment_style_c);
  (".mm", comment_style_c);
  (".ll", comment_style_llvm);
  (".java", comment_style_c);
  (".sh", comment_style_shell);
  (".py", comment_style_shell);
]

let file_should_have_copyright fname =
  IList.mem_assoc Filename.check_suffix fname com_style_of_lang

let get_filename_extension fname =
  try
    let len_without_ext = String.length (Filename.chop_extension fname) in
    String.sub fname len_without_ext (String.length fname - len_without_ext)
  with Not_found -> ""

let output_diff fname lines_arr cstart n cend len mono fb_year com_style prefix =
  let range = cend - cstart in
  let lang = lang_of_com_style com_style in
  L.stderr "%s (start:%d n:%d end:%d len:%d range:%d lang:%s mono:%b year:%d)@."
    fname cstart n cend len range lang mono fb_year;
  for i = cstart to cend do
    L.stdout "%s@." lines_arr.(i)
  done;
  L.stdout "-----@.";
  L.stdout "@[<v>%a@]" (pp_copyright mono fb_year com_style) prefix;
  L.flush_streams ();
  if !update_files then
    update_file fname mono fb_year com_style prefix cstart cend lines_arr

let check_copyright fname = match read_file fname with
  | None -> ()
  | Some lines ->
      match find_copyright_line lines 0 with
      | None ->
          if file_should_have_copyright fname then
            begin
              let year = 1900 + (Unix.localtime (Unix.time ())).Unix.tm_year in
              let ext = get_filename_extension fname in
              let com_style = IList.assoc string_equal ext com_style_of_lang in
              let prefix = prefix_of_comment_style com_style in
              let start = default_start_line_of_com_style com_style in
              output_diff fname (Array.of_list []) start (-1) (-1) 0 false year com_style prefix;
              exit copyright_modified_exit_code
            end
      | Some n ->
          let lines_arr = Array.of_list lines in
          let line = lines_arr.(n) in
          let len = String.length line in
          let (cstart, com_style) = find_comment_start_and_style lines_arr n in
          let cend = find_comment_end lines_arr n com_style in
          if looks_like_copyright_message cstart cend lines_arr then
            begin
              let mono = contains_monoidics cstart cend lines_arr in
              match get_fb_year cstart cend lines_arr with
              | None ->
                  L.stderr "Can't find fb year: %s@." fname;
                  exit copyright_malformed_exit_code
              | Some fb_year ->
                  let prefix = prefix_of_comment_style com_style in
                  if copyright_has_changed mono fb_year com_style prefix cstart cend lines_arr then
                    begin
                      output_diff fname lines_arr cstart n cend len mono fb_year com_style prefix;
                      exit copyright_modified_exit_code
                    end
            end
          else
            begin
              L.stderr "Copyright not recognized: %s@." fname;
              exit copyright_malformed_exit_code
            end


let speclist = [
  "-i",
  Arg.Set update_files,
  "Update copyright notice in-place"
  ;
]

let usage_msg = "checkCopyright [-i] file1 ..."

let () =
  let to_check = ref [] in
  let add_file_to_check fname =
    to_check := fname :: !to_check in
  Arg.parse "CHECKCOPYRIGHT_ARGS" (Arg.align speclist) add_file_to_check usage_msg;
  IList.iter check_copyright (IList.rev !to_check);
  exit 0
