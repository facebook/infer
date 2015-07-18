(*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

module L = Logging
module F = Format
open Utils

(** If active, check copyright messages and exit. *)
let active = Config.from_env_variable "INFER_CHECK_COPYRIGHT"

(** If true, update the copyright message of the files. *)
let update_files = true

let line_contains_copyright line =
  string_contains "opyright " line

let rec find_copyright_line lines n = match lines with
  | [] -> None
  | line :: lines' ->
      if line_contains_copyright line then Some n
      else find_copyright_line lines' (n + 1)

let find_copyright_start lines_arr n check_hash =
  let is_start line =
    if check_hash then
      not (string_is_prefix "#" line)
    else
      string_contains "(*" line
      ||
      string_contains "/*" line in
  let i = ref (n - 1) in
  let found = ref 0 in
  while !i >= 0 && !found = 0 do
    if is_start lines_arr.(!i) then found := !i + 1;
    decr i
  done;
  !found

let find_copyright_end lines_arr n check_hash =
  let is_end line =
    if check_hash then
      not (string_is_prefix "#" line)
    else
      string_contains "*)" line
      ||
      string_contains "*/" line in
  let i = ref (n + 1) in
  let len = Array.length lines_arr in
  let found = ref (len - 1) in
  while !i < len && !found = len - 1 do
    if is_end lines_arr.(!i) then found := !i - 1;
    incr i
  done;
  !found

(** Heuristic to check if this looks like a copyright message. *)
let looks_like_copyright_message cstart cend lines_arr =
  let max_len = 100 in
  let check_len () =
    let ok = ref true in
    for i = cstart to cend do
      if String.length lines_arr.(i) > max_len then ok := false
    done;
    !ok in
  (cend - cstart) <= 10 && check_len ()

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

let pp_copyright mono fb_year check_hash fmt _prefix =
  let prefix = _prefix ^ (if check_hash then "#" else "*") in
  let pp_line str = F.fprintf fmt "%s%s@." prefix str in
  if mono then
    pp_line " Copyright (c) 2009 - 2013 Monoidics ltd.";
  pp_line (F.sprintf " Copyright (c) %d - present Facebook, Inc." fb_year);
  pp_line " All rights reserved.";
  pp_line "";
  pp_line " This source code is licensed under the BSD style license found in the";
  pp_line " LICENSE file in the root directory of this source tree. An additional grant";
  pp_line " of patent rights can be found in the PATENTS file in the same directory."

let copyright_has_changed mono fb_year check_hash cstart cend lines_arr =
  let old_copyright =
    let r = ref "" in
    for i = cstart to cend do
      r := !r ^ lines_arr.(i) ^ "\n"
    done;
    !r in
  let new_copyright =
    let pp fmt () = pp_copyright mono fb_year check_hash fmt "" in
    Utils.pp_to_string pp () in
  old_copyright <> new_copyright

let update_file fname mono fb_year check_hash cstart cend lines_arr =
  try
    let cout = open_out fname in
    let fmt = F.formatter_of_out_channel cout in
    for i = 0 to cstart - 1 do
      F.fprintf fmt "%s@." lines_arr.(i)
    done;
    pp_copyright mono fb_year check_hash fmt "";
    for i = cend + 1 to Array.length lines_arr - 1 do
      F.fprintf fmt "%s@\n" lines_arr.(i)
    done;
    F.fprintf fmt "@?";
    close_out cout
  with _ -> ()

let file_should_have_copyright fname lines =
  let extensions =
    [".ml"; ".mli"; ".ml"; ".mly"; ".java"; ".c";
    ".h"; ".cpp"; ".m"; ".mm"; ".py"; ".sh"] in
  list_exists (Filename.check_suffix fname) extensions


let get_files_from_git () =
  let tmpfile = Filename.temp_file "git_ls" "" in
  let _ = Sys.command ("git ls-files >" ^ tmpfile) in
  let git_files = match read_file tmpfile with
    | Some git_files -> git_files
    | None -> [] in
  Sys.remove tmpfile;
  git_files


let check_copyright () =
  let check_file fname =
    match read_file fname with
    | None -> ()
    | Some lines ->
        begin
          match find_copyright_line lines 0 with
          | None ->
              if file_should_have_copyright fname lines
              then L.stderr "Copyright not found in %s@." fname
          | Some n ->
              let lines_arr = Array.of_list lines in
              let line = lines_arr.(n) in
              let len = String.length line in
              let check_hash = string_is_prefix "#" line in
              let cstart = find_copyright_start lines_arr n check_hash in
              let cend = find_copyright_end lines_arr n check_hash in
              let range = cend - cstart in
              if looks_like_copyright_message cstart cend lines_arr then
                begin
                  let mono = contains_monoidics cstart cend lines_arr in
                  match get_fb_year cstart cend lines_arr with
                  | None ->
                      L.stderr "Can't find fb year: %s@." fname
                  | Some fb_year ->
                      if copyright_has_changed mono fb_year check_hash cstart cend lines_arr then
                        begin
                          L.stderr "%s (start:%d n:%d end:%d len:%d range:%d mono:%b year:%d)@."
                            fname cstart n cend len range mono fb_year;
                          for i = cstart to cend do
                            L.stderr "  %s@." lines_arr.(i)
                          done;
                          L.stderr "-----@.";
                          L.stderr "%a" (pp_copyright mono fb_year check_hash) "  ";
                          if update_files then
                            update_file fname mono fb_year check_hash cstart cend lines_arr
                        end
                end
              else
                L.stderr "Copyright not recognized: %s@." fname
        end in
  list_iter check_file (get_files_from_git ());
  exit 0

let check () =
  if active then check_copyright ()
