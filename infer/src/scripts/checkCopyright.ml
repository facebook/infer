(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

type copyright_event = CopyrightMalformed | CopyrightModified

exception CopyrightEvent of copyright_event

let exit_code_of_event = function CopyrightModified -> 1 | CopyrightMalformed -> 3

type comment_style =
  | Line of string * bool
      (** line comments, eg "#" for shell, and whether there should be a newline before the
          copyright notice *)
  | Block of string * string * string * bool  (** block comments, eg ("(*", "*", "*)") for ocaml *)
[@@deriving compare, equal]

let comment_style_al = Line ("//", false)

let comment_style_c = Block ("/*", "*", "*/", false)

let comment_style_erlang = Line ("%", false)

let comment_style_lisp = Line (";", false)

let comment_style_llvm = Line (";", true)

let comment_style_m4 = Line ("dnl", false)

let comment_style_make = Line ("#", false)

let comment_style_ocaml = Block ("(*", "*", "*)", false)

let comment_style_php = Line ("//", true)

let comment_style_python = Line ("#", false)

let comment_style_shell = Line ("#", true)

let comment_styles_lang =
  [ (comment_style_al, "AL")
  ; (comment_style_c, "C")
  ; (comment_style_erlang, "Erlang")
  ; (comment_style_lisp, "Lisp")
  ; (comment_style_llvm, "LLVM")
  ; (comment_style_m4, "M4")
  ; (comment_style_make, "Makefile")
  ; (comment_style_ocaml, "OCaml")
  ; (comment_style_php, "PHP")
  ; (comment_style_python, "python")
  ; (comment_style_shell, "shell") ]


let lang_of_comment_style style =
  List.Assoc.find_exn ~equal:equal_comment_style comment_styles_lang style


let comment_styles = List.map comment_styles_lang ~f:fst

let starts_with_newline = function
  | Line (_, starts_with_newline) | Block (_, _, _, starts_with_newline) ->
      starts_with_newline


let default_start_line_of_com_style style = if starts_with_newline style then 1 else 0

let indent_of_comment_style = function
  | Line _ ->
      ""
  | Block (_, inter, _, _) ->
      String.make (String.length inter) ' '


let keep_going = ref false

(** If true, update the copyright message of the files. *)
let update_files = ref false

let show_diff = ref false

let line_contains_copyright line = String.is_substring ~substring:"opyright " line

let find_copyright_line lines =
  List.findi lines ~f:(fun _ line -> line_contains_copyright line) |> Option.map ~f:fst


let array_rev_find_mapi_from array ~from ~f =
  let i = ref from in
  let found = ref None in
  while !i >= 0 && Option.is_none !found do
    found := f !i array.(!i) ;
    decr i
  done ;
  !found


let find_comment_start_and_style lines n =
  (* are we in a line comment? *)
  let cur_line_comment =
    List.find comment_styles ~f:(function
      | Line (s, starts_with_newline) when String.is_prefix ~prefix:s lines.(n) ->
          if starts_with_newline then n <> 0 else true
      | _ ->
          false )
  in
  let is_start i line =
    match cur_line_comment with
    | Some (Line _) ->
        cur_line_comment
    | Some (Block _) | None ->
        List.find comment_styles ~f:(function
          | Block (s, _, _, starts_with_newline) when String.is_substring ~substring:s line ->
              if starts_with_newline then i <> 0 else true
          | _ ->
              false )
  in
  let find_in_line i line = is_start i line |> Option.map ~f:(fun style -> (i, style)) in
  array_rev_find_mapi_from lines ~from:n ~f:find_in_line


let find_comment_end lines n com_style =
  let is_end line =
    match com_style with
    | Line (s, _) ->
        (not (String.is_prefix ~prefix:s line), `After)
    | Block (_, _, s, _) ->
        (String.is_substring ~substring:s line, `OnIt)
  in
  let i = ref (n + 1) in
  let len = Array.length lines in
  let found = ref (len - 1) in
  while !i < len && !found = len - 1 do
    ( match is_end lines.(!i) with
    | true, `OnIt ->
        found := !i
    | true, `After ->
        found := !i - 1
    | false, _ ->
        () ) ;
    incr i
  done ;
  match com_style with Line _ -> !found | Block _ -> !found


(** Heuristic to check if this looks like a copyright message. *)
let looks_like_copyright_message cstart cend lines =
  let max_len = 100 in
  let check_len () =
    let ok = ref true in
    for i = cstart to cend do
      if String.length lines.(i) > max_len then ok := false
    done ;
    !ok
  in
  cstart >= 0 && cend - cstart <= 10 && check_len ()


let contains_string ~substring cstart cend lines =
  let found = ref false in
  for i = cstart to cend do
    if String.is_substring ~substring lines.(i) then found := true
  done ;
  !found


let contains_monoidics cstart cend lines = contains_string ~substring:"Monoidics" cstart cend lines

let contains_ropas cstart cend lines = contains_string ~substring:"ROPAS" cstart cend lines

let pp_copyright ~monoidics ~ropas com_style fmt =
  let running_comment = match com_style with Line (s, _) | Block (_, s, _, _) -> s in
  let indent = indent_of_comment_style com_style in
  let pp_line str =
    F.kfprintf (fun fmt -> F.fprintf fmt "@\n") fmt ("%s%s" ^^ str) indent running_comment
  in
  let pp_start () =
    match com_style with Line _ -> () | Block (start, _, _, _) -> F.fprintf fmt "%s@\n" start
  in
  let pp_end () =
    match com_style with
    | Line _ ->
        ()
    | Block (_, _, finish, _) ->
        F.fprintf fmt "%s%s@\n" indent finish
  in
  pp_start () ;
  if ropas then (
    pp_line " Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)" ;
    pp_line "                             Seoul National University, Korea" )
  else if monoidics then pp_line " Copyright (c) 2009-2013, Monoidics ltd." ;
  pp_line " Copyright (c) Facebook, Inc. and its affiliates." ;
  pp_line "" ;
  pp_line " This source code is licensed under the MIT license found in the" ;
  pp_line " LICENSE file in the root directory of this source tree." ;
  pp_end ()


let copyright_has_changed fname lines ~notice_range:(cstart, cend) ~monoidics ~ropas com_style =
  let old_copyright =
    let r = ref "" in
    for i = cstart to cend do
      r := !r ^ lines.(i) ^ "\n"
    done ;
    !r
  in
  let new_copyright = Format.asprintf "%t" (pp_copyright ~monoidics ~ropas com_style) in
  let changed = not (String.equal old_copyright new_copyright) in
  if !show_diff && changed then (
    let with_suffix fname suff = Filename.basename fname ^ suff in
    let orig_fname = with_suffix fname ".orig" in
    let new_fname = with_suffix fname ".new" in
    Out_channel.write_lines orig_fname [old_copyright] ;
    Out_channel.write_lines new_fname [new_copyright] ;
    () ) ;
  changed


type inferred_comment_style =
  | Resolved of comment_style
  | Dune  (** dune files can have either an OCaml or a lisp-style syntax *)

let com_style_of_lang =
  [ (".ac", Resolved comment_style_m4)
  ; (".al", Resolved comment_style_al)
  ; (".app.src", Resolved comment_style_erlang)
  ; (".atd", Resolved comment_style_ocaml)
  ; (".c", Resolved comment_style_c)
  ; (".cpp", Resolved comment_style_c)
  ; (".erl", Resolved comment_style_erlang)
  ; (".h", Resolved comment_style_c)
  ; (".inc", Resolved comment_style_c)
  ; (".java", Resolved comment_style_c)
  ; (".ll", Resolved comment_style_llvm)
  ; (".m", Resolved comment_style_c)
  ; (".m4", Resolved comment_style_m4)
  ; (".make", Resolved comment_style_make)
  ; (".mk", Resolved comment_style_make)
  ; (".ml", Resolved comment_style_ocaml)
  ; (".mli", Resolved comment_style_ocaml)
  ; (".mll", Resolved comment_style_ocaml)
  ; (".mly", Resolved comment_style_c)
  ; (".mm", Resolved comment_style_c)
  ; (".php", Resolved comment_style_php)
  ; (".py", Resolved comment_style_python)
  ; (".re", Resolved comment_style_c)
  ; (".rei", Resolved comment_style_c)
  ; (".sh", Resolved comment_style_shell)
  ; ("dune", Dune)
  ; ("dune.in", Dune)
  ; ("dune.common", Dune)
  ; ("dune.common.in", Dune)
  ; ("dune-project", Resolved comment_style_lisp)
  ; ("dune-workspace", Resolved comment_style_lisp)
  ; ("dune-workspace.in", Resolved comment_style_lisp)
  ; ("Makefile", Resolved comment_style_make)
  ; ("rebar.config", Resolved comment_style_erlang) ]


let tuareg_magic_style_line = "(* -*- tuareg -*- *)"

let comment_style_of_filename fname =
  List.Assoc.find com_style_of_lang ~equal:Filename.check_suffix fname
  |> Option.map ~f:(function
       | Resolved comment_type ->
           comment_type
       | Dune ->
           (* a dune file is in OCaml syntax if and only if its first line is a tuareg style line *)
           let first_line =
             In_channel.with_file fname ~f:(fun ic -> In_channel.input_line ic)
             |> Option.map ~f:String.strip
           in
           if Option.exists first_line ~f:(fun line -> String.equal line tuareg_magic_style_line)
           then comment_style_ocaml
           else comment_style_lisp )


let output_diff ~fname lines ?notice_range ?(monoidics = false) ?(ropas = false) com_style =
  let lang = lang_of_comment_style com_style in
  let pp_range_opt fmt = function
    | None ->
        F.pp_print_string fmt "none"
    | Some (s, e) ->
        F.fprintf fmt "%d-%d" s e
  in
  F.eprintf "%s (lang:%s notice:%a monoidics:%b ropas:%b)\n%!" fname lang pp_range_opt notice_range
    monoidics ropas ;
  let pp_newfile fmt =
    let copy_lines_before, copy_lines_after =
      match notice_range with
      | Some (s, e) ->
          (s - 1, e + 1)
      | None ->
          let insert_notice_at = default_start_line_of_com_style com_style in
          (insert_notice_at - 1, insert_notice_at)
    in
    for i = 0 to copy_lines_before do
      F.fprintf fmt "%s\n" lines.(i)
    done ;
    if
      starts_with_newline com_style && copy_lines_before > 0
      && not (String.is_empty lines.(copy_lines_before - 1))
    then F.fprintf fmt "@\n" ;
    pp_copyright ~monoidics ~ropas com_style fmt ;
    for i = copy_lines_after to Array.length lines - 1 do
      F.fprintf fmt "%s\n" lines.(i)
    done ;
    F.fprintf fmt "%!"
  in
  if !update_files then
    Out_channel.with_file fname ~f:(fun cout ->
        let fmt = F.formatter_of_out_channel cout in
        pp_newfile fmt )
  else pp_newfile F.std_formatter


let check_copyright fname =
  let lines_list = In_channel.read_lines fname in
  let lines = Array.of_list lines_list in
  match (find_copyright_line lines_list, comment_style_of_filename fname) with
  | None, None ->
      ()
  | None, Some com_style ->
      output_diff ~fname lines com_style ;
      raise (CopyrightEvent CopyrightModified)
  | Some n, fname_com_style ->
      let cstart, contents_com_style =
        find_comment_start_and_style lines n |> Option.value ~default:(0, Line ("#", false))
      in
      let com_style =
        match fname_com_style with
        | None ->
            contents_com_style
        | Some fname_com_style ->
            let inferred_styles_agree =
              match (fname_com_style, contents_com_style) with
              | Line (fs, _), Line (cs, _) ->
                  String.equal fs cs
              | Block (fstart, fbody, fend, _), Block (cstart, cbody, cend, _) ->
                  String.equal fstart cstart && String.equal fbody cbody && String.equal fend cend
              | _ ->
                  false
            in
            if not inferred_styles_agree then (
              F.eprintf
                "Inferred comment style doesn't match the filename '%s':@\n\
                 From the filename I was expecting the %s comment style, but looking inside the \
                 file I found %s instead.@."
                fname
                (lang_of_comment_style fname_com_style)
                (lang_of_comment_style contents_com_style) ;
              raise (CopyrightEvent CopyrightMalformed) ) ;
            fname_com_style
      in
      (* hack to detect shebangs regardless of the inferred style *)
      let com_style =
        match com_style with
        | Line ("#", false) when String.is_prefix ~prefix:"#!" lines.(0) ->
            comment_style_shell
        | _ ->
            com_style
      in
      let cend = find_comment_end lines n com_style in
      if not (looks_like_copyright_message cstart cend lines) then (
        F.eprintf "Copyright not recognized: %s@." fname ;
        raise (CopyrightEvent CopyrightMalformed) ) ;
      let monoidics = contains_monoidics cstart cend lines in
      let ropas = contains_ropas cstart cend lines in
      if copyright_has_changed fname lines ~notice_range:(cstart, cend) ~monoidics ~ropas com_style
      then (
        output_diff ~fname lines ~notice_range:(cstart, cend) ~monoidics ~ropas com_style ;
        raise (CopyrightEvent CopyrightModified) )


let speclist =
  [ ("-i", Arg.Set update_files, "Update copyright notice in-place")
  ; ("--inplace", Arg.Set update_files, "Update copyright notice in-place")
  ; ("-k", Arg.Set keep_going, "Exit with code 0 no matter what")
  ; ("--keep-going", Arg.Set keep_going, "Exit with code 0 no matter what")
  ; ( "--show-diff"
    , Arg.Set show_diff
    , "Write file.orig and file.new files to inspect the differences found" ) ]


let usage_msg = "checkCopyright [-i] [-k] [--show-diff] file1 ..."

let () =
  let to_check = ref [] in
  let add_file_to_check fname =
    (* hack: LICENSE looks copyrightable but is not... *)
    if not (String.is_prefix ~prefix:"LICENSE" (Filename.basename fname)) then
      to_check := fname :: !to_check
  in
  Arg.parse (Arg.align speclist) add_file_to_check usage_msg ;
  let to_check = List.rev !to_check in
  let exit_code = ref 0 in
  List.iter to_check ~f:(fun file ->
      try check_copyright file
      with CopyrightEvent event -> if not !keep_going then exit_code := exit_code_of_event event ) ;
  exit !exit_code
