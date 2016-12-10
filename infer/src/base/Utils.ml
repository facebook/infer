(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** General utility functions and definition with global scope *)

module Arg = Core.Std.Arg
module Array = Core.Std.Array
module Bool = Core.Std.Bool
module Bytes = Core.Std.Bytes
module Caml = Core.Std.Caml
module Char = Core.Std.Char
module Filename = Core.Std.Filename
module Fn = Core.Std.Fn
module Gc = Core.Std.Gc
module In_channel = Core.Std.In_channel
module Int = Core.Std.Int
module Int32 = Core.Std.Int32
module Int63 = Core.Std.Int63
module Int64 = Core.Std.Int64
module Option = Core.Std.Option
module Pid = Core.Std.Pid
module Signal = Core.Std.Signal
module String = Core.Std.String
module Sys = struct
  include Core.Std.Sys

  (* Core_sys does not catch Unix_error ENAMETOOLONG, see
     https://github.com/janestreet/core/issues/76 *)
  let file_exists ?follow_symlinks path =
    try file_exists ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown

  let is_directory ?follow_symlinks path =
    try is_directory ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown

  let is_file ?follow_symlinks path =
    try is_file ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown
end
module Unix = Core.Std.Unix

module F = Format

(** List police: don't use the list module to avoid non-tail recursive
    functions and builtin equality. Use IList instead. *)
module List = struct end

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(** initial process times *)
let initial_times = Unix.times ()

(** precise time of day at the start of the analysis *)
let initial_timeofday = Unix.gettimeofday ()

(** {2 Generic Utility Functions} *)

(** Compare police: generic compare disabled. *)
let compare = ()

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x

let int_of_bool b = if b then 1 else 0

(** {2 Useful Modules} *)

(** Set of integers *)
module IntSet = Set.Make(Int)

(** Hash table over strings *)
module StringHash = Hashtbl.Make (String)

(** Set of strings *)
module StringSet = Set.Make(String)

(** Pretty print a set of strings *)
let pp_stringset fmt ss =
  StringSet.iter (fun s -> F.fprintf fmt "%s " s) ss

(** string list -> StringSet.t
    from http://stackoverflow.com/a/2382330 *)
let string_set_of_list list =
  IList.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty list

(** intersection of two string lists, as a StringSet.t
    from http://stackoverflow.com/a/2382330 *)
let string_list_intersection a b =
  StringSet.inter (string_set_of_list a) (string_set_of_list b)

module StringPPSet = PrettyPrintable.MakePPSet(struct
    include String
    let pp_element fmt s = F.fprintf fmt "%s" s
  end)

(** Maps from integers *)
module IntMap = Map.Make (Int)

(** Maps from strings *)
module StringMap = Map.Make (String)

(** {2 Printing} *)

(** Kind of simple printing: default or with full types *)
type pp_simple_kind = PP_SIM_DEFAULT | PP_SIM_WITH_TYP

(** Kind of printing *)
type printkind = PP_TEXT | PP_LATEX | PP_HTML

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Print environment threaded through all the printing functions *)
type printenv = {
  pe_opt : pp_simple_kind; (** Current option for simple printing *)
  pe_kind : printkind; (** Current kind of printing *)
  pe_cmap_norm : colormap; (** Current colormap for the normal part *)
  pe_cmap_foot : colormap; (** Current colormap for the footprint part *)
  pe_color : color; (** Current color *)
  pe_obj_sub : (Obj.t -> Obj.t) option (** generic object substitution *)
}

(** Create a colormap of a given color *)
let colormap_from_color color (_: Obj.t) = color

(** standard colormap: black *)
let colormap_black (_: Obj.t) = Black

(** red colormap *)
let colormap_red (_: Obj.t) = Red

(** Default text print environment *)
let pe_text =
  { pe_opt = PP_SIM_DEFAULT;
    pe_kind = PP_TEXT;
    pe_cmap_norm = colormap_black;
    pe_cmap_foot = colormap_black;
    pe_color = Black;
    pe_obj_sub = None }

(** Default html print environment *)
let pe_html color =
  { pe_text with
    pe_kind = PP_HTML;
    pe_cmap_norm = colormap_from_color color;
    pe_cmap_foot = colormap_from_color color;
    pe_color = color }

(** Default latex print environment *)
let pe_latex color =
  { pe_opt = PP_SIM_DEFAULT;
    pe_kind = PP_LATEX;
    pe_cmap_norm = colormap_from_color color;
    pe_cmap_foot = colormap_from_color color;
    pe_color = color;
    pe_obj_sub = None }

(** Extend the normal colormap for the given object with the given color *)
let pe_extend_colormap pe (x: Obj.t) (c: color) =
  let colormap (y: Obj.t) =
    if x == y then c
    else pe.pe_cmap_norm y in
  { pe with pe_cmap_norm = colormap }

(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
let pe_set_obj_sub pe (sub: 'a -> 'a) =
  let new_obj_sub x =
    let x' = Obj.repr (sub (Obj.obj x)) in
    match pe.pe_obj_sub with
    | None -> x'
    | Some sub' -> sub' x' in
  { pe with pe_obj_sub = Some (new_obj_sub) }

(** Reset the object substitution, so that no substitution takes place *)
let pe_reset_obj_sub pe =
  { pe with pe_obj_sub = None }

(** string representation of colors *)
let color_string = function
  | Black -> "color_black"
  | Blue -> "color_blue"
  | Green -> "color_green"
  | Orange -> "color_orange"
  | Red -> "color_red"

(** Pretty print a space-separated sequence *)
let rec pp_seq pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l -> F.fprintf f "%a %a" pp x (pp_seq pp) l

(** Print a comma-separated sequence *)
let rec pp_comma_seq pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l -> F.fprintf f "%a,%a" pp x (pp_comma_seq pp) l

(** Print a ;-separated sequence. *)
let rec _pp_semicolon_seq oneline pe pp f =
  let pp_sep fmt () =
    if oneline then F.fprintf fmt " " else F.fprintf fmt "@\n" in
  function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l ->
      (match pe.pe_kind with
       | PP_TEXT | PP_HTML ->
           F.fprintf f "%a ; %a%a" pp x pp_sep () (_pp_semicolon_seq oneline pe pp) l
       | PP_LATEX ->
           F.fprintf f "%a ;\\\\%a %a" pp x pp_sep () (_pp_semicolon_seq oneline pe pp) l)

(** Print a ;-separated sequence with newlines. *)
let pp_semicolon_seq pe = _pp_semicolon_seq false pe

(** Print a ;-separated sequence on one line. *)
let pp_semicolon_seq_oneline pe = _pp_semicolon_seq true pe

(** Print an or-separated sequence. *)
let pp_or_seq pe pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l ->
      (match pe.pe_kind with
       | PP_TEXT ->
           F.fprintf f "%a || %a" pp x (pp_semicolon_seq pe pp) l
       | PP_HTML ->
           F.fprintf f "%a &or; %a" pp x (pp_semicolon_seq pe pp) l
       | PP_LATEX ->
           F.fprintf f "%a \\vee %a" pp x (pp_semicolon_seq pe pp) l)

(** Produce a string from a 1-argument pretty printer function *)
let pp_to_string pp x =
  let buf = Buffer.create 1 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@?" pp x;
  Buffer.contents buf

(** Print the current time and date in a format similar to the "date" command *)
let pp_current_time f () =
  let tm = Unix.localtime (Unix.time ()) in
  F.fprintf f "%02d/%02d/%4d %02d:%02d" tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min

(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
let pp_elapsed_time fmt () =
  let elapsed = Unix.gettimeofday () -. initial_timeofday in
  Format.fprintf fmt "%f" elapsed

(** read a source file and return a list of lines, or None in case of error *)
let read_file fname =
  let res = ref [] in
  let cin_ref = ref None in
  let cleanup () =
    match !cin_ref with
    | None -> ()
    | Some cin -> close_in cin in
  try
    let cin = open_in fname in
    cin_ref := Some cin;
    while true do
      let line = input_line cin in
      res := line :: !res
    done;
    assert false
  with
  | End_of_file ->
      cleanup ();
      Some (IList.rev !res)
  | Sys_error _ ->
      cleanup ();
      None

(** copy a source file, return the number of lines, or None in case of error *)
let copy_file fname_from fname_to =
  let res = ref 0 in
  let cin_ref = ref None in
  let cout_ref = ref None in
  let cleanup () =
    begin match !cin_ref with
      | None -> ()
      | Some cin -> close_in cin
    end;
    begin match !cout_ref with
      | None -> ()
      | Some cout -> close_out cout
    end in
  try
    let cin = open_in fname_from in
    cin_ref := Some cin;
    let cout = open_out fname_to in
    cout_ref := Some cout;
    while true do
      let line = input_line cin in
      output_string cout line;
      output_char cout '\n';
      incr res
    done;
    assert false
  with
  | End_of_file ->
      cleanup ();
      Some !res
  | Sys_error _ ->
      cleanup();
      None

(** type for files used for printing *)
type outfile =
  { fname : string; (** name of the file *)
    out_c : out_channel; (** output channel *)
    fmt : F.formatter (** formatter for printing *) }

(** create an outfile for the command line *)
let create_outfile fname =
  try
    let out_c = open_out fname in
    let fmt = F.formatter_of_out_channel out_c in
    Some { fname = fname; out_c = out_c; fmt = fmt }
  with Sys_error _ ->
    F.fprintf F.err_formatter "error: cannot create file %s@." fname;
    None

(** operate on an outfile reference if it is not None *)
let do_outf outf_opt f =
  match outf_opt with
  | None -> ()
  | Some outf ->
      f outf

(** close an outfile *)
let close_outf outf =
  close_out outf.out_c

let ( // ) = Filename.concat

(** convert a filename to absolute path and normalize by removing occurrences of "." and ".." *)
module FileNormalize = struct
  let rec fname_to_list_rev fname =
    if fname = "" then [] else
      let base = Filename.basename fname in
      let dir = Filename.dirname fname in
      let does_not_split = (* make sure it terminates whatever the implementation of Filename *)
        fname = base || String.length dir >= String.length fname in
      if does_not_split then [fname]
      else base :: fname_to_list_rev dir

  (* split a file name into a list of strings representing it as a path *)
  let fname_to_list fname =
    IList.rev (fname_to_list_rev fname)

  (* concatenate a list of strings representing a path into a filename *)
  let rec list_to_fname base path = match path with
    | [] -> base
    | x :: path' -> list_to_fname (base // x) path'

  (* normalize a path where done_l is a reversed path from the root already normalized *)
  (* and todo_l is the path still to normalize *)
  let rec normalize done_l todo_l = match done_l, todo_l with
    | _, y :: tl when y = Filename.current_dir_name -> (* path/. --> path *)
        normalize done_l tl
    | [_], y :: tl when y = Filename.parent_dir_name -> (* /.. --> / *)
        normalize done_l tl
    | _ :: dl, y :: tl when y = Filename.parent_dir_name -> (* path/x/.. --> path *)
        normalize dl tl
    | _, y :: tl -> normalize (y :: done_l) tl
    | _, [] -> IList.rev done_l

  (* check if the filename contains "." or ".." *)
  let fname_contains_current_parent fname =
    let l = fname_to_list fname in
    IList.exists (fun x -> x = Filename.current_dir_name || x = Filename.parent_dir_name) l

  (* convert a filename to absolute path, if necessary, and normalize "." and ".." *)
  let fname_to_absolute_normalize fname =
    let is_relative = Filename.is_relative fname in
    let must_normalize = fname_contains_current_parent fname in
    let simple_case () =
      if is_relative then Unix.getcwd () // fname
      else fname in
    if must_normalize then begin
      let done_l, todo_l =
        if is_relative then
          fname_to_list_rev (Unix.getcwd ()), fname_to_list fname
        else match fname_to_list fname with
          | [] -> [fname], [] (* should not happen *)
          | root :: l -> [root], l in
      let normal_l = normalize done_l todo_l in
      match normal_l with
      | base :: l -> list_to_fname base l
      | [] -> (* should not happen *) simple_case ()
    end
    else simple_case ()

  (*
  let test () =
  let test_fname fname =
  let fname' = fname_to_absolute_normalize fname in
  Format.fprintf Format.std_formatter "fname %s --> %s@." fname fname' in
  let tests = [".";
  "..";
  "aaa.c";
  "/";
  "/..";
  "../test.c";
  "src/../././test.c"] in
  List.map test_fname tests
  *)
end

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
let filename_to_absolute fname =
  FileNormalize.fname_to_absolute_normalize fname

(** Convert an absolute filename to one relative to the current directory. *)
let filename_to_relative root fname =
  let string_strict_subtract s1 s2 =
    let n1, n2 = String.length s1, String.length s2 in
    if n1 < n2 && String.sub s2 ~pos:0 ~len:n1 = s1 then
      String.sub s2 ~pos:(n1 + 1) ~len:(n2 - (n1 + 1))
    else s2 in
  let norm_root = (* norm_root is root without any trailing / *)
    Filename.dirname root // Filename.basename root in
  let remainder = (* remove the path prefix to root including trailing / *)
    string_strict_subtract norm_root fname in
  remainder


(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t

let compare_proc_flags x y =
  let bindings x = Hashtbl.fold (fun k d l -> (k, d) :: l) x [] in
  [%compare: (string * string) list] (bindings x) (bindings y)


let proc_flags_empty () : proc_flags = Hashtbl.create 1

let proc_flag_skip = "skip"
let proc_flag_ignore_return = "ignore_return"

let proc_flags_add proc_flags key value =
  Hashtbl.replace proc_flags key value

let proc_flags_find proc_flags key =
  Hashtbl.find proc_flags key


let directory_fold f init path =
  let collect current_dir (accu, dirs) path =
    let full_path = current_dir // path in
    try
      if Sys.is_directory full_path = `Yes then
        (accu, full_path:: dirs)
      else
        (f accu full_path, dirs)
    with Sys_error _ ->
      (accu, dirs) in
  let rec loop accu dirs =
    match dirs with
    | [] -> accu
    | d:: tl ->
        let (new_accu, new_dirs) = Array.fold ~f:(collect d) ~init:(accu, tl) (Sys.readdir d) in
        loop new_accu new_dirs in
  if Sys.is_directory path = `Yes then
    loop init [path]
  else
    f init path


let directory_iter f path =
  let apply current_dir dirs path =
    let full_path = current_dir // path in
    try
      if Sys.is_directory full_path = `Yes then
        full_path:: dirs
      else
        let () = f full_path in
        dirs
    with Sys_error _ ->
      dirs in
  let rec loop dirs =
    match dirs with
    | [] -> ()
    | d:: tl ->
        let new_dirs = Array.fold ~f:(apply d) ~init:tl (Sys.readdir d) in
        loop new_dirs in
  if Sys.is_directory path = `Yes then
    loop [path]
  else
    f path


let remove_directory_tree path =
  Stream.from (fun _ -> Fts.fts_read (Fts.fts_open ?compar:None ~path_argv:[path] ~options:[]))
  |> Stream.iter (fun ent ->
      match Fts.FTSENT.info ent with
      | FTS_D | FTS_DOT -> ()
      | _ -> Unix.remove (Fts.FTSENT.name ent)
    )


let string_crc_hex32 s = Digest.to_hex (Digest.string s)

let string_append_crc_cutoff ?(cutoff=100) ?(key="") name =
  let name_up_to_cutoff =
    if String.length name <= cutoff
    then name
    else String.sub name ~pos:0 ~len:cutoff in
  let crc_str =
    let name_for_crc = name ^ key in
    string_crc_hex32 name_for_crc in
  name_up_to_cutoff ^ "." ^ crc_str

let read_optional_json_file path =
  if Sys.file_exists path = `Yes then
    try
      Ok (Yojson.Basic.from_file path)
    with Sys_error msg | Yojson.Json_error msg ->
      Error msg
  else Ok (`Assoc [])

let do_finally f g =
  let res = try f () with exc -> g () |> ignore; raise exc in
  let res' = g () in
  (res, res')

let with_file file ~f =
  let oc = open_out file in
  let f () = f oc in
  let g () = close_out oc in
  do_finally f g |> fst

let write_json_to_file destfile json =
  with_file destfile ~f:(fun oc -> Yojson.Basic.pretty_to_channel oc json)

let consume_in chan_in =
  try
    while true do input_line chan_in |> ignore done
  with End_of_file -> ()

let with_process_in command read =
  let chan = Unix.open_process_in command in
  let f () = read chan in
  let g () =
    consume_in chan;
    Unix.close_process_in chan in
  do_finally f g

let failwithf fmt =
  Format.kfprintf (fun _ -> failwith (Format.flush_str_formatter ()))
    Format.str_formatter fmt

let invalid_argf fmt =
  Format.kfprintf (fun _ -> invalid_arg (Format.flush_str_formatter ()))
    Format.str_formatter fmt


(** Create a directory if it does not exist already. *)
let create_dir dir =
  try
    if (Unix.stat dir).Unix.st_kind != Unix.S_DIR then
      failwithf "@.ERROR: file %s exists and is not a directory@." dir
  with Unix.Unix_error _ ->
  try Unix.mkdir dir ~perm:0o700 with
    Unix.Unix_error _ ->
      let created_concurrently = (* check if another process created it meanwhile *)
        try (Unix.stat dir).Unix.st_kind = Unix.S_DIR
        with Unix.Unix_error _ -> false in
      if not created_concurrently then
        failwithf "@.ERROR: cannot create directory %s@." dir

let realpath_cache = Hashtbl.create 1023

let realpath path =
  match Hashtbl.find realpath_cache path with
  | exception Not_found -> (
      match Filename.realpath path with
      | realpath ->
          Hashtbl.add realpath_cache path (Ok realpath);
          realpath
      | exception Unix.Unix_error (code, f, arg) ->
          F.eprintf
            "WARNING: Failed to resolve file %s with \"%s\" \n@." arg (Unix.error_message code);
          (* cache failures as well *)
          Hashtbl.add realpath_cache path (Error (code, f, arg));
          raise (Unix.Unix_error (code, f, arg))
    )
  | Ok path -> path
  | Error (code, f, arg) -> raise (Unix.Unix_error (code, f, arg))
