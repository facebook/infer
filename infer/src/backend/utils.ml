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

let string_equal (s1: string) (s2: string) = s1 = s2

let string_compare (s1: string) (s2: string) = Pervasives.compare s1 s2

let float_compare (f1: float) (f2: float) = Pervasives.compare f1 f2

let bool_compare (b1: bool) (b2: bool) = Pervasives.compare b1 b2

let bool_equal (b1: bool) (b2: bool) = b1 = b2

(** Extend and equality function to an option type. *)
let opt_equal cmp x1 x2 = match x1, x2 with
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> false
  | Some y1, Some y2 -> cmp y1 y2

let opt_compare cmp x1 x2 =
  match x1, x2 with
  | Some y1, Some y2 -> cmp y1 y2
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1

(** Efficient comparison for integers *)
let int_compare (i: int) (j: int) = (Obj.magic (i > j)) - (Obj.magic (i < j))

let int_equal (i: int) (j: int) = i = j

(** Generic comparison of pairs given a compare function for each element of the pair. *)
let pair_compare compare compare' (x1, y1) (x2, y2) =
  let n = compare x1 x2 in
  if n <> 0 then n else compare' y1 y2

(** Generic comparison of pairs given a compare function for each element of the triple *)
let triple_compare compare compare' compare'' (x1, y1, z1) (x2, y2, z2) =
  let n = compare x1 x2 in
  if n <> 0 then n else let n = compare' y1 y2 in
    if n <> 0 then n else compare'' z1 z2

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x

let int_of_bool b = if b then 1 else 0

(** {2 Useful Modules} *)

(** Set of integers *)
module IntSet =
  Set.Make(struct
    type t = int
    let compare = int_compare
  end)

(** Set of strings *)
module StringSet = Set.Make(String)

(** Pretty print a set of strings *)
let pp_stringset fmt ss =
  StringSet.iter (fun s -> F.fprintf fmt "%s " s) ss

(** Maps from integers *)
module IntMap = Map.Make (struct
    type t = int
    let compare = int_compare
  end)

(** Maps from strings *)
module StringMap = Map.Make (struct
    type t = string
    let compare (s1: string) (s2: string) = Pervasives.compare s1 s2 end)

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

(** Check if the lhs is a substring of the rhs. *)
let string_is_prefix s1 s2 =
  String.length s1 <= String.length s2 &&
  String.sub s2 0 (String.length s1) = s1

(** Check if the lhs is a postfix of the rhs. *)
let string_is_suffix s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  l1 <= l2 &&
  String.sub s2 (l2 - l1) l1 = s1

(** Check if the lhs is contained in the rhs. *)
let string_contains s1 s2 =
  let rexp = Str.regexp_string s1 in
  try
    ignore (Str.search_forward rexp s2 0);
    true
  with Not_found -> false

(** Split a string across the given character, if given. (e.g. split first.second with '.').*)
let string_split_character s c =
  try
    let index = String.rindex s c in
    let lhs = String.sub s 0 index in
    let rhs = String.sub s (index + 1) ((String.length s) - (1 + index)) in
    (Some lhs, rhs)
  with Not_found -> (None, s)

let string_value_or_empty_string
    (string_option: string option): string =
  match string_option with
  | Some s -> s
  | None -> ""

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

(** count lines of code of files and keep processed results in a cache *)
module FileLOC = struct
  let include_loc_hash = Hashtbl.create 1

  let reset () = Hashtbl.clear include_loc_hash

  let file_get_loc fname =
    try Hashtbl.find include_loc_hash fname with Not_found ->
      let loc = match read_file fname with
        | None -> 0
        | Some l -> IList.length l in
      Hashtbl.add include_loc_hash fname loc;
      loc
end

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
    if n1 < n2 && String.sub s2 0 n1 = s1 then
      String.sub s2 (n1 + 1) (n2 - (n1 + 1))
    else s2 in
  let norm_root = (* norm_root is root without any trailing / *)
    Filename.dirname root // Filename.basename root in
  let remainder = (* remove the path prefix to root including trailing / *)
    string_strict_subtract norm_root fname in
  remainder


(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t

let proc_flags_empty () : proc_flags = Hashtbl.create 1

let proc_flag_skip = "skip"
let proc_flag_ignore_return = "ignore_return"

let proc_flags_add proc_flags key value =
  Hashtbl.replace proc_flags key value

let proc_flags_find proc_flags key =
  Hashtbl.find proc_flags key

let join_strings sep = function
  | [] -> ""
  | hd:: tl ->
      IList.fold_left (fun str p -> str ^ sep ^ p) hd tl

let next compare =
  fun x y n ->
    if n <> 0 then n
    else compare x y


let directory_fold f init path =
  let collect current_dir (accu, dirs) path =
    let full_path = current_dir // path in
    try
      if Sys.is_directory full_path then
        (accu, full_path:: dirs)
      else
        (f accu full_path, dirs)
    with Sys_error _ ->
      (accu, dirs) in
  let rec loop accu dirs =
    match dirs with
    | [] -> accu
    | d:: tl ->
        let (new_accu, new_dirs) = Array.fold_left (collect d) (accu, tl) (Sys.readdir d) in
        loop new_accu new_dirs in
  if Sys.is_directory path then
    loop init [path]
  else
    f init path


let directory_iter f path =
  let apply current_dir dirs path =
    let full_path = current_dir // path in
    try
      if Sys.is_directory full_path then
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
        let new_dirs = Array.fold_left (apply d) tl (Sys.readdir d) in
        loop new_dirs in
  if Sys.is_directory path then
    loop [path]
  else
    f path

let string_crc_hex32 s = Digest.to_hex (Digest.string s)

let string_append_crc_cutoff ?(cutoff=100) ?(key="") name =
  let name_up_to_cutoff =
    if String.length name <= cutoff
    then name
    else String.sub name 0 cutoff in
  let crc_str =
    let name_for_crc = name ^ key in
    string_crc_hex32 name_for_crc in
  name_up_to_cutoff ^ "." ^ crc_str

let read_optional_json_file path =
  if Sys.file_exists path then
    try
      Ok (Yojson.Basic.from_file path)
    with Sys_error msg | Yojson.Json_error msg ->
      Error msg
  else Ok (`Assoc [])

let with_file file ~f =
  let oc = open_out file in
  try
    let res = f oc in
    close_out oc;
    res
  with exc ->
    close_out oc;
    raise exc

let write_json_to_file destfile json =
  with_file destfile ~f:(fun oc -> Yojson.Basic.pretty_to_channel oc json)

let with_process_in command read =
  let chan = Unix.open_process_in command in
  let res =
    try
      read chan
    with exc ->
      Unix.close_process_in chan |> ignore ;
      raise exc
  in
  Unix.close_process_in chan |> ignore ;
  res

let failwithf fmt =
  Format.kfprintf (fun _ -> failwith (Format.flush_str_formatter ()))
    Format.str_formatter fmt

let invalid_argf fmt =
  Format.kfprintf (fun _ -> invalid_arg (Format.flush_str_formatter ()))
    Format.str_formatter fmt
