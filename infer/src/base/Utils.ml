(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

module F = Format
module Hashtbl = Caml.Hashtbl

(** initial process times *)
let initial_times = Unix.times ()

(** precise time of day at the start of the analysis *)
let initial_timeofday = Unix.gettimeofday ()

(** read a source file and return a list of lines, or None in case of error *)
let read_file fname =
  let res = ref [] in
  let cin_ref = ref None in
  let cleanup () =
    match !cin_ref with
    | None -> ()
    | Some cin -> In_channel.close cin in
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
      | Some cin -> In_channel.close cin
    end;
    begin match !cout_ref with
      | None -> ()
      | Some cout -> Out_channel.close cout
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
  Out_channel.close outf.out_c

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
    | x :: path' -> list_to_fname (base ^/ x) path'

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
      if is_relative then Unix.getcwd () ^/ fname
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
    Filename.dirname root ^/ Filename.basename root in
  let remainder = (* remove the path prefix to root including trailing / *)
    string_strict_subtract norm_root fname in
  remainder


let directory_fold f init path =
  let collect current_dir (accu, dirs) path =
    let full_path = current_dir ^/ path in
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
    let full_path = current_dir ^/ path in
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
  let g () = Out_channel.close oc in
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

(** Create a directory if it does not exist already. *)
let create_dir dir =
  try
    if (Unix.stat dir).Unix.st_kind <> Unix.S_DIR then
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


(* never closed *)
let devnull = lazy (Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY])

let suppress_stderr2 f2 x1 x2 =
  let restore_stderr src =
    Unix.dup2 ~src ~dst:Unix.stderr;
    Unix.close src in
  let orig_stderr = Unix.dup Unix.stderr in
  Unix.dup2 ~src:(Lazy.force devnull) ~dst:Unix.stderr;
  let f () = f2 x1 x2 in
  let finally () = restore_stderr orig_stderr in
  protect ~f ~finally

let compare_versions v1 v2 =
  let int_list_of_version v =
    let lv = String.split ~on:'.' v in
    let int_of_string_or_zero v =
      try int_of_string v
      with Failure _ -> 0 in
    List.map ~f:int_of_string_or_zero lv in
  let lv1  = int_list_of_version v1 in
  let lv2  = int_list_of_version v2 in
  [%compare : int list] lv1 lv2
