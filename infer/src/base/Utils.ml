(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PolyVariantEqual
module F = Format
module Hashtbl = Caml.Hashtbl
module L = Die

let fold_file_tree ~init ~f_dir ~f_reg ~path =
  let rec traverse_dir_aux acc dir_path =
    let aux base_path acc' rel_path =
      let full_path = base_path ^/ rel_path in
      match (Unix.stat full_path).st_kind with
      | S_DIR ->
          traverse_dir_aux (f_dir acc' full_path) full_path
      | S_REG ->
          f_reg acc' full_path
      | _ ->
          acc'
      | exception Unix.Unix_error (ENOENT, _, _) ->
          acc'
    in
    Sys.fold_dir ~init:acc ~f:(aux dir_path) dir_path
  in
  traverse_dir_aux init path


let fold_folders ~init ~f ~path =
  let f_reg acc _ignore_reg = acc in
  fold_file_tree ~init ~f_dir:f ~f_reg ~path


let fold_files ~init ~f ~path =
  let f_dir acc _ignore_dir = acc in
  fold_file_tree ~init ~f_dir ~f_reg:f ~path


(** recursively find all files in [path] with names ending in [extension] *)
let find_files ~path ~extension =
  let f_dir acc _ignore_dir = acc in
  let f_reg acc path = if Filename.check_suffix path extension then path :: acc else acc in
  fold_file_tree ~init:[] ~f_dir ~f_reg ~path


(** read a source file and return a list of lines, or Error in case of error *)
let read_file fname =
  let res = ref [] in
  let cin_ref = ref None in
  let cleanup () = match !cin_ref with None -> () | Some cin -> In_channel.close cin in
  try
    let cin = In_channel.create fname in
    cin_ref := Some cin ;
    while true do
      let line = In_channel.input_line_exn cin in
      res := line :: !res
    done ;
    assert false
  with
  | End_of_file ->
      cleanup () ;
      Ok (List.rev !res)
  | Sys_error error ->
      cleanup () ;
      Error error


(** type for files used for printing *)
type outfile =
  { fname: string  (** name of the file *)
  ; out_c: Out_channel.t  (** output channel *)
  ; fmt: F.formatter  (** formatter for printing *) }

(** create an outfile for the command line *)
let create_outfile fname =
  try
    let out_c = Out_channel.create fname in
    let fmt = F.formatter_of_out_channel out_c in
    Some {fname; out_c; fmt}
  with Sys_error _ ->
    F.fprintf F.err_formatter "error: cannot create file %s@." fname ;
    None


(** close an outfile *)
let close_outf outf = Out_channel.close outf.out_c

let normalize_path_from ~root fname =
  let add_entry (rev_done, rev_root) entry =
    match (entry, rev_done, rev_root) with
    | ".", _, _ ->
        (* path/. --> path *)
        (rev_done, rev_root)
    | "..", [], ["/"] | "..", ["/"], _ ->
        (* /.. -> / *)
        (rev_done, rev_root)
    | "..", [], ("." | "..") :: _ | "..", ("." | "..") :: _, _ ->
        (* id on {.,..}/.. *)
        (entry :: rev_done, rev_root)
    | "..", [], _ :: rev_root_parent ->
        (* eat from the root part if it's not / *)
        ([], rev_root_parent)
    | "..", _ :: rev_done_parent, _ ->
        (* path/dir/.. --> path *)
        (rev_done_parent, rev_root)
    | _ ->
        (entry :: rev_done, rev_root)
  in
  let rev_root =
    (* Remove the leading "." inserted by [Filename.parts] on relative paths. We don't need to do
       that for [Filename.parts fname] because the "." will go away during normalization in
       [add_entry]. *)
    let root_without_leading_dot =
      match Filename.parts root with "." :: (_ :: _ as rest) -> rest | parts -> parts
    in
    List.rev root_without_leading_dot
  in
  let rev_result, rev_root = Filename.parts fname |> List.fold ~init:([], rev_root) ~f:add_entry in
  (* don't use [Filename.of_parts] because it doesn't like empty lists and produces relative paths
     "./like/this" instead of "like/this" *)
  let filename_of_rev_parts = function
    | [] ->
        "."
    | _ :: _ as rev_parts ->
        let parts = List.rev rev_parts in
        if String.equal (List.hd_exn parts) "/" then
          "/" ^ String.concat ~sep:Filename.dir_sep (List.tl_exn parts)
        else String.concat ~sep:Filename.dir_sep parts
  in
  (filename_of_rev_parts rev_result, filename_of_rev_parts rev_root)


let normalize_path fname = fname |> normalize_path_from ~root:"." |> fst

let flatten_path ?(sep = "-") path =
  let normalized_path = normalize_path path in
  let path_parts = Filename.parts normalized_path in
  let process_part = function ".." -> ["dd"] | "." -> [] | other -> [other] in
  List.bind path_parts ~f:process_part |> String.concat ~sep


(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
let filename_to_absolute ~root fname =
  let abs_fname = if Filename.is_absolute fname then fname else root ^/ fname in
  normalize_path_from ~root:"/" abs_fname |> fst


(** Convert an absolute filename to one relative to the given directory. *)
let filename_to_relative ?(force_full_backtrack = false) ?(backtrack = 0) ~root fname =
  let rec relativize_if_under origin target =
    match (origin, target) with
    | x :: xs, y :: ys when String.equal x y ->
        relativize_if_under xs ys
    | _ :: _, _ when force_full_backtrack || backtrack >= List.length origin ->
        let parent_dir = List.init (List.length origin) ~f:(fun _ -> Filename.parent_dir_name) in
        Some (Filename.of_parts (parent_dir @ target))
    | [], [] ->
        Some "."
    | [], ys ->
        Some (Filename.of_parts ys)
    | _ ->
        None
  in
  relativize_if_under (Filename.parts root) (Filename.parts fname)


let directory_fold f init path =
  let collect current_dir (accu, dirs) path =
    let full_path = current_dir ^/ path in
    try
      if Sys.is_directory full_path = `Yes then (accu, full_path :: dirs)
      else (f accu full_path, dirs)
    with Sys_error _ -> (accu, dirs)
  in
  let rec loop accu dirs =
    match dirs with
    | [] ->
        accu
    | d :: tl ->
        let new_accu, new_dirs = Array.fold ~f:(collect d) ~init:(accu, tl) (Sys.readdir d) in
        loop new_accu new_dirs
  in
  if Sys.is_directory path = `Yes then loop init [path] else f init path


let directory_iter f path =
  let apply current_dir dirs path =
    let full_path = current_dir ^/ path in
    try
      if Sys.is_directory full_path = `Yes then full_path :: dirs
      else
        let () = f full_path in
        dirs
    with Sys_error _ -> dirs
  in
  let rec loop dirs =
    match dirs with
    | [] ->
        ()
    | d :: tl ->
        let new_dirs = Array.fold ~f:(apply d) ~init:tl (Sys.readdir d) in
        loop new_dirs
  in
  if Sys.is_directory path = `Yes then loop [path] else f path


let string_crc_hex32 s = Caml.Digest.to_hex (Caml.Digest.string s)

let read_json_file path =
  try Ok (Yojson.Safe.from_file path) with Sys_error msg | Yojson.Json_error msg -> Error msg


let do_finally_swallow_timeout ~f ~finally =
  let res_finally = ref None in
  let finally () = res_finally := Some (finally ()) in
  let res = Exception.try_finally ~f ~finally in
  (res, Option.value_exn !res_finally)


let with_file_in file ~f =
  (* Use custom code instead of In_channel.create to be able to pass [O_SHARE_DELETE] *)
  let ic =
    let fd =
      try Unix.openfile ~mode:[Unix.O_RDONLY; Unix.O_SHARE_DELETE] file
      with Unix.Unix_error (e, _, _) ->
        let msg =
          match e with
          | Unix.ENOENT ->
              ": no such file or directory"
          | _ ->
              ": error " ^ Unix.Error.message e
        in
        raise (Sys_error (file ^ msg))
    in
    Unix.in_channel_of_descr fd
  in
  let f () = f ic in
  let finally () = In_channel.close ic in
  Exception.try_finally ~f ~finally


let with_file_out ?append file ~f =
  let oc = Out_channel.create ?append file in
  let f () = f oc in
  let finally () = Out_channel.close oc in
  Exception.try_finally ~f ~finally


let with_intermediate_temp_file_out ?(retry = false) file ~f =
  let temp_filename, temp_oc = Filename.open_temp_file ~in_dir:(Filename.dirname file) "infer" "" in
  let f () = f temp_oc in
  let finally () =
    Out_channel.close temp_oc ;
    (* Retry n times with exponential backoff when [retry] is true *)
    let rec rename n delay =
      try Unix.rename ~src:temp_filename ~dst:file
      with e ->
        if Int.equal n 0 then raise e
        else
          let delay = delay ** 2.0 in
          ignore (Unix.nanosleep delay) ;
          rename (n - 1) delay
    in
    if retry then rename 5 0.1 else rename 0 0.0
  in
  Exception.try_finally ~f ~finally


let with_channel_in ~f chan_in =
  try
    while true do
      f @@ In_channel.input_line_exn chan_in
    done
  with End_of_file -> ()


let consume_in chan_in = with_channel_in ~f:ignore chan_in

let echo_in chan_in = with_channel_in ~f:print_endline chan_in

let with_process_in command read =
  let chan = Unix.open_process_in command in
  let f () = read chan in
  let finally () =
    consume_in chan ;
    Unix.close_process_in chan
  in
  do_finally_swallow_timeout ~f ~finally


let is_dir_kind (kind : Unix.file_kind) = match kind with S_DIR -> true | _ -> false

(** Recursively create a directory if it does not exist already. *)
let create_dir dir =
  try
    if not (is_dir_kind (Unix.stat dir).Unix.st_kind) then
      L.(die ExternalError) "file '%s' already exists and is not a directory" dir
  with Unix.Unix_error _ -> (
    try Unix.mkdir_p dir ~perm:0o700
    with Unix.Unix_error _ ->
      let created_concurrently =
        (* check if another process created it meanwhile *)
        try Poly.equal (Unix.stat dir).Unix.st_kind Unix.S_DIR with Unix.Unix_error _ -> false
      in
      if not created_concurrently then L.(die ExternalError) "cannot create directory '%s'" dir )


let out_channel_create_with_dir fname =
  try Out_channel.create fname
  with Sys_error _ ->
    Unix.mkdir_p ~perm:0o700 (Filename.dirname fname) ;
    Out_channel.create fname


let realpath_cache = Hashtbl.create 1023

let realpath ?(warn_on_error = true) path =
  match Hashtbl.find realpath_cache path with
  | exception Caml.Not_found -> (
    match Filename.realpath path with
    | realpath ->
        Hashtbl.add realpath_cache path (Ok realpath) ;
        realpath
    | exception (Unix.Unix_error (code, _, arg) as exn) ->
        IExn.reraise_after exn ~f:(fun () ->
            if warn_on_error then
              F.eprintf "WARNING: Failed to resolve file %s with \"%s\" @\n@." arg
                (Unix.Error.message code) ;
            (* cache failures as well *)
            Hashtbl.add realpath_cache path (Error exn) ) )
  | Ok path ->
      path
  | Error exn ->
      raise exn


(* never closed *)
let devnull =
  let file = if Sys.win32 then "NUL" else "/dev/null" in
  lazy (Unix.openfile file ~mode:[Unix.O_WRONLY])


let suppress_stderr2 f2 x1 x2 =
  let restore_stderr src =
    Unix.dup2 ~src ~dst:Unix.stderr () ;
    Unix.close src
  in
  let orig_stderr = Unix.dup Unix.stderr in
  Unix.dup2 ~src:(Lazy.force devnull) ~dst:Unix.stderr () ;
  let f () = f2 x1 x2 in
  let finally () = restore_stderr orig_stderr in
  protect ~f ~finally


let iter_dir name ~f =
  let dir = Unix.opendir name in
  let rec iter dir =
    match Unix.readdir_opt dir with
    | Some entry ->
        if
          not
            ( String.equal entry Filename.current_dir_name
            || String.equal entry Filename.parent_dir_name )
        then f (name ^/ entry) ;
        iter dir
    | None ->
        Unix.closedir dir
  in
  iter dir


let is_string_in_list_option list_opt s =
  Option.exists list_opt ~f:(fun l -> List.mem ~equal:String.equal l s)


(** delete [name] recursively, return whether the file is not there at the end *)
let rec rmtree_ ?except name =
  match (Unix.lstat name).st_kind with
  | S_DIR ->
      if rm_all_in_dir_ ?except name then (
        Unix.rmdir name ;
        true )
      else false
  | _ ->
      if is_string_in_list_option except name then false
      else (
        Unix.unlink name ;
        true )
  | exception Unix.Unix_error (ENOENT, _, _) ->
      (* no entry: already deleted/was never there *)
      true


and rm_all_in_dir_ ?except name =
  let no_files_left = ref true in
  iter_dir name ~f:(fun entry ->
      let entry_was_deleted =
        if is_string_in_list_option except name then false else rmtree_ ?except entry
      in
      no_files_left := !no_files_left && entry_was_deleted ) ;
  !no_files_left


let rm_all_in_dir ?except name = rm_all_in_dir_ ?except name |> ignore

let rmtree ?except name = rmtree_ ?except name |> ignore

let better_hash x = Marshal.to_string x [Marshal.No_sharing] |> Caml.Digest.string

let unlink_file_on_exit temp_file =
  let description = "Cleaning temporary file " ^ temp_file in
  Epilogues.register ~description ~f:(fun () -> try Unix.unlink temp_file with _ -> ())


(** drop at most one layer of well-balanced first and last characters satisfying [drop] from the
    string; for instance,
    [strip_balanced ~drop:(function | 'a' | 'x' -> true | _ -> false) "xaabax"] returns "aaba" *)
let strip_balanced_once ~drop s =
  let n = String.length s in
  if n < 2 then s
  else
    let first = String.unsafe_get s 0 in
    if Char.equal first (String.unsafe_get s (n - 1)) && drop first then String.slice s 1 (n - 1)
    else s


let timeit ~f =
  let start_time = Mtime_clock.counter () in
  let ret_val = f () in
  let duration = Mtime_clock.count start_time in
  (ret_val, duration)


let do_in_dir ~dir ~f =
  let cwd = Unix.getcwd () in
  Unix.chdir dir ;
  Exception.try_finally ~f ~finally:(fun () -> Unix.chdir cwd)


let get_available_memory_MB () =
  let proc_meminfo = "/proc/meminfo" in
  let rec scan_for_expected_output in_channel =
    match In_channel.input_line in_channel with
    | None ->
        None
    | Some line -> (
      try Scanf.sscanf line "MemAvailable: %u kB" (fun mem_kB -> Some (mem_kB / 1024))
      with Scanf.Scan_failure _ -> scan_for_expected_output in_channel )
  in
  if not (ISys.file_exists proc_meminfo) then None
  else with_file_in proc_meminfo ~f:scan_for_expected_output


let fold_infer_deps ~root infer_deps_file ~init ~f =
  let one_line acc line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir] ->
        let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (root ^/ target_results_dir)
          else target_results_dir
        in
        f acc infer_out_src
    | _ ->
        Die.die InternalError "Couldn't parse deps file '%s', line: %s" infer_deps_file line
  in
  match read_file infer_deps_file with
  | Ok lines ->
      List.fold ~init ~f:one_line lines
  | Error error ->
      Die.die InternalError "Couldn't read deps file '%s': %s" infer_deps_file error


let iter_infer_deps ~root infer_deps_file ~f =
  Container.iter ~fold:(fold_infer_deps ~root) infer_deps_file ~f


let inline_argument_files args =
  let expand_arg arg =
    if String.is_prefix ~prefix:"@" arg then
      let file_name = String.chop_prefix_exn ~prefix:"@" arg in
      if not (ISys.file_exists file_name) then [arg]
        (* Arguments that start with @ could mean something different than an arguments file *)
      else
        let expanded_args =
          try with_file_in file_name ~f:In_channel.input_lines
          with exn ->
            Die.die UserError "Could not read from file '%s': %a@\n" file_name Exn.pp exn
        in
        expanded_args
    else [arg]
  in
  List.concat_map ~f:expand_arg args


let cpus = Setcore.numcores ()

let zip_fold ~init ~f ~zip_filename =
  let file_in = Zip.open_in zip_filename in
  let collect acc (entry : Zip.entry) = f acc file_in entry in
  let result = List.fold ~f:collect ~init (Zip.entries file_in) in
  Zip.close_in file_in ;
  result


let is_term_dumb () =
  match Sys.getenv "TERM" with
  | Some kind when String.(equal "dumb" (lowercase kind)) ->
      true
  | _ ->
      false
