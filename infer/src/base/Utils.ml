(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PolyVariantEqual
module F = Format
module Hashtbl = Caml.Hashtbl
module L = Die

(** initial process times *)
let initial_times = Unix.times ()

(** recursively traverse a path for files ending with a given extension *)
let find_files ~path ~extension =
  let rec traverse_dir_aux init dir_path =
    let aux base_path files rel_path =
      let full_path = base_path ^/ rel_path in
      match (Unix.stat full_path).Unix.st_kind with
      | Unix.S_REG when String.is_suffix ~suffix:extension full_path ->
          full_path :: files
      | Unix.S_DIR ->
          traverse_dir_aux files full_path
      | _ ->
          files
    in
    Sys.fold_dir ~init ~f:(aux dir_path) dir_path
  in
  traverse_dir_aux [] path


(** read a source file and return a list of lines, or None in case of error *)
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
      cleanup () ; Error error


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

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
let filename_to_absolute ~root fname =
  let add_entry rev_done entry =
    match (entry, rev_done) with
    | ".", [] ->
        entry :: rev_done (* id on . *)
    | ".", _ ->
        rev_done (* path/. --> path *)
    | "..", ("." | "..") :: _ ->
        entry :: rev_done (* id on {.,..}/.. *)
    | "..", ["/"] ->
        rev_done (* /.. -> / *)
    | "..", _ :: rev_done_parent ->
        rev_done_parent (* path/dir/.. --> path *)
    | _ ->
        entry :: rev_done
  in
  let abs_fname = if Filename.is_absolute fname then fname else root ^/ fname in
  Filename.of_parts (List.rev (List.fold ~f:add_entry ~init:[] (Filename.parts abs_fname)))


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


let directory_is_empty path = Sys.readdir path |> Array.is_empty

let string_crc_hex32 s = Caml.Digest.to_hex (Caml.Digest.string s)

let read_json_file path =
  try Ok (Yojson.Basic.from_file path) with Sys_error msg | Yojson.Json_error msg -> Error msg


let do_finally_swallow_timeout ~f ~finally =
  let res =
    try f () with exc ->
      IExn.reraise_after exc ~f:(fun () ->
          try finally () |> ignore with _ -> (* swallow in favor of the original exception *) () )
  in
  let res' = finally () in
  (res, res')


let try_finally_swallow_timeout ~f ~finally =
  let res, () = do_finally_swallow_timeout ~f ~finally in
  res


let with_file_in file ~f =
  let ic = In_channel.create file in
  let f () = f ic in
  let finally () = In_channel.close ic in
  try_finally_swallow_timeout ~f ~finally


let with_file_out file ~f =
  let oc = Out_channel.create file in
  let f () = f oc in
  let finally () = Out_channel.close oc in
  try_finally_swallow_timeout ~f ~finally


let with_intermediate_temp_file_out file ~f =
  let temp_filename, temp_oc =
    Filename.open_temp_file ~in_dir:(Filename.dirname file) "infer" ""
  in
  let f () = f temp_oc in
  let finally () =
    Out_channel.close temp_oc ;
    Unix.rename ~src:temp_filename ~dst:file
  in
  try_finally_swallow_timeout ~f ~finally


let write_json_to_file destfile json =
  with_file_out destfile ~f:(fun oc -> Yojson.Basic.pretty_to_channel oc json)


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
  let finally () = consume_in chan ; Unix.close_process_in chan in
  do_finally_swallow_timeout ~f ~finally


let with_process_lines ~(debug : ('a, F.formatter, unit) format -> 'a) ~cmd ~tmp_prefix ~f =
  let shell_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
  let verbose_err_file = Filename.temp_file tmp_prefix ".err" in
  let shell_cmd_redirected = Printf.sprintf "%s 2>'%s'" shell_cmd verbose_err_file in
  debug "Trying to execute: %s@\n%!" shell_cmd_redirected ;
  let input_lines chan = In_channel.input_lines ~fix_win_eol:true chan in
  let res = with_process_in shell_cmd_redirected input_lines in
  let verbose_errlog = with_file_in verbose_err_file ~f:In_channel.input_all in
  if not (String.equal verbose_errlog "") then
    debug "@\nlog:@\n<<<<<<@\n%s@\n>>>>>>@\n%!" verbose_errlog ;
  match res with
  | lines, Ok () ->
      f lines
  | lines, (Error _ as err) ->
      let output = String.concat ~sep:"\n" lines in
      L.(die ExternalError)
        "*** Failed to execute: %s@\n*** Command: %s@\n*** Output:@\n%s@."
        (Unix.Exit_or_signal.to_string_hum err)
        shell_cmd output


(** Create a directory if it does not exist already. *)
let create_dir dir =
  try
    if (Unix.stat dir).Unix.st_kind <> Unix.S_DIR then
      L.(die ExternalError) "file '%s' already exists and is not a directory" dir
  with Unix.Unix_error _ -> (
    try Unix.mkdir dir ~perm:0o700 with Unix.Unix_error _ ->
      let created_concurrently =
        (* check if another process created it meanwhile *)
        try Polymorphic_compare.( = ) (Unix.stat dir).Unix.st_kind Unix.S_DIR
        with Unix.Unix_error _ -> false
      in
      if not created_concurrently then L.(die ExternalError) "cannot create directory '%s'" dir )


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
let devnull = lazy (Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY])

let suppress_stderr2 f2 x1 x2 =
  let restore_stderr src = Unix.dup2 ~src ~dst:Unix.stderr ; Unix.close src in
  let orig_stderr = Unix.dup Unix.stderr in
  Unix.dup2 ~src:(Lazy.force devnull) ~dst:Unix.stderr ;
  let f () = f2 x1 x2 in
  let finally () = restore_stderr orig_stderr in
  protect ~f ~finally


let compare_versions v1 v2 =
  let int_list_of_version v =
    let lv = match String.split ~on:'.' v with [v] -> [v; "0"] | v -> v in
    let int_of_string_or_zero v = try int_of_string v with Failure _ -> 0 in
    List.map ~f:int_of_string_or_zero lv
  in
  let lv1 = int_list_of_version v1 in
  let lv2 = int_list_of_version v2 in
  [%compare: int list] lv1 lv2


let write_file_with_locking ?(delete = false) ~f:do_write fname =
  Unix.with_file
    ~mode:Unix.[O_WRONLY; O_CREAT]
    fname
    ~f:(fun file_descr ->
      if Unix.flock file_descr Unix.Flock_command.lock_exclusive then (
        (* make sure we're not writing over some existing, possibly longer content: some other
           process may have snagged the file from under us between open(2) and flock(2) so passing
           O_TRUNC to open(2) above would not be a good substitute for calling ftruncate(2)
           below. *)
        Unix.ftruncate file_descr ~len:Int64.zero ;
        let outc = Unix.out_channel_of_descr file_descr in
        do_write outc ;
        Out_channel.flush outc ;
        ignore (Unix.flock file_descr Unix.Flock_command.unlock) ) ) ;
  if delete then try Unix.unlink fname with Unix.Unix_error _ -> ()


let rec rmtree name =
  match Unix.((lstat name).st_kind) with
  | S_DIR ->
      let dir = Unix.opendir name in
      let rec rmdir dir =
        match Unix.readdir_opt dir with
        | Some entry ->
            if
              not
                ( String.equal entry Filename.current_dir_name
                || String.equal entry Filename.parent_dir_name )
            then rmtree (name ^/ entry) ;
            rmdir dir
        | None ->
            Unix.closedir dir ; Unix.rmdir name
      in
      rmdir dir
  | _ ->
      Unix.unlink name
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()


let better_hash x = Marshal.to_string x [Marshal.No_sharing] |> Caml.Digest.string

let unlink_file_on_exit temp_file =
  let description = "Cleaning temporary file " ^ temp_file in
  Epilogues.register ~description ~f:(fun () -> try Unix.unlink temp_file with _ -> ())


(** drop at most one layer of well-balanced first and last characters satisfying [drop] from the
   string; for instance, [strip_balanced ~drop:(function | 'a' | 'x' -> true | _ -> false) "xaabax"]
   returns "aaba" *)
let strip_balanced_once ~drop s =
  let n = String.length s in
  if n < 2 then s
  else
    let first = String.unsafe_get s 0 in
    if Char.equal first (String.unsafe_get s (n - 1)) && drop first then String.slice s 1 (n - 1)
    else s
