(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let stat_check_exn f ?(follow_symlinks = true) path =
  let rec loop () =
    try f (if follow_symlinks then Unix.stat path else Unix.lstat path) with
    | Unix.Unix_error (EINTR, _, _) ->
        loop ()
    | Unix.Unix_error ((ENOENT | ENOTDIR), _, _) ->
        false
  in
  loop ()


let stat_check f ?follow_symlinks path =
  try if stat_check_exn f ?follow_symlinks path then `Yes else `No
  with Unix.Unix_error ((EACCES | ELOOP), _, _) -> `Unknown


let file_exists = stat_check (fun _ -> true)

let is_directory = stat_check (fun stat -> Poly.equal stat.st_kind Unix.S_DIR)

(* let is_directory_exn = stat_check_exn (fun stat -> Poly.equal stat.st_kind Unix.S_DIR) *)

let is_file = stat_check (fun stat -> Poly.equal stat.st_kind Unix.S_REG)

let file_exists ?follow_symlinks path =
  match file_exists ?follow_symlinks path with `Yes -> true | `No | `Unknown -> false


let fold_dir ~init ~f directory = Array.fold (Stdlib.Sys.readdir directory) ~f ~init
