(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception UnlockNotLocked of Procname.t

let locks_dir = Config.procnames_locks_dir

let setup () = Utils.rmtree locks_dir ; Utils.create_dir locks_dir

let clean () = ()

let filename_from pname = locks_dir ^/ Procname.to_filename pname

let unlock pname =
  try Unix.unlink (filename_from pname)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> raise (UnlockNotLocked pname)


let try_lock pname =
  try
    Unix.openfile ~mode:[O_CREAT; O_EXCL; O_RDONLY] (filename_from pname) |> Unix.close ;
    true
  with Unix.Unix_error (Unix.EEXIST, _, _) -> false
