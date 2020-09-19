(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module U = Unix

let file_exists name =
  try
    U.access name [U.F_OK] ;
    true
  with U.Unix_error _ -> false


(* Forked processes will not start from 0 but this is inessential. *)
let tmp_counter = ref 0

let mktemp base =
  let pid = U.getpid () in
  let rec aux () =
    let name = Printf.sprintf "%s.%d.%08d.tmp" base pid !tmp_counter in
    incr tmp_counter ;
    if
      file_exists name
      (* This should not happen unless the file is very old and the pid is reused. *)
    then aux ()
    else name
  in
  aux ()


let buffer_size = 8192

let tee ic ocs =
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match input ic buffer 0 buffer_size with
    | 0 ->
        ()
    | r ->
        List.iter (fun oc -> output oc buffer 0 r) ocs ;
        loop ()
  in
  loop ()


let gzip ic oc =
  let ocz = Gzip.open_out_chan oc in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match input ic buffer 0 buffer_size with
    | 0 ->
        ()
    | r ->
        Gzip.output ocz buffer 0 r ;
        loop ()
  in
  let success =
    try
      loop () ;
      true
    with Gzip.Error _ -> false
  in
  Gzip.close_out ocz ;
  success


let gunzip ic oc =
  let icz = Gzip.open_in_chan ic in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match Gzip.input icz buffer 0 buffer_size with
    | 0 ->
        ()
    | r ->
        output oc buffer 0 r ;
        loop ()
  in
  let success =
    try
      loop () ;
      true
    with Gzip.Error _ -> false
  in
  Gzip.close_in icz ;
  success


let copy ic oc = tee ic [oc]

let rec restart_on_EINTR f x = try f x with U.Unix_error (U.EINTR, _, _) -> restart_on_EINTR f x

let close fd = try U.close fd with U.Unix_error _ -> ()

let close_in = close_in_noerr

let close_out = close_out_noerr

let wait pid =
  match snd (restart_on_EINTR (U.waitpid []) pid) with U.WEXITED 0 -> true | _ -> false


let exec args stdin stdout stderr =
  wait
    (U.create_process args.(0) args (U.descr_of_in_channel stdin) (U.descr_of_out_channel stdout)
       (U.descr_of_out_channel stderr))


let diff file1 file2 oc = exec [|"diff"; file1; file2|] stdin oc stderr

let fork f =
  let fd_in, fd_out = U.pipe () in
  match U.fork () with
  | 0 -> (
      U.close fd_in ;
      try
        if f (U.out_channel_of_descr fd_out) then exit 0
        else (
          close fd_out ;
          exit 1 )
      with _ ->
        close fd_out ;
        exit 2 )
  | pid ->
      if pid < 0 then failwith "fork error"
      else (
        U.close fd_out ;
        (pid, U.in_channel_of_descr fd_in) )


let compose f g ic oc =
  let pid, ic1 = fork (f ic) in
  let r1 = g ic1 oc in
  let r2 = wait pid in
  close_in ic1 ;
  r1 && r2


let diff_on_same_input f1 f2 ic oc =
  let file = mktemp "input" in
  let ofile = open_out file in
  copy ic ofile ;
  close_out ofile ;
  let ifile1 = open_in file and ifile2 = open_in file in
  let file1 = mktemp "output1" and file2 = mktemp "output2" in
  let ofile1 = open_out file1 and ofile2 = open_out file2 in
  let r1 = f1 ifile1 ofile1 and r2 = f2 ifile2 ofile2 in
  close_in ifile1 ;
  close_in ifile2 ;
  close_out ofile1 ;
  close_out ofile2 ;
  let success = if r1 && r2 then diff file1 file2 oc else false in
  U.unlink file ;
  U.unlink file1 ;
  U.unlink file2 ;
  success
