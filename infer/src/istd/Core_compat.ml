(*

The MIT License

Copyright (c) 2008--2019 Jane Street Group, LLC  opensource@janestreet.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject
to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

(* Most of this code is taken from Core. It acts as a glue to migrate
   Core_kernel into Core, at least for the functions that are used in
   infer *)

module Sys = struct
  include Sys

  let getenv var = match getenv var with v -> Some v | exception _ -> None

  (* All the following functions are taken from Core_sys *)

  let stat_check_exn f ?(follow_symlinks = true) path =
    let rec loop () =
      try f (if follow_symlinks then Unix.LargeFile.stat path else Unix.LargeFile.lstat path) with
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          loop ()
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
          false
    in
    loop ()


  let stat_check f ?(follow_symlinks = true) path =
    try if stat_check_exn ~follow_symlinks f path then `Yes else `No
    with Unix.Unix_error ((Unix.EACCES | Unix.ELOOP), _, _) -> `Unknown


  let file_exists = stat_check (fun _ -> true)

  let file_exists_exn = stat_check_exn (fun _ -> true)

  let is_directory = stat_check (fun stat -> stat.Unix.LargeFile.st_kind = Unix.S_DIR)

  let is_directory_exn = stat_check_exn (fun stat -> stat.Unix.LargeFile.st_kind = Unix.S_DIR)

  let is_file = stat_check (fun stat -> stat.Unix.LargeFile.st_kind = Unix.S_REG)

  let is_file_exn = stat_check_exn (fun stat -> stat.Unix.LargeFile.st_kind = Unix.S_REG)

  let fold_dir ~init ~f directory = Array.fold (readdir directory) ~f ~init
end

let ( ^/ ) = Filename.concat

module Pid = struct
  type t = int

  let pp = Format.pp_print_int

  let to_int (x : t) = x

  let of_int (x : int) = x

  let to_string (x : t) = string_of_int x

  let equal = ( = )
end

module Filename = struct
  include Filename

  external realpath : string -> string = "unix_realpath"

  let split_extension p =
    let e = extension p in
    if e = "" then (p, None)
    else
      let e = String.sub e ~pos:1 ~len:(String.length e - 1) in
      (remove_extension p, Some e)


  let of_parts = function
    | [] ->
        failwith "Filename.of_parts: empty parts list"
    | root :: rest ->
        List.fold rest ~init:root ~f:Caml.Filename.concat


  let parts f =
    let rec aux acc f =
      let b = basename f in
      let d = dirname f in
      if d = f then f :: acc else aux (b :: acc) d
    in
    aux [] f


  let open_temp_file ?(perm = 0o600) ?(in_dir = get_temp_dir_name ()) prefix suffix =
    open_temp_file ~perms:perm ~temp_dir:in_dir prefix suffix


  let temp_file ?perm ?in_dir prefix suffix =
    let name, ch = open_temp_file ?perm ?in_dir prefix suffix in
    Pervasives.close_out ch ; name


  let is_absolute p = not (is_relative p)
end

module Signal = struct
  type t = int

  let kill = Sys.sigkill

  (* TODO: check. We use sigkill here, because
     1) sigterm and sigkill basically do the same thing
     2) since Infer never masks sigterm
     3) and sigterm is not supported under windows
  *)
  let term = Sys.sigkill

  let int = Sys.sigint

  let send s pid =
    assert (s = kill) (* only signal supported under windows *) ;
    match pid with
    | `Pid pid -> (
      try Unix.kill pid s ; `Ok with Unix.Unix_error (Unix.ESRCH, _, _) -> `No_such_process )
    | `My_group | `Group _ ->
        (* TODO; does not seem to be used *)
        `No_such_process


  (* TODO-minor *)
  let to_string s = string_of_int s

  module Expert = struct
    let handle signal handler = ignore (Sys.signal signal (Sys.Signal_handle handler))
  end
end

module Unix = struct
  include Unix

  (* Taken from Core *)
  (* No need to include a counter here. It just doesn't make sense to think we are
     going to be receiving a steady stream of interrupts.
     Glibc's macro doesn't have a counter either.
  *)
  let rec retry_until_no_eintr f =
    try f () with Unix.Unix_error (EINTR, _, _) -> retry_until_no_eintr f


  (* modified from Core: no attempt is made to improve the text of the exception *)
  let improve ?(restart = false) f _make_arg_sexps =
    if restart then retry_until_no_eintr f else f ()


  let mkdir ?(perm = 0o777) name = Unix.mkdir name perm

  let openfile ?(perm = 0o644) ~mode filename = openfile filename mode perm

  (* Prints out in octal, which is much more standard in Unix. *)
  let sexp_of_file_perm fp = Sexp.Atom (Printf.sprintf "0o%03o" fp)

  let is_rw_open_flag = function O_RDONLY | O_WRONLY | O_RDWR -> true | _ -> false

  let with_close fd ~f = protect ~f:(fun () -> f fd) ~finally:(fun () -> close fd)

  let with_file ?perm file ~mode ~f = with_close (openfile file ~mode ?perm) ~f

  type env =
    [ `Replace of (string * string) list
    | `Extend of (string * string) list
    | `Replace_raw of string list ]

  (* Taken from Core *)
  let env_map env =
    let current () =
      List.map (Array.to_list (Unix.environment ())) ~f:(fun s -> String.lsplit2_exn s ~on:'=')
    in
    let map_of_list list = String.Map.of_alist_reduce list ~f:(fun _ x -> x) in
    match env with
    | `Replace env ->
        map_of_list env
    | `Extend extend ->
        map_of_list (current () @ extend)
    | `Override overrides ->
        List.fold_left overrides
          ~init:(map_of_list (current ()))
          ~f:(fun acc (key, v) ->
            match v with None -> Map.remove acc key | Some data -> Map.set acc ~key ~data )


  (* Taken from Core *)
  let env_assignments env =
    match env with
    | `Replace_raw env ->
        env
    | (`Replace _ | `Extend _ | `Override _) as env ->
        Map.fold (env_map env) ~init:[] ~f:(fun ~key ~data acc -> (key ^ "=" ^ data) :: acc)


  let conv_env (env : env) = Fn.compose Array.of_list env_assignments env

  (* Own functions *)

  let putenv ~key ~data = putenv key data

  (* TODO: this does not exactly emulate the Unix semantics, as [getenv key] will return the empty
     string instead of failing. Unclear how to do this correcty under windows. Seems to be ok for
     the usage done by infer. *)
  let unsetenv key = putenv ~key ~data:""

  let rename ~src ~dst = rename src dst

  let symlink ~src ~dst = symlink src dst

  let utimes src ~access ~modif = utimes src access modif

  let read ~pos ~len fd ~buf = read fd buf pos len

  module File_descr = struct
    type t = Unix.file_descr
  end

  let readdir_opt h = try Some (readdir h) with End_of_file -> None

  (* Taken from Core *)
  let mkdir_p ?perm dirname =
    let mkdir_if_missing ?perm dir =
      try mkdir ?perm dir with
      (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already
         exists. *)
      | Unix_error ((EEXIST | EISDIR), _, _) ->
          ()
      | e ->
          raise e
    in
    let init, dirs =
      match Filename.parts dirname with [] -> assert false | init :: dirs -> (init, dirs)
    in
    mkdir_if_missing ?perm init ;
    let _ : string =
      (* just using the fold for the side effects and accumulator *)
      (* This must be [fold_left], not [fold_right]. *)
      List.fold_left dirs ~init ~f:(fun acc dir ->
          let dir = Filename.concat acc dir in
          mkdir_if_missing ?perm dir ; dir )
    in
    ()


  (* Needed: the type from Core is too different *)
  let caml_create_process = create_process

  let create_process ~prog ~args =
    let args = Array.of_list args in
    create_process prog args stdin stdout stderr


  (* Not exported in Core *)
  exception Fork_returned_negative_result of int

  let fork () =
    let pid = Unix.fork () in
    if pid < 0 then raise (Fork_returned_negative_result pid)
    else if pid = 0 then `In_the_child
    else `In_the_parent pid


  (* Own function *)
  let fork_exec ~prog ~argv ?(use_path = true) ?env () =
    (* create_process searches the path by default, so this should be ok *)
    ignore use_path ;
    let args = Array.of_list argv in
    let env = match env with None -> Unix.environment () | Some env -> conv_env env in
    create_process_env prog args env stdin stdout stderr


  (** The termination status of a process. *)
  module Exit = struct
    type error = [`Exit_non_zero of int] [@@deriving compare]

    type t = (unit, error) Result.t

    let to_string_hum = function
      | Ok () ->
          "exited normally"
      | Error (`Exit_non_zero i) ->
          sprintf "exited with code %d" i
  end

  module Exit_or_signal = struct
    (* TODO: slightly problematic in theory as Signal.t is abstract in core *)
    type error = [Exit.error | `Signal of int] [@@deriving compare]

    type t = (unit, error) Result.t

    let to_string_hum = function
      | (Ok () | Error #Exit.error) as e ->
          Exit.to_string_hum e
      | Error (`Signal s) ->
          sprintf "died after receiving %s (signal number %d)" (Signal.to_string s) s


    let or_error = function
      | Ok _ as ok ->
          ok
      | Error error ->
          (* TODO: need sexp_of_error *)
          Or_error.error "Unix.Exit_or_signal" error (fun _ -> Sexp.Atom "")
  end

  let wrap_status status =
    match status with
    | WEXITED r ->
        if r = 0 then Result.Ok () else Result.Error (`Exit_non_zero r)
    | WSIGNALED r | WSTOPPED r ->
        Result.Error (`Signal r)


  let waitpid pid =
    let _, status = Unix.waitpid [] pid in
    wrap_status status


  (* Taken from Core *)
  type wait_on = [`Any | `My_group | `Group of Pid.t | `Pid of Pid.t]

  (* Taken from Core.Unix.wait_gen *)
  let wait_on_pid : wait_on -> Pid.t = function
    | `Any ->
        -1
    | `Group pid ->
        -pid
    | `My_group ->
        0
    | `Pid pid ->
        (* only case supported under windows *) pid


  let wait (wait_on : wait_on) =
    let pid = wait_on_pid wait_on in
    let pid, status = Unix.waitpid [] pid in
    (pid, wrap_status status)


  let wait_nohang (wait_on : wait_on) =
    let pid = wait_on_pid wait_on in
    let pid, status = Unix.waitpid [Unix.WNOHANG] pid in
    if pid = 0 then None else Some (pid, wrap_status status)


  let system s = wrap_status (system s)

  let nanosleep = sleepf

  module Error = struct
    type t = Unix.error

    let message = Unix.error_message
  end

  let close_process_in ch = wrap_status (close_process_in ch)

  let dup2 ~src ~dst = dup2 src dst

  module Select_fds = struct
    type t = {read: File_descr.t list; write: File_descr.t list; except: File_descr.t list}
  end

  (* Taken from Core, except for the Sexp *)
  let select ?restart ~read ~write ~except ~timeout () =
    improve ?restart
      (fun () ->
        let timeout =
          match timeout with
          | `Never ->
              -1.
          | `Immediately ->
              0.
          | `After span ->
              if Time_ns.Span.( < ) span Time_ns.Span.zero then 0. else Time_ns.Span.to_sec span
        in
        let read, write, except = Unix.select read write except timeout in
        {Select_fds.read; write; except} )
      (fun () -> [("read", ""); ("write", ""); ("except", ""); ("timeout", "")])
end
