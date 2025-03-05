(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val rename : src:string -> dst:string -> unit

val mkdir_p : ?perm:int -> string -> unit

val nanosleep : float -> unit

val readdir_opt : Caml_unix.dir_handle -> string option

val mkdtemp : string -> string

val putenv : key:string -> data:string -> unit

module Env = Unix.Env
module Process_info = Unix.Process_info

val create_process_env : prog:string -> args:string list -> env:Env.t -> Process_info.t

val create_process : prog:string -> args:string list -> Process_info.t

val open_process_in : string -> In_channel.t

module Exit_or_signal = Unix.Exit_or_signal

val close_process_in : In_channel.t -> Exit_or_signal.t

module Pid = Pid

val getpid : unit -> Pid.t

val waitpid : Pid.t -> Exit_or_signal.t

val wait_nohang_any : unit -> (Pid.t * Exit_or_signal.t) option

val fork : unit -> [`In_the_child | `In_the_parent of Pid.t]

val symlink : target:string -> link_name:string -> unit

val unlink : string -> unit

module File_descr = Unix.File_descr

val dup2 : ?close_on_exec:bool -> src:File_descr.t -> dst:File_descr.t -> unit -> unit

val read : ?restart:bool -> pos:int -> len:int -> File_descr.t -> buf:Bytes.t -> int

type file_perm = Caml_unix.file_perm

type open_flag = Caml_unix.open_flag

val openfile : ?perm:file_perm -> mode:open_flag list -> string -> File_descr.t

val close : File_descr.t -> unit
