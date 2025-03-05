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

val putenv : key:string -> data:string -> unit

module Env = Core_unix.Env
module Process_info = Core_unix.Process_info

val create_process_env : prog:string -> args:string list -> env:Env.t -> Process_info.t

val create_process : prog:string -> args:string list -> Process_info.t

module Exit_or_signal = Core_unix.Exit_or_signal

val close_process_in : In_channel.t -> Exit_or_signal.t

module Pid = Pid

val getpid : unit -> Pid.t

val waitpid : Pid.t -> Exit_or_signal.t

val wait_nohang_any : unit -> (Pid.t * Exit_or_signal.t) option

val fork : unit -> [`In_the_child | `In_the_parent of Pid.t]

val symlink : target:string -> link_name:string -> unit

module File_descr = Core_unix.File_descr

val dup2 : ?close_on_exec:bool -> src:File_descr.t -> dst:File_descr.t -> unit -> unit

val read : ?restart:bool -> pos:int -> len:int -> File_descr.t -> buf:Bytes.t -> int

type file_perm = Caml_unix.file_perm

type open_flag = Caml_unix.open_flag

val openfile : ?perm:file_perm -> mode:open_flag list -> string -> File_descr.t

type socket_domain = Caml_unix.socket_domain

type socket_type = Caml_unix.socket_type

val socket :
     ?close_on_exec:bool
  -> domain:socket_domain
  -> kind:socket_type
  -> protocol:int
  -> unit
  -> File_descr.t

type sockaddr = Caml_unix.sockaddr

val bind : File_descr.t -> addr:sockaddr -> unit

val listen : File_descr.t -> backlog:int -> unit

module Select_fds = Core_unix.Select_fds

type select_timeout = Core_unix.select_timeout

val select :
     ?restart:bool (** defaults to [false] *)
  -> read:File_descr.t list
  -> write:File_descr.t list
  -> except:File_descr.t list
  -> timeout:select_timeout
  -> unit
  -> Select_fds.t

val system : string -> Exit_or_signal.t

module Error = Core_unix.Error

type env = Core_unix.env [@@deriving sexp]
