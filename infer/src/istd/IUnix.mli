(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Pid = Pid

module Process_info : sig
  type t =
    { pid: Pid.t
    ; stdin: Caml_unix.file_descr
    ; stdout: Caml_unix.file_descr
    ; stderr: Caml_unix.file_descr }
end

module Env : sig
  type t =
    [ `Replace of (string * string) list
    | `Extend of (string * string) list
    | `Override of (string * string option) list
    | `Replace_raw of string list ]
  [@@deriving sexp]
end

module Exit_or_signal : sig
  type error = [`Exit_non_zero of int | `Signal of int] [@@deriving compare, sexp]

  type t = (unit, error) Result.t [@@deriving compare, sexp]

  val to_string_hum : t -> string
end

module Select_fds : sig
  type t =
    { read: Caml_unix.file_descr list
    ; write: Caml_unix.file_descr list
    ; except: Caml_unix.file_descr list }
end

module Error : sig
  type t = Caml_unix.error

  val message : t -> string
end

type select_timeout = [`Never | `Immediately | `After of Time_ns.Span.t]

val rename : src:string -> dst:string -> unit

val mkdir_p : ?perm:int -> string -> unit

val nanosleep : float -> unit

val putenv : key:string -> data:string -> unit

val create_process_env : prog:string -> args:string list -> env:Env.t -> Process_info.t

val create_process : prog:string -> args:string list -> Process_info.t

val close_process_in : In_channel.t -> Exit_or_signal.t

val getpid : unit -> Pid.t

val waitpid : Pid.t -> Exit_or_signal.t

val wait_nohang_any : unit -> (Pid.t * Exit_or_signal.t) option

val fork : unit -> [`In_the_child | `In_the_parent of Pid.t]

val symlink : target:string -> link_name:string -> unit

val dup2 :
  ?close_on_exec:bool -> src:Caml_unix.file_descr -> dst:Caml_unix.file_descr -> unit -> unit

val read : ?restart:bool -> pos:int -> len:int -> Caml_unix.file_descr -> buf:Bytes.t -> int

val openfile :
  ?perm:Caml_unix.file_perm -> mode:Caml_unix.open_flag list -> string -> Caml_unix.file_descr

val socket :
     ?close_on_exec:bool
  -> domain:Caml_unix.socket_domain
  -> kind:Caml_unix.socket_type
  -> protocol:int
  -> unit
  -> Caml_unix.file_descr

val bind : Caml_unix.file_descr -> addr:Caml_unix.sockaddr -> unit

val listen : Caml_unix.file_descr -> backlog:int -> unit

val select :
     ?restart:bool (** defaults to [false] *)
  -> read:Caml_unix.file_descr list
  -> write:Caml_unix.file_descr list
  -> except:Caml_unix.file_descr list
  -> timeout:select_timeout
  -> unit
  -> Select_fds.t

val system : string -> Exit_or_signal.t
