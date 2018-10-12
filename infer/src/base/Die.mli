(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception
  (* WARNING: ONLY USE IF Logging IS NOT AVAILABLE TO YOU FOR SOME REASON (e.g., inside Config). *)
    InferExternalError of
    string

exception InferInternalError of string

exception InferUserError of string

(** This can be used to avoid scattering exit invocations all over the codebase *)
exception InferExit of int

(** kind of error for [die], with similar semantics as [Logging.{external,internal,user}_error] *)
type error = ExternalError | InternalError | UserError

val exit : int -> 'a

val exit_code_of_exception : Exn.t -> int

val set_log_uncaught_exception_callback : (exn -> exitcode:int -> unit) -> unit

val log_uncaught_exception : exn -> exitcode:int -> unit

val die : error -> ('a, Format.formatter, unit, _) format4 -> 'a
(** Raise the corresponding exception. *)

val raise_error : ?backtrace:Caml.Printexc.raw_backtrace -> error -> msg:string -> 'a

type style = Error | Fatal | Normal | Warning

val term_styles_of_style : style -> ANSITerminal.style list
