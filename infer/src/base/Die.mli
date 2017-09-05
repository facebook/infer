(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* WARNING: ONLY USE IF Logging IS NOT AVAILABLE TO YOU FOR SOME REASON (e.g., inside Config). *)

exception InferExternalError of string

exception InferInternalError of string

exception InferUserError of string

(** kind of error for [die], with similar semantics as [Logging.{external,internal,user}_error] *)
type error = ExternalError | InternalError | UserError

val exit_code_of_exception : Exn.t -> int

val die : error -> ('a, Format.formatter, unit, _) format4 -> 'a
(** Raise the corresponding exception. *)

val raise_error : error -> msg:string -> 'a
