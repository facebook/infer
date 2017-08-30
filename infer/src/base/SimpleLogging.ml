(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format

type error = ExternalError | InternalError | UserError

exception InferExternalError of string * string

exception InferInternalError of string * string

exception InferUserError of string * string

let raise_error error msg backtrace =
  match error with
  | ExternalError
   -> raise (InferExternalError (msg, backtrace))
  | InternalError
   -> raise (InferInternalError (msg, backtrace))
  | UserError
   -> raise (InferUserError (msg, backtrace))

let die error msg =
  let backtrace = Exn.backtrace () in
  F.kasprintf (fun s -> raise_error error s backtrace) msg

let exit_code_of_exception = function
  | InferUserError _
   -> 1
  | InferExternalError _
   -> 3
  | InferInternalError _
   -> 4
  | _
   -> (* exit code 2 is used by the OCaml runtime in cases of uncaught exceptions *) 2
