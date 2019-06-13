(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type error = ExternalError | InternalError | UserError

exception InferExternalError of string

exception InferInternalError of string

exception InferUserError of string

exception InferExit of int

let raise_error ?backtrace error ~msg =
  let do_raise exn =
    match backtrace with
    | None ->
        raise exn
    | Some backtrace ->
        Caml.Printexc.raise_with_backtrace exn backtrace
  in
  match error with
  | ExternalError ->
      do_raise (InferExternalError msg)
  | InternalError ->
      do_raise (InferInternalError msg)
  | UserError ->
      do_raise (InferUserError msg)


let log_uncaught_exception_callback_ref = ref (fun _ ~exitcode:_ -> ())

let set_log_uncaught_exception_callback fn = log_uncaught_exception_callback_ref := fn

let log_uncaught_exception exn ~exitcode = !log_uncaught_exception_callback_ref exn ~exitcode

let die error fmt = F.kasprintf (fun msg -> raise_error error ~msg) fmt

let exit exitcode = raise (InferExit exitcode)

let exit_code_of_exception = function
  | InferUserError _ ->
      1
  | InferExternalError _ ->
      3
  | InferInternalError _ ->
      4
  | InferExit exitcode ->
      exitcode
  | _ ->
      (* exit code 2 is used by the OCaml runtime in cases of uncaught exceptions *) 2


type style = Error | Fatal | Normal | Warning

let term_styles_of_style = function
  | Error ->
      ANSITerminal.[Foreground Red]
  | Fatal ->
      ANSITerminal.[Bold; Foreground Red]
  | Normal ->
      [ANSITerminal.default]
  | Warning ->
      ANSITerminal.[Foreground Yellow]
