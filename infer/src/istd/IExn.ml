(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

let reraise_after ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  let () = f () in
  Caml.Printexc.raise_with_backtrace exn backtrace


let reraise_if ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  if f () then Caml.Printexc.raise_with_backtrace exn backtrace
