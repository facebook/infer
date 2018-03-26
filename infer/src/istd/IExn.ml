(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Core

let reraise_after ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  let () = f () in
  Caml.Printexc.raise_with_backtrace exn backtrace


let reraise_if ~f exn =
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  if f () then Caml.Printexc.raise_with_backtrace exn backtrace
