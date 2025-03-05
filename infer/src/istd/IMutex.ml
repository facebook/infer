(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Caml_threads.Mutex.t

let create () = Caml_threads.Mutex.create ()

let critical_section l ~f =
  Caml_threads.Mutex.lock l ;
  Exn.protect ~f ~finally:(fun () -> Caml_threads.Mutex.unlock l)
