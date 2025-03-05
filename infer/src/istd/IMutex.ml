(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Stdlib.Mutex.t

let create () = Stdlib.Mutex.create ()

let critical_section l ~f =
  Stdlib.Mutex.lock l ;
  Exn.protect ~f ~finally:(fun () -> Stdlib.Mutex.unlock l)
