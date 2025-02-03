(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type worker_id = Pid of Pid.t | Domain of int

(** Keep track of whether the current execution is in a child process *)
let in_child = DLS.new_key (fun () -> None)

let get_in_child () = DLS.get in_child

let set_in_child id_opt = DLS.set in_child id_opt

let update_status = ref (fun _ _ -> ())

let update_heap_words = ref (fun () -> ())

let pid = ref (lazy (Unix.getpid ()))

let reset_pid () = pid := lazy (Unix.getpid ())

let get_pid () = Lazy.force !pid
