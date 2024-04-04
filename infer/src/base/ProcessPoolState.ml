(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Keep track of whether the current execution is in a child process *)
let in_child = ref None

let update_status = ref (fun _ _ -> ())

let update_heap_words = ref (fun () -> ())

let pid = ref (lazy (Unix.getpid ()))

let reset_pid () = pid := lazy (Unix.getpid ())

let get_pid () = Lazy.force !pid

let has_running_children = ref false
