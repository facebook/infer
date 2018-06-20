(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Keep track of whether the current execution is in a child process *)
let in_child = ref false

let update_status = ref (fun _ _ -> ())
