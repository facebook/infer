(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Stop analysis when encountering issues *)

exception Stop
exception Reached_goal

let on_unknown_call _ = Trace.kprintf __FUNCTION__ (fun _ -> raise Stop) ""
let on_alarm _ = Trace.kprintf __FUNCTION__ (fun _ -> raise Stop) ""

let on_reached_goal _ =
  Trace.kprintf __FUNCTION__ (fun _ -> raise Reached_goal) ""
