(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Stop analysis when encountering issues *)

exception Stop
exception Reached_goal of {steps: int}
exception Unreachable_goal
exception Unimplemented of {feature: string}

let on_unknown_call _ = Dbg.kprintf __FUNCTION__ (fun _ -> raise Stop) ""
let on_abort _ = Dbg.kprintf __FUNCTION__ (fun _ -> raise Stop) ""

let on_invalid_memory_access _ =
  Dbg.kprintf __FUNCTION__ (fun _ -> raise Stop) ""

let on_alarm a =
  ( match Alarm.kind a with
  | Alarm.Abort -> on_abort ()
  | Alarm.Invalid_memory_access -> on_invalid_memory_access () ) ;
  Dbg.kprintf __FUNCTION__ (fun _ -> raise Stop) ""

let on_reached_goal steps _ =
  Dbg.kprintf __FUNCTION__ (fun _ -> raise (Reached_goal {steps})) ""

let on_unreachable_goal ~dp_path =
  [%Dbg.printf "%t" dp_path] ;
  Dbg.kprintf __FUNCTION__ (fun _ -> raise Unreachable_goal) ""

let on_unimplemented feature _ =
  Dbg.kprintf __FUNCTION__ (fun _ -> raise (Unimplemented {feature})) ""
