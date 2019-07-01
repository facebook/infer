(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Stop analysis when encountering issues *)

let on_unknown_call _ = [%Trace.kprintf (fun _ -> assert false) ""]
let on_invalid_access _ = [%Trace.kprintf (fun _ -> assert false) ""]
