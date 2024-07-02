(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let early_log = ref (Some [])

let get_early_log () =
  match !early_log with
  | None ->
      Die.die InternalError "early logs have been [finish()]d but are being accessed"
  | Some log ->
      log


let log_message ~label ~message =
  early_log := Some (LogEntry.mk_string ~label ~message :: get_early_log ())


let finish () =
  let log = List.rev (get_early_log ()) in
  early_log := None ;
  log
