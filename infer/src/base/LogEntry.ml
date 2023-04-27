(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type count_entry_data = {value: int}

type time_entry_data = {duration_us: int}

type string_data = {message: string}

type entry_data = Count of count_entry_data | Time of time_entry_data | String of string_data

type t = {label: string; created_at_ts: int; data: entry_data}

let mk_count ~label ~value =
  let created_at_ts = Unix.time () |> int_of_float in
  let data = Count {value} in
  {label; created_at_ts; data}


let mk_time ~label ~duration_us =
  let created_at_ts = Unix.time () |> int_of_float in
  let data = Time {duration_us} in
  {label; created_at_ts; data}


let mk_string ~label ~message =
  let created_at_ts = Unix.time () |> int_of_float in
  let data = String {message} in
  {label; created_at_ts; data}


(** What _global_ mean at this point is subject to discussion. Right now there is only one use-case
    which is Scuba+Scribe logging at the end of execution. But there might be more. Let's change the
    naming accordingly when the purpose gets clearer. *)
let global_entry_log : t list ref = ref []

let global_log_get () = List.rev !global_entry_log

let global_log_erase () = global_entry_log := []

let global_log_add entry = global_entry_log := entry :: !global_entry_log
