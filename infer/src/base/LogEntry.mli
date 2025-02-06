(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Log entry data model. *)

type count_entry_data = {value: int}

type time_entry_data = {duration_us: int}

type string_data = {message: string}

type entry_data = Count of count_entry_data | Time of time_entry_data | String of string_data

(** created_at_ts is a unix timestamp (in seconds) *)
type t = {label: string; created_at_ts: int; data: entry_data}

val mk_count : label:string -> value:int -> t

val mk_time : label:string -> duration_us:int -> t

val mk_string : label:string -> message:string -> t
