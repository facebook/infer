(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val parse_al_file : string -> In_channel.t -> CTL.al_file option

val validate_al_files : unit -> (unit, string) Result.t
