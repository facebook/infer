(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type human_readable_version = string

val version_of : string -> human_readable_version option

val pp_diff_of_version_opt :
  Format.formatter -> human_readable_version option * human_readable_version option -> unit
