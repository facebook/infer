(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type machine_readable_version = float
type human_readable_version = string
type t = machine_readable_version * human_readable_version

val version_of : string -> human_readable_version option

val pp_diff_of_version_opt : Format.formatter ->
  (human_readable_version option * human_readable_version option) -> unit
