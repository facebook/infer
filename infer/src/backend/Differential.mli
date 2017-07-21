(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = {introduced: Jsonbug_t.report; fixed: Jsonbug_t.report; preexisting: Jsonbug_t.report}

val of_reports : current_report:Jsonbug_t.report -> previous_report:Jsonbug_t.report -> t

val to_files : t -> string -> unit
