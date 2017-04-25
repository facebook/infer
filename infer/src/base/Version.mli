(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val commit : string
val versionString : string
val versionJson : string

val clang_enabled : bool
val java_enabled : bool
val xcode_enabled : bool

val man_pages_last_modify_date : string
