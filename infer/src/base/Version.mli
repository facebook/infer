(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val major : int

val minor : int

val patch : int

val commit : string

val versionString : string

val versionJson : string

val clang_enabled : bool

val java_enabled : bool

val xcode_enabled : bool

val man_pages_last_modify_date : string
