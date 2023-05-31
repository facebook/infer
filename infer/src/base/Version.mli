(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val major : int

val minor : int

val patch : int

val commit : string

type build_platform = Linux | Darwin | Windows

val build_platform : build_platform

val versionString : string

val versionJson : string

val clang_enabled : bool

val erlang_enabled : bool

val hack_enabled : bool

val java_enabled : bool

val java_version : int option

val xcode_enabled : bool

val man_pages_last_modify_date : string

val python_exe : string [@@warning "-unused-value-declaration"] (* used in unit tests *)

val python_enabled : bool
