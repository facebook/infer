(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** entry points for top-level functionalities such as capture under various build systems,
    analysis, and reporting *)

type build_system

(** based on the build_system and options passed to infer, we run in different driver modes *)
type mode =
  | Analyze
  | BuckGenrule of string
  | BuckCompilationDB of string * string list
  | Clang of Clang.compiler * string * string list
  | ClangCompilationDB of [`Escaped of string | `Raw of string] list
  | Javac of Javac.compiler * string * string list
  | Maven of string * string list
  | PythonCapture of build_system * string list
  | XcodeXcpretty of string * string list
  [@@deriving compare]

val equal_driver_mode : mode -> mode -> bool

val mode_from_command_line : mode Lazy.t
(** driver mode computed from the command-line arguments and settings in Config *)

val run_prologue : mode -> unit
(** prepare the environment for running the given mode *)

val capture : changed_files:SourceFile.Set.t option -> mode -> unit
(** run the capture for the given mode *)

val analyze_and_report : changed_files:SourceFile.Set.t option -> mode -> unit
(** run the analysis for the given mode *)

val run_epilogue : mode -> unit
(** cleanup infer-out/ for Buck, generate stats, and generally post-process the results of a run *)
