(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** entry points for top-level functionalities such as capture under various build systems,
    analysis, and reporting *)

(** based on the build_system and options passed to infer, we run in different driver modes *)
type mode =
  | Analyze
  | BuckGenrule of string
  | BuckCompilationDB of string * string list
  | Clang of Clang.compiler * string * string list
  | ClangCompilationDB of [`Escaped of string | `Raw of string] list
  | Javac of Javac.compiler * string * string list
  | Maven of string * string list
  | PythonCapture of Config.build_system * string list
  | XcodeXcpretty of string * string list
[@@deriving compare]

val equal_mode : mode -> mode -> bool

val mode_from_command_line : mode Lazy.t
(** driver mode computed from the command-line arguments and settings in Config *)

val mode_of_build_command : string list -> mode
(** driver mode computed from the build command alone, eg [["buck"; "build"; ...]] gives [PythonCapture (BBuck, ["buck"; "build"; ...])] *)

val run_prologue : mode -> unit
(** prepare the environment for running the given mode *)

val capture : changed_files:SourceFile.Set.t option -> mode -> unit
(** run the capture for the given mode *)

val analyze_and_report :
  ?suppress_console_report:bool -> changed_files:SourceFile.Set.t option -> mode -> unit
(** run the analysis for the given mode *)

val run_epilogue : mode -> unit
(** cleanup infer-out/ for Buck, generate stats, and generally post-process the results of a run *)

val read_config_changed_files : unit -> SourceFile.Set.t option
(** return the list of changed files as read from Config.changed_files_index and passed to SourceFile.changed_sources_from_changed_files *)
