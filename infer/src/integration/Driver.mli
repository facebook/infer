(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  | AnalyzeJson
  | Ant of {prog: string; args: string list}
  | BuckClangFlavor of {build_cmd: string list}
  | BuckCompilationDB of {deps: BuckMode.clang_compilation_db_deps; prog: string; args: string list}
  | BuckGenrule of {prog: string}
  | BuckJavaFlavor of {build_cmd: string list}
  | Clang of {compiler: Clang.compiler; prog: string; args: string list}
  | ClangCompilationDB of {db_files: [`Escaped of string | `Raw of string] list}
  | Gradle of {prog: string; args: string list}
  | Javac of {compiler: Javac.compiler; prog: string; args: string list}
  | Maven of {prog: string; args: string list}
  | NdkBuild of {build_cmd: string list}
  | XcodeBuild of {prog: string; args: string list}
  | XcodeXcpretty of {prog: string; args: string list}

val is_analyze_mode : mode -> bool

val mode_from_command_line : mode Lazy.t
(** driver mode computed from the command-line arguments and settings in Config *)

val run_prologue : mode -> unit
(** prepare the environment for running the given mode *)

val capture : changed_files:SourceFile.Set.t option -> mode -> unit
(** run the capture for the given mode *)

val analyze_and_report :
  ?suppress_console_report:bool -> changed_files:SourceFile.Set.t option -> mode -> unit
(** run the analysis for the given mode *)

val run_epilogue : unit -> unit
(** cleanup infer-out/ for Buck, generate stats, and generally post-process the results of a run *)

val read_config_changed_files : unit -> SourceFile.Set.t option
(** return the list of changed files as read from Config.changed_files_index and passed to
    SourceFile.changed_sources_from_changed_files *)
