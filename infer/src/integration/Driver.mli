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
  | Ant of {prog: string; args: string list}
  | BuckErlang of {prog: string; args: string list}
  | BuckGenrule of {prog: string}
  | BxlClang of {build_cmd: string list}
  | BxlJava of {build_cmd: string list}
  | BxlPython of {build_cmd: string list}
  | Clang of {compiler: Clang.compiler; prog: string; args: string list}
  | ClangCompilationDB of {db_files: [`Escaped of string | `Raw of string] list}
  | Erlc of {args: string list}
  | Gradle of {prog: string; args: string list}
  | Hackc of {prog: string; args: string list}
  | Javac of {compiler: Javac.compiler; prog: string; args: string list}
  | JsonSIL of {cfg_json: string; tenv_json: string}
  | Kotlinc of {prog: string; args: string list}
  | Llair of {source_file: string; llair_file: string}
  | Maven of {prog: string; args: string list}
  | NdkBuild of {build_cmd: string list}
  | Python of {prog: string; args: string list}
  | PythonBytecode of {files: string list}
  | Rebar3 of {args: string list}
  | Rust of {prog: string; args : string list}
  | Swiftc of {prog: string; args: string list}
  | Textual of {textualfiles: string list}
  | XcodeBuild of {prog: string; args: string list}
  | XcodeXcpretty of {prog: string; args: string list}

val is_analyze_mode : mode -> bool

val mode_from_command_line : mode Lazy.t
(** driver mode computed from the command-line arguments and settings in Config *)

val report : unit -> unit

val run : mode -> unit
