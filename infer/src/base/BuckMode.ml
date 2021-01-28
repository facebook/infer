(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** dependencies to include in the compilation database *)
type clang_compilation_db_deps = NoDependencies | DepsUpToDepth of int | DepsAllDepths

let pp_clang_compilation_db_deps fmt = function
  | NoDependencies ->
      F.pp_print_string fmt "NoDependencies"
  | DepsUpToDepth depth ->
      F.fprintf fmt "DepsUpToDepth %d" depth
  | DepsAllDepths ->
      F.pp_print_string fmt "DepsAllDepths"


type t = ClangFlavors | ClangCompilationDB of clang_compilation_db_deps | JavaFlavor

let is_clang_compilation_db = function
  | ClangCompilationDB _ ->
      true
  | ClangFlavors | JavaFlavor ->
      false


let is_clang_flavors = function ClangFlavors -> true | ClangCompilationDB _ | JavaFlavor -> false
