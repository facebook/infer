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

val pp_clang_compilation_db_deps : F.formatter -> clang_compilation_db_deps -> unit

type t =
  | CombinedGenrule
  | ClangFlavors
  | ClangCompilationDB of clang_compilation_db_deps
  | JavaFlavor
  | JavaGenruleMaster

val is_java_genrule_master_or_combined : t -> bool

val is_clang_compilation_db : t -> bool

val is_clang_flavors : t -> bool
