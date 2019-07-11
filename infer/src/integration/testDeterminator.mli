(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val test_to_run_java : string option -> string option -> string option -> unit

val test_to_run_clang :
     SourceFile.t
  -> process_ast_fn:((Typ.Procname.t -> Location.t * Location.t -> unit) -> unit)
  -> changed_lines_file:string option
  -> test_samples_file:string option
  -> unit

val emit_tests_to_run : unit -> unit

val emit_relevant_methods : unit -> unit

val _get_relevant_test_to_run : unit -> string list
