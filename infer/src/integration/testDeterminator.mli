(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val test_to_run_java : string option -> string option -> string option -> unit

val _test_to_run_clang :
  SourceFile.t -> Procdesc.t Typ.Procname.Hash.t -> string option -> string option -> unit

val emit_tests_to_run : unit -> unit

val _get_relevant_test_to_run : unit -> string list
