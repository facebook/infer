(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val test_to_run_java : string option -> string option -> string option -> unit

val _test_to_run_clang :
  SourceFile.t -> Procdesc.t Typ.Procname.Hash.t -> string option -> string option -> unit

val print_test_to_run : unit -> unit

val _get_relevant_test_to_run : unit -> string list
