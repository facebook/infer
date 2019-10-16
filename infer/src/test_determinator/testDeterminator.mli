(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val compute_and_emit_test_to_run :
     ?clang_range_map:(Location.t * Location.t) Typ.Procname.Map.t
  -> ?source_file:SourceFile.t
  -> unit
  -> unit

val compute_and_emit_relevant_methods :
  clang_range_map:(Location.t * Location.t) Typ.Procname.Map.t -> source_file:SourceFile.t -> unit
