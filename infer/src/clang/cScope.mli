(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val breaks_control_flow : Clang_ast_t.stmt -> bool

module Variables : sig
  val compute_vars_to_destroy_map :
    CContext.t -> Clang_ast_t.stmt -> CContext.var_to_destroy list ClangPointers.Map.t
end

module CXXTemporaries : sig
  val get_destroyable_temporaries :
    CContext.t -> Clang_ast_t.stmt list -> CContext.CXXTemporarySet.t
end
