(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type var_to_destroy =
  { pvar: Pvar.t
  ; typ: Typ.t
  ; qual_type: Clang_ast_t.qual_type
  ; marker: Pvar.t option
        (** [Some m] means that creating [pvar] should also set [m] to [1] so that we know whether
            [pvar] needs to be destroyed after the current full-expression *) }

val breaks_control_flow : Clang_ast_t.stmt -> bool

module Variables : sig
  val compute_vars_to_destroy_map : Clang_ast_t.stmt -> Clang_ast_t.decl list ClangPointers.Map.t
end

module CXXTemporaries : sig
  val get_destroyable_temporaries : CContext.t -> Clang_ast_t.stmt list -> var_to_destroy list
end
