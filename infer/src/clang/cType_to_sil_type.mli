(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_builtin_objc_typename : [< `ObjCClass | `ObjCId] -> Typ.Name.t

val type_of_builtin_type_kind : ?is_const:bool -> Clang_ast_t.builtin_type_kind -> Typ.t

val qual_type_to_sil_type :
  (Tenv.t -> Clang_ast_t.decl -> Typ.desc) -> Tenv.t -> Clang_ast_t.qual_type -> Typ.t
