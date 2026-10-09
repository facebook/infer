(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Translate an enumeration declaration by adding it to the tenv and *)

(** translating the code and adding it to a fake procdesc *)

val enum_decl : Clang_ast_t.decl -> Typ.desc

val add_enum_of_constant : Clang_ast_t.pointer -> unit
(** add the constants of the enum that declares the given enum constant to the map, if needed *)
