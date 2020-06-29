(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val contains_ck_impl : Clang_ast_t.decl list -> bool
(** Returns true if the passed-in list of decls contains an ObjCImplementationDecl of a descendant
    of CKComponent or CKComponentController.

    Does not recurse into hierarchy. *)

val mutable_local_vars_advice :
  CLintersContext.context -> Ctl_parser_types.ast_node -> CIssue.t option

val component_with_multiple_factory_methods_advice :
  CLintersContext.context -> Ctl_parser_types.ast_node -> CIssue.t list
