(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val do_frontend_checks : CFrontend_config.translation_unit_context -> Clang_ast_t.decl -> unit
