(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run_clang : ClangCommand.t -> (In_channel.t -> 'a) -> 'a

val capture : ClangCommand.t -> unit

val al_callback_ref : (CFrontend_config.translation_unit_context -> Clang_ast_t.decl -> unit) ref
(** callback set by AL to avoid circular dependencies between clang/ and al/ without having to
    expose the clang-only types involved all the way to integration/ *)
