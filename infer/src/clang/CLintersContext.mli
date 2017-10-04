(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type if_context =
  { within_responds_to_selector_block: string list
  ; within_available_class_block: string list
  ; ios_version_guard: string list }

type context =
  { translation_unit_context: CFrontend_config.translation_unit_context
  ; current_method: Clang_ast_t.decl option
  ; parent_methods: Clang_ast_t.decl list
  ; in_synchronized_block: bool
        (** True if the translation unit contains an ObjC class impl that's a subclass
      of CKComponent or CKComponentController. *)
  ; is_ck_translation_unit: bool
        (** If inside an objc class, contains the objc class (impl or interface) decl. *)
  ; current_objc_class: Clang_ast_t.decl option
  ; et_evaluation_node: string option
  ; if_context: if_context option
  ; in_for_loop_declaration: bool }

val empty : CFrontend_config.translation_unit_context -> context

val update_current_method : context -> Clang_ast_t.decl -> context
