(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  ; is_ck_translation_unit: bool
        (** True if the translation unit contains an ObjC class impl that's a subclass
            of CKComponent or CKComponentController. *)
  ; current_objc_class: Clang_ast_t.decl option
        (** If inside an objc class, contains the objc class (impl or interface) decl. *)
  ; current_objc_category: Clang_ast_t.decl option
        (** If inside an objc category, contains the objc category (impl or interface) decl. *)
  ; current_objc_protocol: Clang_ast_t.decl option
        (** If inside an objc protocol, contains the objc protocol decl. *)
  ; et_evaluation_node: string option
  ; if_context: if_context option
  ; in_for_loop_declaration: bool }

let empty translation_unit_context =
  { current_method= None
  ; parent_methods= []
  ; translation_unit_context
  ; in_synchronized_block= false
  ; is_ck_translation_unit= false
  ; current_objc_class= None
  ; current_objc_category= None
  ; current_objc_protocol= None
  ; et_evaluation_node= None
  ; if_context= None
  ; in_for_loop_declaration= false }


let add_parent_method decl_opt parent_methods =
  match decl_opt with Some decl -> decl :: parent_methods | None -> parent_methods


let update_current_method context decl =
  { context with
    current_method= Some decl
  ; parent_methods= add_parent_method context.current_method context.parent_methods }
