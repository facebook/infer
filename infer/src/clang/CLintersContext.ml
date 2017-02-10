(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type if_context = {
  within_responds_to_selector_block : string list;
  ios_version_guard : string list
}

type context = {
  translation_unit_context : CFrontend_config.translation_unit_context;
  current_method : Clang_ast_t.decl option;
  in_synchronized_block: bool;
  (** True if the translation unit contains an ObjC class impl that's a subclass
      of CKComponent or CKComponentController. *)
  is_ck_translation_unit: bool;
  (** If inside an objc class impl, contains the objc class impl decl. *)
  current_objc_impl : Clang_ast_t.decl option;
  (** True if inside an objc static factory method (a class-level initializer, like +new) *)
  in_objc_static_factory_method : bool;
  et_evaluation_node : string option;
  if_context : if_context option;
}

let empty translation_unit_context = {
  current_method = None;
  translation_unit_context;
  in_synchronized_block = false;
  is_ck_translation_unit = false;
  current_objc_impl = None;
  in_objc_static_factory_method = false;
  et_evaluation_node = None;
  if_context = None;
}
