(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

open Javalib_pack


let is_suppress_warnings_annotated =
  Inferconfig.suppress_warnings_matcher SourceFile.empty

let suppress_warnings =
  ({ Annot.
     class_name = Annotations.suppress_warnings;
     parameters = ["infer"] },
   true)

(** Translate an annotation. *)
let translate a : Annot.t =
  let class_name = JBasics.cn_name a.JBasics.kind in
  let translate_value_pair (_, value) =
    match value with
    | JBasics.EVArray [JBasics.EVCstString s] ->
        s
    | JBasics.EVCstString s ->
        s
    | _ -> "?" in
  let element_value_pairs = a.JBasics.element_value_pairs in
  { Annot.
    class_name;
    parameters = IList.map translate_value_pair element_value_pairs }


(** Translate an item annotation. *)
let translate_item avlist : Annot.Item.t =
  let trans_vis = function
    | Javalib.RTVisible -> true
    | Javalib.RTInvisible -> false in
  let trans (a, v) = translate a, trans_vis v in
  IList.map trans avlist


(** Translate a method annotation. *)
let translate_method proc_name ann : Annot.Method.t =
  let global_ann = ann.Javalib.ma_global in
  let param_ann = ann.Javalib.ma_parameters in
  let ret_item =
    let base_annotations = translate_item global_ann in
    if is_suppress_warnings_annotated proc_name then
      suppress_warnings :: base_annotations
    else base_annotations in
  let param_items = IList.map translate_item param_ann in
  ret_item, param_items
