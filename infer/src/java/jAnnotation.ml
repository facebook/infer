(*
* Copyright (c) 2009 -2013 Monoidics ltd.
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

open Javalib_pack
open Utils

(** Translate an annotation. *)
let translate a : Sil.annotation =
  let class_name = JBasics.cn_name a.JBasics.kind in
  let translate_value_pair (name, value) =
    match value with
    | JBasics.EVArray [JBasics.EVCstString s] ->
        s
    | JBasics.EVCstString s ->
        s
    | _ -> "?" in
  let element_value_pairs = a.JBasics.element_value_pairs in
  { Sil.class_name = class_name;
    Sil.parameters = list_map translate_value_pair element_value_pairs }


(** Translate an item annotation. *)
let translate_item avlist : Sil.item_annotation =
  let trans_vis = function
    | Javalib.RTVisible -> true
    | Javalib.RTInvisible -> false in
  let trans (a, v) = translate a, trans_vis v in
  list_map trans avlist


(** Translate a method annotation. *)
let translate_method ann : Sil.method_annotation =
  let global_ann = ann.Javalib.ma_global in
  let param_ann = ann.Javalib.ma_parameters in
  let ret_item = translate_item global_ann in
  let param_items = list_map translate_item param_ann in
  ret_item, param_items
