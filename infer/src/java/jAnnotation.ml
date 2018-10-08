(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

(** Translate an annotation. *)
let translate a : Annot.t =
  let class_name = JBasics.cn_name a.JBasics.kind in
  let rec translate_value_pair acc (x, value) =
    match value with
    | JBasics.EVArray (JBasics.EVCstString s :: l) ->
        translate_value_pair (s :: acc) (x, JBasics.EVArray l)
    | JBasics.EVCstString s ->
        s :: acc
    | JBasics.EVCstBoolean 0 ->
        (* just translate bools as strings. means we can't distinguish between a boolean false
           literal parameter and string literal "false" parameter, but that's ok. *)
        "false" :: acc
    | JBasics.EVCstBoolean 1 ->
        "true" :: acc
    | _ ->
        acc
  in
  let parameters =
    List.fold ~f:translate_value_pair ~init:[] a.JBasics.element_value_pairs |> List.rev
  in
  {Annot.class_name; parameters}


(** Translate an item annotation. *)
let translate_item avlist : Annot.Item.t =
  let trans_vis = function Javalib.RTVisible -> true | Javalib.RTInvisible -> false in
  let trans (a, v) = (translate a, trans_vis v) in
  List.map ~f:trans avlist


(** Translate a method annotation. *)
let translate_method ann : Annot.Method.t =
  let global_ann = ann.Javalib.ma_global in
  let param_ann = ann.Javalib.ma_parameters in
  let return = translate_item global_ann in
  let params = List.map ~f:translate_item param_ann in
  {return; params}
