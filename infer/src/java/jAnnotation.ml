(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

let translate_basic_type = function
  | `Bool ->
      StdTyp.boolean
  | `Byte ->
      StdTyp.Java.byte
  | `Char ->
      StdTyp.Java.char
  | `Double ->
      StdTyp.double
  | `Float ->
      StdTyp.float
  | `Int ->
      StdTyp.int
  | `Long ->
      StdTyp.long
  | `Short ->
      StdTyp.Java.short


let rec translate_value_type = function
  | JBasics.TBasic basic ->
      translate_basic_type basic
  | JBasics.TObject obj ->
      translate_object_type obj


and translate_object_type = function
  | JBasics.TClass cn ->
      Typ.mk_struct (Typ.Name.Java.from_string (JBasics.cn_name cn))
  | JBasics.TArray vt ->
      Typ.mk_array (translate_value_type vt)


let rec translate_value_exn = function
  | JBasics.EVCstString s ->
      Annot.Str s
  | JBasics.EVCstBoolean 0 ->
      Annot.Bool false
  | JBasics.EVCstBoolean 1 ->
      Annot.Bool true
  | JBasics.EVEnum (cn, value) ->
      Annot.Enum {class_typ= Typ.mk_struct (Typ.Name.Java.from_string (JBasics.cn_name cn)); value}
  | JBasics.EVArray values ->
      Annot.Array (List.map values ~f:translate_value |> List.filter_opt)
  | JBasics.EVClass (Some typ) ->
      Annot.Class (translate_value_type typ)
  | JBasics.EVClass _ ->
      Annot.Class StdTyp.void
  | JBasics.EVAnnotation ann ->
      Annot.Annot (translate ann)
  | _ ->
      raise (Invalid_argument "Annotation value not supported")


and translate_value element_value =
  match translate_value_exn element_value with
  | value ->
      Some value
  | exception Invalid_argument _ ->
      None


(** Translate an annotation. *)
and translate a : Annot.t =
  let class_name = JBasics.cn_name a.JBasics.kind in
  let translate_value_pair acc (x, value) =
    match translate_value value with
    | Some translated ->
        Annot.{name= Some x; value= translated} :: acc
    | _ ->
        acc
  in
  let parameters =
    List.fold ~f:translate_value_pair ~init:[] a.JBasics.element_value_pairs |> List.rev
  in
  {Annot.class_name; parameters}


(** Translate an item annotation. *)
let translate_item avlist : Annot.Item.t =
  let trans (a, _) = translate a in
  List.map ~f:trans avlist


(** Translate annotations for a method. *)
let translate_method ann : Annot.Item.t * Annot.Item.t list =
  let global_ann = ann.Javalib.ma_global in
  let param_ann = ann.Javalib.ma_parameters in
  let ret_annots = translate_item global_ann in
  let params = List.map ~f:translate_item param_ann in
  (ret_annots, params)
