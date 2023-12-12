(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Default | Local | Strict

let of_annot annot =
  let open IOption.Let_syntax in
  let* mode = Annot.find_parameter annot ~name:"value" in
  match mode with
  | Annot.Enum {value= "STRICT"} ->
      return Strict
  | Annot.Enum {value= "LOCAL"} -> (
    match Annot.find_parameter annot ~name:"trustOnly" with
    | None | Some (Annot.Annot _) ->
        return Local
    | Some _ ->
        None )
  | _ ->
      None


let extract_mode_from_explicit_class_annotation tenv classname =
  match PatternMatch.type_name_get_annotation tenv (Typ.JavaClass classname) with
  | Some annots -> (
      if Annotations.ia_is_nullsafe_strict annots then Strict
      else
        match Annotations.ia_find_nullsafe annots with
        | Some nullsafe_annot ->
            Option.value (of_annot nullsafe_annot) ~default:Default
        | _ ->
            Default )
  | None ->
      Default


let extract_user_defined_class_name java_class_name =
  (* Anonymous inner classes are not proper classes and can not be annotated. Refer to underlying user class *)
  JavaClassName.get_user_defined_class_if_anonymous_inner java_class_name
  |> Option.value ~default:java_class_name


(** Get the minimal mode that is stricter or equal than both of given modes *)
let intersect mode1 mode2 =
  match (mode1, mode2) with
  | Strict, _ | _, Strict ->
      Strict
  | Local, _ | _, Local ->
      Local
  | Default, Default ->
      Default


let of_class tenv class_name =
  (* The mode of the class is the strictest over this class's mode annotation and its outer classes *)
  let rec of_class_and_outer_classes class_name =
    let curr_class_mode = extract_mode_from_explicit_class_annotation tenv class_name in
    match JavaClassName.get_outer_class_name class_name with
    | Some outer_name ->
        intersect curr_class_mode (of_class_and_outer_classes outer_name)
    | None ->
        curr_class_mode
  in
  let user_defined_class = extract_user_defined_class_name class_name in
  of_class_and_outer_classes user_defined_class


let of_java_procname tenv pname =
  let class_name = Procname.Java.get_class_type_name pname in
  of_class tenv (Typ.Name.Java.get_java_class_name_exn class_name)
