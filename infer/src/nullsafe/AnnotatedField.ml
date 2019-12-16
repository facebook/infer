(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO(T54088319) get rid of annotation_deprecated:
  Introduce "field flags" and move all other usages to this dedicated datatype
  *)
type t = {annotation_deprecated: Annot.Item.t; annotated_type: AnnotatedType.t}

let is_class_in_strict_mode tenv typ =
  match PatternMatch.type_get_annotation tenv typ with
  | Some ia ->
      Annotations.ia_is_nullsafe_strict ia
  | None ->
      false


let rec get_type_name {Typ.desc} =
  match desc with Typ.Tstruct name -> Some name | Typ.Tptr (t, _) -> get_type_name t | _ -> None


(* A heuristic to guess if the field is actually a Java enum value. *)
let is_enum_value tenv ~class_typ (field_info : Typ.Struct.field_info) =
  (* It is tricky to get this information with 100% precision, but this works in most of
     practical cases.
     In Java, enums are special classes, and enum values are (implicitly generated) static fields in these classes,
     which are initialized statically via calling the class constructor.
     Though there can be OTHER (user-defined) static fields in the same enum class,
     this is a rare case.*)
  if not field_info.is_static then false
  else
    match (get_type_name class_typ, get_type_name field_info.typ) with
    (* enums values are fields which type is the same as the type of the enum class *)
    | Some class_name, Some field_type_name
      when Typ.equal_name class_name field_type_name && PatternMatch.is_java_enum tenv class_name ->
        true
    (* Could not fetch one of the class names, or they are different. Should not happen for enum values. *)
    | _ ->
        false


let get tenv field_name class_typ =
  let lookup = Tenv.lookup tenv in
  (* We currently don't support field-level strict mode annotation, so fetch it from class *)
  let is_strict_mode = is_class_in_strict_mode tenv class_typ in
  Typ.Struct.get_field_info ~lookup field_name class_typ
  |> Option.map ~f:(fun (Typ.Struct.{typ= field_typ; annotations} as field_info) ->
         let is_enum_value = is_enum_value tenv ~class_typ field_info in
         let nullability =
           AnnotatedNullability.of_type_and_annotation field_typ annotations ~is_strict_mode
         in
         let corrected_nullability =
           match nullability with
           | AnnotatedNullability.DeclaredNonnull _ when is_enum_value ->
               (* Enum values are the special case - they can not be null. So we can strengten nullability.
                  Note that if it is nullable, we do NOT change nullability: in this case this is probably
                  not an enum value, but just a static field annotated as nullable.
               *)
               AnnotatedNullability.Nonnull EnumValue
           | _ ->
               nullability
         in
         let annotated_type = AnnotatedType.{nullability= corrected_nullability; typ= field_typ} in
         {annotation_deprecated= annotations; annotated_type} )
