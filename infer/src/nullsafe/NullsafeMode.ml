(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Trust = struct
  type trust_list = JavaClassName.Set.t [@@deriving compare, equal]

  type t = All | Only of trust_list [@@deriving compare, equal]

  let is_trust_none trust_list = JavaClassName.Set.is_empty trust_list

  let none = Only JavaClassName.Set.empty

  let extract_trust_list = function
    | Annot.Array class_values ->
        (* The only elements of this array can be class names; therefore short-circuit and return None if it's not the case. *)
        let trust_list =
          IList.traverse_opt class_values ~f:(fun el ->
              match el with
              | Annot.Class class_typ ->
                  Typ.name class_typ
                  |> Option.map ~f:(fun name -> Typ.Name.Java.get_java_class_name_exn name)
              | _ ->
                  None )
        in
        Option.map trust_list ~f:JavaClassName.Set.of_list
    | _ ->
        None


  let of_annot annot =
    let open IOption.Let_syntax in
    let trust_all = Annot.find_parameter annot ~name:"trustAll" in
    let* trust_list = Annot.find_parameter annot ~name:"value" in
    let* trust_classes = extract_trust_list trust_list in
    match trust_all with
    | None ->
        return (Only trust_classes)
    | Some (Annot.Bool trust_all') ->
        if trust_all' then return All else return (Only trust_classes)
    | _ ->
        None


  let is_in_trust_list t name =
    match t with
    | All ->
        (* We are interested only in explicit lists *)
        false
    | Only classes ->
        JavaClassName.Set.exists (JavaClassName.equal name) classes


  let is_stricter ~stricter ~weaker =
    let is_stricter_trust_list stricter_set weaker_set =
      (* stricter trust list should be a strict subset of the weaker one *)
      JavaClassName.Set.cardinal stricter_set < JavaClassName.Set.cardinal weaker_set
      && JavaClassName.Set.subset stricter_set weaker_set
    in
    match (stricter, weaker) with
    | All, All | All, Only _ ->
        false
    | Only _, All ->
        true
    | Only stricter_trust_list, Only weaker_trust_list ->
        is_stricter_trust_list stricter_trust_list weaker_trust_list


  let intersect trust1 trust2 =
    match (trust1, trust2) with
    | Only classes, All ->
        Only classes
    | All, Only classes ->
        Only classes
    | Only list1, Only list2 ->
        Only (JavaClassName.Set.inter list1 list2)
    | All, All ->
        All


  let pp fmt t =
    match t with
    | All ->
        F.fprintf fmt "all"
    | Only names when JavaClassName.Set.is_empty names ->
        F.fprintf fmt "none"
    | Only _names ->
        F.fprintf fmt "selected"
end

type t = Default | Local of Trust.t | Strict [@@deriving compare, equal]

let pp fmt t =
  match t with
  | Default ->
      F.fprintf fmt "Def"
  | Strict ->
      F.fprintf fmt "Strict"
  | Local trust ->
      F.fprintf fmt "Local(trust=%a)" Trust.pp trust


let of_annot annot =
  let open IOption.Let_syntax in
  let* mode = Annot.find_parameter annot ~name:"value" in
  match mode with
  | Annot.Enum {value= "STRICT"} ->
      return Strict
  | Annot.Enum {value= "LOCAL"} -> (
    match Annot.find_parameter annot ~name:"trustOnly" with
    | None ->
        (* When trustOnly values is missing, the default is in effect, which is Trust.All *)
        return (Local Trust.All)
    | Some (Annot.Annot trustOnly') ->
        let* trust = Trust.of_annot trustOnly' in
        return (Local trust)
    | Some _ ->
        None )
  | _ ->
      None


let extract_user_defined_class_name java_class_name =
  (* Anonymous inner classes are not proper classes and can not be annotated. Refer to underlying user class *)
  JavaClassName.get_user_defined_class_if_anonymous_inner java_class_name
  |> Option.value ~default:java_class_name


let extract_mode_from_explicit_class_annotation tenv classname =
  match PatternMatch.type_name_get_annotation tenv (Typ.JavaClass classname) with
  | Some annots -> (
      if Annotations.ia_is_nullsafe_strict annots then Strict
      else
        match Annotations.ia_find_nullsafe annots with
        | Some nullsafe_annot ->
            Option.value_exn (of_annot nullsafe_annot)
              ~message:"Unexpected change in @Nullsafe annotation format"
        | _ ->
            Default )
  | None ->
      Default


(** Get the minimal mode that is stricter or equal than both of given modes *)
let intersect mode1 mode2 =
  match (mode1, mode2) with
  | Strict, _ | _, Strict ->
      Strict
  | Local trust1, Local trust2 ->
      Local (Trust.intersect trust1 trust2)
  | Local trust, Default | Default, Local trust ->
      Local trust
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


let of_procname tenv pname =
  match pname with
  | Procname.Java jn ->
      of_java_procname tenv jn
  | _ ->
      Logging.die InternalError "Unexpected non-Java procname %a" Procname.pp pname


let is_stricter_than ~stricter ~weaker =
  let strict_level mode = match mode with Default -> 0 | Local _ -> 1 | Strict -> 2 in
  match (stricter, weaker) with
  | Local stricter_trust, Local weaker_trust ->
      Trust.is_stricter ~stricter:stricter_trust ~weaker:weaker_trust
  | _ ->
      strict_level stricter > strict_level weaker


type nested_class_annotation_problem =
  | RedundantNestedClassAnnotation
  | NestedModeIsWeaker of weak_type

and weak_type = ExtraTrustClass of JavaClassName.t list | Other

let check_problematic_class_annotation tenv user_defined_class =
  if JavaClassName.is_anonymous_inner_class_name user_defined_class then
    Logging.die InternalError
      "check_problematic_class_annotation: should not be called for anonymous classes, but was \
       called for %a"
      JavaClassName.pp user_defined_class ;
  Option.value_map
    (JavaClassName.get_outer_class_name user_defined_class)
    ~f:(fun outer_class_name ->
      (* Check if the mode of the nested class contradicts the outer's one or is redundant *)
      let nested_mode = extract_mode_from_explicit_class_annotation tenv user_defined_class in
      if equal Default nested_mode then (* Nested class is not explicitly annotated. *)
        Ok ()
      else
        let outer_mode = of_class tenv outer_class_name in
        if equal nested_mode outer_mode then Error RedundantNestedClassAnnotation
        else
          match (nested_mode, outer_mode) with
          | Local (Trust.Only nested_trust_list), Local (Trust.Only outer_trust_list) ->
              (* Special processing for pair of two Trust(some).
                 The problem if when there is an attempt to introduce additional trust in nested that did not exist
                 in outer.
              *)
              let extra_trusted_classes_in_nested =
                JavaClassName.Set.diff nested_trust_list outer_trust_list
              in
              if not (JavaClassName.Set.is_empty extra_trusted_classes_in_nested) then
                Error
                  (NestedModeIsWeaker
                     (ExtraTrustClass (JavaClassName.Set.elements extra_trusted_classes_in_nested)))
              else Ok ()
          | _ ->
              if is_stricter_than ~stricter:outer_mode ~weaker:nested_mode then
                Error (NestedModeIsWeaker Other)
              else Ok () )
    ~default:(Ok ())


let is_in_trust_list t name =
  match t with Strict | Default -> false | Local trust -> Trust.is_in_trust_list trust name


let severity = function
  | Strict | Local _ ->
      (* Explicit @Nullsafe modes suppose that enforcement is made on CI side to not allow violations in the codebase.
         Hence it should be an error.
      *)
      IssueType.Error
  | Default ->
      (* Enforcement is not supposed to be setup in default modes. *)
      IssueType.Warning
