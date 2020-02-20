(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Trust = struct
  type t = All | Only of Typ.name list [@@deriving compare, equal]

  let none = Only []

  let extract_trust_list = function
    | Annot.Array class_values ->
        (* The only elements of this array can be class names; therefore short-circuit and return None if it's not the case. *)
        IList.traverse_opt class_values ~f:(fun el ->
            match el with Annot.Class class_typ -> Typ.name class_typ | _ -> None )
    | _ ->
        None


  let of_annot annot =
    let open IOption.Let_syntax in
    let trust_all = Annot.find_parameter annot ~name:"trustAll" in
    let* trust_list = Annot.find_parameter annot ~name:"value" in
    let* trust_classes = extract_trust_list trust_list in
    match trust_all with
    | None ->
        return none
    | Some (Annot.Bool trust_all') ->
        if trust_all' then return All else return (Only trust_classes)
    | _ ->
        None


  let pp fmt t =
    match t with
    | All ->
        F.fprintf fmt "all"
    | Only [] ->
        F.fprintf fmt "none"
    | Only names ->
        F.fprintf fmt "[%a]" (F.pp_print_list ~pp_sep:F.pp_print_space Typ.Name.pp) names
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


let of_class tenv typ_name =
  match PatternMatch.type_name_get_annotation tenv typ_name with
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


let severity = function Strict | Local _ -> Exceptions.Error | Default -> Exceptions.Warning
