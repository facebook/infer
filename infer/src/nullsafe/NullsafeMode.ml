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
        return (Only trust_classes)
    | Some (Annot.Bool trust_all') ->
        if trust_all' then return All else return (Only trust_classes)
    | _ ->
        None


  let is_trusted_name t name =
    match t with All -> true | Only classes -> List.exists classes ~f:(Typ.Name.equal name)


  let pp fmt t =
    match t with
    | All ->
        F.fprintf fmt "all"
    | Only [] ->
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


let of_procname tenv pname =
  let class_name =
    match pname with
    | Procname.Java jn ->
        Procname.Java.get_class_type_name jn
    | _ ->
        Logging.die InternalError "Unexpected non-Java procname %a" Procname.pp pname
  in
  of_class tenv class_name


let is_trusted_name t name =
  match t with Strict -> false | Default -> true | Local trust -> Trust.is_trusted_name trust name


let severity = function
  | Strict | Local _ ->
      (* Explicit @Nullsafe modes suppose that enforcement is made on CI side to not allow violations in the codebase.
         Hence it should be an error.
      *)
      Exceptions.Error
  | Default ->
      (* Enforcement is not supposed to be setup in default modes. *)
      Exceptions.Warning
