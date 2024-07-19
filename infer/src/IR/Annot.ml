(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Annotations *)

open! IStd
module F = Format

(** Type to represent an [@Annotation] with potentially complex parameter values such as arrays or
    other annotations. *)
type t = {class_name: string  (** name of the annotation *); parameters: parameter list}
[@@deriving compare, equal, hash, normalize]

and parameter = {name: string option; value: value} [@@deriving compare]

(** Type to represent possible annotation parameter values. Note that support for numeric parameters
    is missing for now due to an issue with [MaximumSharing] and [int64]. *)
and value =
  | Str of string
  | Bool of bool
  | Enum of {class_typ: Typ.t; value: string}
  | Array of value list
  | Class of Typ.t
  | Annot of t

let abstract = {class_name= "abstract"; parameters= []}

let volatile = {class_name= "volatile"; parameters= []}

let final = {class_name= "final"; parameters= []}

let notnull = {class_name= "notnull"; parameters= []}

let is_abstract x = equal abstract x

let is_final x = equal final x

let is_notnull x = equal notnull x

let rec has_matching_str_value ~pred = function
  | Str s ->
      pred s
  | Array els ->
      List.exists els ~f:(has_matching_str_value ~pred)
  | Enum {value} ->
      pred value
  | _ ->
      false


let find_parameter t ~name =
  let match_name param = Option.exists param.name ~f:(String.equal name) in
  List.find t.parameters ~f:match_name |> Option.map ~f:(fun x -> x.value)


(** Pretty print an annotation. *)
let prefix = match Language.curr_language_is Java with true -> "@" | false -> "_"

let comma_sep fmt _ = F.pp_print_string fmt ", "

let rec pp_value fmt = function
  | Str s ->
      F.pp_print_string fmt s
  | Bool b ->
      F.pp_print_bool fmt b
  | Enum {class_typ; value} ->
      F.fprintf fmt "%a.%s" (Typ.pp Pp.text) class_typ value
  | Array values ->
      F.pp_print_list ~pp_sep:comma_sep pp_value fmt values
  | Class name ->
      F.fprintf fmt "%a" (Typ.pp Pp.text) name
  | Annot a ->
      F.fprintf fmt "%a" pp a


and pp_parameter fmt {name; value} =
  match name with
  | None ->
      F.fprintf fmt "\"%a\"" pp_value value
  | Some name ->
      F.fprintf fmt "%s=\"%a\"" name pp_value value


and pp fmt annotation =
  F.fprintf fmt "%s%s" prefix annotation.class_name ;
  if not (List.is_empty annotation.parameters) then
    F.fprintf fmt "(%a)" (F.pp_print_list ~pp_sep:comma_sep pp_parameter) annotation.parameters


module Item = struct
  (* type alias needed to workaround [t] being used in RHS non-recursively for normalization *)
  type t_ = t [@@deriving compare, equal, hash, normalize]

  (** Annotation for one item: a list of annotations with visibility. *)
  type t = t_ list [@@deriving compare, equal, hash, normalize]

  (** Pretty print an item annotation. *)
  let pp fmt ann = F.fprintf fmt "<%a>" (Pp.seq pp) ann

  (** Empty item annotation. *)
  let empty = []

  let is_abstract ia = List.exists ia ~f:is_abstract

  (** Check if the item annotation is empty. *)
  let is_empty ia = List.is_empty ia

  let is_final ia = List.exists ia ~f:is_final

  let is_notnull ia = List.exists ia ~f:is_notnull
end

module Class = struct
  let objc_str = "ObjC-Class"

  let cpp_str = "Cpp-Class"

  let of_string class_string = [{class_name= class_string; parameters= []}]

  let objc = of_string objc_str

  let cpp = of_string cpp_str
end
