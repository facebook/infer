(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {class_name: Typ.Name.t; field_name: string} [@@deriving compare, equal]

let make class_name field_name = {class_name; field_name}

let get_class_name {class_name} = class_name

let get_field_name {field_name} = field_name

let is_java {class_name} = Typ.Name.Java.is_class class_name

let is_java_synthetic t = is_java t && String.contains (get_field_name t) '$'

module T = struct
  type nonrec t = t [@@deriving compare]
end

module Set = Caml.Set.Make (T)
module Map = Caml.Map.Make (T)

let join ~sep c f = String.concat ~sep [c; f]

let dot_join = join ~sep:"."

let cc_join = join ~sep:"::"

let to_string fld =
  if is_java fld then dot_join (Typ.Name.name fld.class_name) fld.field_name else fld.field_name


let to_simplified_string fld =
  if is_java fld then
    Typ.Name.name fld.class_name |> String.rsplit2 ~on:'.'
    |> Option.value_map ~default:fld.field_name ~f:(fun (_, class_only) ->
           String.concat ~sep:"." [class_only; fld.field_name] )
  else fld.field_name


let to_full_string fld =
  (if is_java fld then dot_join else cc_join) (Typ.Name.name fld.class_name) fld.field_name


let pp f fld = F.pp_print_string f fld.field_name

let is_java_outer_instance ({field_name} as field) =
  is_java field
  &&
  let this = "this$" in
  let last_char = field_name.[String.length field_name - 1] in
  Char.(last_char >= '0' && last_char <= '9')
  && String.is_suffix field_name ~suffix:(this ^ String.of_char last_char)
