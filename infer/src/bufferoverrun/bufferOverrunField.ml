(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(**
  If fn is empty, prints [pp_lhs_alone lhs]
  Otherwise prints [pp_lhs lhs ^ sep ^ fn]

  Create invisible phantom fields by giving them a name ending in '.'
  The name preceeding the '.' will be used in debug mode.
*)
let pp ~pp_lhs ~pp_lhs_alone ~sep f lhs fn =
  let fieldname = Typ.Fieldname.to_flat_string fn in
  if String.is_empty fieldname then
    if Config.bo_debug > 0 then
      let fieldname =
        Option.value ~default:""
          (Typ.Fieldname.to_simplified_string fn |> String.chop_suffix ~suffix:".")
      in
      F.fprintf f "%a%s%s" pp_lhs lhs sep fieldname
    else pp_lhs_alone f lhs
  else F.fprintf f "%a%s%s" pp_lhs lhs sep fieldname


let mk, get_type =
  let classname = "__infer__" in
  let types = ref Typ.Fieldname.Map.empty in
  let mk name typ =
    let fullname = Format.sprintf "%s.%s" classname name in
    let fieldname = Typ.Fieldname.Java.from_string fullname in
    types := Typ.Fieldname.Map.add fieldname typ !types ;
    fieldname
  in
  let get_type fn = Typ.Fieldname.Map.find_opt fn !types in
  (mk, get_type)


let java_collection_internal_array = mk "java.collection.$elements." Typ.(mk_array void)
