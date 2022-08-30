(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let pp ~pp_lhs ~sep f lhs fn = F.fprintf f "%a%s%s" pp_lhs lhs sep (Fieldname.get_field_name fn)

let mk, get_type =
  let class_name = "__infer__" in
  let types = ref Fieldname.Map.empty in
  let mk name typ =
    let fieldname =
      let class_name, field_name = String.rsplit2_exn ~on:'.' (class_name ^ "." ^ name) in
      Fieldname.make (Typ.Name.Java.from_string class_name) field_name
    in
    types := Fieldname.Map.add fieldname typ !types ;
    fieldname
  in
  let get_type fn = Fieldname.Map.find_opt fn !types in
  (mk, get_type)


let java_collection_internal_array = mk "java.collection.elements" Typ.(mk_array StdTyp.void)

let java_linked_list_index = mk "java.linked_list_index" StdTyp.(int)

let java_linked_list_length = mk "java.linked_list_length" StdTyp.(int)

let java_linked_list_next typ = mk "java.linked_list_next" typ

let java_list_files_length = mk "java.list_files_length" StdTyp.(int)

let is_java_collection_internal_array fn = Fieldname.equal fn java_collection_internal_array

let objc_collection_internal_array = mk "nscollection.elements" Typ.(mk_array StdTyp.void)

let objc_iterator_offset = mk "nsiterator.offset" Typ.(mk_array StdTyp.void)

let c_strlen () =
  if Language.curr_language_is Java then mk "length" StdTyp.uint else mk "c.strlen" StdTyp.uint


let cpp_collection_internal_array = mk "cpp.container.elements" Typ.(mk_array StdTyp.void)

let cpp_vector_elem_str = "cpp.vector_elem"

let unknown_cpp_vector_elem_field = mk "unknown.cpp.vector" StdTyp.uint

let cpp_vector_elem ~vec_typ =
  match vec_typ.Typ.desc with
  | Tptr (vec_typ, _) -> (
    match Typ.name vec_typ with
    | None ->
        L.(debug BufferOverrun Verbose)
          "Unknown class name of vector `%a`@\n" (Typ.pp_full Pp.text) vec_typ ;
        unknown_cpp_vector_elem_field
    | Some classname ->
        (* Note: Avoid calling [mk] that has side-effects introducing non-deterministic results *)
        Fieldname.make classname cpp_vector_elem_str )
  | _ ->
      L.(debug BufferOverrun Verbose) "First parameter of constructor should be a pointer.@\n" ;
      unknown_cpp_vector_elem_field


let is_cpp_vector_elem fn = String.equal (Fieldname.get_field_name fn) cpp_vector_elem_str

(** Field domain constructor *)

type 'prim t =
  | Prim of 'prim
  | Field of {prefix: 'prim t; fn: Fieldname.t; typ: Typ.t option [@ignore]}
  | StarField of {prefix: 'prim t; last_field: Fieldname.t}
[@@deriving compare, equal]

let is_field_depth_beyond_limit =
  match Config.bo_field_depth_limit with
  | None ->
      fun _depth -> false
  | Some limit ->
      fun depth -> depth > limit


let mk_append_star_field ~prim_append_star_field p0 fn =
  let rec aux = function
    | Prim prim ->
        prim_append_star_field p0 fn aux prim
    | Field {prefix= p} ->
        aux p
    | StarField {last_field} as p when Fieldname.equal fn last_field ->
        p
    | StarField {prefix} ->
        StarField {last_field= fn; prefix}
  in
  aux p0


let mk_append_field ~prim_append_field ~prim_append_star_field ?typ p0 fn =
  let rec aux ~depth p =
    if is_field_depth_beyond_limit depth then mk_append_star_field ~prim_append_star_field p0 fn
    else
      match p with
      | Prim prim ->
          prim_append_field ?typ p0 fn aux depth prim
      | Field {fn= fn'} when Fieldname.equal fn fn' ->
          StarField {last_field= fn; prefix= p0}
      | Field {prefix= p} ->
          aux ~depth:(depth + 1) p
      | StarField {last_field} as p when Fieldname.equal fn last_field ->
          p
      | StarField {prefix} ->
          StarField {prefix; last_field= fn}
  in
  aux ~depth:0 p0
