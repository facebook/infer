(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type 'typ_name t_ = {class_name: 'typ_name; field_name: string}
[@@deriving compare, equal, yojson_of, sexp, hash]

type t = Typ.Name.t t_ [@@deriving compare, equal, yojson_of, sexp, hash]

let pp f fld = F.pp_print_string f fld.field_name

let compare_name = compare_t_ Typ.Name.compare_name

let make class_name field_name = {class_name; field_name}

let fake_capture_field_prefix = "__capture_"

let fake_capture_field_weak_prefix = fake_capture_field_prefix ^ "weak_"

let string_of_capture_mode = function
  | CapturedVar.ByReference ->
      "by_ref_"
  | CapturedVar.ByValue ->
      "by_value_"


let prefix_of_typ typ =
  match typ.Typ.desc with
  | Tptr (_, (Pk_objc_weak | Pk_objc_unsafe_unretained)) ->
      fake_capture_field_weak_prefix
  | _ ->
      fake_capture_field_prefix


let mk_fake_capture_field ~id typ mode =
  make
    (Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]))
    (Printf.sprintf "%s%s%d" (prefix_of_typ typ) (string_of_capture_mode mode) id)


let is_fake_capture_field {field_name} =
  String.is_prefix ~prefix:fake_capture_field_prefix field_name


let is_fake_capture_field_weak {field_name} =
  String.is_prefix ~prefix:fake_capture_field_weak_prefix field_name


let is_fake_capture_field_by_ref {field_name} =
  let capture_by_ref_str = string_of_capture_mode ByReference in
  let fake_capture_field_by_ref_prefix =
    Printf.sprintf "%s%s" fake_capture_field_prefix capture_by_ref_str
  in
  let fake_capture_field_weak_by_ref_prefix =
    Printf.sprintf "%s%s" fake_capture_field_weak_prefix capture_by_ref_str
  in
  String.is_prefix ~prefix:fake_capture_field_by_ref_prefix field_name
  || String.is_prefix ~prefix:fake_capture_field_weak_by_ref_prefix field_name


let get_capture_field_position ({field_name} as field) =
  if is_fake_capture_field field then
    String.rsplit2 field_name ~on:'_'
    |> Option.bind ~f:(fun (_, str_pos) -> int_of_string_opt str_pos)
  else None


let get_class_name {class_name} = class_name

let get_field_name {field_name} = field_name

let is_java {class_name} = Typ.Name.Java.is_class class_name

let is_java_synthetic t = is_java t && JConfig.is_synthetic_name (get_field_name t)

let is_internal {field_name} =
  String.is_prefix field_name ~prefix:"__"
  || (* NOTE: _M_ is internal field of std::thread::id *)
  String.is_prefix field_name ~prefix:"_M_"


module T = struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end

module Set = Caml.Set.Make (T)
module Map = PrettyPrintable.MakePPMap (T)

let join ~sep c f = String.concat ~sep [c; f]

let dot_join = join ~sep:"."

let cc_join = join ~sep:"::"

let to_string fld =
  if is_java fld then dot_join (Typ.Name.name fld.class_name) fld.field_name else fld.field_name


(** Convert a fieldname to a simplified string with at most one-level path. For example,

    - In C++: "<ClassName>::<FieldName>"
    - In Java, ObjC, C#: "<ClassName>.<FieldName>"
    - In C: "<StructName>.<FieldName>" or "<UnionName>.<FieldName>"
    - In Erlang: "<FieldName>" *)
let to_simplified_string ({class_name; field_name} : t) =
  let last_class_name =
    match class_name with
    | CStruct name | CUnion name | CppClass {name} | ObjcClass name | ObjcProtocol name ->
        QualifiedCppName.extract_last name |> Option.map ~f:fst
    | CSharpClass name ->
        Some (CSharpClassName.classname name)
    | ErlangType _ ->
        None
    | HackClass name ->
        Some (HackClassName.classname name)
    | JavaClass name ->
        Some (JavaClassName.classname name)
    | PythonClass name ->
        Some (PythonClassName.classname name)
  in
  Option.value_map last_class_name ~default:field_name ~f:(fun last_class_name ->
      let sep = match class_name with CppClass _ -> "::" | _ -> "." in
      String.concat ~sep [last_class_name; field_name] )


let to_full_string fld =
  (if is_java fld then dot_join else cc_join) (Typ.Name.name fld.class_name) fld.field_name


let patterns_match patterns fld =
  let s = to_simplified_string fld in
  List.exists patterns ~f:(fun pattern -> Re.Str.string_match pattern s 0)


let is_java_outer_instance ({field_name} as field) =
  is_java field
  &&
  let this = "this$" in
  let last_char = field_name.[String.length field_name - 1] in
  Char.(last_char >= '0' && last_char <= '9')
  && String.is_suffix field_name ~suffix:(this ^ String.of_char last_char)


module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal, hash]

  let normalize t =
    let class_name = Typ.NameNormalizer.normalize t.class_name in
    let field_name = HashNormalizer.StringNormalizer.normalize t.field_name in
    if phys_equal class_name t.class_name && phys_equal field_name t.field_name then t
    else {class_name; field_name}
end)
