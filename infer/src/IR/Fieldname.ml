(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type captured_data = {capture_mode: CapturedVar.capture_mode; is_function_pointer: bool}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

type t =
  { class_name: Typ.Name.t
  ; field_name: string
  ; captured_data: captured_data option
  ; is_weak: bool option [@ignore] }
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let string_of_capture_mode = function
  | CapturedVar.ByReference ->
      "by_ref"
  | CapturedVar.ByValue ->
      "by_value"


let pp_captured_data f (captured_data : captured_data) =
  F.fprintf f "_captured_%s" (string_of_capture_mode captured_data.capture_mode)


let pp f fld =
  let weak_string =
    if Config.developer_mode then
      match fld.is_weak with Some is_weak -> if is_weak then " (weak)" else "" | None -> ""
    else ""
  in
  let captured_string =
    match fld.captured_data with
    | Some captured_data ->
        F.asprintf "%a" pp_captured_data captured_data
    | None ->
        ""
  in
  F.fprintf f "%s%s%s" fld.field_name captured_string weak_string


type name_ = Typ.name

let compare_name_ n1 n2 =
  match (n1, n2) with
  (* Always compare Hack class names equal, so we only use the field name in compare_name *)
  | Typ.HackClass _hcn1, Typ.HackClass _hcn2 ->
      0
  | _, _ ->
      Typ.Name.compare_name n1 n2


let compare_name f f' =
  [%compare: name_ * string * captured_data option]
    (f.class_name, f.field_name, f.captured_data)
    (f'.class_name, f'.field_name, f'.captured_data)


let make ?captured_data ?is_weak class_name field_name =
  {class_name; field_name; captured_data; is_weak}


let mk_capture_field_in_closure var_name captured_data ~is_weak =
  (* We use this type as before rather than the lambda struct name because
     when we want to create the same names in Pulse we don't have easy access
     to that lambda struct name. The struct name as part of the field name is
     there just to help with inheritance anyway and is not used otherwise. *)
  let class_tname = Typ.CStruct (QualifiedCppName.of_list ["std"; "function"]) in
  let name = Printf.sprintf "%s" (Mangled.to_string var_name) in
  make ~captured_data ~is_weak class_tname name


let is_capture_field_in_closure {captured_data} = Option.is_some captured_data

let is_weak fld = fld.is_weak

let is_capture_field_in_closure_by_ref {captured_data} =
  Option.exists captured_data ~f:(fun {capture_mode} -> CapturedVar.is_captured_by_ref capture_mode)


let is_capture_field_function_pointer {captured_data} =
  Option.exists captured_data ~f:(fun {is_function_pointer} -> is_function_pointer)


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

module Set = PrettyPrintable.MakePPSet (T)
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
    | ErlangType _ | ObjcBlock _ | CFunction _ ->
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
  List.exists patterns ~f:(fun pattern -> Str.string_match pattern s 0)


let is_java_outer_instance ({field_name} as field) =
  is_java field
  &&
  let this = "this$" in
  let last_char = field_name.[String.length field_name - 1] in
  Char.(last_char >= '0' && last_char <= '9')
  && String.is_suffix field_name ~suffix:(this ^ String.of_char last_char)
