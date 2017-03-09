/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let module F = Format;


/** Named types. */
type t =
  | TN_csu Csu.t Mangled.t
[@@deriving compare];

let equal = [%compare.equal : t];

let to_string =
  fun
  | TN_csu csu name => Csu.name csu ^ " " ^ Mangled.to_string name;

let pp f typename => F.fprintf f "%s" (to_string typename);

let name =
  fun
  | TN_csu _ name => Mangled.to_string name;

let from_string_kind class_kind class_name_str =>
  TN_csu (Csu.Class class_kind) (Mangled.from_string class_name_str);

let is_class_kind class_kind =>
  fun
  | TN_csu (Class kind) _ when Csu.equal_class_kind class_kind kind => true
  | _ => false;

let module C = {
  let from_string name_str => TN_csu Csu.Struct (Mangled.from_string name_str);
  let union_from_string name_str => TN_csu Csu.Union (Mangled.from_string name_str);
};

let module Java = {
  let from_string = from_string_kind Csu.Java;
  let from_package_class package_name class_name =>
    if (String.equal package_name "") {
      from_string class_name
    } else {
      from_string (package_name ^ "." ^ class_name)
    };
  let is_class = is_class_kind Csu.Java;
  let java_lang_object = from_string "java.lang.Object";
  let java_io_serializable = from_string "java.io.Serializable";
  let java_lang_cloneable = from_string "java.lang.Cloneable";
};

let module Cpp = {
  let from_string = from_string_kind Csu.CPP;
  let is_class = is_class_kind Csu.CPP;
};

let module Objc = {
  let from_string = from_string_kind Csu.Objc;
  let protocol_from_string name_str => TN_csu Csu.Protocol (Mangled.from_string name_str);
  let is_class = is_class_kind Csu.Objc;
};

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};
