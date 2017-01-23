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

let module Java = {
  let from_string class_name_str =>
    TN_csu (Csu.Class Csu.Java) (Mangled.from_string class_name_str);
  let is_class =
    fun
    | TN_csu (Class Java) _ => true
    | _ => false;
  let java_lang_object = from_string "java.lang.Object";
  let java_io_serializable = from_string "java.io.Serializable";
  let java_lang_cloneable = from_string "java.lang.Cloneable";
};

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};
