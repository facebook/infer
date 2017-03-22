/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let module Hashtbl = Caml.Hashtbl;

type t =
  | Hidden /* Backend relies that Hidden is the smallest (first) field in Abs.should_raise_objc_leak */
  | Clang Mangled.t
  | Java string
[@@deriving compare];

let hidden_str = ".hidden";

let equal = [%compare.equal : t];

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Map = Caml.Map.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Clang = {

  /** Create a field name with the given position (field number in the CSU) */
  let create (n: Mangled.t) => Clang n;
};

let module Java = {

  /** Create a field name with the given position (field number in the CSU) */
  let from_string n => Java n;
};


/** Convert a fieldname to a string. */
let to_string =
  fun
  | Hidden => hidden_str
  | Java fname => fname
  | Clang fname => Mangled.to_string fname;


/** Convert a fieldname to a string, including the mangled part. */
let to_complete_string =
  fun
  | Hidden => hidden_str
  | Java fname => fname
  | Clang fname => Mangled.to_string_full fname;


/** Convert a fieldname to a simplified string with at most one-level path. */
let to_simplified_string fn => {
  let s = to_string fn;
  switch (String.rsplit2 s on::'.') {
  | Some (s1, s2) =>
    switch (String.rsplit2 s1 on::'.') {
    | Some (_, s4) => s4 ^ "." ^ s2
    | _ => s
    }
  | _ => s
  }
};


/** Convert a fieldname to a flat string without path. */
let to_flat_string fn => {
  let s = to_string fn;
  switch (String.rsplit2 s on::'.') {
  | Some (_, s2) => s2
  | _ => s
  }
};

let pp f =>
  fun
  | Hidden => Format.fprintf f "%s" hidden_str
  | Java fname => Format.fprintf f "%s" fname
  | Clang fname => Mangled.pp f fname;

let pp_latex style f fn => Latex.pp_string style f (to_string fn);


/** Returns the class part of the fieldname */
let java_get_class fn => {
  let fn = to_string fn;
  let ri = String.rindex_exn fn '.';
  String.slice fn 0 ri
};


/** Returns the last component of the fieldname */
let java_get_field fn => {
  let fn = to_string fn;
  let ri = 1 + String.rindex_exn fn '.';
  String.slice fn ri 0
};


/** Check if the field is the synthetic this$n of a nested class, used to access the n-th outher instance. */
let java_is_outer_instance fn => {
  let fn = to_string fn;
  let fn_len = String.length fn;
  fn_len != 0 && {
    let this = ".this$";
    let last_char = fn.[fn_len - 1];
    (last_char >= '0' && last_char <= '9') &&
    String.is_suffix fn suffix::(this ^ String.of_char last_char)
  }
};


/** hidded fieldname constant */
let hidden = Hidden;


/** hidded fieldname constant */
let is_hidden fn => equal fn hidden;
