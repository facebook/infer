/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;

let module F = Format;


/** Named types. */
type t = | TN_typedef of Mangled.t | TN_enum of Mangled.t | TN_csu of Csu.t Mangled.t;

let to_string =
  fun
  | TN_enum name
  | TN_typedef name => Mangled.to_string name
  | TN_csu csu name => Csu.name csu ^ " " ^ Mangled.to_string name;

let pp f typename => F.fprintf f "%s" (to_string typename);

let name =
  fun
  | TN_enum name
  | TN_typedef name
  | TN_csu _ name => Mangled.to_string name;

let compare tn1 tn2 =>
  switch (tn1, tn2) {
  | (TN_typedef n1, TN_typedef n2) => Mangled.compare n1 n2
  | (TN_typedef _, _) => (-1)
  | (_, TN_typedef _) => 1
  | (TN_enum n1, TN_enum n2) => Mangled.compare n1 n2
  | (TN_enum _, _) => (-1)
  | (_, TN_enum _) => 1
  | (TN_csu csu1 n1, TN_csu csu2 n2) =>
    let n = Csu.compare csu1 csu2;
    if (n != 0) {
      n
    } else {
      Mangled.compare n1 n2
    }
  };

let equal tn1 tn2 => compare tn1 tn2 == 0;

let module Java = {
  let from_string class_name_str =>
    TN_csu (Csu.Class Csu.Java) (Mangled.from_string class_name_str);
};

type typename_t = t;

let module Set = Set.Make {
  type t = typename_t;
  let compare = compare;
};
