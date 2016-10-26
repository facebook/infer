/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;


/** The Smallfoot Intermediate Language: Annotations */
let module L = Logging;

let module F = Format;


/** Type to represent one @Annotation. */
type t = {
  class_name: string, /** name of the annotation */
  parameters: list string /** currently only one string parameter */
};


/** Compare function for annotations. */
let compare a1 a2 => {
  let n = string_compare a1.class_name a2.class_name;
  if (n != 0) {
    n
  } else {
    IList.compare string_compare a1.parameters a2.parameters
  }
};


/** Pretty print an annotation. */
let pp fmt annotation => F.fprintf fmt "@@%s" annotation.class_name;

let module Map = PrettyPrintable.MakePPMap {
  type nonrec t = t;
  let compare = compare;
  let pp_key = pp;
};

let module Item = {

  /** Annotation for one item: a list of annotations with visibility. */
  type nonrec t = list (t, bool);

  /** Compare function for annotation items. */
  let compare ia1 ia2 => {
    let cmp (a1, b1) (a2, b2) => {
      let n = compare a1 a2;
      if (n != 0) {
        n
      } else {
        bool_compare b1 b2
      }
    };
    IList.compare cmp ia1 ia2
  };

  /** Pretty print an item annotation. */
  let pp fmt ann => {
    let pp fmt (a, _) => pp fmt a;
    F.fprintf fmt "<%a>" (pp_seq pp) ann
  };
  let to_string ann => {
    let pp fmt () => pp fmt ann;
    pp_to_string pp ()
  };

  /** Empty item annotation. */
  let empty = [];

  /** Check if the item annodation is empty. */
  let is_empty ia => ia == [];
};

let module Class = {
  let objc_str = "ObjC-Class";
  let cpp_str = "Cpp-Class";
  let of_string class_string => [({class_name: class_string, parameters: []}, true)];
  let objc = of_string objc_str;
  let cpp = of_string cpp_str;
};

let module Method = {

  /** Annotation for a method: return value and list of parameters. */
  type t = (Item.t, list Item.t);

  /** Compare function for Method annotations. */
  let compare (ia1, ial1) (ia2, ial2) => IList.compare Item.compare [ia1, ...ial1] [ia2, ...ial2];

  /** Pretty print a method annotation. */
  let pp s fmt (ia, ial) => F.fprintf fmt "%a %s(%a)" Item.pp ia s (pp_seq Item.pp) ial;

  /** Empty method annotation. */
  let empty = ([], []);

  /** Check if the method annodation is empty. */
  let is_empty (ia, ial) => IList.for_all Item.is_empty [ia, ...ial];
};
