/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language: Annotations */
let module L = Logging;

let module F = Format;


/** Type to represent one @Annotation. */
type t = {
  class_name: string, /** name of the annotation */
  parameters: list string /** currently only one string parameter */
}
[@@deriving compare];

let volatile = {class_name: "volatile", parameters: []};


/** Pretty print an annotation. */
let prefix = Config.curr_language_is Config.Java ? "@" : "_";

let pp fmt annotation => F.fprintf fmt "%s%s" prefix annotation.class_name;

let module Map = PrettyPrintable.MakePPMap {
  type nonrec t = t;
  let compare = compare;
  let pp = pp;
};

let module Item = {

  /** Annotation for one item: a list of annotations with visibility. */
  /* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 */
  /* type nonrec t = list (t, bool) [@@deriving compare]; */
  type _t = list (t, bool) [@@deriving compare];
  type t = _t [@@deriving compare];
  let equal = [%compare.equal : t];

  /** Pretty print an item annotation. */
  let pp fmt ann => {
    let pp fmt (a, _) => pp fmt a;
    F.fprintf fmt "<%a>" (Pp.seq pp) ann
  };
  let to_string ann => {
    let pp fmt => pp fmt ann;
    F.asprintf "%t" pp
  };

  /** Empty item annotation. */
  let empty = [];

  /** Check if the item annodation is empty. */
  let is_empty ia => List.is_empty ia;
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
  type t = (Item.t, list Item.t) [@@deriving compare];

  /** Pretty print a method annotation. */
  let pp s fmt (ia, ial) => F.fprintf fmt "%a %s(%a)" Item.pp ia s (Pp.seq Item.pp) ial;

  /** Empty method annotation. */
  let empty = ([], []);

  /** Check if the method annodation is empty. */
  let is_empty (ia, ial) => IList.for_all Item.is_empty [ia, ...ial];
};
