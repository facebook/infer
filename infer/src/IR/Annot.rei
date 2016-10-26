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
let module F = Format;


/** Type to represent one @Annotation. */
type t = {
  class_name: string, /** name of the annotation */
  parameters: list string /** currently only one string parameter */
};


/** Compare function for annotations. */
let compare: t => t => int;


/** Pretty print an annotation. */
let pp: F.formatter => t => unit;

let module Map: PrettyPrintable.PPMap with type key = t;

let module Item: {

  /** Annotation for one item: a list of annotations with visibility. */
  type nonrec t = list (t, bool);

  /** Compare function for annotation items. */
  let compare: t => t => int;

  /** Pretty print an item annotation. */
  let pp: F.formatter => t => unit;
  let to_string: t => string;

  /** Empty item annotation. */
  let empty: t;

  /** Check if the item annodation is empty. */
  let is_empty: t => bool;
};

let module Class: {let objc: Item.t; let cpp: Item.t;};

let module Method: {

  /** Annotation for a method: return value and list of parameters. */
  type t = (Item.t, list Item.t);

  /** Compare function for Method annotations. */
  let compare: t => t => int;

  /** Empty method annotation. */
  let empty: t;

  /** Check if the method annodation is empty. */
  let is_empty: t => bool;

  /** Pretty print a method annotation. */
  let pp: string => F.formatter => t => unit;
};
