/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Names for fields of class/struct/union */
type t [@@deriving compare];


/** Equality for field names. */
let equal: t => t => bool;


/** Set for fieldnames */
module Set: Caml.Set.S with type elt = t;


/** Map for fieldnames */
module Map: Caml.Map.S with type key = t;

module Clang: {

  /** Create a clang field name from qualified c++ name */
  let from_qualified: QualifiedCppName.t => string => t;
};

module Java: {

  /** Create a java field name from string */
  let from_string: string => t;
};


/** Convert a field name to a string. */
let to_string: t => string;


/** Convert a fieldname to a simplified string with at most one-level path. */
let to_simplified_string: t => string;


/** Convert a fieldname to a flat string without path. */
let to_flat_string: t => string;


/** Pretty print a field name. */
let pp: Format.formatter => t => unit;


/** Pretty print a field name in latex. */
let pp_latex: Latex.style => Format.formatter => t => unit;


/** The class part of the fieldname */
let java_get_class: t => string;


/** The last component of the fieldname */
let java_get_field: t => string;


/** Check if the field is the synthetic this$n of a nested class, used to access the n-th outher instance. */
let java_is_outer_instance: t => bool;


/** get qualified classname of a field if it's coming from clang frontend. returns None otherwise */
let clang_get_qual_class: t => option QualifiedCppName.t;


/** hidded fieldname constant */
let hidden: t;


/** hidded fieldname constant */
let is_hidden: t => bool;
