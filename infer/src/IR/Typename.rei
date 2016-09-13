/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** Named types. */
type t = | TN_csu of Csu.t Mangled.t;


/** convert the typename to a string */
let to_string: t => string;

let pp: Format.formatter => t => unit;


/** name of the typename without qualifier */
let name: t => string;


/** Comparison for typenames */
let compare: t => t => int;


/** Equality for typenames */
let equal: t => t => bool;

let module Java: {
  /** Create a typename from a Java classname in the form "package.class" */
  let from_string: string => t;

  /** [is_class name] holds if [name] names a Java class */
  let is_class: t => bool;
};

let module Set: Set.S with type elt = t;
