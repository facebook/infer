/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Named types. */
type t =
  | TN_csu Csu.t Mangled.t
[@@deriving compare];


/** Equality for typenames */
let equal: t => t => bool;


/** convert the typename to a string */
let to_string: t => string;

let pp: Format.formatter => t => unit;


/** name of the typename without qualifier */
let name: t => string;

let module C: {let from_string: string => t; let union_from_string: string => t;};

let module Java: {

  /** Create a typename from a Java classname in the form "package.class" */
  let from_string: string => t;

  /** Create a typename from a package name and a class name */
  let from_package_class: string => string => t;

  /** [is_class name] holds if [name] names a Java class */
  let is_class: t => bool;
  let java_lang_object: t;
  let java_io_serializable: t;
  let java_lang_cloneable: t;
};

let module Cpp: {

  /** Create a typename from a C++ classname */
  let from_string: string => t;

  /** [is_class name] holds if [name] names a C++ class */
  let is_class: t => bool;
};

let module Objc: {

  /** Create a typename from a Objc classname */
  let from_string: string => t;
  let protocol_from_string: string => t;

  /** [is_class name] holds if [name] names a Objc class */
  let is_class: t => bool;
};

let module Set: Caml.Set.S with type elt = t;
