/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Module for Type Environments. */
type t; /** Type for type environment. */


/** Add a (name,typename) pair to the global type environment. */
let add: t => Typename.t => StructTyp.t => unit;


/** Create a new type environment. */
let create: unit => t;


/** Fold a function over the elements of the type environment. */
let fold: (Typename.t => StructTyp.t => 'a => 'a) => t => 'a => 'a;


/** iterate over a type environment */
let iter: (Typename.t => StructTyp.t => unit) => t => unit;


/** Load a type environment from a file */
let load_from_file: DB.filename => option t;


/** Look up a name in the global type environment. */
let lookup: t => Typename.t => option StructTyp.t;


/** Construct a struct_typ, normalizing field types */
let mk_struct:
  t =>
  default::StructTyp.t? =>
  fields::StructTyp.fields? =>
  statics::StructTyp.fields? =>
  methods::list Procname.t? =>
  supers::list Typename.t? =>
  annots::Annot.Item.t? =>
  Typename.t =>
  StructTyp.t;


/** Check if typename is found in t */
let mem: t => Typename.t => bool;


/** print a type environment */
let pp: Format.formatter => t => unit;


/** Save a type environment into a file */
let store_to_file: DB.filename => t => unit;


/** Get method that is being overriden by java_pname (if any) **/
let get_overriden_method: t => Procname.java => option Procname.t;
