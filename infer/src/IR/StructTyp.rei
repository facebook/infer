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
open! IStd;


/** The Smallfoot Intermediate Language: Struct Types */
let module F = Format;

type field = (Ident.fieldname, Typ.t, Annot.Item.t) [@@deriving compare];

type fields = list field;


/** Type for a structured value. */
type t = private {
  fields: fields, /** non-static fields */
  statics: fields, /** static fields */
  supers: list Typename.t, /** supers */
  methods: list Procname.t, /** methods defined */
  annots: Annot.Item.t /** annotations */
};

type lookup = Typename.t => option t;


/** Pretty print a struct type. */
let pp: Pp.env => Typename.t => F.formatter => t => unit;


/** Construct a struct_typ, normalizing field types */
let internal_mk_struct:
  default::t? =>
  fields::fields? =>
  statics::fields? =>
  methods::list Procname.t? =>
  supers::list Typename.t? =>
  annots::Annot.Item.t? =>
  unit =>
  t;


/** the element typ of the final extensible array in the given typ, if any */
let get_extensible_array_element_typ: lookup::lookup => Typ.t => option Typ.t;


/** If a struct type with field f, return the type of f.
    If not, return the default type if given, otherwise raise an exception */
let fld_typ: lookup::lookup => default::Typ.t => Ident.fieldname => Typ.t => Typ.t;


/** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] */
let get_field_type_and_annotation:
  lookup::lookup => Ident.fieldname => Typ.t => option (Typ.t, Annot.Item.t);


/** Field used for objective-c reference counting */
let objc_ref_counter_field: (Ident.fieldname, Typ.t, Annot.Item.t);

let is_objc_ref_counter_field: (Ident.fieldname, Typ.t, Annot.Item.t) => bool;
