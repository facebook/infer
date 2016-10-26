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


/** The Smallfoot Intermediate Language: Struct Types */
let module L = Logging;

let module F = Format;

type fields = list (Ident.fieldname, Typ.t, Annot.Item.t);


/** Type for a structured value. */
type t = {
  fields: fields, /** non-static fields */
  statics: fields, /** static fields */
  supers: list Typename.t, /** superclasses */
  methods: list Procname.t, /** methods defined */
  annots: Annot.Item.t /** annotations */
};

type lookup = Typename.t => option t;

let fld_typ_ann_compare fta1 fta2 =>
  triple_compare Ident.fieldname_compare Typ.compare Annot.Item.compare fta1 fta2;

let pp pe pp_base name f {fields} =>
  if false {
    /* change false to true to print the details of struct */
    F.fprintf
      f
      "%a {%a} %a"
      Typename.pp
      name
      (pp_seq (fun f (fld, t, _) => F.fprintf f "%a %a" (Typ.pp_full pe) t Ident.pp_fieldname fld))
      fields
      pp_base
      ()
  } else {
    F.fprintf f "%a %a" Typename.pp name pp_base ()
  };

let internal_mk_struct
    default::default=?
    fields::fields=?
    statics::statics=?
    methods::methods=?
    supers::supers=?
    annots::annots=?
    () => {
  let mk_struct_
      default::default={fields: [], statics: [], methods: [], supers: [], annots: Annot.Item.empty}
      fields::fields=default.fields
      statics::statics=default.statics
      methods::methods=default.methods
      supers::supers=default.supers
      annots::annots=default.annots
      () => {
    fields,
    statics,
    methods,
    supers,
    annots
  };
  mk_struct_
    default::?default
    fields::?fields
    statics::?statics
    methods::?methods
    supers::?supers
    annots::?annots
    ()
};


/** the element typ of the final extensible array in the given typ, if any */
let rec get_extensible_array_element_typ lookup::lookup (typ: Typ.t) =>
  switch typ {
  | Tarray typ _ => Some typ
  | Tstruct name =>
    switch (lookup name) {
    | Some {fields} =>
      switch (IList.last fields) {
      | Some (_, fld_typ, _) => get_extensible_array_element_typ lookup::lookup fld_typ
      | None => None
      }
    | None => None
    }
  | _ => None
  };


/** If a struct type with field f, return the type of f. If not, return the default */
let fld_typ lookup::lookup default::default fn (typ: Typ.t) =>
  switch typ {
  | Tstruct name =>
    switch (lookup name) {
    | Some {fields} =>
      try (snd3 (IList.find (fun (f, _, _) => Ident.fieldname_equal f fn) fields)) {
      | Not_found => default
      }
    | None => default
    }
  | _ => default
  };

let get_field_type_and_annotation lookup::lookup fn (typ: Typ.t) =>
  switch typ {
  | Tstruct name
  | Tptr (Tstruct name) _ =>
    switch (lookup name) {
    | Some {fields, statics} =>
      try {
        let (_, t, a) =
          IList.find (fun (f, _, _) => Ident.fieldname_equal f fn) (fields @ statics);
        Some (t, a)
      } {
      | Not_found => None
      }
    | None => None
    }
  | _ => None
  };

let objc_ref_counter_annot = [({Annot.class_name: "ref_counter", parameters: []}, false)];


/** Field used for objective-c reference counting */
let objc_ref_counter_field = (Ident.fieldname_hidden, Typ.Tint IInt, objc_ref_counter_annot);

let is_objc_ref_counter_field (fld, _, a) =>
  Ident.fieldname_is_hidden fld && Annot.Item.compare a objc_ref_counter_annot == 0;
