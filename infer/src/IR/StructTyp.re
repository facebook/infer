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


/** The Smallfoot Intermediate Language: Struct Types */
let module L = Logging;

let module F = Format;

type field = (Ident.fieldname, Typ.t, Annot.Item.t) [@@deriving compare];

type fields = list field;


/** Type for a structured value. */
type t = {
  fields: fields, /** non-static fields */
  statics: fields, /** static fields */
  supers: list Typename.t, /** superclasses */
  methods: list Procname.t, /** methods defined */
  annots: Annot.Item.t /** annotations */
};

type lookup = Typename.t => option t;

let pp pe name f {fields, supers, methods, annots} =>
  if Config.debug_mode {
    /* change false to true to print the details of struct */
    F.fprintf
      f
      "%a \n\tfields: {%a\n\t}\n\tsupers: {%a\n\t}\n\tmethods: {%a\n\t}\n\tannots: {%a\n\t}"
      Typename.pp
      name
      (
        Pp.seq (
          fun f (fld, t, _) => F.fprintf f "\n\t\t%a %a" (Typ.pp_full pe) t Ident.pp_fieldname fld
        )
      )
      fields
      (Pp.seq (fun f n => F.fprintf f "\n\t\t%a" Typename.pp n))
      supers
      (Pp.seq (fun f m => F.fprintf f "\n\t\t%a" Procname.pp m))
      methods
      Annot.Item.pp
      annots
  } else {
    F.fprintf f "%a" Typename.pp name
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
      switch (List.last fields) {
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
      List.find f::(fun (f, _, _) => Ident.equal_fieldname f fn) fields |>
      Option.value_map f::snd3 default::default
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
      List.find_map
        f::(fun (f, t, a) => Ident.equal_fieldname f fn ? Some (t, a) : None) (fields @ statics)
    | None => None
    }
  | _ => None
  };

let objc_ref_counter_annot = [({Annot.class_name: "ref_counter", parameters: []}, false)];


/** Field used for objective-c reference counting */
let objc_ref_counter_field = (Ident.fieldname_hidden, Typ.Tint IInt, objc_ref_counter_annot);

let is_objc_ref_counter_field (fld, _, a) =>
  Ident.fieldname_is_hidden fld && Annot.Item.equal a objc_ref_counter_annot;
