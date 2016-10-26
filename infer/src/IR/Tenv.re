/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;


/** Module for Type Environments. */

/** Hash tables on strings. */
let module TypenameHash = Hashtbl.Make {
  type t = Typename.t;
  let equal tn1 tn2 => Typename.equal tn1 tn2;
  let hash = Hashtbl.hash;
};


/** Type for type environment. */
type t = TypenameHash.t StructTyp.t;


/** Create a new type environment. */
let create () => TypenameHash.create 1000;


/** Construct a struct type in a type environment */
let mk_struct
    tenv
    default::default=?
    fields::fields=?
    statics::statics=?
    methods::methods=?
    supers::supers=?
    annots::annots=?
    name => {
  let struct_typ =
    StructTyp.internal_mk_struct
      default::?default
      fields::?fields
      statics::?statics
      methods::?methods
      supers::?supers
      annots::?annots
      ();
  TypenameHash.replace tenv name struct_typ;
  struct_typ
};


/** Check if typename is found in tenv */
let mem tenv name => TypenameHash.mem tenv name;


/** Look up a name in the global type environment. */
let lookup tenv name :option StructTyp.t =>
  try (Some (TypenameHash.find tenv name)) {
  | Not_found =>
    /* ToDo: remove the following additional lookups once C/C++ interop is resolved */
    switch (name: Typename.t) {
    | TN_csu Struct m =>
      try (Some (TypenameHash.find tenv (TN_csu (Class CPP) m))) {
      | Not_found => None
      }
    | TN_csu (Class CPP) m =>
      try (Some (TypenameHash.find tenv (TN_csu Struct m))) {
      | Not_found => None
      }
    | _ => None
    }
  };


/** Add a (name,type) pair to the global type environment. */
let add tenv name struct_typ => TypenameHash.replace tenv name struct_typ;


/** Get method that is being overriden by java_pname (if any) **/
let get_overriden_method tenv pname_java => {
  let struct_typ_get_method_by_name (struct_typ: StructTyp.t) method_name =>
    IList.find (fun meth => method_name == Procname.get_method meth) struct_typ.methods;
  let rec get_overriden_method_in_supers pname_java supers =>
    switch supers {
    | [superclass, ...supers_tail] =>
      switch (lookup tenv superclass) {
      | Some struct_typ =>
        try (Some (struct_typ_get_method_by_name struct_typ (Procname.java_get_method pname_java))) {
        | Not_found => get_overriden_method_in_supers pname_java (supers_tail @ struct_typ.supers)
        }
      | None => get_overriden_method_in_supers pname_java supers_tail
      }
    | [] => None
    };
  switch (lookup tenv (Procname.java_get_class_type_name pname_java)) {
  | Some {supers} => get_overriden_method_in_supers pname_java supers
  | _ => None
  }
};


/** Serializer for type environments */
let tenv_serializer: Serialization.serializer t = Serialization.create_serializer Serialization.tenv_key;

let global_tenv: Lazy.t (option t) =
  lazy (Serialization.from_file tenv_serializer DB.global_tenv_fname);


/** Load a type environment from a file */
let load_from_file (filename: DB.filename) :option t =>
  if (filename == DB.global_tenv_fname) {
    Lazy.force global_tenv
  } else {
    Serialization.from_file tenv_serializer filename
  };


/** Save a type environment into a file */
let store_to_file (filename: DB.filename) (tenv: t) =>
  Serialization.to_file tenv_serializer filename tenv;

let iter f tenv => TypenameHash.iter f tenv;

let fold f tenv => TypenameHash.fold f tenv;

let pp fmt (tenv: t) =>
  TypenameHash.iter
    (
      fun name typ => {
        Format.fprintf fmt "@[<6>NAME: %s@." (Typename.to_string name);
        Format.fprintf fmt "@[<6>TYPE: %a@." (StructTyp.pp pe_text (fun _ () => ()) name) typ
      }
    )
    tenv;
