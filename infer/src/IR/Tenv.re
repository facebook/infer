/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

module Hashtbl = Caml.Hashtbl;


/** Module for Type Environments. */

/** Hash tables on strings. */
module TypenameHash =
  Hashtbl.Make {
    type t = Typ.Name.t;
    let equal tn1 tn2 => Typ.Name.equal tn1 tn2;
    let hash = Hashtbl.hash;
  };


/** Type for type environment. */
type t = TypenameHash.t Typ.Struct.t;

let pp fmt (tenv: t) =>
  TypenameHash.iter
    (
      fun name typ => {
        Format.fprintf fmt "@[<6>NAME: %s@." (Typ.Name.to_string name);
        Format.fprintf fmt "@[<6>TYPE: %a@." (Typ.Struct.pp Pp.text name) typ
      }
    )
    tenv;


/** Create a new type environment. */
let create () => TypenameHash.create 1000;


/** Construct a struct type in a type environment */
let mk_struct tenv ::default=? ::fields=? ::statics=? ::methods=? ::supers=? ::annots=? name => {
  let struct_typ =
    Typ.Struct.internal_mk_struct
      ::?default ::?fields ::?statics ::?methods ::?supers ::?annots ();
  TypenameHash.replace tenv name struct_typ;
  struct_typ
};


/** Check if typename is found in tenv */
let mem tenv name => TypenameHash.mem tenv name;


/** Look up a name in the global type environment. */
let lookup tenv name :option Typ.Struct.t =>
  try (Some (TypenameHash.find tenv name)) {
  | Not_found =>
    /* ToDo: remove the following additional lookups once C/C++ interop is resolved */
    switch (name: Typ.Name.t) {
    | CStruct m =>
      try (Some (TypenameHash.find tenv (CppClass m NoTemplate))) {
      | Not_found => None
      }
    | CppClass m NoTemplate =>
      try (Some (TypenameHash.find tenv (CStruct m))) {
      | Not_found => None
      }
    | _ => None
    }
  };


/** Add a (name,type) pair to the global type environment. */
let add tenv name struct_typ => TypenameHash.replace tenv name struct_typ;


/** Get method that is being overriden by java_pname (if any) **/
let get_overriden_method tenv pname_java => {
  let struct_typ_get_method_by_name (struct_typ: Typ.Struct.t) method_name =>
    List.find_exn
      f::(fun meth => String.equal method_name (Typ.Procname.get_method meth)) struct_typ.methods;
  let rec get_overriden_method_in_supers pname_java supers =>
    switch supers {
    | [superclass, ...supers_tail] =>
      switch (lookup tenv superclass) {
      | Some struct_typ =>
        try (
          Some (struct_typ_get_method_by_name struct_typ (Typ.Procname.java_get_method pname_java))
        ) {
        | Not_found => get_overriden_method_in_supers pname_java (supers_tail @ struct_typ.supers)
        }
      | None => get_overriden_method_in_supers pname_java supers_tail
      }
    | [] => None
    };
  switch (lookup tenv (Typ.Procname.java_get_class_type_name pname_java)) {
  | Some {supers} => get_overriden_method_in_supers pname_java supers
  | _ => None
  }
};


/** Serializer for type environments */
let tenv_serializer: Serialization.serializer t =
  Serialization.create_serializer Serialization.Key.tenv;

let global_tenv: ref (option t) = ref None;


/** Load a type environment from a file */
let load_from_file (filename: DB.filename) :option t =>
  if (DB.equal_filename filename DB.global_tenv_fname) {
    if (is_none !global_tenv) {
      global_tenv := Serialization.read_from_file tenv_serializer DB.global_tenv_fname
    };
    !global_tenv
  } else {
    Serialization.read_from_file tenv_serializer filename
  };


/** Save a type environment into a file */
let store_to_file (filename: DB.filename) (tenv: t) => {
  /* update in-memory global tenv for later uses by this process, e.g. in single-core mode the
     frontend and backend run in the same process */
  if (DB.equal_filename filename DB.global_tenv_fname) {
    global_tenv := Some tenv
  };
  Serialization.write_to_file tenv_serializer filename data::tenv;
  if Config.debug_mode {
    let debug_filename = DB.filename_to_string (DB.filename_add_suffix filename ".debug");
    let out_channel = Out_channel.create debug_filename;
    let fmt = Format.formatter_of_out_channel out_channel;
    Format.fprintf fmt "%a" pp tenv;
    Out_channel.close out_channel
  }
};

let iter f tenv => TypenameHash.iter f tenv;

let fold f tenv => TypenameHash.fold f tenv;
