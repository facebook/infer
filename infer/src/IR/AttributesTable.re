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

let module F = Format;

let module L = Logging;


/** Module to manage the table of attributes. */
let serializer: Serialization.serializer ProcAttributes.t = Serialization.create_serializer Serialization.attributes_key;

let attributes_filename pname => {
  let pname_file = Procname.to_filename pname;
  pname_file ^ ".attr"
};


/** path to the .attr file for the given procedure in the current results directory */
let res_dir_attr_filename pname => {
  let attr_fname = attributes_filename pname;
  let bucket_dir = {
    let base = Filename.chop_extension attr_fname;
    let len = String.length base;
    if (len < 2) {
      Filename.current_dir_name
    } else {
      String.sub base (len - 2) 2
    }
  };
  let filename =
    DB.Results_dir.path_to_filename
      DB.Results_dir.Abs_root [Config.attributes_dir_name, bucket_dir, attr_fname];
  DB.filename_create_dir filename;
  filename
};

let store_attributes proc_attributes => {
  let proc_name = proc_attributes.ProcAttributes.proc_name;
  let attributes_file = res_dir_attr_filename proc_name;
  let should_write =
    /* only overwrite defined procedures */
    proc_attributes.ProcAttributes.is_defined || not (DB.file_exists attributes_file);
  if should_write {
    Serialization.to_file serializer attributes_file proc_attributes
  }
};

let load_attributes proc_name => {
  let attributes_file = res_dir_attr_filename proc_name;
  Serialization.from_file serializer attributes_file
};


/** Given a procdesure name, find the file where it is defined and */
/** its corresponding type environment */
let find_tenv_from_class_of_proc procname =>
  switch (load_attributes procname) {
  | None => None
  | Some attrs =>
    let source_file = attrs.ProcAttributes.loc.Location.file;
    let source_dir = DB.source_dir_from_source_file source_file;
    let tenv_fname = DB.source_dir_get_internal_file source_dir ".tenv";
    Tenv.load_from_file tenv_fname
  };


/** Given an ObjC class c, extract the type from the tenv where the class was */
/** defined. We do this by adding a method that is unique to each class, and then */
/** finding the tenv that corresponds to the class definition.  */
let get_correct_type_from_objc_class_name c => {
  let class_method = Procname.get_default_objc_class_method (Mangled.to_string c);
  switch (find_tenv_from_class_of_proc class_method) {
  | None => None
  | Some tenv =>
    let type_name = Typename.TN_csu (Csu.Class Csu.Objc) c;
    Option.map (fun st => Sil.Tstruct st) (Tenv.lookup tenv type_name)
  }
};


/** Returns true if the method is defined as a C++ model */
let pname_is_cpp_model callee_pname =>
  switch (load_attributes callee_pname) {
  | Some attrs =>
    let file = DB.source_file_to_string attrs.ProcAttributes.loc.Location.file;
    DB.file_is_in_cpp_model file
  | None => false
  };
