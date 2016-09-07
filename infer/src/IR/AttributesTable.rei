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
/** Module to manage the table of attributes. */

open! Utils;


/** Save .attr file for the procedure into the attributes database. */
let store_attributes: ProcAttributes.t => unit;


/** Load the attributes for the procedure from the attributes database. */
let load_attributes: Procname.t => option ProcAttributes.t;


/** Given a procedure name, find the file where it is defined and its corresponding type
    environment */
let find_tenv_from_class_of_proc: Procname.t => option Tenv.t;


/** Given a procedure name, find the file where it is defined and its corresponding type
    environment, or create an empty tenv if necessary. */
let get_tenv: Procname.t => Tenv.t;


/** Given the name of an ObjC class, extract the type from the tenv where the class was defined. We
    do this by adding a method that is unique to each class, and then finding the tenv that
    corresponds to the class definition. */
let get_correct_type_from_objc_class_name: Typename.t => option Typ.t;


/** Returns true if the method is defined as a C++ model */
let pname_is_cpp_model: Procname.t => bool;

/* Find the file where the procedure is defined according to the attributes,
   if a cfg for that file exist. */
let file_defining_procedure: Procname.t => option DB.source_file;

let is_whitelisted_cpp_method: string => bool;

type t;

let stats: unit => t;

let to_json: t => Yojson.Basic.json;

let from_json: Yojson.Basic.json => t;

let aggregate: list t => Yojson.Basic.json;
