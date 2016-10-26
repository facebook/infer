/*
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


/** Given the name of an ObjC class, extract the type from the tenv where the class was defined. We
    do this by adding a method that is unique to each class, and then finding the tenv that
    corresponds to the class definition. */
let get_correct_type_from_objc_class_name: Typename.t => option Typ.t;


/** Returns true if the method is defined as a C++ model */
let pname_is_cpp_model: Procname.t => bool;

/* Find the file where the procedure was captured, if a cfg for that file exists.
   Return also a boolean indicating whether the procedure is defined in an
   include file. */
let find_file_capturing_procedure: Procname.t => option (DB.source_file, [ | `Include | `Source]);

let is_whitelisted_cpp_method: string => bool;

type t;

let stats: unit => t;

let to_json: t => Yojson.Basic.json;

let from_json: Yojson.Basic.json => t;

let aggregate: list t => Yojson.Basic.json;
