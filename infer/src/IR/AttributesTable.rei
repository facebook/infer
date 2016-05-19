/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** Module to manage the table of attributes. */
/** Save .attr file for the procedure into the attributes database. */
let store_attributes: ProcAttributes.t => unit;


/** Load the attributes for the procedure from the attributes database. */
let load_attributes: Procname.t => option ProcAttributes.t;


/** Given a procdesure name, find the file where it is defined and */
/** its corresponding type environment */
let find_tenv_from_class_of_proc: Procname.t => option Tenv.t;


/** Given an ObjC class c, extract the type from the tenv where the class was */
/** defined. We do this by adding a method that is unique to each class, and then */
/** finding the tenv that corresponds to the class definition.  */
let get_correct_type_from_objc_class_name: Mangled.t => option Sil.typ;

/** Returns true if the method is defined as a C++ model */
let pname_is_cpp_model : Procname.t => bool;
