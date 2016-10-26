/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;


/** Attributes of a procedure. */
type objc_accessor_type =
  | Objc_getter Ident.fieldname
  | Objc_setter Ident.fieldname;

type t = {
  access: PredSymb.access, /** visibility access */
  captured: list (Mangled.t, Typ.t), /** name and type of variables captured in blocks */
  mutable changed: bool, /** true if proc has changed since last analysis */
  mutable did_preanalysis: bool, /** true if we performed preanalysis on the CFG for this proc */
  err_log: Errlog.t, /** Error log for the procedure */
  exceptions: list string, /** exceptions thrown by the procedure */
  formals: list (Mangled.t, Typ.t), /** name and type of formal parameters */
  const_formals: list int, /** list of indices of formals that are const-qualified */
  func_attributes: list PredSymb.func_attribute,
  is_abstract: bool, /** the procedure is abstract */
  is_bridge_method: bool, /** the procedure is a bridge method */
  is_defined: bool, /** true if the procedure is defined, and not just declared */
  is_objc_instance_method: bool, /** the procedure is an objective-C instance method */
  is_cpp_instance_method: bool, /** the procedure is an C++ instance method */
  is_java_synchronized_method: bool, /** the procedure is a Java synchronized method */
  is_synthetic_method: bool, /** the procedure is a synthetic method */
  language: Config.language, /** language of the procedure */
  loc: Location.t, /** location of this procedure in the source code */
  translation_unit: option DB.source_file, /** translation unit to which the procedure belongs */
  mutable locals: list (Mangled.t, Typ.t), /** name and type of local variables */
  method_annotation: Annot.Method.t, /** annotations for java methods */
  objc_accessor: option objc_accessor_type, /** type of ObjC accessor, if any */
  proc_flags: proc_flags, /** flags of the procedure */
  proc_name: Procname.t, /** name of the procedure */
  ret_type: Typ.t, /** return type */
  source_file_captured: DB.source_file /** source file where the procedure was captured */
};


/** Create a proc_attributes with default values. */
let default: Procname.t => Config.language => t;
