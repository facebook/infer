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


/** Module for Procedure Names. */

/** Type of java procedure names. */
type java;


/** Type of c procedure names. */
type c;


/** Type of Objective C and C++ procedure names. */
type objc_cpp;


/** Type of Objective C block names. */
type block;


/** Type of procedure names. */
type t =
  | Java java
  | C c
  | Linters_dummy_method
  | Block block
  | ObjC_Cpp objc_cpp
[@@deriving compare];


/** Equality for proc names. */
let equal: t => t => bool;

type java_type = (option string, string);

type method_kind =
  | Non_Static /* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface */
  | Static /* in Java, procedures called with invokestatic */;

type objc_cpp_method_kind =
  | CPPMethod (option string) /** with mangling */
  | CPPConstructor (option string, bool) /** with mangling + is it constexpr? */
  | ObjCClassMethod
  | ObjCInstanceMethod
  | ObjCInternalMethod;


/** Hash tables with proc names as keys. */
let module Hash: Caml.Hashtbl.S with type key = t;


/** Maps from proc names. */
let module Map: Caml.Map.S with type key = t;


/** Sets of proc names. */
let module Set: Caml.Set.S with type elt = t;


/** Create a C procedure name from plain and mangled name. */
let c: string => string => c;


/** Empty block name. */
let empty_block: t;


/** Convert a string to a proc name. */
let from_string_c_fun: string => t;


/** Return the language of the procedure. */
let get_language: t => Config.language;


/** Return the method/function of a procname. */
let get_method: t => string;


/** Hash function for procname. */
let hash_pname: t => int;


/** Check if a class string is an anoynmous inner class name. */
let is_anonymous_inner_class_name: string => bool;


/** Check if this is an Objective-C/C++ method name. */
let is_c_method: t => bool;


/** Check if this is a constructor method in Objective-C. */
let is_objc_constructor: string => bool;


/** Check if this is a constructor. */
let is_constructor: t => bool;


/** Check if this is a constexpr function. */
let is_constexpr: t => bool;


/** Check if this is a Java procedure name. */
let is_java: t => bool;


/** Check if this is a dealloc method in Objective-C. */
let is_objc_dealloc: string => bool;


/** Check if this is a dealloc method. */
let is_destructor: t => bool;


/** Create a Java procedure name from its
    class_name method_name args_type_name return_type_name method_kind. */
let java: java_type => option java_type => string => list java_type => method_kind => java;


/** Replace the parameters of a java procname. */
let java_replace_parameters: java => list java_type => java;


/** Replace the method of a java procname. */
let java_replace_return_type: java => java_type => java;


/** Create an objc block name. */
let mangled_objc_block: string => t;


/** Create an objc procedure name from a class_name and method_name. */
let objc_cpp: string => string => objc_cpp_method_kind => objc_cpp;

let get_default_objc_class_method: string => t;


/** Get the class name of a Objective-C/C++ procedure name. */
let objc_cpp_get_class_name: objc_cpp => string;


/** Create ObjC method type from a bool is_instance. */
let objc_method_kind_of_bool: bool => objc_cpp_method_kind;


/** Return the class name of a java procedure name. */
let java_get_class_name: java => string;


/** Return the class name as a typename of a java procedure name. */
let java_get_class_type_name: java => Typename.t;


/** Return the simple class name of a java procedure name. */
let java_get_simple_class_name: java => string;


/** Return the package name of a java procedure name. */
let java_get_package: java => option string;


/** Return the method name of a java procedure name. */
let java_get_method: java => string;


/** Return the return type of a java procedure name. */
let java_get_return_type: java => string;


/** Return the parameters of a java procedure name. */
let java_get_parameters: java => list java_type;


/** Return the parameters of a java procname as strings. */
let java_get_parameters_as_strings: java => list string;


/** Check if the procedure name is an acess method (e.g. access$100 used to
    access private members from a nested class. */
let java_is_access_method: t => bool;


/** Check if the procedure name is of an auto-generated method containing '$'. */
let java_is_autogen_method: t => bool;


/** Check if the procedure belongs to an anonymous inner class. */
let java_is_anonymous_inner_class: t => bool;


/** Check if the procedure name is an anonymous inner class constructor. */
let java_is_anonymous_inner_class_constructor: t => bool;


/** Check if the method name is "close". */
let java_is_close: t => bool;


/** Check if the java procedure is static. */
let java_is_static: t => bool;


/** Check if the proc name has the type of a java vararg.
    Note: currently only checks that the last argument has type Object[]. */
let java_is_vararg: t => bool;


/** Check if the last parameter is a hidden inner class, and remove it if present.
    This is used in private constructors, where a proxy constructor is generated
    with an extra parameter and calls the normal constructor. */
let java_remove_hidden_inner_class_parameter: t => option t;


/** Replace the method name of an existing java procname. */
let java_replace_method: java => string => java;


/** Convert a java type to a string. */
let java_type_to_string: java_type => string;


/** Check if this is a class initializer. */
let is_class_initializer: t => bool;


/** Check if this is a special Infer undefined procedure. */
let is_infer_undefined: t => bool;


/** Return the name of the global for which this procedure is the initializer if this is an
    initializer, None otherwise. */
let get_global_name_of_initializer: t => option string;


/** Pretty print a proc name. */
let pp: Format.formatter => t => unit;


/** Pretty print a set of proc names. */
let pp_set: Format.formatter => Set.t => unit;


/** Replace the class name component of a procedure name.
    In case of Java, replace package and class name. */
let replace_class: t => string => t;


/** Given a package.class_name string, look for the latest dot and split the string
    in two (package, class_name). */
let split_classname: string => (option string, string);


/** Convert a proc name to a string for the user to see. */
let to_string: t => string;


/** Convert a proc name into a easy string for the user to see in an IDE. */
let to_simplified_string: withclass::bool? => t => string;


/** Convert a proc name into a unique identifier. */
let to_unique_id: t => string;


/** Convert a proc name to a filename. */
let to_filename: t => string;

let get_qualifiers: t => list string;
