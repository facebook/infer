(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for Procedure Names. *)

(** Type of java procedure names. *)
type java

(** Type of c procedure names. *)
type c

(** Type of Objective C and C++ procedure names. *)
type objc_cpp

(** Type of Objective C block names. *)
type block

(** Type of procedure names. *)
type t =
  | Java of java
  | C of c
  | ObjC_Cpp of objc_cpp
  | Block of block

type java_type = string option * string

type method_kind =
  | Static (* in Java, procedures called with invokestatic *)
  | Non_Static (* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)

type objc_method_kind =
  | Instance_objc_method (* for instance methods in ObjC *)
  | Class_objc_method (* for class methods in ObjC *)

(** Hash tables with proc names as keys. *)
module Hash : Hashtbl.S with type key = t

(** Maps from proc names. *)
module Map : Map.S with type key = t

(** Sets of proc names. *)
module Set : Set.S with type elt = t

(** Create a C procedure name from plain and mangled name. *)
val c : string -> string -> c

(** Comparison for proc names. *)
val compare : t -> t -> int

(** Empty block name. *)
val empty_block : t

(** Equality for proc names. *)
val equal : t -> t -> bool

(** Convert a string to a proc name. *)
val from_string_c_fun : string -> t

(** Return the language of the procedure. *)
val get_language : t -> Config.language

(** Return the method/function of a procname. *)
val get_method : t -> string

(** Hash function for procname. *)
val hash_pname : t -> int

(** Check if a class string is an anoynmous inner class name. *)
val is_anonymous_inner_class_name : string -> bool

(** Check if this is an Objective-C/C++ method name. *)
val is_c_method : t -> bool

(** Check if this is a constructor. *)
val is_constructor : t -> bool

(** Check if this is a Java procedure name. *)
val is_java : t -> bool

(** Check if this is a dealloc method in Objective-C. *)
val is_objc_dealloc : t -> bool

(** Create a Java procedure name from its
    class_name method_name args_type_name return_type_name method_kind. *)
val java : java_type -> java_type option -> string -> java_type list -> method_kind -> java

(** Replace the parameters of a java procname. *)
val java_replace_parameters : java -> java_type list -> java

(** Replace the method of a java procname. *)
val java_replace_return_type : java -> java_type -> java

(** Create an objc block name. *)
val mangled_objc_block : string -> t

(** Mangled string for method types. *)
val mangled_of_objc_method_kind : objc_method_kind -> string option

(** Create an objc procedure name from a class_name and method_name. *)
val objc_cpp : string -> string -> string option -> objc_cpp

(** Get the class name of a Objective-C/C++ procedure name. *)
val objc_cpp_get_class_name : objc_cpp -> string

(** Create ObjC method type from a bool is_instance. *)
val objc_method_kind_of_bool : bool -> objc_method_kind

(** Return the class name of a java procedure name. *)
val java_get_class_name : java -> string

(** Return the simple class name of a java procedure name. *)
val java_get_simple_class_name : java -> string

(** Return the package name of a java procedure name. *)
val java_get_package : java -> string option

(** Return the method name of a java procedure name. *)
val java_get_method : java -> string

(** Return the return type of a java procedure name. *)
val java_get_return_type : java -> string

(** Return the parameters of a java procedure name. *)
val java_get_parameters : java -> java_type list

(** Return the parameters of a java procname as strings. *)
val java_get_parameters_as_strings : java -> string list

(** Check if the procedure name is an acess method (e.g. access$100 used to
    access private members from a nested class. *)
val java_is_access_method : t -> bool

(** Check if the procedure belongs to an anonymous inner class. *)
val java_is_anonymous_inner_class : t -> bool

(** Check if the procedure name is an anonymous inner class constructor. *)
val java_is_anonymous_inner_class_constructor : t -> bool

(** Check if the method name is "close". *)
val java_is_close : t -> bool

(** Check if the java procedure is static. *)
val java_is_static : t -> bool

(** Check if the proc name has the type of a java vararg.
    Note: currently only checks that the last argument has type Object[]. *)
val java_is_vararg : t -> bool

(** Check if the last parameter is a hidden inner class, and remove it if present.
    This is used in private constructors, where a proxy constructor is generated
    with an extra parameter and calls the normal constructor. *)
val java_remove_hidden_inner_class_parameter : t -> t option

(** Replace the method name of an existing java procname. *)
val java_replace_method : java -> string -> java

(** Convert a java type to a string. *)
val java_type_to_string : java_type -> string

(** Check if this is a class initializer. *)
val is_class_initializer : t -> bool

(** Check if this is a special Infer undefined procedure. *)
val is_infer_undefined : t -> bool

(** Pretty print a proc name. *)
val pp : Format.formatter -> t -> unit

(** Pretty print a set of proc names. *)
val pp_set : Format.formatter -> Set.t -> unit

(** Replace the class name component of a procedure name.
    In case of Java, replace package and class name. *)
val replace_class : t -> string -> t

(** Given a package.class_name string, look for the latest dot and split the string
    in two (package, class_name). *)
val split_classname : string -> string option * string

(** Convert a proc name to a string for the user to see. *)
val to_string : t -> string

(** Convert a proc name into a easy string for the user to see in an IDE. *)
val to_simplified_string : ?withclass: bool -> t -> string

(** Convert a proc name into a unique identifier. *)
val to_unique_id : t -> string

(** Convert a proc name to a filename. *)
val to_filename : t -> string
