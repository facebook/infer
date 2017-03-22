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


/** The Smallfoot Intermediate Language: Types */
let module F = Format;


/** Kinds of integers */
type ikind =
  | IChar /** [char] */
  | ISChar /** [signed char] */
  | IUChar /** [unsigned char] */
  | IBool /** [bool] */
  | IInt /** [int] */
  | IUInt /** [unsigned int] */
  | IShort /** [short] */
  | IUShort /** [unsigned short] */
  | ILong /** [long] */
  | IULong /** [unsigned long] */
  | ILongLong /** [long long] (or [_int64] on Microsoft Visual C) */
  | IULongLong /** [unsigned long long] (or [unsigned _int64] on Microsoft Visual C) */
  | I128 /** [__int128_t] */
  | IU128 /** [__uint128_t] */
[@@deriving compare];


/** Check wheter the integer kind is a char */
let ikind_is_char: ikind => bool;


/** Check wheter the integer kind is unsigned */
let ikind_is_unsigned: ikind => bool;


/** Convert an int64 into an IntLit.t given the kind:
    the int64 is interpreted as unsigned according to the kind */
let int_of_int64_kind: int64 => ikind => IntLit.t;


/** Kinds of floating-point numbers */
type fkind =
  | FFloat /** [float] */
  | FDouble /** [double] */
  | FLongDouble /** [long double] */
[@@deriving compare];


/** kind of pointer */
type ptr_kind =
  | Pk_pointer /** C/C++, Java, Objc standard/__strong pointer */
  | Pk_reference /** C++ reference */
  | Pk_objc_weak /** Obj-C __weak pointer */
  | Pk_objc_unsafe_unretained /** Obj-C __unsafe_unretained pointer */
  | Pk_objc_autoreleasing /** Obj-C __autoreleasing pointer */
[@@deriving compare];

let equal_ptr_kind: ptr_kind => ptr_kind => bool;


/** statically determined length of an array type, if any */
type static_length = option IntLit.t [@@deriving compare];


/** types for sil (structured) expressions */
type t =
  | Tint ikind /** integer type */
  | Tfloat fkind /** float type */
  | Tvoid /** void type */
  | Tfun bool /** function type with noreturn attribute */
  | Tptr t ptr_kind /** pointer type */
  | Tstruct name /** structured value type name */
  | Tarray t static_length /** array type with statically fixed length */
[@@deriving compare]
and name =
  | CStruct Mangled.t
  | CUnion Mangled.t
  | CppClass Mangled.t template_spec_info
  | JavaClass Mangled.t
  | ObjcClass Mangled.t
  | ObjcProtocol Mangled.t
[@@deriving compare]
and template_spec_info =
  | NoTemplate
  | Template (string, list (option t))
[@@deriving compare];

let module Name: {

  /** Named types. */
  type t = name [@@deriving compare];

  /** Equality for typenames */
  let equal: t => t => bool;

  /** convert the typename to a string */
  let to_string: t => string;
  let pp: Format.formatter => t => unit;

  /** [is_class name] holds if [name] names CPP/Objc/Java class */
  let is_class: t => bool;

  /** [is_class name1 name2] holds if [name1] and [name2] name same kind of type */
  let is_same_type: t => t => bool;

  /** name of the typename without qualifier */
  let name: t => string;
  let module C: {let from_string: string => t; let union_from_string: string => t;};
  let module Java: {

    /** Create a typename from a Java classname in the form "package.class" */
    let from_string: string => t;

    /** Create a typename from a package name and a class name */
    let from_package_class: string => string => t;

    /** [is_class name] holds if [name] names a Java class */
    let is_class: t => bool;
    let java_lang_object: t;
    let java_io_serializable: t;
    let java_lang_cloneable: t;
  };
  let module Cpp: {

    /** Create a typename from a C++ classname */
    let from_string: string => t;
    let from_template_string: template_spec_info => string => t;

    /** [is_class name] holds if [name] names a C++ class */
    let is_class: t => bool;
  };
  let module Objc: {

    /** Create a typename from a Objc classname */
    let from_string: string => t;
    let protocol_from_string: string => t;

    /** [is_class name] holds if [name] names a Objc class */
    let is_class: t => bool;
  };
  let module Set: Caml.Set.S with type elt = t;
};


/** Equality for types. */
let equal: t => t => bool;


/** Sets of types. */
let module Set: Caml.Set.S with type elt = t;


/** Maps with type keys. */
let module Map: Caml.Map.S with type key = t;

let module Tbl: Caml.Hashtbl.S with type key = t;


/** type comparison that treats T* [] and T** as the same type. Needed for C/C++ */
let array_sensitive_compare: t => t => int;


/** Pretty print a type with all the details. */
let pp_full: Pp.env => F.formatter => t => unit;


/** Pretty print a type. */
let pp: Pp.env => F.formatter => t => unit;

let to_string: t => string;


/** Dump a type with all the details. */
let d_full: t => unit;


/** Dump a list of types. */
let d_list: list t => unit;


/** The name of a type */
let name: t => option Name.t;


/** turn a *T into a T. fails if [t] is not a pointer type */
let strip_ptr: t => t;


/** If an array type, return the type of the element.
    If not, return the default type if given, otherwise raise an exception */
let array_elem: option t => t => t;

let is_objc_class: t => bool;

let is_cpp_class: t => bool;

let is_java_class: t => bool;

let is_array_of_cpp_class: t => bool;

let is_pointer_to_cpp_class: t => bool;

let has_block_prefix: string => bool;


/** Check if type is a type for a block in objc */
let is_block_type: t => bool;

let unsome: string => option t => t;

type typ = t;

let module Procname: {

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
  let c: string => string => template_spec_info => c;

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
  let is_anonymous_inner_class_name: Name.t => bool;

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
  let java: Name.t => option java_type => string => list java_type => method_kind => java;

  /** Replace the parameters of a java procname. */
  let java_replace_parameters: java => list java_type => java;

  /** Replace the method of a java procname. */
  let java_replace_return_type: java => java_type => java;

  /** Create an objc block name. */
  let mangled_objc_block: string => t;

  /** Create an objc procedure name from a class_name and method_name. */
  let objc_cpp: Name.t => string => objc_cpp_method_kind => template_spec_info => objc_cpp;
  let get_default_objc_class_method: Name.t => t;

  /** Get the class name of a Objective-C/C++ procedure name. */
  let objc_cpp_get_class_name: objc_cpp => string;
  let objc_cpp_get_class_type_name: objc_cpp => Name.t;

  /** Create ObjC method type from a bool is_instance. */
  let objc_method_kind_of_bool: bool => objc_cpp_method_kind;

  /** Return the class name of a java procedure name. */
  let java_get_class_name: java => string;

  /** Return the class name as a typename of a java procedure name. */
  let java_get_class_type_name: java => Name.t;

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

  /** Check if the proc name comes from a lambda expression */
  let java_is_lambda: t => bool;

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
  let replace_class: t => Name.t => t;

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

  /** get qualifiers of C/objc/C++ method/function */
  let get_qualifiers: t => QualifiedCppName.t;

  /** get qualifiers of a class owning objc/C++ method */
  let objc_cpp_get_class_qualifiers: objc_cpp => QualifiedCppName.t;
};


/** Return the return type of [pname_java]. */
let java_proc_return_typ: Procname.java => t;

let module Struct: {
  type field = (Fieldname.t, typ, Annot.Item.t) [@@deriving compare];
  type fields = list field;

  /** Type for a structured value. */
  type t = private {
    fields: fields, /** non-static fields */
    statics: fields, /** static fields */
    supers: list Name.t, /** supers */
    methods: list Procname.t, /** methods defined */
    annots: Annot.Item.t, /** annotations */
    specialization: template_spec_info /** template specialization */
  };
  type lookup = Name.t => option t;

  /** Pretty print a struct type. */
  let pp: Pp.env => Name.t => F.formatter => t => unit;

  /** Construct a struct_typ, normalizing field types */
  let internal_mk_struct:
    default::t? =>
    fields::fields? =>
    statics::fields? =>
    methods::list Procname.t? =>
    supers::list Name.t? =>
    annots::Annot.Item.t? =>
    specialization::template_spec_info? =>
    unit =>
    t;

  /** the element typ of the final extensible array in the given typ, if any */
  let get_extensible_array_element_typ: lookup::lookup => typ => option typ;

  /** If a struct type with field f, return the type of f.
      If not, return the default type if given, otherwise raise an exception */
  let fld_typ: lookup::lookup => default::typ => Fieldname.t => typ => typ;

  /** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] */
  let get_field_type_and_annotation:
    lookup::lookup => Fieldname.t => typ => option (typ, Annot.Item.t);

  /** Field used for objective-c reference counting */
  let objc_ref_counter_field: (Fieldname.t, typ, Annot.Item.t);
  let is_objc_ref_counter_field: (Fieldname.t, typ, Annot.Item.t) => bool;
};
