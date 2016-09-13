/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** The Smallfoot Intermediate Language: Types */
let module F = Format;


/** Type to represent one @Annotation. */
type annotation = {
  class_name: string, /** name of the annotation */
  parameters: list string /** currently only one string parameter */
};


/** Compare function for annotations. */
let annotation_compare: annotation => annotation => int;


/** Pretty print an annotation. */
let pp_annotation: F.formatter => annotation => unit;

let module AnnotMap: PrettyPrintable.PPMap with type key = annotation;


/** Annotation for one item: a list of annotations with visibility. */
type item_annotation = list (annotation, bool);


/** Compare function for annotation items. */
let item_annotation_compare: item_annotation => item_annotation => int;


/** Pretty print an item annotation. */
let pp_item_annotation: F.formatter => item_annotation => unit;

let item_annotation_to_string: item_annotation => string;


/** Empty item annotation. */
let item_annotation_empty: item_annotation;


/** Check if the item annodation is empty. */
let item_annotation_is_empty: item_annotation => bool;

let objc_class_annotation: item_annotation;

let cpp_class_annotation: item_annotation;


/** Annotation for a method: return value and list of parameters. */
type method_annotation = (item_annotation, list item_annotation);


/** Compare function for Method annotations. */
let method_annotation_compare: method_annotation => method_annotation => int;


/** Empty method annotation. */
let method_annotation_empty: method_annotation;


/** Check if the method annodation is empty. */
let method_annotation_is_empty: method_annotation => bool;


/** Pretty print a method annotation. */
let pp_method_annotation: string => F.formatter => method_annotation => unit;


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
  | IU128 /** [__uint128_t] */;


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
  | FLongDouble /** [long double] */;


/** kind of pointer */
type ptr_kind =
  | Pk_pointer /** C/C++, Java, Objc standard/__strong pointer */
  | Pk_reference /** C++ reference */
  | Pk_objc_weak /** Obj-C __weak pointer */
  | Pk_objc_unsafe_unretained /** Obj-C __unsafe_unretained pointer */
  | Pk_objc_autoreleasing /** Obj-C __autoreleasing pointer */;


/** Comparision for ptr_kind */
let ptr_kind_compare: ptr_kind => ptr_kind => int;


/** statically determined length of an array type, if any */
type static_length = option IntLit.t;


/** types for sil (structured) expressions */
type t =
  | Tint of ikind /** integer type */
  | Tfloat of fkind /** float type */
  | Tvoid /** void type */
  | Tfun of bool /** function type with noreturn attribute */
  | Tptr of t ptr_kind /** pointer type */
  | Tstruct of Typename.t /** structured value type name */
  | Tarray of t static_length /** array type with statically fixed length */;

type struct_fields = list (Ident.fieldname, t, item_annotation);


/** Type for a structured value. */
type struct_typ = private {
  name: Typename.t, /** name */
  fields: struct_fields, /** non-static fields */
  statics: struct_fields, /** static fields */
  supers: list Typename.t, /** supers */
  methods: list Procname.t, /** methods defined */
  annots: item_annotation /** annotations */
};

type lookup = Typename.t => option struct_typ;


/** Comparision for fieldnames * types * item annotations. */
let fld_typ_ann_compare:
  (Ident.fieldname, t, item_annotation) => (Ident.fieldname, t, item_annotation) => int;

let struct_typ_equal: struct_typ => struct_typ => bool;


/** Comparision for types. */
let compare: t => t => int;


/** Equality for types. */
let equal: t => t => bool;

let pp_struct_typ: printenv => (F.formatter => unit => unit) => F.formatter => struct_typ => unit;


/** [pp_decl pe pp_base f typ] pretty prints a type declaration.
    pp_base prints the variable for a declaration, or can be skip to print only the type */
let pp_decl: printenv => (F.formatter => unit => unit) => F.formatter => t => unit;


/** Pretty print a type with all the details. */
let pp_full: printenv => F.formatter => t => unit;


/** Pretty print a type. */
let pp: printenv => F.formatter => t => unit;

let to_string: t => string;


/** Dump a type with all the details. */
let d_full: t => unit;


/** Dump a list of types. */
let d_list: list t => unit;


/** Sets of types. */
let module StructSet: Set.S with type elt = struct_typ;

let module Set: Set.S with type elt = t;


/** Maps with type keys. */
let module Map: Map.S with type key = t;

let module Tbl: Hashtbl.S with type key = t;


/** Construct a struct_typ, normalizing field types */
let internal_mk_struct:
  default::struct_typ? =>
  fields::struct_fields? =>
  statics::struct_fields? =>
  methods::list Procname.t? =>
  supers::list Typename.t? =>
  annots::item_annotation? =>
  Typename.t =>
  struct_typ;


/** The name of a type */
let name: t => option Typename.t;


/** turn a *T into a T. fails if [t] is not a pointer type */
let strip_ptr: t => t;


/** If an array type, return the type of the element.
    If not, return the default type if given, otherwise raise an exception */
let array_elem: option t => t => t;


/** the element typ of the final extensible array in the given typ, if any */
let get_extensible_array_element_typ: lookup::lookup => t => option t;


/** If a struct type with field f, return the type of f.
    If not, return the default type if given, otherwise raise an exception */
let struct_typ_fld: lookup::lookup => default::t => Ident.fieldname => t => t;


/** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] */
let get_field_type_and_annotation:
  lookup::lookup => Ident.fieldname => t => option (t, item_annotation);


/** if [struct_typ] is a class, return its class kind (Java, CPP, or Obj-C) */
let struct_typ_get_class_kind: struct_typ => option Csu.class_kind;

let is_objc_class: t => bool;

let is_cpp_class: t => bool;

let is_java_class: t => bool;

let is_array_of_cpp_class: t => bool;

let is_pointer_to_cpp_class: t => bool;

let has_block_prefix: string => bool;


/** Check if type is a type for a block in objc */
let is_block_type: t => bool;


/** Field used for objective-c reference counting */
let objc_ref_counter_field: (Ident.fieldname, t, item_annotation);

let is_objc_ref_counter_field: (Ident.fieldname, t, item_annotation) => bool;

let unsome: string => option t => t;


/** Return the return type of [pname_java]. */
let java_proc_return_typ: Procname.java => t;
