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
let module L = Logging;

let module F = Format;


/** Type to represent one @Annotation. */
type annotation = {
  class_name: string, /** name of the annotation */
  parameters: list string /** currently only one string parameter */
};


/** Compare function for annotations. */
let annotation_compare a1 a2 => {
  let n = string_compare a1.class_name a2.class_name;
  if (n != 0) {
    n
  } else {
    IList.compare string_compare a1.parameters a2.parameters
  }
};


/** Pretty print an annotation. */
let pp_annotation fmt annotation => F.fprintf fmt "@@%s" annotation.class_name;

let module AnnotMap = PrettyPrintable.MakePPMap {
  type t = annotation;
  let compare = annotation_compare;
  let pp_key = pp_annotation;
};


/** Annotation for one item: a list of annotations with visibility. */
type item_annotation = list (annotation, bool);


/** Compare function for annotation items. */
let item_annotation_compare ia1 ia2 => {
  let cmp (a1, b1) (a2, b2) => {
    let n = annotation_compare a1 a2;
    if (n != 0) {
      n
    } else {
      bool_compare b1 b2
    }
  };
  IList.compare cmp ia1 ia2
};


/** Pretty print an item annotation. */
let pp_item_annotation fmt item_annotation => {
  let pp fmt (a, _) => pp_annotation fmt a;
  F.fprintf fmt "<%a>" (pp_seq pp) item_annotation
};

let item_annotation_to_string ann => {
  let pp fmt () => pp_item_annotation fmt ann;
  pp_to_string pp ()
};


/** Empty item annotation. */
let item_annotation_empty = [];


/** Check if the item annodation is empty. */
let item_annotation_is_empty ia => ia == [];

let objc_class_str = "ObjC-Class";

let cpp_class_str = "Cpp-Class";

let class_annotation class_string => [({class_name: class_string, parameters: []}, true)];

let objc_class_annotation = class_annotation objc_class_str;

let cpp_class_annotation = class_annotation cpp_class_str;


/** Annotation for a method: return value and list of parameters. */
type method_annotation = (item_annotation, list item_annotation);


/** Compare function for Method annotations. */
let method_annotation_compare (ia1, ial1) (ia2, ial2) =>
  IList.compare item_annotation_compare [ia1, ...ial1] [ia2, ...ial2];


/** Pretty print a method annotation. */
let pp_method_annotation s fmt (ia, ial) =>
  F.fprintf fmt "%a %s(%a)" pp_item_annotation ia s (pp_seq pp_item_annotation) ial;


/** Empty method annotation. */
let method_annotation_empty = ([], []);


/** Check if the method annodation is empty. */
let method_annotation_is_empty (ia, ial) => IList.for_all item_annotation_is_empty [ia, ...ial];


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


/** comparison for ikind */
let ikind_compare k1 k2 =>
  switch (k1, k2) {
  | (IChar, IChar) => 0
  | (IChar, _) => (-1)
  | (_, IChar) => 1
  | (ISChar, ISChar) => 0
  | (ISChar, _) => (-1)
  | (_, ISChar) => 1
  | (IUChar, IUChar) => 0
  | (IUChar, _) => (-1)
  | (_, IUChar) => 1
  | (IBool, IBool) => 0
  | (IBool, _) => (-1)
  | (_, IBool) => 1
  | (IInt, IInt) => 0
  | (IInt, _) => (-1)
  | (_, IInt) => 1
  | (IUInt, IUInt) => 0
  | (IUInt, _) => (-1)
  | (_, IUInt) => 1
  | (IShort, IShort) => 0
  | (IShort, _) => (-1)
  | (_, IShort) => 1
  | (IUShort, IUShort) => 0
  | (IUShort, _) => (-1)
  | (_, IUShort) => 1
  | (ILong, ILong) => 0
  | (ILong, _) => (-1)
  | (_, ILong) => 1
  | (IULong, IULong) => 0
  | (IULong, _) => (-1)
  | (_, IULong) => 1
  | (ILongLong, ILongLong) => 0
  | (ILongLong, _) => (-1)
  | (_, ILongLong) => 1
  | (IULongLong, IULongLong) => 0
  | (IULongLong, _) => (-1)
  | (_, IULongLong) => 1
  | (I128, I128) => 0
  | (I128, _) => (-1)
  | (_, I128) => 1
  | (IU128, IU128) => 0
  };

let ikind_to_string =
  fun
  | IChar => "char"
  | ISChar => "signed char"
  | IUChar => "unsigned char"
  | IBool => "_Bool"
  | IInt => "int"
  | IUInt => "unsigned int"
  | IShort => "short"
  | IUShort => "unsigned short"
  | ILong => "long"
  | IULong => "unsigned long"
  | ILongLong => "long long"
  | IULongLong => "unsigned long long"
  | I128 => "__int128_t"
  | IU128 => "__uint128_t";

let ikind_is_char =
  fun
  | IChar
  | ISChar
  | IUChar => true
  | _ => false;

let ikind_is_unsigned =
  fun
  | IUChar
  | IUInt
  | IUShort
  | IULong
  | IULongLong => true
  | _ => false;

let int_of_int64_kind i ik => IntLit.of_int64_unsigned i (ikind_is_unsigned ik);


/** Kinds of floating-point numbers */
type fkind =
  | FFloat /** [float] */
  | FDouble /** [double] */
  | FLongDouble /** [long double] */;


/** comparison for fkind */
let fkind_compare k1 k2 =>
  switch (k1, k2) {
  | (FFloat, FFloat) => 0
  | (FFloat, _) => (-1)
  | (_, FFloat) => 1
  | (FDouble, FDouble) => 0
  | (FDouble, _) => (-1)
  | (_, FDouble) => 1
  | (FLongDouble, FLongDouble) => 0
  };

let fkind_to_string =
  fun
  | FFloat => "float"
  | FDouble => "double"
  | FLongDouble => "long double";


/** kind of pointer */
type ptr_kind =
  | Pk_pointer /** C/C++, Java, Objc standard/__strong pointer */
  | Pk_reference /** C++ reference */
  | Pk_objc_weak /** Obj-C __weak pointer */
  | Pk_objc_unsafe_unretained /** Obj-C __unsafe_unretained pointer */
  | Pk_objc_autoreleasing /** Obj-C __autoreleasing pointer */;

let ptr_kind_compare pk1 pk2 =>
  switch (pk1, pk2) {
  | (Pk_pointer, Pk_pointer) => 0
  | (Pk_pointer, _) => (-1)
  | (_, Pk_pointer) => 1
  | (Pk_reference, Pk_reference) => 0
  | (_, Pk_reference) => (-1)
  | (Pk_reference, _) => 1
  | (Pk_objc_weak, Pk_objc_weak) => 0
  | (Pk_objc_weak, _) => (-1)
  | (_, Pk_objc_weak) => 1
  | (Pk_objc_unsafe_unretained, Pk_objc_unsafe_unretained) => 0
  | (Pk_objc_unsafe_unretained, _) => (-1)
  | (_, Pk_objc_unsafe_unretained) => 1
  | (Pk_objc_autoreleasing, Pk_objc_autoreleasing) => 0
  };

let ptr_kind_string =
  fun
  | Pk_reference => "&"
  | Pk_pointer => "*"
  | Pk_objc_weak => "__weak *"
  | Pk_objc_unsafe_unretained => "__unsafe_unretained *"
  | Pk_objc_autoreleasing => "__autoreleasing *";


/** statically determined length of an array type, if any */
type static_length = option IntLit.t;

type struct_fields = list (Ident.fieldname, t, item_annotation)
/** Type for a structured value. */
and struct_typ = {
  instance_fields: struct_fields, /** non-static fields */
  static_fields: struct_fields, /** static fields */
  csu: Csu.t, /** class/struct/union */
  struct_name: option Mangled.t, /** name */
  superclasses: list Typename.t, /** list of superclasses */
  def_methods: list Procname.t, /** methods defined */
  struct_annotations: item_annotation /** annotations */
}
/** types for sil (structured) expressions */
and t =
  | Tvar of Typename.t /** named type */
  | Tint of ikind /** integer type */
  | Tfloat of fkind /** float type */
  | Tvoid /** void type */
  | Tfun of bool /** function type with noreturn attribute */
  | Tptr of t ptr_kind /** pointer type */
  | Tstruct of struct_typ /** Type for a structured value */
  | Tarray of t static_length /** array type with statically fixed length */;

let cname_opt_compare nameo1 nameo2 =>
  switch (nameo1, nameo2) {
  | (None, None) => 0
  | (None, _) => (-1)
  | (_, None) => 1
  | (Some n1, Some n2) => Mangled.compare n1 n2
  };

let rec fld_typ_ann_compare fta1 fta2 =>
  triple_compare Ident.fieldname_compare compare item_annotation_compare fta1 fta2
and fld_typ_ann_list_compare ftal1 ftal2 => IList.compare fld_typ_ann_compare ftal1 ftal2
and struct_typ_compare struct_typ1 struct_typ2 =>
  if (struct_typ1.csu == Csu.Class Csu.Java && struct_typ2.csu == Csu.Class Csu.Java) {
    cname_opt_compare struct_typ1.struct_name struct_typ2.struct_name
  } else {
    let n = fld_typ_ann_list_compare struct_typ1.instance_fields struct_typ2.instance_fields;
    if (n != 0) {
      n
    } else {
      let n = fld_typ_ann_list_compare struct_typ1.static_fields struct_typ2.static_fields;
      if (n != 0) {
        n
      } else {
        let n = Csu.compare struct_typ1.csu struct_typ2.csu;
        if (n != 0) {
          n
        } else {
          cname_opt_compare struct_typ1.struct_name struct_typ2.struct_name
        }
      }
    }
  }
/** Comparision for types. */
and compare t1 t2 =>
  if (t1 === t2) {
    0
  } else {
    switch (t1, t2) {
    | (Tvar tn1, Tvar tn2) => Typename.compare tn1 tn2
    | (Tvar _, _) => (-1)
    | (_, Tvar _) => 1
    | (Tint ik1, Tint ik2) => ikind_compare ik1 ik2
    | (Tint _, _) => (-1)
    | (_, Tint _) => 1
    | (Tfloat fk1, Tfloat fk2) => fkind_compare fk1 fk2
    | (Tfloat _, _) => (-1)
    | (_, Tfloat _) => 1
    | (Tvoid, Tvoid) => 0
    | (Tvoid, _) => (-1)
    | (_, Tvoid) => 1
    | (Tfun noreturn1, Tfun noreturn2) => bool_compare noreturn1 noreturn2
    | (Tfun _, _) => (-1)
    | (_, Tfun _) => 1
    | (Tptr t1' pk1, Tptr t2' pk2) =>
      let n = compare t1' t2';
      if (n != 0) {
        n
      } else {
        ptr_kind_compare pk1 pk2
      }
    | (Tptr _, _) => (-1)
    | (_, Tptr _) => 1
    | (Tstruct struct_typ1, Tstruct struct_typ2) => struct_typ_compare struct_typ1 struct_typ2
    | (Tstruct _, _) => (-1)
    | (_, Tstruct _) => 1
    | (Tarray t1 _, Tarray t2 _) => compare t1 t2
    }
  };

let struct_typ_equal struct_typ1 struct_typ2 => struct_typ_compare struct_typ1 struct_typ2 == 0;

let equal t1 t2 => compare t1 t2 == 0;

let rec pp_struct_typ pe pp_base f struct_typ =>
  switch struct_typ.struct_name {
  | Some name when false =>
    /* remove "when false" to print the details of struct */
    F.fprintf
      f
      "%s %a {%a} %a"
      (Csu.name struct_typ.csu)
      Mangled.pp
      name
      (pp_seq (fun f (fld, t, _) => F.fprintf f "%a %a" (pp_full pe) t Ident.pp_fieldname fld))
      struct_typ.instance_fields
      pp_base
      ()
  | Some name => F.fprintf f "%s %a %a" (Csu.name struct_typ.csu) Mangled.pp name pp_base ()
  | None =>
    F.fprintf
      f
      "%s {%a} %a"
      (Csu.name struct_typ.csu)
      (pp_seq (fun f (fld, t, _) => F.fprintf f "%a %a" (pp_full pe) t Ident.pp_fieldname fld))
      struct_typ.instance_fields
      pp_base
      ()
  }
/** Pretty print a type declaration.
    pp_base prints the variable for a declaration, or can be skip to print only the type */
and pp_decl pe pp_base f =>
  fun
  | Tvar tname => F.fprintf f "%s %a" (Typename.to_string tname) pp_base ()
  | Tint ik => F.fprintf f "%s %a" (ikind_to_string ik) pp_base ()
  | Tfloat fk => F.fprintf f "%s %a" (fkind_to_string fk) pp_base ()
  | Tvoid => F.fprintf f "void %a" pp_base ()
  | Tfun false => F.fprintf f "_fn_ %a" pp_base ()
  | Tfun true => F.fprintf f "_fn_noreturn_ %a" pp_base ()
  | Tptr ((Tarray _ | Tfun _) as typ) pk => {
      let pp_base' fmt () => F.fprintf fmt "(%s%a)" (ptr_kind_string pk) pp_base ();
      pp_decl pe pp_base' f typ
    }
  | Tptr typ pk => {
      let pp_base' fmt () => F.fprintf fmt "%s%a" (ptr_kind_string pk) pp_base ();
      pp_decl pe pp_base' f typ
    }
  | Tstruct struct_typ => pp_struct_typ pe pp_base f struct_typ
  | Tarray typ static_len => {
      let pp_array_static_len fmt => (
        fun
        | Some static_len => IntLit.pp fmt static_len
        | None => F.fprintf fmt "_"
      );
      let pp_base' fmt () => F.fprintf fmt "%a[%a]" pp_base () pp_array_static_len static_len;
      pp_decl pe pp_base' f typ
    }
/** Pretty print a type with all the details, using the C syntax. */
and pp_full pe => pp_decl pe (fun _ () => ())
/** Pretty print a type. Do nothing by default. */
and pp pe f te =>
  if Config.print_types {
    pp_full pe f te
  } else {
    ()
  };

let to_string typ => {
  let pp fmt () => pp_full pe_text fmt typ;
  pp_to_string pp ()
};


/** dump a type with all the details. */
let d_full (t: t) => L.add_print_action (L.PTtyp_full, Obj.repr t);


/** dump a list of types. */
let d_list (tl: list t) => L.add_print_action (L.PTtyp_list, Obj.repr tl);


/** {2 Sets and maps of types} */
let module StructSet = Set.Make {
  type t = struct_typ;
  let compare = struct_typ_compare;
};

let module Set = Set.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Map = Map.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Tbl = Hashtbl.Make {
  type nonrec t = t;
  let equal = equal;
  let hash = Hashtbl.hash;
};

let unsome s =>
  fun
  | Some default_typ => default_typ
  | None => {
      L.err "No default typ in %s@." s;
      assert false
    };


/** turn a *T into a T. fails if [typ] is not a pointer type */
let strip_ptr =
  fun
  | Tptr t _ => t
  | _ => assert false;


/** If an array type, return the type of the element.
    If not, return the default type if given, otherwise raise an exception */
let array_elem default_opt =>
  fun
  | Tarray t_el _ => t_el
  | _ => unsome "array_elem" default_opt;


/** the element typ of the final extensible array in the given typ, if any */
let rec get_extensible_array_element_typ =
  fun
  | Tarray typ _ => Some typ
  | Tstruct {instance_fields} =>
    Option.map_default
      (fun (_, fld_typ, _) => get_extensible_array_element_typ fld_typ)
      None
      (IList.last instance_fields)
  | _ => None;


/** If a struct type with field f, return the type of f.
    If not, return the default type if given, otherwise raise an exception */
let struct_typ_fld default_opt f => {
  let def () => unsome "struct_typ_fld" default_opt;
  fun
  | Tstruct struct_typ =>
    try (
      (fun (_, y, _) => y) (
        IList.find (fun (_f, _, _) => Ident.fieldname_equal _f f) struct_typ.instance_fields
      )
    ) {
    | Not_found => def ()
    }
  | _ => def ()
};


/** if [struct_typ] is a class, return its class kind (Java, CPP, or Obj-C) */
let struct_typ_get_class_kind struct_typ =>
  switch struct_typ.csu {
  | Csu.Class class_kind => Some class_kind
  | _ => None
  };


/** return true if [struct_typ] is a Java class */
let struct_typ_is_java_class struct_typ =>
  switch (struct_typ_get_class_kind struct_typ) {
  | Some Csu.Java => true
  | _ => false
  };


/** return true if [struct_typ] is a C++ class. Note that this returns false for raw structs. */
let struct_typ_is_cpp_class struct_typ =>
  switch (struct_typ_get_class_kind struct_typ) {
  | Some Csu.CPP => true
  | _ => false
  };


/** return true if [struct_typ] is an Obj-C class. Note that this returns false for raw structs. */
let struct_typ_is_objc_class struct_typ =>
  switch (struct_typ_get_class_kind struct_typ) {
  | Some Csu.Objc => true
  | _ => false
  };

let is_class_of_kind typ ck =>
  switch typ {
  | Tstruct {csu: Csu.Class ck'} => ck == ck'
  | _ => false
  };

let is_objc_class typ => is_class_of_kind typ Csu.Objc;

let is_cpp_class typ => is_class_of_kind typ Csu.CPP;

let is_java_class typ => is_class_of_kind typ Csu.Java;

let rec is_array_of_cpp_class typ =>
  switch typ {
  | Tarray typ _ => is_array_of_cpp_class typ
  | _ => is_cpp_class typ
  };

let is_pointer_to_cpp_class typ =>
  switch typ {
  | Tptr t _ => is_cpp_class t
  | _ => false
  };

let has_block_prefix s =>
  switch (Str.split_delim (Str.regexp_string Config.anonymous_block_prefix) s) {
  | [_, _, ..._] => true
  | _ => false
  };


/** Check if type is a type for a block in objc */
let is_block_type typ => has_block_prefix (to_string typ);

let objc_ref_counter_annot = [({class_name: "ref_counter", parameters: []}, false)];


/** Field used for objective-c reference counting */
let objc_ref_counter_field = (Ident.fieldname_hidden, Tint IInt, objc_ref_counter_annot);

let is_objc_ref_counter_field (fld, _, a) =>
  Ident.fieldname_is_hidden fld && item_annotation_compare a objc_ref_counter_annot == 0;
