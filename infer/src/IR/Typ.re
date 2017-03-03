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

let module Hashtbl = Caml.Hashtbl;


/** The Smallfoot Intermediate Language: Types */
let module L = Logging;

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
  | FLongDouble /** [long double] */
[@@deriving compare];


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
  | Pk_objc_autoreleasing /** Obj-C __autoreleasing pointer */
[@@deriving compare];

let equal_ptr_kind = [%compare.equal : ptr_kind];

let ptr_kind_string =
  fun
  | Pk_reference => "&"
  | Pk_pointer => "*"
  | Pk_objc_weak => "__weak *"
  | Pk_objc_unsafe_unretained => "__unsafe_unretained *"
  | Pk_objc_autoreleasing => "__autoreleasing *";


/** statically determined length of an array type, if any */
type static_length = option IntLit.t [@@deriving compare];

let module T = {

  /** types for sil (structured) expressions */
  type t =
    | Tint ikind /** integer type */
    | Tfloat fkind /** float type */
    | Tvoid /** void type */
    | Tfun bool /** function type with noreturn attribute */
    | Tptr t ptr_kind /** pointer type */
    | Tstruct Typename.t /** structured value type name */
    | Tarray t static_length /** array type with statically fixed length */
  [@@deriving compare];
  let equal = [%compare.equal : t];
  let hash = Hashtbl.hash;
};

include T;


/** {2 Sets and maps of types} */
let module Set = Caml.Set.Make T;

let module Map = Caml.Map.Make T;

let module Tbl = Hashtbl.Make T;


/** type comparison that treats T* [] and T** as the same type. Needed for C/C++ */
let array_sensitive_compare t1 t2 =>
  switch (t1, t2) {
  | (Tptr (Tptr ptr_typ _) _, Tarray (Tptr array_typ _) _) => compare ptr_typ array_typ
  | (Tarray (Tptr array_typ _) _, Tptr (Tptr ptr_typ _) _) => compare array_typ ptr_typ
  | _ => compare t1 t2
  };


/** Pretty print a type with all the details, using the C syntax. */
let rec pp_full pe f =>
  fun
  | Tstruct tname =>
    if (Pp.equal_print_kind pe.Pp.kind Pp.HTML) {
      F.fprintf f "%s" (Typename.to_string tname |> Escape.escape_xml)
    } else {
      F.fprintf f "%s" (Typename.to_string tname)
    }
  | Tint ik => F.fprintf f "%s" (ikind_to_string ik)
  | Tfloat fk => F.fprintf f "%s" (fkind_to_string fk)
  | Tvoid => F.fprintf f "void"
  | Tfun false => F.fprintf f "_fn_"
  | Tfun true => F.fprintf f "_fn_noreturn_"
  | Tptr ((Tarray _ | Tfun _) as typ) pk =>
    F.fprintf f "%a(%s)" (pp_full pe) typ (ptr_kind_string pk)
  | Tptr typ pk => F.fprintf f "%a%s" (pp_full pe) typ (ptr_kind_string pk)
  | Tarray typ static_len => {
      let pp_array_static_len fmt => (
        fun
        | Some static_len => IntLit.pp fmt static_len
        | None => F.fprintf fmt "_"
      );
      F.fprintf f "%a[%a]" (pp_full pe) typ pp_array_static_len static_len
    };


/** Pretty print a type. Do nothing by default. */
let pp pe f te =>
  if Config.print_types {
    pp_full pe f te
  } else {
    ()
  };

let to_string typ => {
  let pp fmt => pp_full Pp.text fmt typ;
  F.asprintf "%t" pp
};


/** dump a type with all the details. */
let d_full (t: t) => L.add_print_action (L.PTtyp_full, Obj.repr t);


/** dump a list of types. */
let d_list (tl: list t) => L.add_print_action (L.PTtyp_list, Obj.repr tl);

let name =
  fun
  | Tstruct name => Some name
  | _ => None;

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

let is_class_of_kind typ ck =>
  switch typ {
  | Tstruct (TN_csu (Class ck') _) => Csu.equal_class_kind ck ck'
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


/** Java types by name */
let rec java_from_string =
  fun
  | ""
  | "void" => Tvoid
  | "int" => Tint IInt
  | "byte" => Tint IShort
  | "short" => Tint IShort
  | "boolean" => Tint IBool
  | "char" => Tint IChar
  | "long" => Tint ILong
  | "float" => Tfloat FFloat
  | "double" => Tfloat FDouble
  | typ_str when String.contains typ_str '[' => {
      let stripped_typ = String.sub typ_str pos::0 len::(String.length typ_str - 2);
      Tptr (Tarray (java_from_string stripped_typ) None) Pk_pointer
    }
  | typ_str => Tstruct (Typename.Java.from_string typ_str);


/** Return the return type of [pname_java]. */
let java_proc_return_typ pname_java =>
  switch (java_from_string (Procname.java_get_return_type pname_java)) {
  | Tstruct _ as typ => Tptr typ Pk_pointer
  | typ => typ
  };

type typ = t;

let module Struct = {
  type field = (Ident.fieldname, T.t, Annot.Item.t) [@@deriving compare];
  type fields = list field;

  /** Type for a structured value. */
  type t = {
    fields: fields, /** non-static fields */
    statics: fields, /** static fields */
    supers: list Typename.t, /** superclasses */
    methods: list Procname.t, /** methods defined */
    annots: Annot.Item.t /** annotations */
  };
  type lookup = Typename.t => option t;
  let pp pe name f {fields, supers, methods, annots} =>
    if Config.debug_mode {
      /* change false to true to print the details of struct */
      F.fprintf
        f
        "%a \n\tfields: {%a\n\t}\n\tsupers: {%a\n\t}\n\tmethods: {%a\n\t}\n\tannots: {%a\n\t}"
        Typename.pp
        name
        (
          Pp.seq (
            fun f (fld, t, a) =>
              F.fprintf f "\n\t\t%a %a %a" (pp_full pe) t Ident.pp_fieldname fld Annot.Item.pp a
          )
        )
        fields
        (Pp.seq (fun f n => F.fprintf f "\n\t\t%a" Typename.pp n))
        supers
        (Pp.seq (fun f m => F.fprintf f "\n\t\t%a" Procname.pp m))
        methods
        Annot.Item.pp
        annots
    } else {
      F.fprintf f "%a" Typename.pp name
    };
  let internal_mk_struct
      default::default=?
      fields::fields=?
      statics::statics=?
      methods::methods=?
      supers::supers=?
      annots::annots=?
      () => {
    let mk_struct_
        default::
          default={fields: [], statics: [], methods: [], supers: [], annots: Annot.Item.empty}
        fields::fields=default.fields
        statics::statics=default.statics
        methods::methods=default.methods
        supers::supers=default.supers
        annots::annots=default.annots
        () => {
      fields,
      statics,
      methods,
      supers,
      annots
    };
    mk_struct_
      default::?default
      fields::?fields
      statics::?statics
      methods::?methods
      supers::?supers
      annots::?annots
      ()
  };

  /** the element typ of the final extensible array in the given typ, if any */
  let rec get_extensible_array_element_typ lookup::lookup (typ: T.t) =>
    switch typ {
    | Tarray typ _ => Some typ
    | Tstruct name =>
      switch (lookup name) {
      | Some {fields} =>
        switch (List.last fields) {
        | Some (_, fld_typ, _) => get_extensible_array_element_typ lookup::lookup fld_typ
        | None => None
        }
      | None => None
      }
    | _ => None
    };

  /** If a struct type with field f, return the type of f. If not, return the default */
  let fld_typ lookup::lookup default::default fn (typ: T.t) =>
    switch typ {
    | Tstruct name =>
      switch (lookup name) {
      | Some {fields} =>
        List.find f::(fun (f, _, _) => Ident.equal_fieldname f fn) fields |>
        Option.value_map f::snd3 default::default
      | None => default
      }
    | _ => default
    };
  let get_field_type_and_annotation lookup::lookup fn (typ: T.t) =>
    switch typ {
    | Tstruct name
    | Tptr (Tstruct name) _ =>
      switch (lookup name) {
      | Some {fields, statics} =>
        List.find_map
          f::(fun (f, t, a) => Ident.equal_fieldname f fn ? Some (t, a) : None) (fields @ statics)
      | None => None
      }
    | _ => None
    };
  let objc_ref_counter_annot = [({Annot.class_name: "ref_counter", parameters: []}, false)];

  /** Field used for objective-c reference counting */
  let objc_ref_counter_field = (Ident.fieldname_hidden, T.Tint IInt, objc_ref_counter_annot);
  let is_objc_ref_counter_field (fld, _, a) =>
    Ident.fieldname_is_hidden fld && Annot.Item.equal a objc_ref_counter_annot;
};
