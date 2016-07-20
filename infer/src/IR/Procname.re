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


/** Module for Procedure Names */
let module L = Logging;

let module F = Format;

type java_type = (option string, string); /* e.g. ("", "int") for primitive types or ("java.io", "PrintWriter") for objects */

type method_kind =
  | Static /* in Java, procedures called with invokestatic */
  | Non_Static /* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface */;


/** Type of java procedure names. */
type java = {
  class_name: java_type,
  return_type: option java_type, /* option because constructors have no return type */
  method_name: string,
  parameters: list java_type,
  kind: method_kind
};


/** Type of c procedure names. */
type c = (string, option string);

type objc_cpp_method_kind =
  | CPPMethod of (option string) /** with mangling */
  | CPPConstructor of (option string) /** with mangling */
  | ObjCInstanceMethod
  | ObjCInternalMethod
  | ObjCClassMethod;


/** Type of Objective C and C++ procedure names: method signatures. */
type objc_cpp = {class_name: string, method_name: string, kind: objc_cpp_method_kind};


/** Type of Objective C block names. */
type block = string;


/** Type of procedure names. */
type t = | Java of java | C of c | ObjC_Cpp of objc_cpp | Block of block;


/** Level of verbosity of some to_string functions. */
type detail_level = | Verbose | Non_verbose | Simple;

let objc_method_kind_of_bool is_instance =>
  if is_instance {
    ObjCInstanceMethod
  } else {
    ObjCClassMethod
  };

let empty_block = Block "";

let is_verbose v =>
  switch v {
  | Verbose => true
  | _ => false
  };

type proc_name = t;

let mangled_compare so1 so2 =>
  switch (so1, so2) {
  | (None, None) => 0
  | (None, Some _) => (-1)
  | (Some _, None) => 1
  | (Some s1, Some s2) => string_compare s1 s2
  };

let method_kind_compare k0 k1 =>
  switch (k0, k1) {
  | _ when k0 == k1 => 0
  | (Static, _) => 1
  | (Non_Static, _) => (-1)
  };


/** A type is a pair (package, type_name) that is translated in a string package.type_name */
let java_type_to_string_verbosity p verbosity =>
  switch p {
  | (None, typ) => typ
  | (Some p, cls) =>
    if (is_verbose verbosity) {
      p ^ "." ^ cls
    } else {
      cls
    }
  };

let java_type_to_string p => java_type_to_string_verbosity p Verbose;


/** Given a list of types, it creates a unique string of types separated by commas */
let rec java_param_list_to_string inputList verbosity =>
  switch inputList {
  | [] => ""
  | [head] => java_type_to_string_verbosity head verbosity
  | [head, ...rest] =>
    java_type_to_string_verbosity head verbosity ^ "," ^ java_param_list_to_string rest verbosity
  };


/** It is the same as java_type_to_string, but Java return types are optional because of constructors without type */
let java_return_type_to_string j verbosity =>
  switch j.return_type {
  | None => ""
  | Some typ => java_type_to_string_verbosity typ verbosity
  };

let java_type_compare (p1, c1) (p2, c2) => string_compare c1 c2 |> next mangled_compare p1 p2;

let rec java_type_list_compare jt1 jt2 =>
  switch (jt1, jt2) {
  | ([], []) => 0
  | ([], _) => (-1)
  | (_, []) => 1
  | ([x1, ...rest1], [x2, ...rest2]) =>
    java_type_compare x1 x2 |> next java_type_list_compare rest1 rest2
  };

let java_return_type_compare jr1 jr2 =>
  switch (jr1, jr2) {
  | (None, None) => 0
  | (None, Some _) => (-1)
  | (Some _, None) => 1
  | (Some jt1, Some jt2) => java_type_compare jt1 jt2
  };


/** Compare java procedure names. */
let java_compare (j1: java) (j2: java) =>
  string_compare j1.method_name j2.method_name |>
    next java_type_list_compare j1.parameters j2.parameters |>
    next java_type_compare j1.class_name j2.class_name |>
    next java_return_type_compare j1.return_type j2.return_type |>
    next method_kind_compare j1.kind j2.kind;

let objc_cpp_method_kind_compare k1 k2 =>
  switch (k1, k2) {
  | (CPPMethod mangled1, CPPMethod mangled2) => mangled_compare mangled1 mangled2
  | (CPPMethod _, _) => (-1)
  | (_, CPPMethod _) => 1
  | (CPPConstructor mangled1, CPPConstructor mangled2) => mangled_compare mangled1 mangled2
  | (CPPConstructor _, _) => (-1)
  | (_, CPPConstructor _) => 1
  | (ObjCClassMethod, ObjCClassMethod) => 0
  | (ObjCClassMethod, _) => (-1)
  | (_, ObjCClassMethod) => 1
  | (ObjCInstanceMethod, ObjCInstanceMethod) => 0
  | (ObjCInstanceMethod, _) => (-1)
  | (_, ObjCInstanceMethod) => 1
  | (ObjCInternalMethod, ObjCInternalMethod) => 0
  };


/** Compare c_method signatures. */
let c_meth_sig_compare osig1 osig2 =>
  string_compare osig1.method_name osig2.method_name |>
    next string_compare osig1.class_name osig2.class_name |>
    next objc_cpp_method_kind_compare osig1.kind osig2.kind;


/** Given a package.class_name string, it looks for the latest dot and split the string
    in two (package, class_name) */
let split_classname package_classname => string_split_character package_classname '.';

let from_string_c_fun (s: string) => C (s, None);

let c (plain: string) (mangled: string) => (plain, Some mangled);

let java class_name return_type method_name parameters kind => {
  class_name,
  return_type,
  method_name,
  parameters,
  kind
};


/** Create an objc procedure name from a class_name and method_name. */
let objc_cpp class_name method_name kind => {class_name, method_name, kind};

let get_default_objc_class_method objc_class => {
  let objc_cpp = objc_cpp objc_class "__find_class_" ObjCInternalMethod;
  ObjC_Cpp objc_cpp
};


/** Create an objc procedure name from a class_name and method_name. */
let mangled_objc_block name => Block name;

let is_java =
  fun
  | Java _ => true
  | _ => false;

let is_c_method =
  fun
  | ObjC_Cpp _ => true
  | _ => false;


/** Replace the class name component of a procedure name.
    In case of Java, replace package and class name. */
let replace_class t new_class =>
  switch t {
  | Java j => Java {...j, class_name: split_classname new_class}
  | ObjC_Cpp osig => ObjC_Cpp {...osig, class_name: new_class}
  | C _
  | Block _ => t
  };


/** Get the class name of a Objective-C/C++ procedure name. */
let objc_cpp_get_class_name objc_cpp => objc_cpp.class_name;


/** Return the package.classname of a java procname. */
let java_get_class_name (j: java) => java_type_to_string j.class_name;


/** Return the class name of a java procedure name. */
let java_get_simple_class_name (j: java) => snd j.class_name;


/** Return the package of a java procname. */
let java_get_package (j: java) => fst j.class_name;


/** Return the method of a java procname. */
let java_get_method (j: java) => j.method_name;


/** Replace the method of a java procname. */
let java_replace_method (j: java) mname => {...j, method_name: mname};


/** Replace the return type of a java procname. */
let java_replace_return_type j ret_type => {...j, return_type: Some ret_type};


/** Replace the parameters of a java procname. */
let java_replace_parameters j parameters => {...j, parameters};


/** Return the method/function of a procname. */
let get_method =
  fun
  | ObjC_Cpp name => name.method_name
  | C (name, _) => name
  | Block name => name
  | Java j => j.method_name;


/** Return the language of the procedure. */
let get_language =
  fun
  | ObjC_Cpp _ => Config.Clang
  | C _ => Config.Clang
  | Block _ => Config.Clang
  | Java _ => Config.Java;


/** Return the return type of a java procname. */
let java_get_return_type (j: java) => java_return_type_to_string j Verbose;


/** Return the parameters of a java procname. */
let java_get_parameters j => j.parameters;


/** Return the parameters of a java procname as strings. */
let java_get_parameters_as_strings j =>
  IList.map (fun param => java_type_to_string param) j.parameters;


/** Return true if the java procedure is static */
let java_is_static =
  fun
  | Java j => j.kind == Static
  | _ => false;


/** Prints a string of a java procname with the given level of verbosity */
let java_to_string withclass::withclass=false (j: java) verbosity =>
  switch verbosity {
  | Verbose
  | Non_verbose =>
    /* if verbose, then package.class.method(params): rtype,
       else rtype package.class.method(params)
       verbose is used for example to create unique filenames, non_verbose to create reports */
    let return_type = java_return_type_to_string j verbosity;
    let params = java_param_list_to_string j.parameters verbosity;
    let class_name = java_type_to_string_verbosity j.class_name verbosity;
    let separator =
      switch (j.return_type, verbosity) {
      | (None, _) => ""
      | (Some _, Verbose) => ":"
      | _ => " "
      };
    let output = class_name ^ "." ^ j.method_name ^ "(" ^ params ^ ")";
    if (verbosity == Verbose) {
      output ^ separator ^ return_type
    } else {
      return_type ^ separator ^ output
    }
  | Simple =>
    /* methodname(...) or without ... if there are no parameters */
    let cls_prefix =
      if withclass {
        java_type_to_string_verbosity j.class_name verbosity ^ "."
      } else {
        ""
      };
    let params =
      switch j.parameters {
      | [] => ""
      | _ => "..."
      };
    let method_name =
      if (j.method_name == "<init>") {
        java_get_simple_class_name j
      } else {
        cls_prefix ^ j.method_name
      };
    method_name ^ "(" ^ params ^ ")"
  };


/** Check if the class name is for an anonymous inner class. */
let is_anonymous_inner_class_name class_name =>
  switch (string_split_character class_name '$') {
  | (Some _, s) =>
    let is_int =
      try {
        ignore (int_of_string (String.trim s));
        true
      } {
      | Failure _ => false
      };
    is_int
  | (None, _) => false
  };


/** Check if the procedure belongs to an anonymous inner class. */
let java_is_anonymous_inner_class =
  fun
  | Java j => is_anonymous_inner_class_name (snd j.class_name)
  | _ => false;


/** Check if the last parameter is a hidden inner class, and remove it if present.
    This is used in private constructors, where a proxy constructor is generated
    with an extra parameter and calls the normal constructor. */
let java_remove_hidden_inner_class_parameter =
  fun
  | Java js =>
    switch (IList.rev js.parameters) {
    | [(_, s), ...par'] =>
      if (is_anonymous_inner_class_name s) {
        Some (Java {...js, parameters: IList.rev par'})
      } else {
        None
      }
    | [] => None
    }
  | _ => None;


/** Check if the procedure name is an anonymous inner class constructor. */
let java_is_anonymous_inner_class_constructor =
  fun
  | Java js => {
      let (_, name) = js.class_name;
      is_anonymous_inner_class_name name
    }
  | _ => false;


/** Check if the procedure name is an acess method (e.g. access$100 used to
    access private members from a nested class. */
let java_is_access_method =
  fun
  | Java js =>
    switch (string_split_character js.method_name '$') {
    | (Some "access", s) =>
      let is_int =
        try {
          ignore (int_of_string s);
          true
        } {
        | Failure _ => false
        };
      is_int
    | _ => false
    }
  | _ => false;


/** Check if the proc name has the type of a java vararg.
    Note: currently only checks that the last argument has type Object[]. */
let java_is_vararg =
  fun
  | Java js =>
    switch (IList.rev js.parameters) {
    | [(_, "java.lang.Object[]"), ..._] => true
    | _ => false
    }
  | _ => false;

let is_objc_constructor method_name => method_name == "new" || string_is_prefix "init" method_name;

let is_objc_kind =
  fun
  | ObjCClassMethod
  | ObjCInstanceMethod
  | ObjCInternalMethod => true
  | _ => false;


/** [is_constructor pname] returns true if [pname] is a constructor */
let is_constructor =
  fun
  | Java js => js.method_name == "<init>"
  | ObjC_Cpp {kind: CPPConstructor _} => true
  | ObjC_Cpp {kind, method_name} when is_objc_kind kind => is_objc_constructor method_name
  | _ => false;

let is_objc_dealloc method_name => method_name == "dealloc";


/** [is_dealloc pname] returns true if [pname] is the dealloc method in Objective-C
    TODO: add case for C++ */
let is_destructor =
  fun
  | ObjC_Cpp name => is_objc_dealloc name.method_name
  | _ => false;

let java_is_close =
  fun
  | Java js => js.method_name == "close"
  | _ => false;


/** [is_class_initializer pname] returns true if [pname] is a class initializer */
let is_class_initializer =
  fun
  | Java js => js.method_name == "<clinit>"
  | _ => false;


/** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc */
let is_infer_undefined pn =>
  switch pn {
  | Java j =>
    let regexp = Str.regexp "com.facebook.infer.models.InferUndefined";
    Str.string_match regexp (java_get_class_name j) 0
  | _ =>
    /* TODO: add cases for obj-c, c, c++ */
    false
  };


/** to_string for C_function type */
let to_readable_string (c1, c2) verbose => {
  let plain = c1;
  if verbose {
    switch c2 {
    | None => plain
    | Some s => plain ^ "{" ^ s ^ "}"
    }
  } else {
    plain
  }
};

let c_method_to_string osig detail_level =>
  switch detail_level {
  | Simple => osig.method_name
  | Non_verbose => osig.class_name ^ "_" ^ osig.method_name
  | Verbose =>
    let m_str =
      switch osig.kind {
      | CPPMethod m =>
        "(" ^
          (
            switch m {
            | None => ""
            | Some s => s
            }
          ) ^
          ")"
      | CPPConstructor m =>
        "{" ^
          (
            switch m {
            | None => ""
            | Some s => s
            }
          ) ^
          "}"
      | ObjCClassMethod => "class"
      | ObjCInstanceMethod => "instance"
      | ObjCInternalMethod => "internal"
      };
    osig.class_name ^ "_" ^ osig.method_name ^ m_str
  };


/** Very verbose representation of an existing Procname.t */
let to_unique_id pn =>
  switch pn {
  | Java j => java_to_string j Verbose
  | C (c1, c2) => to_readable_string (c1, c2) true
  | ObjC_Cpp osig => c_method_to_string osig Verbose
  | Block name => name
  };


/** Convert a proc name to a string for the user to see */
let to_string p =>
  switch p {
  | Java j => java_to_string j Non_verbose
  | C (c1, c2) => to_readable_string (c1, c2) false
  | ObjC_Cpp osig => c_method_to_string osig Non_verbose
  | Block name => name
  };


/** Convenient representation of a procname for external tools (e.g. eclipse plugin) */
let to_simplified_string withclass::withclass=false p =>
  switch p {
  | Java j => java_to_string withclass::withclass j Simple
  | C (c1, c2) => to_readable_string (c1, c2) false ^ "()"
  | ObjC_Cpp osig => c_method_to_string osig Simple
  | Block _ => "block"
  };


/** Convert a proc name to a filename */
let to_filename proc_name =>
  Escape.escape_filename @@ string_append_crc_cutoff @@ to_unique_id proc_name;


/** Pretty print a proc name */
let pp f pn => F.fprintf f "%s" (to_string pn);


/** Compare function for Procname.t types.
    These rules create an ordered set of procnames grouped with the following
    priority (lowest to highest): */
let compare pn1 pn2 =>
  switch (pn1, pn2) {
  | (Java j1, Java j2) => java_compare j1 j2
  | (Java _, _) => (-1)
  | (_, Java _) => 1
  | (
      C (c1, c2), /* Compare C_function types */
      C (c3, c4)
    ) =>
    string_compare c1 c3 |> next mangled_compare c2 c4
  | (C _, _) => (-1)
  | (_, C _) => 1
  | (
      Block s1, /* Compare ObjC_block types */
      Block s2
    ) =>
    string_compare s1 s2
  | (Block _, _) => (-1)
  | (_, Block _) => 1
  | (ObjC_Cpp osig1, ObjC_Cpp osig2) => c_meth_sig_compare osig1 osig2
  };

let equal pn1 pn2 => compare pn1 pn2 == 0;


/** hash function for procname */
let hash_pname = Hashtbl.hash;

let module Hash = Hashtbl.Make {
  type t = proc_name;
  let equal = equal;
  let hash = hash_pname;
};

let module Map = Map.Make {
  type t = proc_name;
  let compare = compare;
};

let module Set = Set.Make {
  type t = proc_name;
  let compare = compare;
};


/** Pretty print a set of proc names */
let pp_set fmt set => Set.iter (fun pname => F.fprintf fmt "%a " pp pname) set;
