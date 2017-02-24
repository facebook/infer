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


/** Module for Procedure Names */
let module L = Logging;

let module F = Format;

/* e.g. ("", "int") for primitive types or ("java.io", "PrintWriter") for objects */
type java_type = (option string, string);

/* compare in inverse order */
let compare_java_type (p1, c1) (p2, c2) => [%compare : (string, option string)] (c1, p1) (c2, p2);

type method_kind =
  | Non_Static /* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface */
  | Static /* in Java, procedures called with invokestatic */
[@@deriving compare];

let equal_method_kind = [%compare.equal : method_kind];


/** Type of java procedure names. */
type java = {
  method_name: string,
  parameters: list java_type,
  class_name: java_type,
  return_type: option java_type, /* option because constructors have no return type */
  kind: method_kind
}
[@@deriving compare];


/** Type of c procedure names. */
type c = (string, option string) [@@deriving compare];

type objc_cpp_method_kind =
  | CPPMethod (option string) /** with mangling */
  | CPPConstructor (option string, bool) /** with mangling + is it constexpr? */
  | ObjCClassMethod
  | ObjCInstanceMethod
  | ObjCInternalMethod
[@@deriving compare];


/** Type of Objective C and C++ procedure names: method signatures. */
type objc_cpp = {method_name: string, class_name: string, kind: objc_cpp_method_kind}
[@@deriving compare];


/** Type of Objective C block names. */
type block = string [@@deriving compare];


/** Type of procedure names. */
type t =
  | Java java
  | C c
  | Linters_dummy_method
  | Block block
  | ObjC_Cpp objc_cpp
[@@deriving compare];

let equal = [%compare.equal : t];


/** Level of verbosity of some to_string functions. */
type detail_level =
  | Verbose
  | Non_verbose
  | Simple
[@@deriving compare];

let equal_detail_level = [%compare.equal : detail_level];

let objc_method_kind_of_bool is_instance =>
  if is_instance {ObjCInstanceMethod} else {ObjCClassMethod};

let empty_block = Block "";

let is_verbose v =>
  switch v {
  | Verbose => true
  | _ => false
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


/** Given a package.class_name string, it looks for the latest dot and split the string
    in two (package, class_name) */
let split_classname package_classname =>
  switch (String.rsplit2 package_classname on::'.') {
  | Some (x, y) => (Some x, y)
  | None => (None, package_classname)
  };

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

let is_constexpr =
  fun
  | ObjC_Cpp {kind: CPPConstructor (_, true)} => true
  | _ => false;


/** Replace the class name component of a procedure name.
    In case of Java, replace package and class name. */
let replace_class t new_class =>
  switch t {
  | Java j => Java {...j, class_name: split_classname new_class}
  | ObjC_Cpp osig => ObjC_Cpp {...osig, class_name: new_class}
  | C _
  | Block _
  | Linters_dummy_method => t
  };


/** Get the class name of a Objective-C/C++ procedure name. */
let objc_cpp_get_class_name objc_cpp => objc_cpp.class_name;


/** Return the package.classname of a java procname. */
let java_get_class_name (j: java) => java_type_to_string j.class_name;


/** Return the package.classname as a typename of a java procname. */
let java_get_class_type_name (j: java) => Typename.Java.from_string (
  java_type_to_string j.class_name
);


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
  | Java j => j.method_name
  | Linters_dummy_method => "Linters_dummy_method";


/** Return the language of the procedure. */
let get_language =
  fun
  | ObjC_Cpp _ => Config.Clang
  | C _ => Config.Clang
  | Block _ => Config.Clang
  | Linters_dummy_method => Config.Clang
  | Java _ => Config.Java;


/** Return the return type of a java procname. */
let java_get_return_type (j: java) => java_return_type_to_string j Verbose;


/** Return the parameters of a java procname. */
let java_get_parameters j => j.parameters;


/** Return the parameters of a java procname as strings. */
let java_get_parameters_as_strings j =>
  List.map f::(fun param => java_type_to_string param) j.parameters;


/** Return true if the java procedure is static */
let java_is_static =
  fun
  | Java j => equal_method_kind j.kind Static
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
    if (equal_detail_level verbosity Verbose) {
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
      if (String.equal j.method_name "<init>") {
        java_get_simple_class_name j
      } else {
        cls_prefix ^ j.method_name
      };
    method_name ^ "(" ^ params ^ ")"
  };


/** Check if the class name is for an anonymous inner class. */
let is_anonymous_inner_class_name class_name =>
  switch (String.rsplit2 class_name on::'$') {
  | Some (_, s) =>
    let is_int =
      try {
        ignore (int_of_string (String.strip s));
        true
      } {
      | Failure _ => false
      };
    is_int
  | None => false
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
    switch (List.rev js.parameters) {
    | [(_, s), ...par'] =>
      if (is_anonymous_inner_class_name s) {
        Some (Java {...js, parameters: List.rev par'})
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
    switch (String.rsplit2 js.method_name on::'$') {
    | Some ("access", s) =>
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


/** Check if the procedure name is of an auto-generated method containing '$'. */
let java_is_autogen_method =
  fun
  | Java js => String.contains js.method_name '$'
  | _ => false;


/** Check if the proc name has the type of a java vararg.
    Note: currently only checks that the last argument has type Object[]. */
let java_is_vararg =
  fun
  | Java js =>
    switch (List.rev js.parameters) {
    | [(_, "java.lang.Object[]"), ..._] => true
    | _ => false
    }
  | _ => false;

let is_objc_constructor method_name =>
  String.equal method_name "new" || String.is_prefix prefix::"init" method_name;

let is_objc_kind =
  fun
  | ObjCClassMethod
  | ObjCInstanceMethod
  | ObjCInternalMethod => true
  | _ => false;


/** [is_constructor pname] returns true if [pname] is a constructor */
let is_constructor =
  fun
  | Java js => String.equal js.method_name "<init>"
  | ObjC_Cpp {kind: CPPConstructor _} => true
  | ObjC_Cpp {kind, method_name} when is_objc_kind kind => is_objc_constructor method_name
  | _ => false;

let is_objc_dealloc method_name => String.equal method_name "dealloc";


/** [is_dealloc pname] returns true if [pname] is the dealloc method in Objective-C
    TODO: add case for C++ */
let is_destructor =
  fun
  | ObjC_Cpp name => is_objc_dealloc name.method_name
  | _ => false;

let java_is_close =
  fun
  | Java js => String.equal js.method_name "close"
  | _ => false;


/** [is_class_initializer pname] returns true if [pname] is a class initializer */
let is_class_initializer =
  fun
  | Java js => String.equal js.method_name "<clinit>"
  | _ => false;


/** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc */
let is_infer_undefined pn =>
  switch pn {
  | Java j =>
    let regexp = Str.regexp "com.facebook.infer.builtins.InferUndefined";
    Str.string_match regexp (java_get_class_name j) 0
  | _ =>
    /* TODO: add cases for obj-c, c, c++ */
    false
  };

let get_global_name_of_initializer =
  fun
  | C (name, _) when String.is_prefix prefix::Config.clang_initializer_prefix name => {
      let prefix_len = String.length Config.clang_initializer_prefix;
      Some (String.sub name pos::prefix_len len::(String.length name - prefix_len))
    }
  | _ => None;


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
        ) ^ ")"
      | CPPConstructor (m, is_constexpr) =>
        "{" ^
        (
          switch m {
          | None => ""
          | Some s => s
          }
        ) ^
        (if is_constexpr {"|constexpr"} else {""}) ^ "}"
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
  | Linters_dummy_method => "Linters_dummy_method"
  };


/** Convert a proc name to a string for the user to see */
let to_string p =>
  switch p {
  | Java j => java_to_string j Non_verbose
  | C (c1, c2) => to_readable_string (c1, c2) false
  | ObjC_Cpp osig => c_method_to_string osig Non_verbose
  | Block name => name
  | Linters_dummy_method => to_unique_id p
  };


/** Convenient representation of a procname for external tools (e.g. eclipse plugin) */
let to_simplified_string withclass::withclass=false p =>
  switch p {
  | Java j => java_to_string withclass::withclass j Simple
  | C (c1, c2) => to_readable_string (c1, c2) false ^ "()"
  | ObjC_Cpp osig => c_method_to_string osig Simple
  | Block _ => "block"
  | Linters_dummy_method => to_unique_id p
  };


/** Convert a proc name to a filename */
let to_filename proc_name =>
  Escape.escape_filename @@ Utils.string_append_crc_cutoff @@ to_unique_id proc_name;


/** Pretty print a proc name */
let pp f pn => F.fprintf f "%s" (to_string pn);


/** hash function for procname */
let hash_pname = Hashtbl.hash;

let module Hash = Hashtbl.Make {
  type nonrec t = t;
  let equal = equal;
  let hash = hash_pname;
};

let module Map = Caml.Map.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};


/** Pretty print a set of proc names */
let pp_set fmt set => Set.iter (fun pname => F.fprintf fmt "%a " pp pname) set;

let get_qualifiers pname =>
  switch pname {
  | C c => fst c |> QualifiedCppName.qualifiers_of_qual_name
  | ObjC_Cpp objc_cpp =>
    List.append
      (QualifiedCppName.qualifiers_of_qual_name objc_cpp.class_name) [objc_cpp.method_name]
  | _ => []
  };
