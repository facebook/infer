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

(** Module for Procedure Names *)

module L = Logging
module F = Format

type java_type = string option * string (* e.g. ("", "int") for primitive types or ("java.io", "PrintWriter") for objects *)

type method_kind =
  | Static (* in Java, procedures called with invokestatic *)
  | Non_Static (* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)

(** Type of java procedure names. *)
type java = {
  class_name: java_type;
  return_type: java_type option; (* option because constructors have no return type *)
  method_name: string;
  parameters: java_type list;
  kind: method_kind
}

(** Type of c procedure names. *)
type c = string * (string option)

(** Type of Objective C and C++ procedure names: method signatures. *)
type objc_cpp = {
  class_name: string;
  method_name: string;
  mangled: string option;
}

(** Type of Objective C block names. *)
type block = string

(** Type of procedure names. *)
type t =
  | Java of java
  | C of c
  | ObjC_Cpp of objc_cpp
  | Block of block

(** Level of verbosity of some to_string functions. *)
type detail_level =
  | Verbose
  | Non_verbose
  | Simple

type objc_method_kind =
  | Instance_objc_method
  | Class_objc_method

let mangled_of_objc_method_kind kind =
  match kind with
  | Instance_objc_method -> Some "instance"
  | Class_objc_method -> Some "class"

let objc_method_kind_of_bool is_instance =
  if is_instance then Instance_objc_method
  else Class_objc_method

let empty_block = Block ""

let is_verbose v =
  match v with
  | Verbose -> true
  | _ -> false

type proc_name = t

let mangled_compare so1 so2 = match so1, so2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some s1, Some s2 -> string_compare s1 s2

let method_kind_compare k0 k1 =
  match k0, k1 with
  | _ when k0 = k1 -> 0
  | Static, _ -> 1
  | Non_Static, _ -> -1

(** A type is a pair (package, type_name) that is translated in a string package.type_name *)
let java_type_to_string_verbosity p verbosity =
  match p with
  | (None, typ) -> typ
  | (Some p, cls) ->
      if is_verbose verbosity then p ^ "." ^ cls
      else cls

let java_type_to_string p =
  java_type_to_string_verbosity p Verbose

(** Given a list of types, it creates a unique string of types separated by commas *)
let rec java_param_list_to_string inputList verbosity =
  match inputList with
  | [] -> ""
  | [head] -> java_type_to_string_verbosity head verbosity
  | head :: rest ->
      (java_type_to_string_verbosity head verbosity) ^ "," ^ (java_param_list_to_string rest verbosity)

(** It is the same as java_type_to_string, but Java return types are optional because of constructors without type *)
let java_return_type_to_string j verbosity =
  match j.return_type with
  | None -> ""
  | Some typ ->
      java_type_to_string_verbosity typ verbosity

let java_type_compare (p1, c1) (p2, c2) =
  string_compare c1 c2 |> next mangled_compare p1 p2

let rec java_type_list_compare jt1 jt2 =
  match jt1, jt2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (x1:: rest1), (x2:: rest2) ->
      java_type_compare x1 x2 |> next java_type_list_compare rest1 rest2

let java_return_type_compare jr1 jr2 =
  match jr1, jr2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some jt1 , Some jt2 -> java_type_compare jt1 jt2

(** Compare java procedure names. *)
let java_compare (j1: java) (j2 : java) =
  string_compare j1.method_name j2.method_name
  |> next java_type_list_compare j1.parameters j2.parameters
  |> next java_type_compare j1.class_name j2.class_name
  |> next java_return_type_compare j1.return_type j2.return_type
  |> next method_kind_compare j1.kind j2.kind

let c_function_mangled_compare mangled1 mangled2 =
  match mangled1, mangled2 with
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0
  | Some mangled1, Some mangled2 ->
      string_compare mangled1 mangled2

(** Compare c_method signatures. *)
let c_meth_sig_compare osig1 osig2 =
  string_compare osig1.method_name osig2.method_name
  |> next string_compare osig1.class_name osig2.class_name
  |> next c_function_mangled_compare osig1.mangled osig2.mangled

(** Given a package.class_name string, it looks for the latest dot and split the string
    in two (package, class_name) *)
let split_classname package_classname =
  string_split_character package_classname '.'

let from_string_c_fun (s: string) = C (s, None)

let c (plain: string) (mangled: string) = (plain, Some mangled)

let java class_name return_type method_name parameters kind =
  {
    class_name;
    return_type;
    method_name;
    parameters;
    kind;
  }

(** Create an objc procedure name from a class_name and method_name. *)
let objc_cpp class_name method_name mangled =
  {
    class_name = class_name;
    method_name = method_name;
    mangled = mangled;
  }

(** Create an objc procedure name from a class_name and method_name. *)
let mangled_objc_block name =
  Block name

let is_java = function
  | Java _ -> true
  | _ -> false

let is_c_method = function
  | ObjC_Cpp _ -> true
  | _ -> false

(** Replace the class name component of a procedure name.
    In case of Java, replace package and class name. *)
let replace_class t new_class = match t with
  | Java j ->
      Java { j with class_name = (split_classname new_class) }
  | ObjC_Cpp osig ->
      ObjC_Cpp { osig with class_name = new_class }
  | C _
  | Block _ ->
      t

(** Get the class name of a Objective-C/C++ procedure name. *)
let objc_cpp_get_class_name objc_cpp =
  objc_cpp.class_name

(** Return the package.classname of a java procname. *)
let java_get_class_name (j : java) =
  java_type_to_string j.class_name

(** Return the class name of a java procedure name. *)
let java_get_simple_class_name (j : java) =
  snd j.class_name

(** Return the package of a java procname. *)
let java_get_package (j : java) =
  fst j.class_name

(** Return the method of a java procname. *)
let java_get_method (j : java) =
  j.method_name

(** Replace the method of a java procname. *)
let java_replace_method (j : java) mname =
  { j with method_name = mname }

(** Replace the return type of a java procname. *)
let java_replace_return_type j ret_type =
  { j with return_type = Some ret_type }

(** Replace the parameters of a java procname. *)
let java_replace_parameters j parameters =
  { j with parameters }

(** Return the method/function of a procname. *)
let get_method = function
  | ObjC_Cpp name ->
      name.method_name
  | C (name, _) ->
      name
  | Block name ->
      name
  | Java j ->
      j.method_name

(** Return the language of the procedure. *)
let get_language = function
  | ObjC_Cpp _ ->
      Config.C_CPP
  | C _ ->
      Config.C_CPP
  | Block _ ->
      Config.C_CPP
  | Java _ ->
      Config.Java


(** Return the return type of a java procname. *)
let java_get_return_type (j : java) =
  java_return_type_to_string j Verbose

(** Return the parameters of a java procname. *)
let java_get_parameters j =
  j.parameters

(** Return the parameters of a java procname as strings. *)
let java_get_parameters_as_strings j =
  IList.map (fun param -> java_type_to_string param) j.parameters

(** Return true if the java procedure is static *)
let java_is_static = function
  | Java j ->
      j.kind = Static
  | _ ->
      false

(** Prints a string of a java procname with the given level of verbosity *)
let java_to_string ?(withclass = false) (j : java) verbosity =
  match verbosity with
  | Verbose | Non_verbose ->
      (* if verbose, then package.class.method(params): rtype,
         else rtype package.class.method(params)
         verbose is used for example to create unique filenames, non_verbose to create reports *)
      let return_type = java_return_type_to_string j verbosity in
      let params = java_param_list_to_string j.parameters verbosity in
      let class_name = java_type_to_string_verbosity j.class_name verbosity in
      let separator =
        match j.return_type, verbosity with
        | (None, _) -> ""
        | (Some _, Verbose) -> ":"
        | _ -> " " in
      let output = class_name ^ "." ^ j.method_name ^ "(" ^ params ^ ")" in
      if verbosity = Verbose then output ^ separator ^ return_type
      else return_type ^ separator ^ output
  | Simple -> (* methodname(...) or without ... if there are no parameters *)
      let cls_prefix =
        if withclass then
          java_type_to_string_verbosity j.class_name verbosity ^ "."
        else "" in
      let params =
        match j.parameters with
        | [] -> ""
        | _ -> "..." in
      let method_name =
        if j.method_name = "<init>" then
          java_get_simple_class_name j
        else
          cls_prefix ^ j.method_name in
      method_name ^ "(" ^ params ^ ")"

(** Check if the class name is for an anonymous inner class. *)
let is_anonymous_inner_class_name class_name =
  match string_split_character class_name '$' with
  | Some _, s ->
      let is_int =
        try ignore (int_of_string (String.trim s)); true with Failure _ -> false in
      is_int
  | None, _ -> false

(** Check if the procedure belongs to an anonymous inner class. *)
let java_is_anonymous_inner_class = function
  | Java j -> is_anonymous_inner_class_name (snd j.class_name)
  | _ -> false

(** Check if the last parameter is a hidden inner class, and remove it if present.
    This is used in private constructors, where a proxy constructor is generated
    with an extra parameter and calls the normal constructor. *)
let java_remove_hidden_inner_class_parameter = function
  | Java js ->
      (match IList.rev js.parameters with
       | (_, s) :: par' ->
           if is_anonymous_inner_class_name s
           then Some (Java { js with parameters = IList.rev par'})
           else None
       | [] -> None)
  | _ -> None

(** Check if the procedure name is an anonymous inner class constructor. *)
let java_is_anonymous_inner_class_constructor = function
  | Java js ->
      let _, name = js.class_name in
      is_anonymous_inner_class_name name
  | _ -> false

(** Check if the procedure name is an acess method (e.g. access$100 used to
    access private members from a nested class. *)
let java_is_access_method = function
  | Java js ->
      (match string_split_character js.method_name '$' with
       | Some "access", s ->
           let is_int =
             try ignore (int_of_string s); true with Failure _ -> false in
           is_int
       | _ -> false)
  | _ -> false

(** Check if the proc name has the type of a java vararg.
    Note: currently only checks that the last argument has type Object[]. *)
let java_is_vararg = function
  | Java js ->
      begin
        match (IList.rev js.parameters) with
        | (_,"java.lang.Object[]") :: _ -> true
        | _ -> false
      end
  | _ -> false

(** [is_constructor pname] returns true if [pname] is a constructor *)
let is_constructor = function
  | Java js -> js.method_name = "<init>"
  | ObjC_Cpp name ->
      (name.method_name = "new") ||
      string_is_prefix "init" name.method_name
  | _ -> false

(** [is_objc_dealloc pname] returns true if [pname] is the dealloc method in Objective-C *)
let is_objc_dealloc = function
  | ObjC_Cpp name -> name.method_name = "dealloc"
  | _ -> false

let java_is_close = function
  | Java js -> js.method_name = "close"
  | _ -> false

(** [is_class_initializer pname] returns true if [pname] is a class initializer *)
let is_class_initializer = function
  | Java js -> js.method_name = "<clinit>"
  | _ -> false

(** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc *)
let is_infer_undefined pn = match pn with
  | Java j ->
      let regexp = Str.regexp "com.facebook.infer.models.InferUndefined" in
      Str.string_match regexp (java_get_class_name j) 0
  | _ ->
      (* TODO: add cases for obj-c, c, c++ *)
      false

(** to_string for C_function type *)
let to_readable_string (c1, c2) verbose =
  let plain = c1 in
  if verbose then
    match c2 with
    | None -> plain
    | Some s -> plain ^ "{" ^ s ^ "}"
  else
    plain

let c_method_to_string osig detail_level =
  match detail_level with
  | Simple -> osig.method_name
  | Non_verbose -> osig.class_name ^ "_" ^ osig.method_name
  | Verbose ->
      let m_str = match osig.mangled with
        | None -> ""
        | Some s -> "{" ^ s ^ "}" in
      osig.class_name ^ "_" ^ osig.method_name ^ m_str

(** Very verbose representation of an existing Procname.t *)
let to_unique_id pn =
  match pn with
  | Java j -> java_to_string j Verbose
  | C (c1, c2) -> to_readable_string (c1, c2) true
  | ObjC_Cpp osig -> c_method_to_string osig Verbose
  | Block name -> name

(** Convert a proc name to a string for the user to see *)
let to_string p =
  match p with
  | Java j -> (java_to_string j Non_verbose)
  | C (c1, c2) ->
      to_readable_string (c1, c2) false
  | ObjC_Cpp osig -> c_method_to_string osig Non_verbose
  | Block name -> name

(** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
let to_simplified_string ?(withclass = false) p =
  match p with
  | Java j ->
      (java_to_string ~withclass j Simple)
  | C (c1, c2) ->
      to_readable_string (c1, c2) false ^ "()"
  | ObjC_Cpp osig ->
      c_method_to_string osig Simple
  | Block _ ->
      "block"

(** Convert a proc name to a filename *)
let to_filename proc_name =
  Escape.escape_filename @@ string_append_crc_cutoff @@ to_unique_id proc_name

(** Pretty print a proc name *)
let pp f pn =
  F.fprintf f "%s" (to_string pn)

(** Compare function for Procname.t types.
    These rules create an ordered set of procnames grouped with the following
    priority (lowest to highest): *)
let compare pn1 pn2 = match pn1, pn2 with
  | Java j1, Java j2 ->
      java_compare j1 j2
  | Java _, _ ->
      -1
  | _, Java _ ->
      1
  | C (c1, c2), C (c3, c4) -> (* Compare C_function types *)
      string_compare c1 c3
      |> next mangled_compare c2 c4
  | C _, _ ->
      -1
  | _, C _ ->
      1
  | Block s1, Block s2 -> (* Compare ObjC_block types *)
      string_compare s1 s2
  | Block _, _ ->
      -1
  | _, Block _ ->
      1
  | ObjC_Cpp osig1, ObjC_Cpp osig2 ->
      c_meth_sig_compare osig1 osig2

let equal pn1 pn2 =
  compare pn1 pn2 = 0

(** hash function for procname *)
let hash_pname = Hashtbl.hash

module Hash =
  Hashtbl.Make(struct
    type t = proc_name
    let equal = equal
    let hash = hash_pname
  end)

module Map = Map.Make (struct
    type t = proc_name
    let compare = compare end)

module Set = Set.Make(struct
    type t = proc_name
    let compare = compare
  end)

(** Pretty print a set of proc names *)
let pp_set fmt set =
  Set.iter (fun pname -> F.fprintf fmt "%a " pp pname) set
