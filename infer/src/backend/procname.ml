(*
* Copyright (c) 2009 - 2013 Monoidics ltd.
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Module for Procedure Names *)

module L = Logging
module F = Format
open Utils
open Str

type java_type = string option * string (* e.g. ("", "int") for primitive types or ("java.io", "PrintWriter") for objects *)

type method_kind =
  | Static (* in Java, procedures called with invokestatic *)
  | Non_Static (* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)

(* java_signature extends base_signature with a classname and a package *)
type java_signature = {
  classname: java_type;
  returntype: java_type option; (* option because constructors have no return type *)
  methodname: string;
  parameters: java_type list;
  kind: method_kind
}

(* C++/ObjC method signature *)
type c_method_signature = {
  class_name: string;
  method_name: string;
  mangled: string option;
}

type t =
  | JAVA of java_signature
  | C_FUNCTION of string * (string option) (* it is a pair (plain, mangled optional) *)
  | STATIC of string * (string option) (* it is a pair (plain name, filename optional) *)
  | C_METHOD of c_method_signature
  | OBJC_BLOCK of string

(* Defines the level of verbosity of some to_string functions *)
type detail_level =
  | VERBOSE
  | NON_VERBOSE
  | SIMPLE

let is_verbose v =
  match v with
  | VERBOSE -> true
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
let java_type_to_string p verbosity =
  match p with
  | (None, typ) -> typ
  | (Some p, cls) ->
      if is_verbose verbosity then p ^ "." ^ cls
      else cls

(** Given a list of types, it creates a unique string of types separated by commas *)
let rec java_param_list_to_string inputList verbosity =
  match inputList with
  | [] -> ""
  | [head] -> java_type_to_string head verbosity
  | head :: rest -> (java_type_to_string head verbosity) ^ "," ^ (java_param_list_to_string rest verbosity)

(** It is the same as java_type_to_string, but Java return types are optional because of constructors without type *)
let java_return_type_to_string j verbosity =
  match j.returntype with
  | None -> ""
  | Some typ ->
      java_type_to_string typ verbosity

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

(** Compare java signatures. *)
let java_sig_compare js1 js2 =
  string_compare js1.methodname js2.methodname
  |> next java_type_list_compare js1.parameters js2.parameters
  |> next java_type_compare js1.classname js2.classname
  |> next java_return_type_compare js1.returntype js2.returntype
  |> next method_kind_compare js1.kind js2.kind

(** Compare c_method signatures. *)
let c_meth_sig_compare osig1 osig2 =
  let n = string_compare osig1.class_name osig2.class_name in
  if n <> 0 then n else string_compare osig1.method_name osig2.method_name

(** Given a package.classname string, it looks for the latest dot and split the string in two (package, classname) *)
let split_classname package_classname =
  string_split_character package_classname '.'

let from_string_c_fun (s: string) = C_FUNCTION (s, None)

let mangled_c_fun (plain: string) (mangled: string) = C_FUNCTION (plain, Some mangled)

(** Create a static procedure name from a plain name and source file *)
let mangled_static (plain: string) (source_file: DB.source_file) =
  let mangled =
    if !Config.long_static_proc_names then Some (DB.source_file_encoding source_file)
    else None in STATIC (plain, mangled)

(** Creates a java procname, given classname, return type, method name and its parameters *)
let mangled_java class_name ret_type method_name params _kind =
  JAVA {
    classname = class_name;
    returntype = ret_type;
    methodname = method_name;
    parameters = params;
    kind = _kind
  }

(** Create an objc procedure name from a class_name and method_name. *)
let mangled_c_method class_name method_name mangled =
  C_METHOD {
    class_name = class_name;
    method_name = method_name;
    mangled = mangled;
  }

(** Create an objc procedure name from a class_name and method_name. *)
let mangled_objc_block name =
  OBJC_BLOCK name

let is_java = function
  | JAVA _ -> true
  | _ -> false

let is_c_method = function
  | C_METHOD _ -> true
  | _ -> false

(** Replace package and classname of a java procname. *)
let java_replace_class p package_classname =
  match p with
  | JAVA j -> JAVA { j with classname = (split_classname package_classname) }
  | _ -> assert false

(** Replace the class name of an objc procedure name. *)
let c_method_replace_class t class_name =
  match t with
  | C_METHOD osig -> C_METHOD { osig with class_name = class_name }
  | _ -> assert false

(** Return the package.classname of a java procname. *)
let java_get_class = function
  | JAVA j -> java_type_to_string j.classname VERBOSE
  | _ -> assert false

(** Return path components of a java class name *)
let java_get_class_components proc_name =
  Str.split (Str.regexp (Str.quote ".")) (java_get_class proc_name)

(** Return the class name of a java procedure name. *)
let java_get_simple_class proc_name =
  list_hd (list_rev (java_get_class_components proc_name))

(** Return the method of a java procname. *)
let java_get_method = function
  | JAVA j -> j.methodname
  | _ -> assert false

(** Replace the method of a java procname. *)
let java_replace_method p mname = match p with
  | JAVA p -> JAVA { p with methodname = mname }
  | _ -> assert false

(** Replace the return type of a java procname. *)
let java_replace_return_type p ret_type = match p with
  | JAVA p -> JAVA { p with returntype = Some ret_type }
  | _ -> assert false

(** Return the method of a objc/c++ procname. *)
let c_get_method = function
  | C_METHOD name -> name.method_name
  | C_FUNCTION (name, _) -> name
  | OBJC_BLOCK name -> name
  | _ -> assert false

(** Return the return type of a java procname. *)
let java_get_return_type = function
  | JAVA j -> java_return_type_to_string j VERBOSE
  | _ -> assert false

(** Return the parameters of a java procname. *)
let java_get_parameters = function
  | JAVA j -> list_map (fun param -> java_type_to_string param VERBOSE) j.parameters
  | _ -> assert false

(** Return true if the java procedure is static *)
let java_is_static = function
  | JAVA j -> j.kind = Static
  | _ -> assert false

(** Prints a string of a java procname with the given level of verbosity *)
let java_to_string ?withclass: (wc = false) j verbosity =
  match verbosity with
  | VERBOSE | NON_VERBOSE ->
  (* if verbose, then package.class.method(params): rtype,
  else rtype package.class.method(params)
  verbose is used for example to create unique filenames, non_verbose to create reports *)
      let return_type = java_return_type_to_string j verbosity in
      let params = java_param_list_to_string j.parameters verbosity in
      let classname = java_type_to_string j.classname verbosity in
      let separator =
        match j.returntype, verbosity with
        | (None, _) -> ""
        | (Some _, VERBOSE) -> ":"
        | _ -> " " in
      let output = classname ^ "." ^ j.methodname ^ "(" ^ params ^ ")" in
      if verbosity = VERBOSE then output ^ separator ^ return_type
      else return_type ^ separator ^ output
  | SIMPLE -> (* methodname(...) or without ... if there are no parameters *)
      let cls_prefix =
        if wc then
          java_type_to_string j.classname verbosity ^ "."
        else "" in
      let params =
        match j.parameters with
        | [] -> ""
        | _ -> "..." in
      let methodname = if j.methodname = "<init>" then java_get_simple_class (JAVA j) else j.methodname in
      cls_prefix ^ methodname ^ "(" ^ params ^ ")"

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
  | JAVA j -> is_anonymous_inner_class_name (snd j.classname)
  | _ -> false

(** Check if the last parameter is a hidden inner class, and remove it if present.
This is used in private constructors, where a proxy constructor is generated
with an extra parameter and calls the normal constructor. *)
let java_remove_hidden_inner_class_parameter = function
  | JAVA js ->
      (match list_rev js.parameters with
        | (so, s) :: par' ->
            if is_anonymous_inner_class_name s
            then Some (JAVA { js with parameters = list_rev par'})
            else None
        | [] -> None)
  | _ -> None

(** Check if the procedure name is an anonymous inner class constructor. *)
let java_is_anonymous_inner_class_constructor = function
  | JAVA js ->
      let _, name = js.classname in
      is_anonymous_inner_class_name name
  | _ -> false

(** Check if the procedure name is an acess method (e.g. access$100 used to
access private members from a nested class. *)
let java_is_access_method = function
  | JAVA js ->
      (match string_split_character js.methodname '$' with
        | Some "access", s ->
            let is_int =
              try ignore (int_of_string s); true with Failure _ -> false in
            is_int
        | _ -> false)
  | _ -> false

(** Check if the proc name has the type of a java vararg.
Note: currently only checks that the last argument has type Object[]. *)
let java_is_vararg = function
  | JAVA js ->
      begin
        match (list_rev js.parameters) with
        | (_,"java.lang.Object[]") :: _ -> true
        | _ -> false
      end
  | _ -> false

(** [is_constructor pname] returns true if [pname] is a constructor *)
let is_constructor = function
  | JAVA js -> js.methodname = "<init>"
  | C_METHOD name -> Utils.string_is_prefix "init" name.method_name
  | _ -> false


let java_is_close = function
  | JAVA js -> js.methodname = "close"
  | _ -> false


(** [is_class_initializer pname] returns true if [pname] is a class initializer *)
let is_class_initializer = function
  | JAVA js -> js.methodname = "<clinit>"
  | _ -> false

(** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc *)
let is_infer_undefined pn = match pn with
  | JAVA j ->
      let regexp = Str.regexp "com.facebook.infer.models.InferUndefined" in
      Str.string_match regexp (java_get_class pn) 0
  | _ ->
  (* TODO: add cases for obj-c, c, c++ *)
      false

(** to_string for C_FUNCTION and STATIC types *)
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
  | SIMPLE ->
      osig.method_name
  | VERBOSE | NON_VERBOSE ->
      let m_str = match osig.mangled with
        | None -> ""
        | Some s -> "{" ^ s ^ "}" in
      osig.class_name ^ "_" ^ osig.method_name ^ m_str

(** Very verbose representation of an existing Procname.t *)
let to_unique_id pn =
  match pn with
  | JAVA j -> java_to_string j VERBOSE
  | C_FUNCTION (c1, c2) -> to_readable_string (c1, c2) true
  | STATIC (s1, s2) -> to_readable_string (s1, s2) true
  | C_METHOD osig -> c_method_to_string osig VERBOSE
  | OBJC_BLOCK name -> name

(** Convert a proc name to a string for the user to see *)
let to_string p =
  match p with
  | JAVA j -> (java_to_string j NON_VERBOSE)
  | C_FUNCTION (c1, c2) | STATIC (c1, c2) ->
      to_readable_string (c1, c2) false
  | C_METHOD osig -> c_method_to_string osig NON_VERBOSE
  | OBJC_BLOCK name -> name

(** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
let to_simplified_string ?withclass: (wc = false) p =
  match p with
  | JAVA j -> (java_to_string ~withclass: wc j SIMPLE)
  | C_FUNCTION (c1, c2) | STATIC (c1, c2) ->
      to_readable_string (c1, c2) false ^ "()"
  | C_METHOD osig -> c_method_to_string osig SIMPLE
  | OBJC_BLOCK name -> "block"

(** Convert a proc name to a filename *)
let to_filename (pn : proc_name) =
  let cutoff_length = 100 in (** if longer than cutoff, cut it and append CRC *)
  let name = to_unique_id pn in
  if String.length name <= cutoff_length then name
  else
    let pname_first_100 = String.sub name 0 cutoff_length in
    let crc_str = CRC.crc16 name in
    pname_first_100 ^ crc_str

(** Pretty print a proc name *)
let pp f pn =
  F.fprintf f "%s" (to_string pn)

(** Compare function for Procname.t types *)
(* These rules create an ordered set of procnames grouped with the following priority (lowest to highest): *)
(* JAVA, C_FUNCTION, STATIC, OBJC *)
let compare pn1 pn2 = match pn1, pn2 with
  | JAVA j1, JAVA j2 -> java_sig_compare j1 j2
  | JAVA _, _ -> -1
  | _, JAVA _ -> 1
  | C_FUNCTION (c1, c2), C_FUNCTION (c3, c4) -> (* Compare C_FUNCTION types *)
      let n = string_compare c1 c3 in
      if n <> 0 then n else mangled_compare c2 c4
  | C_FUNCTION _, _ -> -1
  | _, C_FUNCTION _ -> 1
  | STATIC (c1, c2), STATIC (c3, c4) -> (* Compare STATIC types *)
      let n = string_compare c1 c3 in
      if n <> 0 then n else mangled_compare c2 c4
  | STATIC _, _ -> -1
  | _, STATIC _ -> 1
  | OBJC_BLOCK s1, OBJC_BLOCK s2 -> (* Compare OBJC_BLOCK types *)
      string_compare s1 s2
  | OBJC_BLOCK _, _ -> -1
  | _, OBJC_BLOCK _ -> 1
  | C_METHOD osig1, C_METHOD osig2 -> c_meth_sig_compare osig1 osig2

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
