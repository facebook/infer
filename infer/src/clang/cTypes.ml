(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Utility module for retrieving types *)

open Utils
open CFrontend_utils
module L = Logging

let get_type_from_expr_info ei =
  ei.Clang_ast_t.ei_type_ptr

let get_name_from_struct s =
  match s with
  | Sil.Tstruct(_, _, _, Some n, _, _, _) -> n
  | _ -> assert false

let rec get_type_list nn ll =
  match ll with
  | [] -> []
  | (n, t):: ll' -> (* Printing.log_out ">>>>>Searching for type '%s'. Seen '%s'.@." nn n; *)
      if n = nn then (
        Printing.log_out ">>>>>>>>>>>>>>>>>>>>>>>NOW Found, Its type is: '%s'@." (Sil.typ_to_string t);
        [t]
      ) else get_type_list nn ll'

let add_pointer_to_typ typ =
  Sil.Tptr(typ, Sil.Pk_pointer)

let remove_pointer_to_typ typ =
  match typ with
  | Sil.Tptr(typ, Sil.Pk_pointer) -> typ
  | _ -> typ

let classname_of_type typ =
  match typ with
  | Sil.Tvar (Typename.TN_csu (_, name) )
  | Sil.Tstruct(_, _, _, (Some name), _, _, _)
  | Sil.Tvar (Typename.TN_typedef name) -> Mangled.to_string name
  | Sil.Tfun _ -> CFrontend_config.objc_object
  | _ ->
      Printing.log_out
        "Classname of type cannot be extracted in type %s" (Sil.typ_to_string typ);
      "undefined"

(* Iterates over the tenv to find the value of the enumeration constant    *)
(* using its name Here we assume that the enumeration constant have        *)
(* different names. Note: this assumption may not be true all the time. So *)
(* need to be careful and give name that cane ensure uniqueness. In case   *)
(* of repeated names it gets the last.                                     *)
let search_enum_type_by_name tenv name =
  let found = ref None in
  let mname = Mangled.from_string name in
  let f tn typ =
    match typ with
    | Sil.Tenum enum_constants ->
        IList.iter (fun (c, v) -> if Mangled.equal c mname then found:= Some v else ()) enum_constants
    | _ -> () in
  Sil.tenv_iter f tenv;
  !found

let mk_classname n = Typename.TN_csu (Csu.Class, Mangled.from_string n)

let mk_structname n = Typename.TN_csu (Csu.Struct, Mangled.from_string n)

let mk_enumname n = Typename.TN_enum (Mangled.from_string n)

let is_class typ =
  match typ with
  | Sil.Tptr( Sil.Tstruct(_, _, _, (Some name), _, _, _), _)
  | Sil.Tptr( Sil.Tvar (Typename.TN_csu (_, name) ), _) ->
      (Mangled.to_string name) = CFrontend_config.objc_class
  | _ -> false

let rec return_type_of_function_type_ptr type_ptr =
  let open Clang_ast_t in
  match Ast_utils.get_type type_ptr with
  | Some FunctionProtoType (type_info, function_type_info, _)
  | Some FunctionNoProtoType (type_info, function_type_info) ->
      function_type_info.Clang_ast_t.fti_return_type
  | Some BlockPointerType (type_info, in_type_ptr) ->
      return_type_of_function_type_ptr in_type_ptr
  | Some _ ->
      Printing.log_err "Warning: Type pointer %s is not a function type."
        (Clang_ast_types.type_ptr_to_string type_ptr);
      `ErrorType
  | None ->
      Printing.log_err "Warning: Type pointer %s not found."
        (Clang_ast_types.type_ptr_to_string type_ptr);
      `ErrorType

let return_type_of_function_type tp =
  return_type_of_function_type_ptr tp

let is_block_type tp =
  let open Clang_ast_t in
  match Ast_utils.get_desugared_type tp with
  | Some BlockPointerType _ -> true
  | _ -> false

let is_reference_type tp =
  match Ast_utils.get_desugared_type tp with
  | Some Clang_ast_t.LValueReferenceType _ -> true
  | _ -> false

(* Expand a named type Tvar if it has a definition in tenv. This is used for Tenum, Tstruct, etc. *)
let rec expand_structured_type tenv typ =
  match typ with
  | Sil.Tvar tn ->
      (match Sil.tenv_lookup tenv tn with
       | Some t ->
           Printing.log_out "   Type expanded with type '%s' found in tenv@." (Sil.typ_to_string t);
           if Sil.typ_equal t typ then
             typ
           else expand_structured_type tenv t
       | None -> typ)
  | Sil.Tptr(t, _) -> typ (*do not expand types under pointers *)
  | _ -> typ

(* To be called with strings of format "<pointer_type_info>*<class_name>" *)
let get_name_from_type_pointer custom_type_pointer =
  match Str.split (Str.regexp "*") custom_type_pointer with
  | [pointer_type_info; class_name] -> pointer_type_info, class_name
  | _ -> assert false
