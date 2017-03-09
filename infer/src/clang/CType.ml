(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module for retrieving types *)

module L = Logging

let add_pointer_to_typ typ =
  Typ.Tptr(typ, Typ.Pk_pointer)

let remove_pointer_to_typ typ =
  match typ with
  | Typ.Tptr(typ, Typ.Pk_pointer) -> typ
  | _ -> typ

let objc_classname_of_type typ =
  match typ with
  | Typ.Tstruct name -> name
  | Typ.Tfun _ -> Typename.Objc.from_string CFrontend_config.objc_object
  | _ ->
      Logging.out_debug
        "Classname of type cannot be extracted in type %s" (Typ.to_string typ);
      Typename.Objc.from_string "undefined"

let is_class typ =
  match typ with
  | Typ.Tptr (Tstruct name, _) ->
      String.equal (Typename.name name) CFrontend_config.objc_class
  | _ -> false

let rec return_type_of_function_type_ptr type_ptr =
  let open Clang_ast_t in
  match CAst_utils.get_type type_ptr with
  | Some FunctionProtoType (_, function_type_info, _)
  | Some FunctionNoProtoType (_, function_type_info) ->
      function_type_info.Clang_ast_t.fti_return_type
  | Some BlockPointerType (_, in_type_ptr) ->
      return_type_of_function_type_ptr in_type_ptr
  | Some _ ->
      Logging.err_debug "Warning: Type pointer %s is not a function type."
        (Clang_ast_types.type_ptr_to_string type_ptr);
      `ErrorType
  | None ->
      Logging.err_debug "Warning: Type pointer %s not found."
        (Clang_ast_types.type_ptr_to_string type_ptr);
      `ErrorType

let return_type_of_function_type tp =
  return_type_of_function_type_ptr tp

let is_block_type tp =
  let open Clang_ast_t in
  match CAst_utils.get_desugared_type tp with
  | Some BlockPointerType _ -> true
  | _ -> false

let is_reference_type tp =
  match CAst_utils.get_desugared_type tp with
  | Some Clang_ast_t.LValueReferenceType _ -> true
  | Some Clang_ast_t.RValueReferenceType _ -> true
  | _ -> false

(* To be called with strings of format "<pointer_type_info>*<class_name>" *)
let get_name_from_type_pointer custom_type_pointer =
  match Str.split (Str.regexp "*") custom_type_pointer with
  | [pointer_type_info; class_name] -> pointer_type_info, class_name
  | _ -> assert false

(*
let rec get_type_list nn ll =
  match ll with
  | [] -> []
  | (n, t):: ll' -> (* Logging.out_debug ">>>>>Searching for type '%s'. Seen '%s'.@." nn n; *)
      if n = nn then (
        Logging.out_debug ">>>>>>>>>>>>>>>>>>>>>>>NOW Found, Its type is: '%s'@."
          (Typ.to_string t);
        [t]
      ) else get_type_list nn ll'
*)
