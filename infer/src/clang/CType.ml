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

let add_pointer_to_typ typ = Typ.mk (Tptr (typ, Typ.Pk_pointer))

let remove_pointer_to_typ typ =
  match typ.Typ.desc with Typ.Tptr (typ, Typ.Pk_pointer) -> typ | _ -> typ

let objc_classname_of_type typ =
  match typ.Typ.desc with
  | Typ.Tstruct name
   -> name
  | Typ.Tfun _
   -> Typ.Name.Objc.from_string CFrontend_config.objc_object
  | _
   -> L.(debug Capture Verbose)
        "Classname of type cannot be extracted in type %s" (Typ.to_string typ) ;
      Typ.Name.Objc.from_string "undefined"

let is_class typ =
  match typ.Typ.desc with
  | Typ.Tptr ({desc= Tstruct name}, _)
   -> String.equal (Typ.Name.name name) CFrontend_config.objc_class
  | _
   -> false

let rec return_type_of_function_qual_type (qual_type: Clang_ast_t.qual_type) =
  let open Clang_ast_t in
  match CAst_utils.get_type qual_type.qt_type_ptr with
  | Some FunctionProtoType (_, function_type_info, _)
  | Some FunctionNoProtoType (_, function_type_info)
   -> function_type_info.Clang_ast_t.fti_return_type
  | Some BlockPointerType (_, in_qual)
   -> return_type_of_function_qual_type in_qual
  | Some _
   -> L.(debug Capture Verbose)
        "Warning: Type pointer %s is not a function type."
        (Clang_ast_extend.type_ptr_to_string qual_type.qt_type_ptr) ;
      {qual_type with qt_type_ptr= Clang_ast_extend.ErrorType}
  | None
   -> L.(debug Capture Verbose)
        "Warning: Type pointer %s not found."
        (Clang_ast_extend.type_ptr_to_string qual_type.qt_type_ptr) ;
      {qual_type with qt_type_ptr= Clang_ast_extend.ErrorType}

let return_type_of_function_type qual_type = return_type_of_function_qual_type qual_type

let is_block_type {Clang_ast_t.qt_type_ptr} =
  let open Clang_ast_t in
  match CAst_utils.get_desugared_type qt_type_ptr with
  | Some BlockPointerType _
   -> true
  | _
   -> false

let is_reference_type {Clang_ast_t.qt_type_ptr} =
  match CAst_utils.get_desugared_type qt_type_ptr with
  | Some Clang_ast_t.LValueReferenceType _
   -> true
  | Some Clang_ast_t.RValueReferenceType _
   -> true
  | _
   -> false

(* To be called with strings of format "<pointer_type_info>*<class_name>" *)
let get_name_from_type_pointer custom_type_pointer =
  match Str.split (Str.regexp "*") custom_type_pointer with
  | [pointer_type_info; class_name]
   -> (pointer_type_info, class_name)
  | _
   -> assert false

(*
let rec get_type_list nn ll =
  match ll with
  | [] -> []
  | (n, t):: ll' ->
      (* L.(debug Capture Verbose) ">>>>>Searching for type '%s'. Seen '%s'.@." nn n; *)
      if n = nn then (
        L.(debug Capture Verbose) ">>>>>>>>>>>>>>>>>>>>>>>NOW Found, Its type is: '%s'@."
          (Typ.to_string t);
        [t]
      ) else get_type_list nn ll'
*)
