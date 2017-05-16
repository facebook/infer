(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(* Types used by the ctl parser *)

(** the kind of AST nodes where formulas are evaluated *)
type ast_node =
  | Stmt of Clang_ast_t.stmt
  | Decl of Clang_ast_t.decl

let ast_node_name an =
  let open Clang_ast_t in
  match an with
  | Decl dec ->
      (match Clang_ast_proj.get_named_decl_tuple dec with
       | Some (_, n) -> n.Clang_ast_t.ni_name
       | None -> "")
  | Stmt (DeclRefExpr (_, _, _, drti)) ->
      (match drti.drti_decl_ref with
       | Some dr ->
           let ndi, _, _ = CAst_utils.get_info_from_decl_ref dr in
           ndi.ni_name
       | _ -> "")
  | Stmt (ObjCIvarRefExpr(_, _, _, obj_c_ivar_ref_expr_info)) ->
      let ndi, _, _ = CAst_utils.get_info_from_decl_ref obj_c_ivar_ref_expr_info.ovrei_decl_ref in
      ndi.ni_name
  | Stmt (ObjCMessageExpr (_, _, _, {omei_selector})) ->
      omei_selector
  | _ -> ""

let infer_prefix = "__infer_ctl_"
let report_when_const = "report_when"
let message_const = "message"
let suggestion_const = "suggestion"
let severity_const = "severity"
let mode_const = "mode"

exception ALParsingException of string

(** Data structures for type parser.
    Correspondence with clang types inferred from
    StringRef BuiltinType::getName in
    https://clang.llvm.org/doxygen/Type_8cpp_source.html
*)
type builtin_kind =
  | Void (** void *)
  | Bool (** bool *)
  | Char_U (** char *)
  | UChar  (** unsigned char *)
  | WChar_U (** wchar_t *)
  | Char16  (** char16_t *)
  | Char32  (** char32_t *)
  | UShort (** unsigned short *)
  | UInt  (** unsigned int *)
  | ULong (** unsigned long *)
  | ULongLong (** unsigned long long *)
  | Int128 (** __int128 *)
  | UInt128 (** unsigned __int128 *)
  | SChar (** signed char *)
  | Short (** short *)
  | Int (** int *)
  | Long (** long *)
  | LongLong (** long long *)
  | Half (** half of __fp16 *)
  | Float (** float *)
  | Double (** double *)
  | LongDouble (** long double *)
  | Float128 (** __float128 *)
  | NullPtr (** nullptr_t *)
  | ObjCId (** id *)
  | ObjCClass (** Class *)
  | ObjCSel (** SEL *)
(*  | OCLSampler | OCLEvent | OCLClkEvent | OCLQueue | OCLNDRange
    | OCLReserveID | Dependent | Overload | BoundMember | PseudoObject
    | UnknownAny | BuiltinFn | ARCUnbridgedCast | OMPArraySection *)

let builtin_kind_to_string t =
  match t with
  | Char_U -> "char"
  | Char16 -> "char16_t"
  | Char32 -> "char32_t"
  | WChar_U -> "wchar_t"
  | Bool -> "bool"
  | Short -> "short"
  | Int -> "int"
  | Long -> "long"
  | Float -> "float"
  | Double -> "double"
  | Void -> "void"
  | SChar -> "signed char";
  | LongLong -> "long long";
  | UChar -> "unsigned char";
  | UShort -> "unsigned short";
  | UInt -> "unsigned int";
  | ULong -> "unsigned long";
  | ULongLong -> "unsigned long long";
  | LongDouble -> "long double";
  | Int128 -> "__int128"
  | Float128 -> "__float128"
  | UInt128 -> "unsigned __int128"
  | Half -> "half"
  | NullPtr -> "nullptr_t"
  | ObjCId -> "id"
  | ObjCClass -> "Class"
  | ObjCSel -> "SEL"

type abs_ctype =
  | BuiltIn of builtin_kind
  | Pointer of abs_ctype


let rec abs_ctype_to_string t =
  match t with
  | BuiltIn t' -> "BuiltIn (" ^ (builtin_kind_to_string t') ^ ")"
  | Pointer t' -> "Pointer (" ^ (abs_ctype_to_string t') ^ ")"

(* Temporary, partial equality function. Cover only what's covered
   by the types_parser. It needs to be replaced by a real
   comparison function for Clang_ast_t.c_type *)
let rec tmp_c_type_equal c_type abs_ctype =
  Logging.out
    "Comparing c_type/abs_ctype for equality... \
     Type compared: \nc_type = `%s`  \nabs_ctype =`%s`\n"
    (Clang_ast_j.string_of_c_type c_type)
    (abs_ctype_to_string abs_ctype);
  let open Clang_ast_t in
  match c_type, abs_ctype with
  | BuiltinType (_ , `Char_U), BuiltIn (Char_U)
  | BuiltinType (_ , `Char_S), BuiltIn (Char_U)
  | BuiltinType (_, `Char16), BuiltIn (Char16)
  | BuiltinType (_, `Char32), BuiltIn (Char32)
  | BuiltinType (_, `WChar_U), BuiltIn (WChar_U)
  | BuiltinType (_, `WChar_S), BuiltIn (WChar_U)
  | BuiltinType (_, `Bool), BuiltIn (Bool)
  | BuiltinType (_, `Short), BuiltIn (Short)
  | BuiltinType (_, `Int), BuiltIn (Int)
  | BuiltinType (_, `Long), BuiltIn (Long)
  | BuiltinType (_, `Float), BuiltIn (Float)
  | BuiltinType (_, `Double), BuiltIn (Double)
  | BuiltinType (_, `Void), BuiltIn (Void)
  | BuiltinType (_, `SChar), BuiltIn (SChar)
  | BuiltinType (_, `LongLong), BuiltIn (LongLong)
  | BuiltinType (_, `UChar), BuiltIn (UChar)
  | BuiltinType (_, `UShort), BuiltIn (UShort)
  | BuiltinType (_, `UInt), BuiltIn (UInt)
  | BuiltinType (_, `ULong), BuiltIn (ULong)
  | BuiltinType (_, `ULongLong), BuiltIn (ULongLong)
  | BuiltinType (_, `LongDouble), BuiltIn (LongDouble)
  | BuiltinType (_, `Int128), BuiltIn (Int128)
  | BuiltinType (_, `UInt128), BuiltIn (UInt128)
  | BuiltinType (_, `Float128), BuiltIn (Float128)
  | BuiltinType (_, `NullPtr), BuiltIn (NullPtr)
  | BuiltinType (_, `ObjCId), BuiltIn (ObjCId)
  | BuiltinType (_, `ObjCClass), BuiltIn (ObjCClass)
  | BuiltinType (_, `ObjCSel), BuiltIn (ObjCSel)
  | BuiltinType (_, `Half), BuiltIn (Half) -> true
  | PointerType (_, qt), Pointer abs_ctype' ->
      (match CAst_utils.get_type qt.qt_type_ptr with
       | Some c_type' ->
           tmp_c_type_equal c_type' abs_ctype'
       | None -> false)
  | _, _ ->
      Logging.out
        "[WARNING:] Type Comparison failed... \
         This might indicate that the types are different or the specified type \
         is internally represented in a different way and therefore not recognized.\n";
      false

(* to be extended with more types *)
let typ_string_of_type_ptr type_ptr =
  match CAst_utils.get_type type_ptr with
  | Some Clang_ast_t.BuiltinType (_, bt) ->
      Clang_ast_j.string_of_builtin_type_kind bt
  | _ -> ""

let ast_node_type an =
  match an with
  | Stmt stmt ->
      (match Clang_ast_proj.get_expr_tuple stmt with
       | Some (_, _, expr_info) ->
           typ_string_of_type_ptr expr_info.ei_qual_type.qt_type_ptr
       | _ -> "")
  | Decl decl ->
      (match CAst_utils.type_of_decl decl with
       | Some type_ptr ->
           typ_string_of_type_ptr type_ptr
       | _ -> "")
