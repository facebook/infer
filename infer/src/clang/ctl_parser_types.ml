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


let infer_prefix = "__infer_ctl_"
let report_when_const = "report_when"
let message_const = "message"
let suggestion_const = "suggestion"
let severity_const = "severity"
let mode_const = "mode"

exception ALParsingException of string


(* Data structures for type parser.
   Correspondence with clang types inferred from
   StringRef BuiltinType::getName in
   https://clang.llvm.org/doxygen/Type_8cpp_source.html
*)
type builtin_kind =
  | Void (* void *)
  | Bool (* bool *)
  | Char_U (* char *)
  | UChar  (* unsigned char *)
  | WChar_U (* wchar_t *)
  | Char16  (* char16_t *)
  | Char32  (* char32_t *)
  | UShort (* unsigned short *)
  | UInt  (* unsigned int *)
  | ULong (* unsigned long *)
  | ULongLong (* unsigned long long *)
  | Int128 (* __int128 *)
  | UInt128 (* unsigned __int128 *)
  | SChar (* signed char *)
  | Short (* short *)
  | Int (* int *)
  | Long (* long *)
  | LongLong (* long long *)
  | Half (* half of __fp16 *)
  | Float (* float *)
  | Double (* double *)
  | LongDouble (* long double *)
  | Float128 (* __float128 *)
  | NullPtr (* nullptr_t *)
  | ObjCId (* id *)
  | ObjCClass (* Class *)
  | ObjCSel (* SEL *)
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

let abs_ctype_to_string t =
  match t with
  | BuiltIn t' -> "BuiltIn (" ^ (builtin_kind_to_string t') ^ ")"

(* Temporary, partial equality function. Cover only what's covered
   by the types_parser. It needs to be replaced by a real
   comparison function for Clang_ast_t.c_type *)
let tmp_c_type_equal ?name_c_type c_type abs_ctype =
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
  | _, _ ->
      let name = (match name_c_type with
          | None -> ""
          | Some n -> n) in
      Logging.out
        "[WARNING:] Type Comparison failed... \
         This might indicate that the types are different or the specified type \
         is internally represented in a different way and therefore not recognized. \
         Type compared: c_type = `%s`  abs_ctype =`%s`\n"
        name (abs_ctype_to_string abs_ctype);
      false
