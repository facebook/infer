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

let rec eventual_child_name an =
  match an with
  | Stmt stmt ->
      (let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
       match stmts with
       | [stmt] ->
           let name = ast_node_name (Stmt stmt) in
           if String.length name > 0 then
             name
           else eventual_child_name (Stmt stmt)
       | _ -> "")
  | _ -> ""

let infer_prefix = "__infer_ctl_"

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
  | TypeName of ALVar.alexp

let display_equality_warning () =
  Logging.out
    "[WARNING:] Type Comparison failed... \
     This might indicate that the types are different or the specified type \
     is internally represented in a different way and therefore not recognized.\n"

let rec abs_ctype_to_string t =
  match t with
  | BuiltIn t' -> "BuiltIn (" ^ (builtin_kind_to_string t') ^ ")"
  | Pointer t' -> "Pointer (" ^ (abs_ctype_to_string t') ^ ")"
  | TypeName ae -> "TypeName (" ^ (ALVar.alexp_to_string ae) ^ ")"


let builtin_equal bi abi =
  match bi, abi with
  | `Char_U, Char_U
  | `Char_S, Char_U
  | `Char16, Char16
  | `Char32, Char32
  | `WChar_U, WChar_U
  | `WChar_S, WChar_U
  | `Bool, Bool
  | `Short, Short
  | `Int, Int
  | `Long, Long
  | `Float, Float
  | `Double, Double
  | `Void, Void
  | `SChar, SChar
  | `LongLong, LongLong
  | `UChar, UChar
  | `UShort, UShort
  | `UInt, UInt
  | `ULong, ULong
  | `ULongLong, ULongLong
  | `LongDouble, LongDouble
  | `Int128, Int128
  | `UInt128, UInt128
  | `Float128, Float128
  | `NullPtr, NullPtr
  | `ObjCId, ObjCId
  | `ObjCClass, ObjCClass
  | `ObjCSel, ObjCSel
  | `Half, Half -> true
  | _, _ -> display_equality_warning ();
      false

let typename_to_string pointer =
  match CAst_utils.get_decl pointer  with
  | Some decl ->
      (match Clang_ast_proj.get_named_decl_tuple decl with
       | Some (_, name_decl) -> Some name_decl.ni_name
       | None -> None)
  | _ -> None

let rec pointer_type_equal p ap =
  let open Clang_ast_t in
  match p, ap with
  | PointerType (_, qt), Pointer abs_ctype'
  | ObjCObjectPointerType (_, qt), Pointer abs_ctype' ->
      (match CAst_utils.get_type qt.qt_type_ptr with
       | Some c_type' ->
           c_type_equal c_type' abs_ctype'
       | None -> false)
  | _, _ -> display_equality_warning ();
      false

and typename_equal pointer typename =
  match typename_to_string pointer  with
  | Some name ->
      ALVar.compare_str_with_alexp name typename
  | None -> false

(* Temporary, partial equality function. Cover only what's covered
   by the types_parser. It needs to be replaced by a real
   comparison function for Clang_ast_t.c_type *)
and c_type_equal c_type abs_ctype =
  Logging.out
    "Comparing c_type/abs_ctype for equality... \
     Type compared: \nc_type = `%s`  \nabs_ctype =`%s`\n"
    (Clang_ast_j.string_of_c_type c_type)
    (abs_ctype_to_string abs_ctype);
  let open Clang_ast_t in
  match c_type, abs_ctype with
  | BuiltinType (_ , bi), BuiltIn abi ->
      builtin_equal bi abi
  | PointerType _, Pointer _
  | ObjCObjectPointerType _, Pointer _ ->
      pointer_type_equal c_type abs_ctype
  | ObjCInterfaceType (_, pointer), TypeName ae ->
      typename_equal pointer ae
  | TypedefType (_, tdi), TypeName ae ->
      typename_equal tdi.tti_decl_ptr ae
  | _, _ -> display_equality_warning ();
      false

(* to be extended with more types *)
let rec typ_string_of_type_ptr type_ptr =
  let open Clang_ast_t in
  match CAst_utils.get_type type_ptr with
  | Some BuiltinType (_, bt) ->
      Clang_ast_j.string_of_builtin_type_kind bt
  | Some PointerType (_, qt)
  | Some ObjCObjectPointerType (_, qt) ->
      (typ_string_of_type_ptr qt.qt_type_ptr) ^ "*"
  | Some ObjCInterfaceType (_, pointer) ->
      Option.value ~default:"" (typename_to_string pointer)
  | Some TypedefType (_, tdi) ->
      Option.value ~default:"" (typename_to_string tdi.tti_decl_ptr)
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

let stmt_node_child_type an =
  match an with
  | Stmt stmt ->
      (let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
       match stmts with
       | [stmt] -> ast_node_type (Stmt stmt)
       | _ -> "")
  | _ -> ""

let ast_node_cast_kind an =
  match an with
  | Decl _ -> ""
  | Stmt stmt ->
      match Clang_ast_proj.get_cast_kind stmt with
      | Some cast_kind -> Clang_ast_proj.string_of_cast_kind cast_kind
      | None -> ""
