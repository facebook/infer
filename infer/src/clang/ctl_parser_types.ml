(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
(* Types used by the ctl parser *)

open! IStd
module L = Logging

(** the kind of AST nodes where formulas are evaluated *)
type ast_node = Stmt of Clang_ast_t.stmt | Decl of Clang_ast_t.decl

let rec ast_node_name an =
  let open Clang_ast_t in
  match an with
  | Decl dec -> (
    match Clang_ast_proj.get_named_decl_tuple dec with
    | Some (_, n)
     -> n.Clang_ast_t.ni_name
    | None
     -> "" )
  | Stmt DeclRefExpr (_, _, _, drti) -> (
    match drti.drti_decl_ref with
    | Some dr
     -> let ndi, _, _ = CAst_utils.get_info_from_decl_ref dr in
        ndi.ni_name
    | _
     -> "" )
  | Stmt ObjCIvarRefExpr (_, _, _, obj_c_ivar_ref_expr_info)
   -> let ndi, _, _ = CAst_utils.get_info_from_decl_ref obj_c_ivar_ref_expr_info.ovrei_decl_ref in
      ndi.ni_name
  | Stmt ObjCMessageExpr (_, _, _, {omei_selector})
   -> omei_selector
  | Stmt IntegerLiteral (_, _, _, integer_literal_info)
   -> integer_literal_info.ili_value
  | Stmt CStyleCastExpr (_, _, _, cast_expr_info, _) -> (
    match cast_expr_info.cei_cast_kind with `NullToPointer -> "nil" | _ -> "" )
  | Stmt ObjCSubscriptRefExpr (_, [stmt; stmt_index], _, _)
   -> ast_node_name (Stmt stmt) ^ "[" ^ ast_node_name (Stmt stmt_index) ^ "]"
  | Stmt OpaqueValueExpr (_, _, _, opaque_value_expr_info) -> (
    match opaque_value_expr_info.ovei_source_expr with
    | Some stmt
     -> ast_node_name (Stmt stmt)
    | None
     -> "" )
  | Stmt ImplicitCastExpr (_, [stmt], _, _)
  | Stmt PseudoObjectExpr (_, stmt :: _, _)
  | Stmt ParenExpr (_, [stmt], _)
   -> ast_node_name (Stmt stmt)
  | Stmt CallExpr (_, func :: _, _)
   -> let func_str = ast_node_name (Stmt func) in
      func_str ^ "(...)"
  | Stmt ObjCPropertyRefExpr (_, [stmt], _, obj_c_property_ref_expr_info)
   -> let property_str =
        match obj_c_property_ref_expr_info.oprei_kind with
        | `MethodRef obj_c_method_ref_info -> (
          match (obj_c_method_ref_info.mri_getter, obj_c_method_ref_info.mri_setter) with
          | Some name, _
           -> name
          | _, Some name
           -> name
          | _
           -> "" )
        | `PropertyRef decl_ref ->
          match decl_ref.dr_name with Some name -> name.ni_name | None -> ""
      in
      ast_node_name (Stmt stmt) ^ "." ^ property_str
  | Stmt StringLiteral (_, _, _, s)
   -> s
  | Stmt ObjCStringLiteral (_, [stmt], _)
   -> "@" ^ ast_node_name (Stmt stmt)
  | Stmt ObjCBoxedExpr (_, [stmt], _, objc_boxed_expr_info)
   -> let selector =
        match objc_boxed_expr_info.obei_boxing_method with Some sel -> sel | None -> ""
      in
      selector ^ ast_node_name (Stmt stmt)
  | _
   -> ""

let infer_prefix = "__infer_ctl_"

exception ALParsingException of string

(** Data structures for type parser.
    Correspondence with clang types inferred from
    StringRef BuiltinType::getName in
    https://clang.llvm.org/doxygen/Type_8cpp_source.html
*)
type builtin_kind =
  | Void  (** void *)
  | Bool  (** bool *)
  | Char_U  (** char *)
  | UChar  (** unsigned char *)
  | WChar_U  (** wchar_t *)
  | Char16  (** char16_t *)
  | Char32  (** char32_t *)
  | UShort  (** unsigned short *)
  | UInt  (** unsigned int *)
  | ULong  (** unsigned long *)
  | ULongLong  (** unsigned long long *)
  | Int128  (** __int128 *)
  | UInt128  (** unsigned __int128 *)
  | SChar  (** signed char *)
  | Short  (** short *)
  | Int  (** int *)
  | Long  (** long *)
  | LongLong  (** long long *)
  | Half  (** half of __fp16 *)
  | Float  (** float *)
  | Double  (** double *)
  | LongDouble  (** long double *)
  | Float128  (** __float128 *)
  | NullPtr  (** nullptr_t *)
  | ObjCId  (** id *)
  | ObjCClass  (** Class *)
  | ObjCSel  (** SEL *)
  [@@deriving compare]

(*  | OCLSampler | OCLEvent | OCLClkEvent | OCLQueue | OCLNDRange
    | OCLReserveID | Dependent | Overload | BoundMember | PseudoObject
    | UnknownAny | BuiltinFn | ARCUnbridgedCast | OMPArraySection *)

let equal_builtin_kind = [%compare.equal : builtin_kind]

let builtin_kind_to_string t =
  match t with
  | Char_U
   -> "char"
  | Char16
   -> "char16_t"
  | Char32
   -> "char32_t"
  | WChar_U
   -> "wchar_t"
  | Bool
   -> "bool"
  | Short
   -> "short"
  | Int
   -> "int"
  | Long
   -> "long"
  | Float
   -> "float"
  | Double
   -> "double"
  | Void
   -> "void"
  | SChar
   -> "signed char"
  | LongLong
   -> "long long"
  | UChar
   -> "unsigned char"
  | UShort
   -> "unsigned short"
  | UInt
   -> "unsigned int"
  | ULong
   -> "unsigned long"
  | ULongLong
   -> "unsigned long long"
  | LongDouble
   -> "long double"
  | Int128
   -> "__int128"
  | Float128
   -> "__float128"
  | UInt128
   -> "unsigned __int128"
  | Half
   -> "half"
  | NullPtr
   -> "nullptr_t"
  | ObjCId
   -> "id"
  | ObjCClass
   -> "Class"
  | ObjCSel
   -> "SEL"

type abs_ctype =
  | BuiltIn of builtin_kind
  | Pointer of abs_ctype
  | TypeName of ALVar.alexp
  | ObjCGenProt of abs_ctype * abs_ctype

(* Objective-C Protocol or Generics *)

let display_equality_warning () =
  L.(debug Linters Medium)
    "[WARNING:] Type Comparison failed... This might indicate that the types are different or the specified type is internally represented in a different way and therefore not recognized.@\n"

let rec abs_ctype_to_string t =
  match t with
  | BuiltIn t'
   -> "BuiltIn (" ^ builtin_kind_to_string t' ^ ")"
  | Pointer t'
   -> "Pointer (" ^ abs_ctype_to_string t' ^ ")"
  | TypeName ae
   -> "TypeName (" ^ ALVar.alexp_to_string ae ^ ")"
  | ObjCGenProt (b, p)
   -> "ObjCGenProt (" ^ abs_ctype_to_string b ^ "," ^ abs_ctype_to_string p ^ ")"

let builtin_type_kind_assoc =
  [ (`Char_U, Char_U)
  ; (`Char_S, Char_U)
  ; (`Char16, Char16)
  ; (`Char32, Char32)
  ; (`WChar_U, WChar_U)
  ; (`WChar_S, WChar_U)
  ; (`Bool, Bool)
  ; (`Short, Short)
  ; (`Int, Int)
  ; (`Long, Long)
  ; (`Float, Float)
  ; (`Double, Double)
  ; (`Void, Void)
  ; (`SChar, SChar)
  ; (`LongLong, LongLong)
  ; (`UChar, UChar)
  ; (`UShort, UShort)
  ; (`UInt, UInt)
  ; (`ULong, ULong)
  ; (`ULongLong, ULongLong)
  ; (`LongDouble, LongDouble)
  ; (`Int128, Int128)
  ; (`UInt128, UInt128)
  ; (`Float128, Float128)
  ; (`NullPtr, NullPtr)
  ; (`ObjCId, ObjCId)
  ; (`ObjCClass, ObjCClass)
  ; (`ObjCSel, ObjCSel)
  ; (`Half, Half) ]

let builtin_equal (bi: Clang_ast_t.builtin_type_kind) (abi: builtin_kind) =
  match List.Assoc.find ~equal:PVariant.( = ) builtin_type_kind_assoc bi with
  | Some assoc_abi when equal_builtin_kind assoc_abi abi
   -> true
  | _
   -> display_equality_warning () ; false

let typename_to_string pointer =
  match CAst_utils.get_decl pointer with
  | Some decl -> (
    match Clang_ast_proj.get_named_decl_tuple decl with
    | Some (_, name_decl)
     -> Some name_decl.ni_name
    | None
     -> None )
  | _
   -> None

let rec pointer_type_equal p ap =
  let open Clang_ast_t in
  match (p, ap) with
  | PointerType (_, qt), Pointer abs_ctype' | ObjCObjectPointerType (_, qt), Pointer abs_ctype'
   -> check_type_ptr qt.qt_type_ptr abs_ctype'
  | _, _
   -> display_equality_warning () ; false

and objc_object_type_equal c_type abs_ctype =
  let open Clang_ast_t in
  let check_type_args abs_arg_type qt = check_type_ptr qt.qt_type_ptr abs_arg_type in
  let check_prot prot_name pointer =
    match prot_name with TypeName ae -> typename_equal pointer ae | _ -> false
  in
  match (c_type, abs_ctype) with
  | ObjCObjectType (_, ooti), ObjCGenProt (base, args) -> (
    match (CAst_utils.get_type ooti.base_type, ooti.protocol_decls_ptr, ooti.type_args) with
    | Some base_type, _ :: _, []
     -> c_type_equal base_type base && List.for_all ~f:(check_prot args) ooti.protocol_decls_ptr
    | Some base_type, [], _ :: _
     -> c_type_equal base_type base && List.for_all ~f:(check_type_args args) ooti.type_args
    | _
     -> false )
  | _
   -> false

and typename_equal pointer typename =
  match typename_to_string pointer with
  | Some name
   -> L.(debug Linters Medium)
        "Comparing typename '%s' and pointer '%s' for equality...@\n"
        (ALVar.alexp_to_string typename) name ;
      ALVar.compare_str_with_alexp name typename
  | None
   -> false

and check_type_ptr type_ptr abs_ctype =
  match CAst_utils.get_type type_ptr with
  | Some c_type'
   -> c_type_equal c_type' abs_ctype
  | None
   -> false

(* Temporary, partial equality function. Cover only what's covered
   by the types_parser. It needs to be replaced by a real
   comparison function for Clang_ast_t.c_type *)
and c_type_equal c_type abs_ctype =
  L.(debug Linters Medium)
    "@\nComparing c_type/abs_ctype for equality... Type compared: @\nc_type = `%s`  @\nabs_ctype =`%s`@\n"
    (Clang_ast_j.string_of_c_type c_type) (abs_ctype_to_string abs_ctype) ;
  let open Clang_ast_t in
  match (c_type, abs_ctype) with
  | BuiltinType (_, bi), BuiltIn abi
   -> builtin_equal bi abi
  | PointerType _, Pointer _ | ObjCObjectPointerType _, Pointer _
   -> pointer_type_equal c_type abs_ctype
  | ObjCObjectPointerType (_, qt), ObjCGenProt _
   -> check_type_ptr qt.qt_type_ptr abs_ctype
  | ObjCObjectType _, ObjCGenProt _
   -> objc_object_type_equal c_type abs_ctype
  | ObjCInterfaceType (_, pointer), TypeName ae
   -> typename_equal pointer ae
  | TypedefType (_, tdi), TypeName ae
   -> typename_equal tdi.tti_decl_ptr ae
  | TypedefType (ti, _), ObjCGenProt _ -> (
    match ti.ti_desugared_type with Some dt -> check_type_ptr dt abs_ctype | None -> false )
  | _, _
   -> display_equality_warning () ; false

(* to be extended with more types *)
let rec typ_string_of_type_ptr type_ptr =
  let open Clang_ast_t in
  match CAst_utils.get_type type_ptr with
  | Some BuiltinType (_, bt) -> (
    match List.Assoc.find ~equal:Poly.equal builtin_type_kind_assoc bt with
    | Some abt
     -> builtin_kind_to_string abt
    | None
     -> "" )
  | Some PointerType (_, qt) | Some ObjCObjectPointerType (_, qt)
   -> typ_string_of_type_ptr qt.qt_type_ptr ^ "*"
  | Some ObjCInterfaceType (_, pointer)
   -> Option.value ~default:"" (typename_to_string pointer)
  | Some TypedefType (_, tdi)
   -> Option.value ~default:"" (typename_to_string tdi.tti_decl_ptr)
  | _
   -> ""

let ast_node_type an =
  let typ_str =
    match an with
    | Stmt stmt -> (
      match Clang_ast_proj.get_expr_tuple stmt with
      | Some (_, _, expr_info)
       -> typ_string_of_type_ptr expr_info.ei_qual_type.qt_type_ptr
      | _
       -> "" )
    | Decl decl ->
      match CAst_utils.type_of_decl decl with
      | Some type_ptr
       -> typ_string_of_type_ptr type_ptr
      | _
       -> ""
  in
  if String.length typ_str > 0 then typ_str else "<type not known>"

let stmt_node_child_type an =
  match an with
  | Stmt stmt
   -> (
      let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
      match stmts with [stmt] -> ast_node_type (Stmt stmt) | _ -> "" )
  | _
   -> ""

let ast_node_cast_kind an =
  match an with
  | Decl _
   -> ""
  | Stmt stmt ->
    match Clang_ast_proj.get_cast_kind stmt with
    | Some cast_kind
     -> Clang_ast_proj.string_of_cast_kind cast_kind
    | None
     -> ""
