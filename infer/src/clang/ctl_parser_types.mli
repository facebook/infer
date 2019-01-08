(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* Types used by the ctl parser *)

open! IStd

(** the kind of AST nodes where formulas are evaluated *)
type ast_node = Stmt of Clang_ast_t.stmt | Decl of Clang_ast_t.decl

val ast_node_equal : ast_node -> ast_node -> bool

val ast_node_name : ast_node -> string

val ast_node_cxx_fully_qualified_name : ast_node -> string

val ast_node_type : ast_node -> string

val ast_node_kind : ast_node -> string

val ast_node_pointer : ast_node -> int

val ast_node_has_kind : ALVar.alexp list -> ast_node -> bool

val ast_node_unique_string_id : ast_node -> string

val stmt_node_child_type : ast_node -> string

val ast_node_cast_kind : ast_node -> string

val is_node_successor_of : is_successor:ast_node -> ast_node -> bool

val get_direct_successor_nodes : ast_node -> ast_node list

val infer_prefix : string

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

type abs_ctype =
  | BuiltIn of builtin_kind
  | Pointer of abs_ctype
  | Reference of abs_ctype
  | TypeName of ALVar.alexp
  | ObjCGenProt of abs_ctype * abs_ctype

(* Objective-C Protocol or Generics *)

val c_type_equal : Clang_ast_t.c_type -> abs_ctype -> bool

val abs_ctype_to_string : abs_ctype -> string
