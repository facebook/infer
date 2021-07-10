(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Clang_ast_t

#define s(x) #x

let get_decl_kind_string = function
#define DECL(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> s(DERIVED) ^ "Decl"
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>

let get_decl_tuple = function
#define DECL(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> (decl_tuple)
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>

let update_decl_tuple __f = function
#define DECL(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (decl_tuple) = __f (decl_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>


let get_decl_context_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define MY_DECL_CONTEXT(DERIVED) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (decl_context_tuple)
(* skipping Function and ObjCMethod *)
#define TAG(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define OBJCCONTAINER(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define CAPTURED(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define LINKAGESPEC(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define NAMESPACE(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define TRANSLATIONUNIT(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#include <clang/AST/DeclNodes.inc>
#undef MY_DECL_CONTEXT
| _ -> None

let update_decl_context_tuple __f = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define MY_DECL_CONTEXT(DERIVED) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (decl_context_tuple) = __f (decl_context_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
(* skipping Function and ObjCMethod *)
#define TAG(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define OBJCCONTAINER(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define CAPTURED(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define LINKAGESPEC(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define NAMESPACE(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#define TRANSLATIONUNIT(DERIVED, BASE) MY_DECL_CONTEXT(DERIVED)
#include <clang/AST/DeclNodes.inc>
#undef MY_DECL_CONTEXT
| x -> x


let get_function_decl_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define FUNCTION(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (function_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| _ -> None

let get_named_decl_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define NAMED(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (named_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| _ -> None

let update_named_decl_tuple __f = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define NAMED(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (named_decl_tuple) = __f (named_decl_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| x -> x


let get_type_decl_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define TYPE(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (type_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| _ -> None

let update_type_decl_tuple __f = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define TYPE(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (type_decl_tuple) = __f (type_decl_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| x -> x


let get_tag_decl_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define TAG(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (tag_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| _ -> None

let update_tag_decl_tuple __f = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define TAG(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (tag_decl_tuple) = __f (tag_decl_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| x -> x


let get_var_decl_tuple = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define VAR(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> Some (var_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| _ -> None

let update_var_decl_tuple __f = function
#define DECL(DERIVED, BASE)
#define ABSTRACT_DECL(DECL)
#define VAR(DERIVED, BASE) | DERIVED@@Decl (@DERIVED@_decl_tuple) -> \
    let (var_decl_tuple) = __f (var_decl_tuple) in DERIVED@@Decl (@DERIVED@_decl_tuple)
#include <clang/AST/DeclNodes.inc>
| x -> x


let get_stmt_kind_string = function
#define STMT(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> s(CLASS)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>

let get_stmt_tuple = function
#define STMT(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> (stmt_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>

let update_stmt_tuple __f = function
#define STMT(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> \
    let (stmt_tuple) = __f (stmt_tuple) in CLASS (@CLASS@_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>


let get_expr_tuple = function
#define STMT(CLASS, PARENT)
#define EXPR(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> Some (expr_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
| _ -> None

let update_expr_tuple __f = function
#define STMT(CLASS, PARENT)
#define EXPR(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> \
    let (expr_tuple) = __f (expr_tuple) in CLASS (@CLASS@_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
| x -> x

let get_cxx_construct_expr_tuple = function
#define STMT(CLASS, PARENT)
#define CXXCONSTRUCTEXPR(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> Some (cxx_construct_expr_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
| _ -> None

let update_cxx_construct_expr_tuple __f = function
#define STMT(CLASS, PARENT)
#define CXXCONSTRUCTEXPR(CLASS, PARENT) | CLASS (@CLASS@_tuple) -> \
    let (cxx_construct_expr_tuple) = __f (cxx_construct_expr_tuple) in CLASS (@CLASS@_tuple)
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
| x -> x

let get_type_tuple = function
#define TYPE(DERIVED, BASE) | DERIVED@@Type (@DERIVED@_type_tuple) -> (type_tuple)
#define ABSTRACT_TYPE(DERIVED, BASE)
TYPE(None, Type) (*  special case for nullptr type *)
#include <clang/AST/TypeNodes.inc>

let is_valid_binop_kind_name = function
#define BINARY_OPERATION(Name, Spelling) | s(Name) -> true
#include <clang/AST/OperationKinds.def>
| _ -> false

let is_valid_unop_kind_name = function
#define UNARY_OPERATION(Name, Spelling) | s(Name) -> true
#include <clang/AST/OperationKinds.def>
| _ -> false

let string_of_binop_kind = function
#define BINARY_OPERATION(Name, Spelling) | `Name -> s(Name)
#include <clang/AST/OperationKinds.def>

let string_of_unop_kind = function
#define UNARY_OPERATION(Name, Spelling) | `Name -> s(Name)
#include <clang/AST/OperationKinds.def>

let is_valid_astnode_kind = function
#define DECL(DERIVED, BASE) | s(DERIVED@@Decl) -> true
#define ABSTRACT_DECL(DECL)
#include <clang/AST/DeclNodes.inc>
#define STMT(CLASS, PARENT) | s(CLASS) -> true
#define ABSTRACT_STMT(STMT)
#include <clang/AST/StmtNodes.inc>
| _ -> false

let string_of_cast_kind = function
#define CAST_OPERATION(Name) | `Name -> s(Name)
#include <clang/AST/OperationKinds.def>

let get_cast_kind = function
#define STMT(CLASS, PARENT)
#define ABSTRACT_STMT(TYPE)
#define CASTEXPR(Type, Base) | Type (_, _, _, cast_expr_info)
#define EXPLICITCASTEXPR(Type, Base) | Type (_, _, _, cast_expr_info, _)
#define IMPLICITCASTEXPR(Type, Base) | Type (_, _, _, cast_expr_info, _)
#define CXXNAMEDCASTEXPR(Type, Base) | Type (_, _, _, cast_expr_info, _, _)
#define OBJCBRIDGEDCASTEXPR(Type, Base) | Type (_, _, _, cast_expr_info, _, _)
#include<clang/AST/StmtNodes.inc>
-> Some cast_expr_info.cei_cast_kind
| _ -> None
