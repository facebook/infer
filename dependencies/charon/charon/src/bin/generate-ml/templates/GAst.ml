(** WARNING: this file is partially auto-generated. Do not edit `GAst.ml`
    by hand. Edit `GAst.template.ml` instead, or improve the code
    generation tool so avoid the need for hand-writing things.

    `GAst.template.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`.
 *)

open Types
open Meta
open Expressions

module FunDeclId = Expressions.FunDeclId
module GlobalDeclId = Expressions.GlobalDeclId
module TraitDeclId = Types.TraitDeclId
module TraitImplId = Types.TraitImplId
module TraitClauseId = Types.TraitClauseId

(* Imports *)
type builtin_fun_id = Types.builtin_fun_id [@@deriving show, ord]
type fun_id = Types.fun_id [@@deriving show, ord]
type fun_id_or_trait_method_ref = Types.fun_id_or_trait_method_ref [@@deriving show, ord]
type fun_decl_id = Types.fun_decl_id [@@deriving show, ord]

(* __REPLACE0__ *)

(* __REPLACE1__ *)

(* __REPLACE2__ *)

(* __REPLACE3__ *)

(* __REPLACE4__ *)
[@@deriving show]
