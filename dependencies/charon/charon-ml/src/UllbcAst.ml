open Types
open Values
open Expressions
open Meta
open Identifiers
include GAst
include Generated_UllbcAst

type expr_body = blocks gexpr_body [@@deriving show]
type fun_body = expr_body [@@deriving show]
type fun_decl = blocks gfun_decl [@@deriving show]

(** ULLBC crate *)
type crate = blocks gcrate [@@deriving show]
