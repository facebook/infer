(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Abstract synyax tree for code queries. *)

module L = Logging
module F = Format

type expr =
  | Null
  | Ident of string
  | ConstString of string

type rule =
  | Call of expr * expr
  | MethodCall of expr * expr * (expr list option)
  | If of expr * string * expr * rule

type action =
  | Noaction
  | Source of (int * int) option (* (x,y) represents x lines before and y lines after the source location *)
  | Error of string option (* print an error *)

type query =
  rule * action

let pp_action fmt = function
  | Noaction -> ()
  | Source None -> F.fprintf fmt "@source"
  | Source (Some(x, y)) -> F.fprintf fmt "@source(%d,%d)" x y
  | Error None -> F.fprintf fmt "@error"
  | Error (Some s) -> F.fprintf fmt "@error(%s)" s

let pp_expr fmt = function
  | Null -> F.fprintf fmt "null"
  | Ident s -> F.fprintf fmt "%s" s
  | ConstString s -> F.fprintf fmt "%s" s

let pp_expr_list_option fmt = function
  | None -> F.fprintf fmt "*"
  | Some el -> pp_comma_seq pp_expr fmt el

let rec pp_rule fmt = function
  | Call (ae1, ae2) -> F.fprintf fmt "%a(%a)" pp_expr ae1 pp_expr ae2
  | MethodCall (ae1, ae2, elo) -> F.fprintf fmt "%a.%a(%a)" pp_expr ae1 pp_expr ae2 pp_expr_list_option elo
  | If (ae1, s, ae2, rule) -> F.fprintf fmt "if(%a %s %a) ... %a" pp_expr ae1 s pp_expr ae2 pp_rule rule

let pp_query fmt (rule, action) =
  F.fprintf fmt "%a; %a" pp_rule rule pp_action action
