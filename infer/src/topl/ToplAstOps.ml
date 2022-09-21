(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_pattern f (pattern : ToplAst.label_pattern) =
  match pattern with
  | ArrayWritePattern ->
      F.fprintf f "#ArrayWrite"
  | ProcedureNamePattern procedure_name ->
      F.fprintf f "\"%s\"" procedure_name


let pp_constant f (constant : ToplAst.constant) =
  match constant with LiteralInt x -> F.fprintf f "%d" x


let pp_register = F.pp_print_string

let pp_variable = F.pp_print_string

let pp_value f (value : ToplAst.value) =
  match value with
  | Constant c ->
      pp_constant f c
  | Register r ->
      pp_register f r
  | Binding v ->
      pp_variable f v


let pp_binop f (binop : ToplAst.binop) =
  match binop with
  | LeadsTo ->
      F.fprintf f "~~>"
  | OpEq ->
      F.fprintf f "=="
  | OpNe ->
      F.fprintf f "!="
  | OpGe ->
      F.fprintf f ">="
  | OpGt ->
      F.fprintf f ">"
  | OpLe ->
      F.fprintf f "<="
  | OpLt ->
      F.fprintf f "<"


let pp_predicate f (predicate : ToplAst.predicate) =
  match predicate with
  | Binop (op, l, r) ->
      F.fprintf f "@[%a%a%a@]@," pp_value l pp_binop op pp_value r
  | Value v ->
      F.fprintf f "@[%a@]" pp_value v


let pp_condition f (condition : ToplAst.condition) =
  match condition with
  | [] ->
      ()
  | predicates ->
      F.fprintf f "@ @[when@ %a@]" (Pp.seq ~sep:" && " pp_predicate) predicates


let pp_assignment f (register, variable) =
  F.fprintf f "@,@[%a=%a@]" pp_register register pp_variable variable


let pp_action f action =
  match action with
  | [] ->
      ()
  | assignments ->
      F.fprintf f "@ @[=>@ %a@]" (Pp.seq ~sep:"; " pp_assignment) assignments


let pp_arguments f arguments =
  match arguments with
  | None ->
      ()
  | Some arguments ->
      F.fprintf f "(%a)" (Pp.seq ~sep:"," pp_variable) arguments


let pp_raw_label f {ToplAst.pattern; arguments; condition; action} =
  F.fprintf f "@[%a%a@,%a%a@]" pp_pattern pattern pp_arguments arguments pp_condition condition
    pp_action action


let pp_label f label =
  match label with None -> F.fprintf f "*" | Some raw_label -> pp_raw_label f raw_label
