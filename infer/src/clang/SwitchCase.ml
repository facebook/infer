(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type condition = Case of Clang_ast_t.stmt | Default

type t = {condition: condition; stmt_info: Clang_ast_t.stmt_info; root_nodes: Procdesc.Node.t list}

let current_cases : t list ref = ref []

let in_switch_body ~f x =
  let outer_switch_cases = !current_cases in
  current_cases := [] ;
  let res = f x in
  let rev_switch_cases = !current_cases in
  current_cases := outer_switch_cases ;
  (List.rev rev_switch_cases, res)


let add switch_case = current_cases := switch_case :: !current_cases

let pp_condition fmt = function
  | Case stmt ->
      F.fprintf fmt "case %a:" (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string) stmt
  | Default ->
      F.pp_print_string fmt "default:"


let pp fmt {condition; root_nodes} =
  F.fprintf fmt "%a->@[<h>[%a]@]" pp_condition condition
    (Pp.semicolon_seq Procdesc.Node.pp)
    root_nodes
