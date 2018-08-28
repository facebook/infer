(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type condition = Case of Clang_ast_t.stmt | Default

type t = {condition: condition; stmt_info: Clang_ast_t.stmt_info; root_nodes: Procdesc.Node.t list}

val in_switch_body : f:('a -> 'b) -> 'a -> t list * 'b

val add : t -> unit

val pp_condition : F.formatter -> condition -> unit [@@warning "-32"]

val pp : F.formatter -> t -> unit [@@warning "-32"]
