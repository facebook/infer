(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type exception_details =
  { msg: string
  ; position: Logging.ocaml_pos
  ; source_range: Clang_ast_t.source_range
  ; ast_node: string option }

exception Unimplemented of exception_details

exception IncorrectAssumption of exception_details

exception Invalid_declaration

let unimplemented position source_range ?ast_node fmt =
  F.kasprintf (fun msg -> raise (Unimplemented {msg; position; source_range; ast_node})) fmt


let incorrect_assumption position source_range ?ast_node fmt =
  F.kasprintf (fun msg -> raise (IncorrectAssumption {msg; position; source_range; ast_node})) fmt
