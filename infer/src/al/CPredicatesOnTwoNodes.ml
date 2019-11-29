(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let decl_name_is_contained_in_name_of_decl (node1 : Ctl_parser_types.ast_node)
    (node2 : Ctl_parser_types.ast_node) =
  let get_name decl =
    match Clang_ast_proj.get_named_decl_tuple decl with
    | Some (_, ndi) ->
        Some ndi.ni_name
    | None ->
        None
  in
  match (node1, node2) with
  | Decl decl1, Decl decl2 -> (
    match (get_name decl1, get_name decl2) with
    | Some name1, Some name2 ->
        String.is_substring name2 ~substring:name1
    | _ ->
        false )
  | _ ->
      false
