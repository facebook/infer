(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type cst_node =
  { tag: string
  ; field: string option
  ; srow: int
  ; scol: int
  ; erow: int
  ; ecol: int
  ; text: string
  ; children: cst_node list }

external parse_file : string -> cst_node = "caml_tree_sitter_parse_file"

let rec pp_cst_node fmt ?(indent = 0) node =
  let pad = String.make (indent * 2) ' ' in
  let field_str = match node.field with Some f -> F.sprintf " field=%s" f | None -> "" in
  if List.is_empty node.children then
    F.fprintf fmt "%s(%s%s %d:%d-%d:%d %S)@\n" pad node.tag field_str node.srow node.scol node.erow
      node.ecol node.text
  else (
    F.fprintf fmt "%s(%s%s %d:%d-%d:%d%s@\n" pad node.tag field_str node.srow node.scol node.erow
      node.ecol
      (if String.is_empty node.text then "" else F.sprintf " %S" node.text) ;
    List.iter node.children ~f:(pp_cst_node fmt ~indent:(indent + 1)) ;
    F.fprintf fmt "%s)@\n" pad )
