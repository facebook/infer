(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** FFI bindings to the bundled tree-sitter runtime and C grammar. Parses C source files directly
    without shelling out to the tree-sitter CLI. *)

(** Tree-sitter CST node, matching the structure produced by both the XML parser and the C FFI. *)
type cst_node =
  { tag: string  (** node type, e.g. "function_definition", "identifier" *)
  ; field: string option  (** field name in parent, e.g. "type", "declarator", "body" *)
  ; srow: int
  ; scol: int
  ; erow: int
  ; ecol: int
  ; text: string  (** text content for leaf nodes *)
  ; children: cst_node list }

val parse_file : string -> cst_node
(** Parse a C source file and return the root CST node (translation_unit). *)

val pp_cst_node : Format.formatter -> ?indent:int -> cst_node -> unit
(** Pretty-print a CST node tree in S-expression style for debugging. *)
