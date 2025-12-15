(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver
open PythonSourceAst

(* currying AST -> CongruenceClosure terms *)
let rec curry cc ast =
  let mk_const header = CC.mk_term cc (CC.mk_header cc header) [] in
  let mk_term header args = CC.mk_term cc (CC.mk_header cc header) args in
  match (ast : Node.t) with
  | Null ->
      mk_const "Null"
  | Bool b ->
      mk_const (F.asprintf "%b" b)
  | Float f ->
      mk_const (F.asprintf "%f" f)
  | Int i ->
      mk_const (F.asprintf "%d" i)
  | Str s ->
      mk_const s
  | List l ->
      mk_term "List" (List.map ~f:(curry cc) l)
  | Dict dict ->
      let header, assoc = Node.assoc_of_dict dict in
      let mk_atom_binding (field_name, ast) =
        let left = mk_const field_name in
        let right = curry cc ast in
        CC.mk_app cc ~left ~right
      in
      mk_term header (List.map ~f:mk_atom_binding assoc)


let store_ast ?(debug = false) ast =
  let cc = CC.init ~debug:false in
  let atom = curry cc ast in
  if debug then F.printf "%a" (CC.pp_nested_term cc) atom
