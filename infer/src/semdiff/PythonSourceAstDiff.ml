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
  match (ast : Node.t) with
  | Null ->
      CC.mk_atom cc "Null"
  | Bool b ->
      CC.mk_atom cc (F.asprintf "%b" b)
  | Float f ->
      CC.mk_atom cc (F.asprintf "%f" f)
  | Int i ->
      CC.mk_atom cc (F.asprintf "%d" i)
  | Str s ->
      CC.mk_atom cc s
  | List l ->
      CC.mk_term cc ~header:"List" ~args:(List.map ~f:(curry cc) l)
  | Dict dict ->
      let header, assoc = Node.assoc_of_dict dict in
      let mk_atom_binding (field_name, ast) =
        let left = CC.mk_atom cc field_name in
        let right = curry cc ast in
        CC.mk_app cc ~left ~right
      in
      CC.mk_term cc ~header ~args:(List.map ~f:mk_atom_binding assoc)


let store_ast ast =
  let cc = CC.init ~debug:false in
  let atom = curry cc ast in
  CC.pp_nested_term cc atom
