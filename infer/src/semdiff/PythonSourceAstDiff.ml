(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CC = CongruenceClosureSolver
module Rewrite = CongruenceClosureRewrite
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
      let mk_atom_binding (field_name, ast) = mk_term field_name [curry cc ast] in
      mk_term header (List.map ~f:mk_atom_binding assoc)


let are_ast_equivalent cc ast1 ast2 rules =
  let atom1 = curry cc ast1 in
  let atom2 = curry cc ast2 in
  let _rounds = Rewrite.Rule.full_rewrite cc rules in
  CC.is_equiv cc atom1 atom2


let build_rules cc : Rewrite.Rule.t list =
  List.map
    ~f:(fun prog ->
      match Rewrite.parse_rule cc prog with
      | Ok ast ->
          ast
      | Error err ->
          L.die InternalError "%a" Rewrite.pp_parse_error err )
    [ "(List ... Null ...) ==> (List ...)"
    ; "(ImportFrom (level ?L) (module ?M) (names ?N)) ==> Null"
    ; "(Import (names ?N)) ==> Null"
    ; "(returns ?X) ==> (returns Null)"
    ; "(annotation ?X) ==> (annotation Null)"
    ; {|(AnnAssign (annotation ?A) (simple ?S) (target ?N) (value ?V))
        ==>
        (Assign (targets (List ?N)) (type_comment Null) (value ?V))|}
    ; {|(If
              (body ?BODY) (orelse ?LIST) (test
                                              (Compare
                                                  (comparators
                                                      (List (Name (ctx Load) (id str))))
                                                  (left
                                                      (Attribute
                                                          (attr __class__)
                                                          (ctx Load)
                                                          (value ?NAME)))
                                                  (ops (List Eq)))))
          ==>
          (If
              (body ?BODY) (orelse ?LIST) (test
                                              (Call
                                                  (args
                                                      (List ?NAME (Name (ctx Load) (id str))))
                                                  (func
                                                      (Name (ctx Load) (id isinstance)))
                                                  (keywords
                                                      List))))|}
    ]


let check_equivalence ast1 ast2 =
  let cc = CC.init ~debug:false in
  let rules = build_rules cc in
  are_ast_equivalent cc ast1 ast2 rules


module TestOnly = struct
  let store_ast ?(debug = false) ast =
    let cc = CC.init ~debug:false in
    let atom = curry cc ast in
    if debug then F.printf "%a" (CC.pp_nested_term cc) atom


  let are_ast_equivalent = are_ast_equivalent
end
