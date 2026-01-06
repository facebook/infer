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
let mk_const cc header = CC.mk_term cc (CC.mk_header cc header) []

let mk_term cc header args = CC.mk_term cc (CC.mk_header cc header) args

let mk_null cc = mk_const cc "Null"

let mk_list cc args = mk_term cc "List" args

let rec curry cc ast =
  match (ast : Node.t) with
  | Null ->
      mk_null cc
  | Bool b ->
      mk_const cc (F.asprintf "%b" b)
  | Float f ->
      mk_const cc (F.asprintf "%f" f)
  | Int i ->
      mk_const cc (F.asprintf "%d" i)
  | Str s ->
      mk_const cc s
  | List l ->
      mk_list cc (List.map ~f:(curry cc) l)
  | Dict dict ->
      let header, assoc = Node.assoc_of_dict dict in
      let mk_atom_binding (field_name, ast) = mk_term cc field_name [curry cc ast] in
      mk_term cc header (List.map ~f:mk_atom_binding assoc)


let are_ast_equivalent cc ast1 ast2 rules =
  let atom1 = curry cc ast1 in
  let atom2 = curry cc ast2 in
  let _rounds = Rewrite.Rule.full_rewrite cc rules in
  CC.is_equiv cc atom1 atom2


let diff_header_name = "@DIFF"

let build_diff cc ast1 ast2 =
  let atom1 = curry cc ast1 in
  let atom2 = curry cc ast2 in
  mk_term cc diff_header_name [atom1; atom2] |> ignore


let is_header_equivalent cc (header1 : CC.header) (header2 : CC.header) =
  CC.is_equiv cc (header1 :> CC.Atom.t) (header2 :> CC.Atom.t)


let gen_diff_commutative_rules cc =
  let open Rewrite in
  let open Pattern in
  let mk_args arity prefix =
    List.init arity ~f:(fun i -> Var (Var.of_string (prefix ^ string_of_int i)))
  in
  let resolved = mk_const cc "__DONE__" in
  let diff_header = CC.mk_header cc diff_header_name in
  let mk_diff_pattern pat1 pat2 = Term {header= diff_header; args= [pat1; pat2]} in
  CC.headers_with_arity cc
  |> List.filter_map ~f:(fun (header, arity) ->
         if is_header_equivalent cc header diff_header then None
         else if arity > 0 then
           let args1 = mk_args arity "X" in
           let args2 = mk_args arity "Y" in
           let lhs = mk_diff_pattern (Term {header; args= args1}) (Term {header; args= args2}) in
           let rhs = Term {header; args= List.map2_exn args1 args2 ~f:mk_diff_pattern} in
           Some (Rule.Regular {lhs; rhs; exclude= [resolved]})
         else None )


let gen_diff_rules cc =
  let open Rewrite in
  let open Pattern in
  let open Rule in
  let diff_header = CC.mk_header cc diff_header_name in
  let resolved_header = CC.mk_header cc "__DONE__" in
  let x = Var (Var.of_string "X") in
  Regular
    { lhs= Term {header= diff_header; args= [x; x]}
    ; rhs= Term {header= resolved_header; args= []}
    ; exclude= [] }
  :: gen_diff_commutative_rules cc


let get_unresolved_diffs cc =
  let resolved_atom = CC.mk_header cc "__DONE__" in
  let diff = CC.mk_header cc diff_header_name in
  let _is_marked_as_resolved_diff_root root =
    CC.is_equiv cc (root :> CC.Atom.t) (resolved_atom :> CC.Atom.t)
  in
  let is_a_diff_root =
    let set =
      CC.fold_term_roots cc diff ~init:CC.Atom.Set.empty ~f:(fun root set ->
          let root' = CC.representative cc root in
          CC.Atom.Set.add root' set )
    in
    fun atom -> CC.Atom.Set.mem atom set
  in
  let is_only_equivalent_to_diff_terms atom =
    CC.equiv_atoms cc atom |> List.for_all ~f:is_a_diff_root
  in
  CC.fold_term_roots cc diff ~init:[] ~f:(fun root acc ->
      let root' = CC.representative cc root in
      if is_only_equivalent_to_diff_terms root' then
        CC.equiv_terms cc root'
        |> List.fold ~init:acc ~f:(fun acc {CC.left; right= right_arg} ->
               CC.representative cc left |> CC.equiv_terms cc
               |> List.fold ~init:acc ~f:(fun acc {CC.right= left_arg} ->
                      (left_arg, right_arg) :: acc ) )
      else acc )


let parse_rules cc str_rules : Rewrite.Rule.t list =
  List.map str_rules ~f:(fun prog ->
      match Rewrite.parse_rule cc prog with
      | Ok ast ->
          ast
      | Error err ->
          L.die InternalError "%a" Rewrite.pp_parse_error err )


let gen_all_rules cc : Rewrite.Rule.t list =
  CC.set_app_right_neutral cc (mk_const cc "_epsilon_") ;
  parse_rules cc
    [ "(ImportFrom (level ?L) (module ?M) (names ?N)) ==> _epsilon_"
    ; "(Import (names ?N)) ==> _epsilon_"
    ; "(@DIFF (returns Null) (returns ?X)) ==> __DONE__"
    ; "(@DIFF (annotation Null) (annotation ?X)) ==> __DONE__"
    ; "(@DIFF (id Any) (id ?X)) ==> __DONE__"
    ; "(@DIFF (id Dict) (id dict)) ==> __DONE__"
    ; "(@DIFF (id FrozenSet) (id frozenset)) ==> __DONE__"
    ; "(@DIFF (id List) (id list)) ==> __DONE__"
    ; "(@DIFF (id Set) (id set)) ==> __DONE__"
    ; "(@DIFF (id Tuple) (id tuple)) ==> __DONE__"
    ; {|(Assign (targets (List ?N)) (type_comment Null) (value ?V))
        ==>
        (AnnAssign (annotation Null) (simple 1) (target ?N) (value ?V))|}
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
  @ gen_diff_rules cc


let check_equivalence ?expected ?(debug = false) ast1 ast2 =
  let cc = CC.init ~debug:false in
  build_diff cc ast1 ast2 ;
  let rules = gen_all_rules cc in
  let _rounds = Rewrite.Rule.full_rewrite ~debug cc rules in
  let diffs = get_unresolved_diffs cc in
  let res = List.is_empty diffs in
  if Option.exists expected ~f:(fun b -> not (Bool.equal b res)) then
    List.iter diffs ~f:(fun (left, right) ->
        F.printf "@[<hv 4>(DIFF@ %a@ %a)@]@." (CC.pp_nested_term cc) left (CC.pp_nested_term cc)
          right ) ;
  res


module TestOnly = struct
  let store_ast ?(debug = false) ast =
    let cc = CC.init ~debug:false in
    let atom = curry cc ast in
    if debug then F.printf "%a" (CC.pp_nested_term cc) atom


  let are_ast_equivalent = are_ast_equivalent

  let gen_diff_rules = gen_diff_rules

  let gen_all_rules = gen_all_rules
end
