(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open !Utils

module F = Format

(* string set domain we use to ensure we're getting the expected traces *)
module MockTraceDomain =
  AbstractDomain.FiniteSet
    (PrettyPrintable.MakePPSet(struct
       include String
       let pp_element fmt s = Format.fprintf fmt "%s" s
     end))

module Domain = AccessTree.Make (MockTraceDomain)

let make_base base_str =
  Pvar.mk (Mangled.from_string base_str) Procname.empty_block, Typ.Tvoid

let make_field_access access_str =
  AccessPath.FieldAccess (Ident.create_fieldname (Mangled.from_string access_str) 0, Typ.Tvoid)

let make_access_path base_str accesses =
  let rec make_accesses accesses_acc = function
    | [] -> accesses_acc
    | access_str :: l -> make_accesses ((make_field_access access_str) :: accesses_acc) l in
  let accesses = make_accesses [] accesses in
  make_base base_str, IList.rev accesses

let tests =
  let x_base = make_base "x" in
  let y_base = make_base "y" in
  let z_base = make_base "z" in

  let f = make_field_access "f" in
  let g = make_field_access "g" in

  let xF = AccessPath.Exact (make_access_path "x" ["f"]) in
  let xG = AccessPath.Exact (make_access_path "x" ["g"]) in
  let xFG = AccessPath.Exact (make_access_path "x" ["f"; "g"]) in
  let yF = AccessPath.Exact (make_access_path "y" ["f"]) in
  let yG = AccessPath.Exact (make_access_path "y" ["g"]) in
  let yFG = AccessPath.Exact (make_access_path "y" ["f"; "g"]) in
  let z = AccessPath.Exact (make_access_path "z" []) in
  let zF = AccessPath.Exact (make_access_path "z" ["f"]) in
  let zFG = AccessPath.Exact (make_access_path "z" ["f"; "g"]) in

  let a_star = AccessPath.Abstracted (make_access_path "a" []) in
  let x_star = AccessPath.Abstracted (make_access_path "x" []) in
  let xF_star = AccessPath.Abstracted (make_access_path "x" ["f"]) in
  let xG_star = AccessPath.Abstracted (make_access_path "x" ["g"]) in
  let y_star = AccessPath.Abstracted (make_access_path "y" []) in
  let yF_star = AccessPath.Abstracted (make_access_path "y" ["f"]) in
  let z_star = AccessPath.Abstracted (make_access_path "z" []) in

  let x_trace = MockTraceDomain.singleton "x" in
  let y_trace = MockTraceDomain.singleton "y" in
  let z_trace = MockTraceDomain.singleton "z" in
  let xF_trace = MockTraceDomain.singleton "xF" in
  let yF_trace = MockTraceDomain.singleton "yF" in
  let xFG_trace = MockTraceDomain.singleton "xFG" in
  let x_star_trace = MockTraceDomain.of_list ["x"; "xF"; "xFG"] in

  let x_subtree =
    let g_subtree = Domain.make_access_node xF_trace g xFG_trace in
    Domain.AccessMap.singleton f g_subtree
    |> Domain.make_node x_trace in
  let y_subtree =
    let yF_subtree = Domain.make_starred_leaf yF_trace in
    Domain.AccessMap.singleton f yF_subtree
    |> Domain.make_node y_trace in
  let z_subtree = Domain.make_starred_leaf z_trace in

  let tree =
    Domain.BaseMap.singleton x_base x_subtree
    |> Domain.BaseMap.add y_base y_subtree
    |> Domain.BaseMap.add z_base z_subtree in
  let x_base_tree = Domain.BaseMap.singleton x_base Domain.empty_node in
  let y_base_tree = Domain.BaseMap.singleton y_base Domain.empty_node in
  let x_y_base_tree = Domain.BaseMap.add y_base Domain.empty_node x_base_tree in
  let xFG_tree = Domain.BaseMap.singleton x_base x_subtree in

  let x_star_tree = Domain.BaseMap.singleton x_base (Domain.make_starred_leaf x_trace) in
  let yF_star_tree = Domain.BaseMap.singleton y_base y_subtree in
  let x_yF_star_tree = Domain.BaseMap.add y_base y_subtree x_star_tree in
  let x_star_tree_xFG_trace =
    Domain.BaseMap.singleton x_base (Domain.make_starred_leaf x_star_trace) in

  let open OUnit2 in
  let no_trace = "NONE" in

  let get_trace_str access_path tree =
    match Domain.get_trace access_path tree with
    | Some trace -> pp_to_string MockTraceDomain.pp trace
    | None -> no_trace in

  let assert_traces_eq access_path tree expected_trace_str =
    let actual_trace_str = get_trace_str access_path tree in
    let pp_diff fmt (actual, expected) =
      F.fprintf fmt "Expected to retrieve trace %s but got %s" expected actual in
    assert_equal ~pp_diff actual_trace_str expected_trace_str in

  let assert_trace_not_found access_path tree =
    assert_traces_eq access_path tree no_trace in

  let get_trace_test =
    let get_trace_test_ _ =
      (* exact access path tests *)
      assert_traces_eq z tree "{ z }";
      assert_traces_eq xF tree "{ xF }";
      assert_traces_eq yF tree "{ yF }";
      assert_traces_eq xFG tree "{ xFG }";
      assert_trace_not_found xG tree;

      (* starred access path tests *)
      assert_traces_eq x_star tree "{ x, xF, xFG }";
      assert_traces_eq xF_star tree "{ xF, xFG }";
      assert_trace_not_found xG_star tree;
      assert_trace_not_found a_star tree;

      (* starred tree tests *)
      assert_traces_eq zF tree "{ z }";
      assert_traces_eq zFG tree "{ z }";
      assert_traces_eq z_star tree "{ z }";
      assert_traces_eq y_star tree "{ y, yF }";
      assert_traces_eq yF_star tree "{ yF }";
      assert_traces_eq yFG tree "{ yF }";
      assert_trace_not_found yG tree in
    "get_trace">::get_trace_test_ in

  let lteq_test =
    let lteq_test_ _ =
      (* regular tree tests *)
      assert_bool "<= equal;" (Domain.(<=) ~lhs:tree ~rhs:tree);
      assert_bool "<= bases" (Domain.(<=) ~lhs:x_base_tree ~rhs:x_y_base_tree);
      assert_bool "<= regular1" (Domain.(<=) ~lhs:x_base_tree ~rhs:xFG_tree);
      assert_bool "<= regular2" (Domain.(<=) ~lhs:xFG_tree ~rhs:tree);
      assert_bool "<= regular3" (Domain.(<=) ~lhs:y_base_tree ~rhs:tree);
      assert_bool "<= bases negative1" (not (Domain.(<=) ~lhs:x_y_base_tree ~rhs:x_base_tree));
      assert_bool "<= bases negative2" (not (Domain.(<=) ~lhs:x_base_tree ~rhs:y_base_tree));
      assert_bool "<= negative1" (not (Domain.(<=) ~lhs:xFG_tree ~rhs:y_base_tree));
      assert_bool "<= negative2" (not (Domain.(<=) ~lhs:tree ~rhs:xFG_tree));

      (* star tree tests *)
      assert_bool "<= star lhs equal" (Domain.(<=) ~lhs:x_star_tree ~rhs:x_star_tree);
      assert_bool "<= star rhs1" (Domain.(<=) ~lhs:x_base_tree ~rhs:x_star_tree);
      assert_bool "<= star rhs2" (Domain.(<=) ~lhs:xFG_tree ~rhs:x_star_tree);
      assert_bool "<= star rhs3" (Domain.(<=) ~lhs:y_base_tree ~rhs:yF_star_tree);
      assert_bool "<= star rhs4" (Domain.(<=) ~lhs:yF_star_tree ~rhs:tree);
      assert_bool "<= star lhs negative1" (not (Domain.(<=) ~lhs:x_star_tree ~rhs:x_base_tree));
      assert_bool "<= star lhs negative2" (not (Domain.(<=) ~lhs:x_star_tree ~rhs:xFG_tree));
      assert_bool "<= star lhs negative3" (not (Domain.(<=) ~lhs:yF_star_tree ~rhs:y_base_tree));
      assert_bool "<= star lhs negative4" (not (Domain.(<=) ~lhs:tree ~rhs:yF_star_tree));

      (* <= tree but not <= trace tests *)
      (* same as x_base_tree, but with a trace higher in the traces lattice *)
      let x_base_tree_higher_trace =
        Domain.BaseMap.singleton x_base (Domain.make_normal_leaf y_trace) in
      (* same as x_star_tree, but with a trace incomparable in the traces lattice *)
      let x_star_tree_diff_trace =
        Domain.BaseMap.singleton x_base (Domain.make_starred_leaf y_trace) in
      assert_bool
        "(x, {}) <= (x, {y})"
        (Domain.(<=) ~lhs:x_base_tree ~rhs:x_base_tree_higher_trace);
      assert_bool
        "(x, {y}) not <= (x, {})"
        (not (Domain.(<=) ~lhs:x_base_tree_higher_trace ~rhs:x_base_tree));
      assert_bool
        "(x*, {y})* not <= (x*, {x})"
        (not (Domain.(<=) ~lhs:x_star_tree_diff_trace ~rhs:x_star_tree));
      assert_bool
        "(x*, {x})* not <= (x*, {y})"
        (not (Domain.(<=) ~lhs:x_star_tree ~rhs:x_star_tree_diff_trace)) in
    "lteq">::lteq_test_ in

  let assert_trees_equal tree1 tree2 =
    let rec access_tree_equal (trace1, subtree1) (trace2, subtree2) =
      MockTraceDomain.equal trace1 trace2 && match subtree1, subtree2 with
      | Domain.Star, Domain.Star -> true
      | Domain.Subtree t1, Domain.Subtree t2 -> Domain.AccessMap.equal access_tree_equal t1 t2
      | _ -> false in
    let base_tree_equal tree1 tree2 =
      Domain.BaseMap.equal access_tree_equal tree1 tree2 in
    let pp_diff fmt (actual, expected) =
      F.fprintf fmt "Expected to get tree %a but got %a" Domain.pp expected Domain.pp actual in
    assert_equal ~cmp:base_tree_equal ~pp_diff tree1 tree2 in

  let join_test =
    let join_test_ _ =
      (* normal |_| normal *)
      assert_trees_equal (Domain.join x_base_tree y_base_tree) x_y_base_tree;
      assert_trees_equal (Domain.join y_base_tree x_base_tree) x_y_base_tree;
      assert_trees_equal (Domain.join x_y_base_tree x_base_tree) x_y_base_tree;
      assert_trees_equal (Domain.join x_base_tree xFG_tree) xFG_tree;

      (* starred |_| starred *)
      assert_trees_equal (Domain.join x_star_tree yF_star_tree) x_yF_star_tree;

      (* normal |_| starred *)
      assert_trees_equal (Domain.join tree xFG_tree) tree;
      (* [x_star_tree] and [x_base_tree] both have trace "{ x }" associated with x... *)
      assert_trees_equal (Domain.join x_star_tree x_base_tree) x_star_tree;
      (* ...but [xFG_tree] has some nested traces that should get joined with "{ x }" *)
      assert_trees_equal (Domain.join x_star_tree xFG_tree) x_star_tree_xFG_trace in
    "join">::join_test_ in

  "access_tree_suite">:::[get_trace_test; lteq_test; join_test]
