(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* string set domain we use to ensure we're getting the expected traces *)
module MockTraceDomain = struct
  include AbstractDomain.FiniteSet (struct
    include String

    let pp = F.pp_print_string
  end)

  let top_str = "T"

  let top = singleton top_str

  let singleton e =
    assert (String.(e <> top_str)) ;
    singleton e


  (* total hack of a widening just to test that widening of traces is working *)
  let widen ~prev ~next ~num_iters:_ =
    let trace_diff = diff next prev in
    if not (is_empty trace_diff) then top else join prev next


  (* similarly, hack printing so top looks different *)
  let pp fmt s = if phys_equal s top then F.pp_print_char fmt 'T' else pp fmt s
end

module MakeTree (Config : AccessTree.Config) = struct
  include AccessTree.Make (MockTraceDomain) (Config)

  let assert_trees_equal tree1 tree2 =
    let rec access_tree_equal (trace1, subtree1) (trace2, subtree2) =
      MockTraceDomain.equal trace1 trace2
      &&
      match (subtree1, subtree2) with
      | Star, Star ->
          true
      | Subtree t1, Subtree t2 ->
          AccessMap.equal access_tree_equal t1 t2
      | _ ->
          false
    in
    let base_tree_equal tree1 tree2 = BaseMap.equal access_tree_equal tree1 tree2 in
    let pp_diff fmt (actual, expected) =
      F.fprintf fmt "Expected to get tree %a but got %a" pp expected pp actual
    in
    OUnit2.assert_equal ~cmp:base_tree_equal ~pp_diff tree1 tree2
end

module Domain = MakeTree (AccessTree.DefaultConfig)

let tests =
  let open AccessPathTestUtils in
  let x_base = make_base "x" in
  let y_base = make_base "y" in
  let z_base = make_base "z" in
  let f = make_field_access "f" in
  let g = make_field_access "g" in
  let array = make_array_access StdTyp.void in
  let x = AccessPath.Abs.Exact (make_access_path "x" []) in
  let xF = AccessPath.Abs.Exact (make_access_path "x" ["f"]) in
  let xG = AccessPath.Abs.Exact (make_access_path "x" ["g"]) in
  let xFG = AccessPath.Abs.Exact (make_access_path "x" ["f"; "g"]) in
  let y = AccessPath.Abs.Exact (make_access_path "y" []) in
  let yF = AccessPath.Abs.Exact (make_access_path "y" ["f"]) in
  let yG = AccessPath.Abs.Exact (make_access_path "y" ["g"]) in
  let yFG = AccessPath.Abs.Exact (make_access_path "y" ["f"; "g"]) in
  let z = AccessPath.Abs.Exact (make_access_path "z" []) in
  let zF = AccessPath.Abs.Exact (make_access_path "z" ["f"]) in
  let zFG = AccessPath.Abs.Exact (make_access_path "z" ["f"; "g"]) in
  let xArr = AccessPath.Abs.Exact (make_base "x", [array]) in
  let xArrF =
    let accesses = [array; make_field_access "f"] in
    AccessPath.Abs.Exact (make_base "x", accesses)
  in
  let a_star = AccessPath.Abs.Abstracted (make_access_path "a" []) in
  let x_star = AccessPath.Abs.Abstracted (make_access_path "x" []) in
  let xF_star = AccessPath.Abs.Abstracted (make_access_path "x" ["f"]) in
  let xG_star = AccessPath.Abs.Abstracted (make_access_path "x" ["g"]) in
  let y_star = AccessPath.Abs.Abstracted (make_access_path "y" []) in
  let yF_star = AccessPath.Abs.Abstracted (make_access_path "y" ["f"]) in
  let z_star = AccessPath.Abs.Abstracted (make_access_path "z" []) in
  let x_trace = MockTraceDomain.singleton "x" in
  let y_trace = MockTraceDomain.singleton "y" in
  let z_trace = MockTraceDomain.singleton "z" in
  let xF_trace = MockTraceDomain.singleton "xF" in
  let yF_trace = MockTraceDomain.singleton "yF" in
  let xFG_trace = MockTraceDomain.singleton "xFG" in
  let array_f_trace = MockTraceDomain.singleton "arrayF" in
  let x_star_trace = MockTraceDomain.of_list ["x"; "xF"; "xFG"] in
  let g_subtree = Domain.make_access_node xF_trace g xFG_trace in
  let x_subtree = Domain.AccessMap.singleton f g_subtree |> Domain.make_node x_trace in
  let yF_subtree = Domain.make_starred_leaf yF_trace in
  let y_subtree = Domain.AccessMap.singleton f yF_subtree |> Domain.make_node y_trace in
  let z_subtree = Domain.make_starred_leaf z_trace in
  let tree =
    Domain.BaseMap.singleton x_base x_subtree
    |> Domain.BaseMap.add y_base y_subtree
    |> Domain.BaseMap.add z_base z_subtree
  in
  let x_base_tree = Domain.BaseMap.singleton x_base Domain.empty_node in
  let y_base_tree = Domain.BaseMap.singleton y_base Domain.empty_node in
  let x_y_base_tree = Domain.BaseMap.add y_base Domain.empty_node x_base_tree in
  let xFG_tree = Domain.BaseMap.singleton x_base x_subtree in
  let x_star_tree = Domain.BaseMap.singleton x_base (Domain.make_starred_leaf x_trace) in
  let yF_star_tree = Domain.BaseMap.singleton y_base y_subtree in
  let x_yF_star_tree = Domain.BaseMap.add y_base y_subtree x_star_tree in
  let x_star_tree_xFG_trace =
    Domain.BaseMap.singleton x_base (Domain.make_starred_leaf x_star_trace)
  in
  let open OUnit2 in
  let no_trace = "NONE" in
  let get_trace_str access_path tree =
    match Domain.get_trace access_path tree with
    | Some trace ->
        F.asprintf "%a" MockTraceDomain.pp trace
    | None ->
        no_trace
  in
  let assert_traces_eq access_path tree expected_trace_str =
    let actual_trace_str = get_trace_str access_path tree in
    let pp_diff fmt (actual, expected) =
      F.fprintf fmt "Expected to retrieve trace %s but got %s" expected actual
    in
    assert_equal ~pp_diff actual_trace_str expected_trace_str
  in
  let assert_trace_not_found access_path tree = assert_traces_eq access_path tree no_trace in
  let assert_node_equal access_path tree expected_node =
    match Domain.get_node access_path tree with
    | Some actual_node ->
        let pp_diff fmt (actual, expected) =
          F.fprintf fmt "Expected to retrieve node %a but got %a" Domain.pp_node expected
            Domain.pp_node actual
        in
        assert_equal ~pp_diff expected_node actual_node
    | None ->
        assert false
  in
  let get_trace_test =
    let get_trace_test_ _ =
      (* exact access path tests *)
      assert_traces_eq z tree "{ z }" ;
      assert_traces_eq xF tree "{ xF }" ;
      assert_traces_eq yF tree "{ yF }" ;
      assert_traces_eq xFG tree "{ xFG }" ;
      assert_trace_not_found xG tree ;
      (* starred access path tests *)
      assert_traces_eq x_star tree "{ x, xF, xFG }" ;
      assert_traces_eq xF_star tree "{ xF, xFG }" ;
      assert_trace_not_found xG_star tree ;
      assert_trace_not_found a_star tree ;
      (* starred tree tests *)
      assert_traces_eq zF tree "{ z }" ;
      assert_traces_eq zFG tree "{ z }" ;
      assert_traces_eq z_star tree "{ z }" ;
      assert_traces_eq y_star tree "{ y, yF }" ;
      assert_traces_eq yF_star tree "{ yF }" ;
      assert_traces_eq yFG tree "{ yF }" ;
      assert_trace_not_found yG tree ;
      (* get_trace is just (fst get_node), so light tests here *)
      (* exact access path tests *)
      assert_node_equal z tree z_subtree ;
      assert_node_equal xF tree g_subtree ;
      assert_node_equal xFG tree (Domain.make_normal_leaf xFG_trace) ;
      (* starred tree tests *)
      assert_node_equal yFG tree yF_subtree ;
      (* starred access path tests *)
      let joined_y_subtree =
        Domain.AccessMap.singleton f yF_subtree
        |> Domain.make_node (MockTraceDomain.join y_trace yF_trace)
      in
      assert_node_equal y_star tree joined_y_subtree
    in
    "get_trace" >:: get_trace_test_
  in
  let add_trace_test =
    let add_trace_test_ _ =
      (* special trace to indicate that we've added successfully *)
      let added_trace = MockTraceDomain.singleton "added" in
      let mk_x_y_base_tree trace =
        Domain.BaseMap.singleton x_base (Domain.make_normal_leaf trace)
        |> Domain.BaseMap.add y_base Domain.empty_node
      in
      let mk_xFG_node leaf_trace =
        Domain.make_access_node MockTraceDomain.empty g leaf_trace
        |> Domain.AccessMap.singleton f
        |> Domain.make_node MockTraceDomain.empty
      in
      let mk_xFG_tree leaf_trace = mk_xFG_node leaf_trace |> Domain.BaseMap.singleton x_base in
      let mk_xArrF_tree leaf_trace =
        Domain.make_access_node MockTraceDomain.empty f leaf_trace
        |> Domain.AccessMap.singleton array
        |> Domain.make_node MockTraceDomain.empty
        |> Domain.BaseMap.singleton x_base
      in
      (* normal tests *)
      (* add base when absent *)
      let x_y_base_tree_with_added_trace = mk_x_y_base_tree added_trace in
      Domain.assert_trees_equal
        (Domain.add_trace x added_trace y_base_tree)
        x_y_base_tree_with_added_trace ;
      (* add base when present *)
      Domain.assert_trees_equal
        (Domain.add_trace x added_trace x_y_base_tree)
        x_y_base_tree_with_added_trace ;
      let x_y_base_tree_with_y_trace = mk_x_y_base_tree y_trace in
      Domain.assert_trees_equal
        (Domain.add_trace x added_trace x_y_base_tree_with_y_trace)
        x_y_base_tree_with_added_trace ;
      (* add path when absent *)
      let xFG_tree_added_trace = mk_xFG_tree added_trace in
      Domain.assert_trees_equal (Domain.add_trace xFG added_trace x_base_tree) xFG_tree_added_trace ;
      (* add path when present *)
      let xFG_tree_y_trace = mk_xFG_tree y_trace in
      Domain.assert_trees_equal
        (Domain.add_trace xFG added_trace xFG_tree_y_trace)
        xFG_tree_added_trace ;
      (* add starred path when base absent *)
      let xF_star_tree_added_trace =
        Domain.make_starred_leaf added_trace
        |> Domain.AccessMap.singleton f
        |> Domain.make_node MockTraceDomain.empty
        |> Domain.BaseMap.singleton x_base
      in
      Domain.assert_trees_equal
        (Domain.add_trace xF_star added_trace Domain.bottom)
        xF_star_tree_added_trace ;
      (* add starred path when base present *)
      Domain.assert_trees_equal
        (Domain.add_trace xF_star added_trace x_base_tree)
        xF_star_tree_added_trace ;
      (* adding array path should do weak updates *)
      let aArrF_tree = mk_xArrF_tree array_f_trace in
      let aArrF_tree_joined_trace =
        mk_xArrF_tree (MockTraceDomain.join added_trace array_f_trace)
      in
      Domain.assert_trees_equal
        (Domain.add_trace xArrF added_trace aArrF_tree)
        aArrF_tree_joined_trace ;
      (* starred tests *)
      (* we should do a strong update when updating x.f* with x.f *)
      let yF_tree_added_trace =
        Domain.make_normal_leaf added_trace
        |> Domain.AccessMap.singleton f |> Domain.make_node y_trace
        |> Domain.BaseMap.singleton y_base
      in
      Domain.assert_trees_equal (Domain.add_trace yF added_trace yF_star_tree) yF_tree_added_trace ;
      (* but not when updating x* with x.f *)
      let x_star_tree_added_trace =
        let joined_trace = MockTraceDomain.join x_trace added_trace in
        Domain.BaseMap.singleton x_base (Domain.make_starred_leaf joined_trace)
      in
      Domain.assert_trees_equal
        (Domain.add_trace xF added_trace x_star_tree)
        x_star_tree_added_trace ;
      (* when updating x.f.g with x.f*, we should remember traces associated with f and g even as
         we replace that subtree with a * *)
      let xF_star_tree_joined_traces =
        let joined_trace =
          MockTraceDomain.join added_trace xFG_trace |> MockTraceDomain.join xF_trace
        in
        Domain.make_starred_leaf joined_trace
        |> Domain.AccessMap.singleton f |> Domain.make_node x_trace
        |> Domain.BaseMap.singleton x_base
      in
      Domain.assert_trees_equal
        (Domain.add_trace xF_star added_trace xFG_tree)
        xF_star_tree_joined_traces ;
      (* [add_node] tests are sparse, since [add_trace] is just [add_node] <empty node>. main things
         to test are (1) adding a non-empty node works, (2) adding a non-empty node does the proper
         joins in the weak update case *)
      (* case (1): adding XFG to y base tree works *)
      let y_xFG_tree = Domain.BaseMap.add y_base Domain.empty_node (mk_xFG_tree xFG_trace) in
      Domain.assert_trees_equal (Domain.add_node x (mk_xFG_node xFG_trace) y_base_tree) y_xFG_tree ;
      (* case (2): adding a non-empty node does weak updates when required *)
      let arr_tree =
        let arr_subtree =
          Domain.AccessMap.singleton f (Domain.make_normal_leaf array_f_trace)
          |> Domain.AccessMap.add g (Domain.make_normal_leaf xFG_trace)
        in
        Domain.AccessMap.singleton array (Domain.make_node xF_trace arr_subtree)
        |> Domain.make_node MockTraceDomain.empty
        |> Domain.BaseMap.singleton x_base
      in
      Domain.assert_trees_equal (Domain.add_node xArr g_subtree aArrF_tree) arr_tree
    in
    "add_trace" >:: add_trace_test_
  in
  let lteq_test =
    let lteq_test_ _ =
      (* regular tree tests *)
      assert_bool "<= equal;" (Domain.leq ~lhs:tree ~rhs:tree) ;
      assert_bool "<= bases" (Domain.leq ~lhs:x_base_tree ~rhs:x_y_base_tree) ;
      assert_bool "<= regular1" (Domain.leq ~lhs:x_base_tree ~rhs:xFG_tree) ;
      assert_bool "<= regular2" (Domain.leq ~lhs:xFG_tree ~rhs:tree) ;
      assert_bool "<= regular3" (Domain.leq ~lhs:y_base_tree ~rhs:tree) ;
      assert_bool "<= bases negative1" (not (Domain.leq ~lhs:x_y_base_tree ~rhs:x_base_tree)) ;
      assert_bool "<= bases negative2" (not (Domain.leq ~lhs:x_base_tree ~rhs:y_base_tree)) ;
      assert_bool "<= negative1" (not (Domain.leq ~lhs:xFG_tree ~rhs:y_base_tree)) ;
      assert_bool "<= negative2" (not (Domain.leq ~lhs:tree ~rhs:xFG_tree)) ;
      (* star tree tests *)
      assert_bool "<= star lhs equal" (Domain.leq ~lhs:x_star_tree ~rhs:x_star_tree) ;
      assert_bool "<= star rhs1" (Domain.leq ~lhs:x_base_tree ~rhs:x_star_tree) ;
      assert_bool "<= star rhs2" (Domain.leq ~lhs:xFG_tree ~rhs:x_star_tree) ;
      assert_bool "<= star rhs3" (Domain.leq ~lhs:y_base_tree ~rhs:yF_star_tree) ;
      assert_bool "<= star rhs4" (Domain.leq ~lhs:yF_star_tree ~rhs:tree) ;
      assert_bool "<= star lhs negative1" (not (Domain.leq ~lhs:x_star_tree ~rhs:x_base_tree)) ;
      assert_bool "<= star lhs negative2" (not (Domain.leq ~lhs:x_star_tree ~rhs:xFG_tree)) ;
      assert_bool "<= star lhs negative3" (not (Domain.leq ~lhs:yF_star_tree ~rhs:y_base_tree)) ;
      assert_bool "<= star lhs negative4" (not (Domain.leq ~lhs:tree ~rhs:yF_star_tree)) ;
      (* <= tree but not <= trace tests *)
      (* same as x_base_tree, but with a trace higher in the traces lattice *)
      let x_base_tree_higher_trace =
        Domain.BaseMap.singleton x_base (Domain.make_normal_leaf y_trace)
      in
      (* same as x_star_tree, but with a trace incomparable in the traces lattice *)
      let x_star_tree_diff_trace =
        Domain.BaseMap.singleton x_base (Domain.make_starred_leaf y_trace)
      in
      assert_bool "(x, {}) <= (x, {y})" (Domain.leq ~lhs:x_base_tree ~rhs:x_base_tree_higher_trace) ;
      assert_bool "(x, {y}) not <= (x, {})"
        (not (Domain.leq ~lhs:x_base_tree_higher_trace ~rhs:x_base_tree)) ;
      assert_bool "(x*, {y})* not <= (x*, {x})"
        (not (Domain.leq ~lhs:x_star_tree_diff_trace ~rhs:x_star_tree)) ;
      assert_bool "(x*, {x})* not <= (x*, {y})"
        (not (Domain.leq ~lhs:x_star_tree ~rhs:x_star_tree_diff_trace))
    in
    "lteq" >:: lteq_test_
  in
  let join_test =
    let join_test_ _ =
      (* normal |_| normal *)
      Domain.assert_trees_equal (Domain.join x_base_tree y_base_tree) x_y_base_tree ;
      Domain.assert_trees_equal (Domain.join y_base_tree x_base_tree) x_y_base_tree ;
      Domain.assert_trees_equal (Domain.join x_y_base_tree x_base_tree) x_y_base_tree ;
      Domain.assert_trees_equal (Domain.join x_base_tree xFG_tree) xFG_tree ;
      (* starred |_| starred *)
      Domain.assert_trees_equal (Domain.join x_star_tree yF_star_tree) x_yF_star_tree ;
      (* normal |_| starred *)
      Domain.assert_trees_equal (Domain.join tree xFG_tree) tree ;
      (* [x_star_tree] and [x_base_tree] both have trace "{ x }" associated with x... *)
      Domain.assert_trees_equal (Domain.join x_star_tree x_base_tree) x_star_tree ;
      (* ...but [xFG_tree] has some nested traces that should get joined with "{ x }" *)
      Domain.assert_trees_equal (Domain.join x_star_tree xFG_tree) x_star_tree_xFG_trace
    in
    "join" >:: join_test_
  in
  let widen_test =
    let widen_test_ _ =
      let make_x_base_tree trace =
        Domain.BaseMap.singleton x_base (Domain.make_normal_leaf trace)
      in
      let widen prev next = Domain.widen ~prev ~next ~num_iters:4 in
      (* a bit light on the tests here, since widen is implemented as a simple wrapper of join *)
      (* widening traces works:
         x |-> ("x", empty) \/ x |-> ("y", empty) =
         x |-> (T, empty)
      *)
      let x_tree_x_trace = make_x_base_tree x_trace in
      let x_tree_y_trace = make_x_base_tree y_trace in
      let x_tree_top_trace = make_x_base_tree MockTraceDomain.top in
      Domain.assert_trees_equal (widen x_tree_x_trace x_tree_y_trace) x_tree_top_trace ;
      (* adding stars to a base works:
         x |-> ({}, empty) \/ y |-> ({}, empty) =
         (x |-> ({}, empty), y |-> ({}, Star) )
      *)
      let x_y_star_base_tree =
        Domain.BaseMap.add y_base (Domain.make_starred_leaf MockTraceDomain.empty) x_base_tree
      in
      Domain.assert_trees_equal (widen x_base_tree y_base_tree) x_y_star_base_tree ;
      (* adding stars to a subtree works:
         x |-> ("y", empty) \/
         x |-> ("x" , f |-> ("f", g |-> ("g", empty))) =
         x |-> (T , f |-> (T, * ))
      *)
      let xF_star_tree =
        Domain.AccessMap.singleton f (Domain.make_starred_leaf MockTraceDomain.top)
        |> Domain.make_node MockTraceDomain.top
        |> Domain.BaseMap.singleton x_base
      in
      Domain.assert_trees_equal (widen x_tree_y_trace xFG_tree) xF_star_tree ;
      (* widening is not commutative, and is it not join:
         x |-> ("x" , f |-> ("f", g |-> ("g", empty))) \/
         x |-> ("y", empty) =
         x |-> (T , f |-> ("f", g |-> ("g", empty)))
      *)
      let xFG_tree_widened_trace =
        let _, xFG_node = x_subtree in
        Domain.BaseMap.singleton x_base (MockTraceDomain.top, xFG_node)
      in
      Domain.assert_trees_equal (widen xFG_tree x_tree_y_trace) xFG_tree_widened_trace
    in
    "widen" >:: widen_test_
  in
  let fold_test =
    let fold_test_ _ =
      let collect_ap_traces acc ap trace = (ap, trace) :: acc in
      let ap_traces = Domain.trace_fold collect_ap_traces tree [] in
      let has_ap_trace_pair ap_in trace_in =
        List.exists
          ~f:(fun (ap, trace) ->
            AccessPath.Abs.equal ap ap_in && MockTraceDomain.equal trace trace_in )
          ap_traces
      in
      assert_bool "Should have six ap/trace pairs" (Int.equal (List.length ap_traces) 6) ;
      assert_bool "has x pair" (has_ap_trace_pair x x_trace) ;
      assert_bool "has xF pair" (has_ap_trace_pair xF xF_trace) ;
      assert_bool "has xFG pair" (has_ap_trace_pair xFG xFG_trace) ;
      assert_bool "has y pair" (has_ap_trace_pair y y_trace) ;
      assert_bool "has yF* pair" (has_ap_trace_pair yF_star yF_trace) ;
      assert_bool "has z pair" (has_ap_trace_pair z_star z_trace)
    in
    "fold" >:: fold_test_
  in
  let depth_test =
    let depth_test_ _ =
      assert_equal (Domain.depth Domain.bottom) 0 ;
      assert_equal (Domain.depth x_base_tree) 1 ;
      assert_equal (Domain.depth x_y_base_tree) 1 ;
      assert_equal (Domain.depth xFG_tree) 3 ;
      assert_equal (Domain.depth x_star_tree) 1 ;
      assert_equal (Domain.depth yF_star_tree) 2 ;
      assert_equal (Domain.depth x_yF_star_tree) 2
    in
    "depth" >:: depth_test_
  in
  let max_depth_test =
    let max_depth_test_ _ =
      let module Max1 = MakeTree (struct
        let max_depth = 1

        let max_width = Int.max_value / 2
      end) in
      let f_node =
        Max1.AccessMap.singleton f (Max1.make_normal_leaf x_trace)
        |> Max1.make_node MockTraceDomain.empty
      in
      let x_tree = Max1.BaseMap.singleton x_base (Max1.make_normal_leaf x_trace) in
      let x_star_tree = Max1.BaseMap.singleton x_base (Max1.make_starred_leaf x_trace) in
      (* adding (x.f, "x") to a tree with max height 1 should yield  x |-> ("x", * ) *)
      Max1.assert_trees_equal (Max1.add_trace xF x_trace Max1.bottom) x_star_tree ;
      (* same, but with (x.f.g, "x") *)
      Max1.assert_trees_equal (Max1.add_trace xFG x_trace Max1.bottom) x_star_tree ;
      (* adding node (f, "x") via access path x should also yield the same tree *)
      Max1.assert_trees_equal (Max1.add_node x f_node Max1.bottom) x_star_tree ;
      (* adding (x, "x") shouldn't add stars *)
      Max1.assert_trees_equal (Max1.add_trace x x_trace Max1.bottom) x_tree ;
      let module Max2 = MakeTree (struct
        let max_depth = 2

        let max_width = Int.max_value / 2
      end) in
      let f_node =
        Max2.AccessMap.singleton f (Max2.make_normal_leaf x_trace)
        |> Max2.make_node MockTraceDomain.empty
      in
      let fG_node =
        Max2.make_access_node MockTraceDomain.empty g x_trace
        |> Max2.AccessMap.singleton f
        |> Max2.make_node MockTraceDomain.empty
      in
      let f_star_node =
        Max2.AccessMap.singleton f (Max2.make_starred_leaf x_trace)
        |> Max2.make_node MockTraceDomain.empty
      in
      let x_tree = Max2.BaseMap.singleton x_base Max2.empty_node in
      let xF_tree = Max2.BaseMap.singleton x_base f_node in
      let xF_star_tree = Max2.BaseMap.singleton x_base f_star_node in
      (* adding x.f to an empty tree should't add stars... *)
      Max2.assert_trees_equal (Max2.add_trace xF x_trace Max2.bottom) xF_tree ;
      (* ... but adding x.f.g should *)
      Max2.assert_trees_equal (Max2.add_trace xFG x_trace Max2.bottom) xF_star_tree ;
      (* adding the node (f.g, "x") to a tree with x should produce the same result *)
      Max2.assert_trees_equal (Max2.add_node x fG_node x_tree) xF_star_tree
    in
    "max_depth" >:: max_depth_test_
  in
  let max_width_test =
    let max_width_test_ _ =
      let module Max1 = MakeTree (struct
        let max_depth = Int.max_value / 2

        let max_width = 1
      end) in
      let x_base_tree = Max1.BaseMap.singleton x_base Max1.empty_node in
      let y_base_tree = Max1.BaseMap.singleton y_base Max1.empty_node in
      let x_y_base_tree = Max1.BaseMap.add y_base Max1.empty_node x_base_tree in
      let f_node =
        Max1.AccessMap.singleton f (Max1.make_normal_leaf y_trace)
        |> Max1.make_node MockTraceDomain.empty
      in
      let g_node =
        Max1.AccessMap.singleton g (Max1.make_normal_leaf z_trace)
        |> Max1.make_node MockTraceDomain.empty
      in
      let star_node = Max1.make_starred_leaf (MockTraceDomain.join y_trace z_trace) in
      let xF_tree = Max1.BaseMap.singleton x_base f_node in
      let xG_tree = Max1.BaseMap.singleton x_base g_node in
      let x_star_tree = Max1.BaseMap.singleton x_base star_node in
      (* adding x.f to a tree containing just x should work *)
      Max1.assert_trees_equal (Max1.add_trace xF y_trace Max1.bottom) xF_tree ;
      (* but adding x.g to a tree containing x.f should create a star *)
      Max1.assert_trees_equal (Max1.add_trace xG z_trace xF_tree) x_star_tree ;
      (* joining the x.f and x.g trees should also create a star *)
      Max1.assert_trees_equal (Max1.join xF_tree xG_tree) x_star_tree ;
      (* adding x.f to a tree where it's already present shouldn't create a star *)
      Max1.assert_trees_equal (Max1.add_trace xF y_trace xF_tree) xF_tree ;
      (* and joining the same tree with itself shouldn't either *)
      Max1.assert_trees_equal (Max1.join xF_tree xF_tree) xF_tree ;
      (* note that the width limit doesn't apply to the base layer *)
      Max1.assert_trees_equal (Max1.join x_base_tree y_base_tree) x_y_base_tree
    in
    "max_width" >:: max_width_test_
  in
  "access_tree_suite"
  >::: [ get_trace_test
       ; add_trace_test
       ; lteq_test
       ; join_test
       ; widen_test
       ; fold_test
       ; depth_test
       ; max_depth_test
       ; max_width_test ]
