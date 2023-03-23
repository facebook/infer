(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
open DifferentialTestsUtils

type 'a outcome = Return of 'a | Raise of exn

let test_file_renamings_from_json =
  let create_test test_input expected_output _ =
    let test_output input =
      DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_json input
    in
    let pp_diff fmt (expected, actual) =
      let pp = DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.pp in
      Format.fprintf fmt "Expected %a but got %a" pp expected pp actual
    in
    match expected_output with
    | Return exp ->
        assert_equal ~pp_diff
          ~cmp:DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.equal exp
          (test_output test_input)
    | Raise exc ->
        assert_raises exc (fun () -> test_output test_input)
  in
  [ ( "test_file_renamings_from_json_with_good_input"
    , "[" ^ "{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"},"
      ^ "{\"current\": \"ccc.java\", \"previous\": \"DDD.java\"},"
      ^ "{\"current\": \"eee.java\", \"previous\": \"FFF.java\"}" ^ "]"
    , Return
        DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.(
          of_list
            [ {current= "aaa.java"; previous= "BBB.java"}
            ; {current= "ccc.java"; previous= "DDD.java"}
            ; {current= "eee.java"; previous= "FFF.java"} ] ) )
  ; ( "test_file_renamings_from_json_with_good_empty_input"
    , "[]"
    , Return (DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.of_list [])
    )
  ; ( "test_file_renamings_from_json_with_well_formed_but_unexpected_input"
    , "{}"
    , Raise (Logging.InferUserError "Expected JSON list but got '{}'") )
  ; ( "test_file_renamings_from_json_with_well_formed_but_unexpected_value"
    , "[{\"current\": 1, \"previous\": \"BBB.java\"}]"
    , Raise
        (Logging.InferUserError
           ( "Error parsing file renamings: \"current\" field is not a string"
           ^ "\nExpected JSON object of the following form: "
           ^ "'{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"}', "
           ^ "but instead got: '{\"current\":1,\"previous\":\"BBB.java\"}'" ) ) )
  ; ( "test_file_renamings_from_json_with_malformed_input"
    , "A"
    , Raise (Yojson.Json_error "Line 1, bytes 0-1:\nInvalid token 'A'") ) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_file_renamings_find_previous =
  let renamings =
    DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.(
      of_list
        [ {current= "aaa.java"; previous= "BBB.java"}
        ; {current= "ccc.java"; previous= "DDD.java"}
        ; {current= "eee.java"; previous= "FFF.java"} ] )
  in
  let find_previous =
    DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.find_previous
  in
  let pp_diff fmt (expected, actual) =
    Format.fprintf fmt "Expected '%s' but got '%s'" expected actual
  in
  let create_test input expected_previous _ =
    assert_equal ~cmp:String.equal ~pp_diff expected_previous (find_previous renamings input)
  in
  [ ("test_file_renamings_find_previous_with_existing_value", "ccc.java", "DDD.java")
  ; ("test_file_renamings_find_previous_with_existing_value", "abc.java", "abc.java") ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_relative_complements =
  let create_test pred (l1, l2) (expected_l1, expected_l2, expected_l3) _ =
    let output_l1, output_l2, output_l3 =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.relative_complements
        ~compare:Int.compare ~pred l1 l2
    in
    let list_equal l1 l2 = List.equal Int.equal l1 l2 in
    assert_equal ~pp_diff:(pp_diff_of_int_list "First list") ~cmp:list_equal expected_l1 output_l1 ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Second list") ~cmp:list_equal expected_l2 output_l2 ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Third list") ~cmp:list_equal expected_l3 output_l3
  in
  [ ( "test_relative_complements_with_always_true_pred"
    , (fun _ -> true)
    , ([0; 1; 2; 3; 4; 5], [5; 3; 7; 1; 1; 2])
    , ([4; 0], [5; 3; 2; 1], [7]) )
  ; ( "test_relative_complements_with_even_numbers_pred"
    , (fun i -> Int.equal (i mod 2) 0)
    , (* skip when even, keep odd *)
      ([0; 1; 2; 3; 4; 5], [5; 3; 7; 1; 1; 2])
    , ([5; 4; 3; 1; 0], [2], [7; 5; 3; 1; 1]) )
  ; ( "test_relative_complements_with_even_numbers_pred_2"
    , (fun i -> Int.equal (i mod 2) 0)
    , (* skip when even, keep odd *)
      ([0; 1; 2; 3; 5; 5], [1; 1; 2; 3; 4; 7])
    , ([5; 5; 3; 1; 0], [2], [7; 4; 3; 1; 1]) )
  ; ( "test_relative_complements_with_always_true_pred_and_disjoint_lists_of_different_length"
    , (fun _ -> true)
    , ([0; 3; 2; 3; 5], [9; 7; 6; 8; 4; 6; 9])
    , ([5; 3; 3; 2; 0], [], [9; 9; 8; 7; 6; 6; 4]) )
  ; ( "test_relative_complements_with_always_true_pred_and_lists_of_different_length"
    , (fun _ -> true)
    , ([0; 3; 2; 3], [9; 7; 3; 8; 0; 6; 9; 4])
    , ([2], [3; 0], [9; 9; 8; 7; 6; 4]) )
  ; ( "test_relative_complements_with_odd_numbers_on_lists_of_different_length"
    , (fun i -> Int.equal (i mod 2) 1)
    , (* skip when odd, keep even *)
      ([0; 3; 2; 3], [9; 7; 3; 8; 0; 6; 9; 4])
    , ([2; 0], [3], [9; 9; 8; 7; 6; 4; 0]) )
  ; ( "test_relative_complements_with_singleton_lists1"
    , (fun _ -> true)
    , ([0], [0; 1; 0; 0])
    , ([], [0], [1]) )
  ; ( "test_relative_complements_with_singleton_lists2"
    , (fun _ -> true)
    , ([0; 1; 0; 0], [0])
    , ([1], [0], []) )
  ; ("test_relative_complements_with_singleton_lists3", (fun _ -> true), ([0], [0]), ([], [0], []))
  ; ("test_relative_complements_with_singleton_lists4", (fun _ -> true), ([0], [1]), ([0], [], [1]))
  ; ( "test_relative_complements_with_empty_lists1"
    , (fun _ -> true)
    , ([], [0; 1; 0; 0])
    , ([], [], [1; 0; 0; 0]) )
  ; ( "test_relative_complements_with_empty_lists2"
    , (fun _ -> true)
    , ([0; 1; 0; 0], [])
    , ([1; 0; 0; 0], [], []) )
  ; ("test_relative_complements_with_empty_lists3", (fun _ -> true), ([], []), ([], [], [])) ]
  |> List.map ~f:(fun (name, pred, test_input, expected_output) ->
         name >:: create_test pred test_input expected_output )


let test_skip_duplicated_types_on_filenames =
  let current_report =
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:"22" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1'.java" ~hash:"11" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1'.java" ~hash:"111" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_4.java" ~hash:"4" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:"222" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_5.java" ~hash:"55" () ]
  in
  let previous_report =
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:"222" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_5.java" ~hash:"5" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:"1" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_3.java" ~hash:"3" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2.java" ~hash:"2" () ]
  in
  let renamings =
    DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.(
      of_list
        [ {current= "file_2'.java"; previous= "file_2.java"}
        ; {current= "file_1'.java"; previous= "file_1.java"} ] )
  in
  let current_costs = [] in
  let previous_costs = [] in
  let current_config_impact = [] in
  let previous_config_impact = [] in
  let diff =
    Differential.issues_of_reports ~current_report ~previous_report ~current_costs ~previous_costs
      ~current_config_impact ~previous_config_impact
  in
  let diff' =
    DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.skip_duplicated_types_on_filenames
      renamings diff
  in
  let do_assert _ =
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of introduced")
      ["4"]
      (sorted_hashes_of_issues diff'.introduced) ;
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of fixed")
      ["3"]
      (sorted_hashes_of_issues diff'.fixed) ;
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of preexisting")
      ["111"; "22"; "222"; "55"]
      (sorted_hashes_of_issues diff'.preexisting)
  in
  "test_skip_duplicated_types_on_filenames" >:: do_assert


let test_interesting_paths_filter =
  let report =
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:"1" ()
    ; create_fake_jsonbug ~bug_type:IssueType.null_dereference.unique_id ~file:"file_2.java"
        ~hash:"2" ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_4.java" ~hash:"4" () ]
  in
  let create_test interesting_paths expected_hashes _ =
    let filter =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.interesting_paths_filter
        interesting_paths
    in
    let filtered_report = filter report in
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Bug hash")
      expected_hashes
      (sorted_hashes_of_issues filtered_report)
  in
  [ ("test_interesting_paths_filter_with_none_interesting_paths", None, ["1"; "2"; "4"])
  ; ( "test_interesting_paths_filter_with_some_interesting_paths"
    , Some
        [ SourceFile.create ~check_rel_path:false "file_not_existing.java"
        ; SourceFile.create ~check_rel_path:false "file_4.java" ]
    , ["4"] )
  ; ( "test_interesting_paths_filter_with_some_interesting_paths_that_are_not_in_report"
    , Some
        [ SourceFile.create ~check_rel_path:false "file_not_existing.java"
        ; SourceFile.create ~check_rel_path:false "file_whatever.java" ]
    , [] ) ]
  |> List.map ~f:(fun (name, interesting_paths, expected_output) ->
         name >:: create_test interesting_paths expected_output )


let tests =
  "differential_filters_suite"
  >::: test_file_renamings_from_json @ test_file_renamings_find_previous @ test_relative_complements
       @ test_interesting_paths_filter
       @ [test_skip_duplicated_types_on_filenames]
