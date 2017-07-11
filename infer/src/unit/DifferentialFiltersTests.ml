(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open OUnit2
open DifferentialTestsUtils

type 'a outcome = Return of 'a | Raise of exn

let test_file_renamings_from_json =
  let create_test test_input expected_output _ =
    let test_output input = DifferentialFilters.FileRenamings.from_json input in
    let pp_diff fmt (expected, actual) =
      let pp = DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.pp in
      Format.fprintf fmt "Expected %a but got %a" pp expected pp actual
    in
    match expected_output with
    | Return exp
     -> assert_equal ~pp_diff
          ~cmp:DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.equal exp
          (test_output test_input)
    | Raise exc
     -> assert_raises exc (fun () -> test_output test_input)
  in
  [ ( "test_file_renamings_from_json_with_good_input"
    , "[" ^ "{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"},"
      ^ "{\"current\": \"ccc.java\", \"previous\": \"DDD.java\"},"
      ^ "{\"current\": \"eee.java\", \"previous\": \"FFF.java\"}" ^ "]"
    , Return
        (DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_renamings
           [ {DifferentialFilters.FileRenamings.current= "aaa.java"; previous= "BBB.java"}
           ; {DifferentialFilters.FileRenamings.current= "ccc.java"; previous= "DDD.java"}
           ; {DifferentialFilters.FileRenamings.current= "eee.java"; previous= "FFF.java"} ]) )
  ; ( "test_file_renamings_from_json_with_good_empty_input"
    , "[]"
    , Return
        (DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_renamings
           []) )
  ; ( "test_file_renamings_from_json_with_well_formed_but_unexpected_input"
    , "{}"
    , Raise (Failure "Expected JSON list but got '{}'") )
  ; ( "test_file_renamings_from_json_with_well_formed_but_unexpected_value"
    , "[{\"current\": 1, \"previous\": \"BBB.java\"}]"
    , Raise
        (Failure
           ( "Expected JSON object of the following form: "
           ^ "'{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"}', "
           ^ "but instead got: '{\"current\":1,\"previous\":\"BBB.java\"}'" )) )
  ; ( "test_file_renamings_from_json_with_malformed_input"
    , "A"
    , Raise (Yojson.Json_error "Line 1, bytes 0-1:\nInvalid token 'A'") ) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )

let test_file_renamings_find_previous =
  let renamings =
    DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_renamings
      [ {DifferentialFilters.FileRenamings.current= "aaa.java"; previous= "BBB.java"}
      ; {DifferentialFilters.FileRenamings.current= "ccc.java"; previous= "DDD.java"}
      ; {DifferentialFilters.FileRenamings.current= "eee.java"; previous= "FFF.java"} ]
  in
  let cmp s1 s2 = [%compare.equal : string option] s1 s2 in
  let find_previous = DifferentialFilters.FileRenamings.find_previous in
  let pp_diff fmt (expected, actual) =
    let pp_str_opt fmt str_opt =
      let out = match str_opt with Some str -> "Some " ^ str | None -> "None" in
      Format.fprintf fmt "%s" out
    in
    Format.fprintf fmt "Expected '%a' but got '%a'" pp_str_opt expected pp_str_opt actual
  in
  let create_test input expected_previous _ =
    assert_equal ~cmp ~pp_diff expected_previous (find_previous renamings input)
  in
  [ ("test_file_renamings_find_previous_with_existing_value", "ccc.java", Some "DDD.java")
  ; ("test_file_renamings_find_previous_with_existing_value", "abc.java", None) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )

let test_relative_complements =
  let create_test pred (l1, l2) (expected_l1, expected_l2, expected_l3) _ =
    let cmp = Int.compare in
    let output_l1, output_l2, output_l3 =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.relative_complements ~cmp ~pred
        l1 l2
    in
    let list_equal l1 l2 = List.equal ~equal:(fun v1 v2 -> Int.equal (cmp v1 v2) 0) l1 l2 in
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
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:22 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1'.java" ~hash:11 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1'.java" ~hash:111 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_4.java" ~hash:4 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:222 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_5.java" ~hash:55 () ]
  in
  let previous_report =
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2'.java" ~hash:222 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_5.java" ~hash:5 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:1 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_2" ~file:"file_3.java" ~hash:3 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_2.java" ~hash:2 () ]
  in
  let renamings =
    DifferentialFilters.FileRenamings.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.from_renamings
      [ {DifferentialFilters.FileRenamings.current= "file_2'.java"; previous= "file_2.java"}
      ; {DifferentialFilters.FileRenamings.current= "file_1'.java"; previous= "file_1.java"} ]
  in
  let diff = Differential.of_reports ~current_report ~previous_report in
  let diff' =
    DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.skip_duplicated_types_on_filenames
      renamings diff
  in
  let do_assert _ =
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of introduced") [4]
      (sorted_hashes_of_issues diff'.introduced) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of fixed") [3]
      (sorted_hashes_of_issues diff'.fixed) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of preexisting") [22; 55; 111; 222]
      (sorted_hashes_of_issues diff'.preexisting)
  in
  "test_skip_duplicated_types_on_filenames" >:: do_assert

let test_value_of_qualifier_tag =
  let qts = [{Jsonbug_t.tag= "tag1"; value= "value1"}; {Jsonbug_t.tag= "tag2"; value= "value2"}] in
  let pp_diff fmt (expected, actual) =
    let to_str v = Option.value v ~default:"NONE" in
    Format.fprintf fmt "Expected: %s Found: %s" (to_str expected) (to_str actual)
  in
  let do_assert _ =
    assert_equal ~cmp:(Option.equal String.equal) ~pp_diff (Some "value2")
      (DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.value_of_qualifier_tag qts
         "tag2") ;
    assert_equal ~cmp:(Option.equal String.equal) ~pp_diff None
      (DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.value_of_qualifier_tag qts
         "tag3") ;
    assert_equal ~cmp:(Option.equal String.equal) ~pp_diff (Some "value1")
      (DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.value_of_qualifier_tag qts
         "tag1")
  in
  "test_value_of_qualifier_tag" >:: do_assert

let test_skip_anonymous_class_renamings =
  let qt1 = [{Jsonbug_t.tag= "call_procedure"; value= "aValue1"}] in
  let qt2 = [{Jsonbug_t.tag= "call_procedure"; value= "aValue2"}] in
  let create_test input_diff (exp_introduced, exp_fixed, exp_preexisting) _ =
    let diff' =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.skip_anonymous_class_renamings
        input_diff
    in
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of introduced") exp_introduced
      (sorted_hashes_of_issues diff'.introduced) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of fixed") exp_fixed
      (sorted_hashes_of_issues diff'.fixed) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of preexisting") exp_preexisting
      (sorted_hashes_of_issues diff'.preexisting)
  in
  (* [(test_name, diff, expected hashes); ...] *)
  [ ( "test_skip_anonymous_class_renamings_with_long_procedure_ids"
    , Differential.of_reports
        ~current_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                ( "com.whatever.package00.abcd."
                ^ "ABasicExampleFragment$83.onMenuItemActionExpand(android.view.MenuItem):b."
                ^ "5ab5e18cae498c35d887ce88f3d5fa82" )
              ~file:"a.java" ~key:1 ~qualifier_tags:qt1 ~hash:3 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                ( "com.whatever.package00.abcd."
                ^ "ABasicExampleFragment$83$7.onMenuItemActionExpand(android.view.MenuItem)."
                ^ "522cc747174466169781c9d2fc980dbc" )
              ~file:"a.java" ~key:1 ~qualifier_tags:qt1 ~hash:4 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:"procid5.c854fd4a98113d9ab5b82deb3545de89" ~file:"b.java" ~key:5
              ~hash:5 () ]
        ~previous_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                ( "com.whatever.package00.abcd."
                ^ "ABasicExampleFragment$9.onMenuItemActionExpand(android.view.MenuItem):bo."
                ^ "ba1776155fba2899542401da5bc779a5" )
              ~file:"a.java" ~key:1 ~qualifier_tags:qt1 ~hash:1 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:"procid2.92095aee3f1884c37e96feae031f4931" ~file:"b.java" ~key:2
              ~hash:2 () ]
    , ([4; 5], [2], [3]) )
  ; ( "test_skip_anonymous_class_renamings_with_empty_qualifier_tags"
    , Differential.of_reports
        ~current_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$1.foo():bool.bf13089cf4c47ff8ff089a1a4767324f"
              ~file:"a.java" ~key:1 ~hash:1 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:
                "com.whatever.package.Class$1.foo():bool.bf13089cf4c47ff8ff089a1a4767324f"
              ~file:"a.java" ~key:1 ~hash:3 () ]
        ~previous_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$21$1.foo():bool.db89561ad9dab28587c8c04833f09b03"
              ~file:"a.java" ~key:1 ~hash:2 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:
                "com.whatever.package.Class$8.foo():bool.cffd4e941668063eb802183dbd3e856d"
              ~file:"a.java" ~key:1 ~hash:4 () ]
    , ([1], [2], [3]) )
  ; ( "test_skip_anonymous_class_renamings_with_matching_non_anonymous_procedure_ids"
    , Differential.of_reports
        ~current_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class.foo():bool.919f37fd0993058a01f438210ba8a247"
              ~file:"a.java" ~key:1 ~hash:1 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class.foo():bool.919f37fd0993058a01f438210ba8a247"
              ~file:"a.java" ~key:1 ~hash:3 () ]
        ~previous_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class.foo():bool.919f37fd0993058a01f438210ba8a247"
              ~file:"a.java" ~key:1 ~hash:2 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class.foo():bool.919f37fd0993058a01f438210ba8a247"
              ~file:"a.java" ~key:1 ~hash:4 () ]
    , ([1; 3], [2; 4], []) )
  ; ( "test_skip_anonymous_class_renamings_with_non_java_files"
    , Differential.of_reports
        ~current_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$3$1.foo():bool.9ff39eb5c53c81da9f6a7ade324345b6"
              ~file:"a.java" ~key:1 ~hash:1 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:
                "com.whatever.package.Class$1.foo():bool.bf13089cf4c47ff8ff089a1a4767324f"
              ~file:"a.mm" ~key:1 ~hash:3 () ]
        ~previous_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$21$1.foo():bool.db89561ad9dab28587c8c04833f09b03"
              ~file:"a.java" ~key:1 ~hash:2 ()
          ; create_fake_jsonbug ~bug_type:"bug_type_2"
              ~procedure_id:
                "com.whatever.package.Class$8.foo():bool.cffd4e941668063eb802183dbd3e856d"
              ~file:"a.mm" ~key:1 ~hash:4 () ]
    , ([3], [4], [1]) )
  ; ( "test_skip_anonymous_class_renamings_with_different_call_procedure_qualifier_tags"
    , Differential.of_reports
        ~current_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$3$1.foo():bool.9ff39eb5c53c81da9f6a7ade324345b6"
              ~file:"a.java" ~key:1 ~qualifier_tags:qt1 ~hash:1 () ]
        ~previous_report:
          [ create_fake_jsonbug ~bug_type:"bug_type_1"
              ~procedure_id:
                "com.whatever.package.Class$21$1.foo():bool.db89561ad9dab28587c8c04833f09b03"
              ~file:"a.java" ~key:1 ~qualifier_tags:qt2 ~hash:2 () ]
    , ([1], [2], []) ) ]
  |> List.map ~f:(fun (name, diff, expected_output) -> name >:: create_test diff expected_output)

let test_resolve_infer_eradicate_conflict =
  let fake_filters_factory analyzer =
    match analyzer with
    | Config.Eradicate
     -> { Inferconfig.path_filter= (function _ -> true)
        ; (* all paths are whitelisted *)
        error_filter= (function _ -> failwith "error_filter is not needed")
        ; proc_filter= (function _ -> failwith "proc_filter is not needed") }
    | _
     -> failwith "This mock only supports Eradicate"
  in
  let create_test analyzer (exp_introduced, exp_fixed, exp_preexisting) _ =
    let null_dereference = Localise.to_issue_id Localise.null_dereference in
    let current_report =
      [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:1 ()
      ; create_fake_jsonbug ~bug_type:null_dereference ~file:"file_2.java" ~hash:2 ()
      ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_4.java" ~hash:4 () ]
    in
    let previous_report =
      [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:11 ()
      ; create_fake_jsonbug ~bug_type:null_dereference ~file:"file_3.java" ~hash:3 ()
      ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_4.java" ~hash:4 () ]
    in
    let diff = Differential.of_reports ~current_report ~previous_report in
    let diff' =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.resolve_infer_eradicate_conflict
        analyzer fake_filters_factory diff
    in
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of introduced") exp_introduced
      (sorted_hashes_of_issues diff'.introduced) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of fixed") exp_fixed
      (sorted_hashes_of_issues diff'.fixed) ;
    assert_equal ~pp_diff:(pp_diff_of_int_list "Hashes of preexisting") exp_preexisting
      (sorted_hashes_of_issues diff'.preexisting)
  in
  (* [(test_name, analyzer, expected_hashes); ...] *)
  [ ( "test_resolve_infer_eradicate_conflict_runs_with_infer_analyzer"
    , Config.BiAbduction
    , ([1], [11], [4]) )
  ; ( "test_resolve_infer_eradicate_conflict_skips_with_checkers_analyzer"
    , Config.Checkers
    , ([1; 2], [3; 11], [4]) )
  ; ( "test_resolve_infer_eradicate_conflict_skips_with_linters_analyzer"
    , Config.Linters
    , ([1; 2], [3; 11], [4]) ) ]
  |> List.map ~f:(fun (name, analyzer, expected_output) ->
         name >:: create_test analyzer expected_output )

let test_interesting_paths_filter =
  let report =
    [ create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_1.java" ~hash:1 ()
    ; create_fake_jsonbug ~bug_type:(Localise.to_issue_id Localise.null_dereference)
        ~file:"file_2.java" ~hash:2 ()
    ; create_fake_jsonbug ~bug_type:"bug_type_1" ~file:"file_4.java" ~hash:4 () ]
  in
  let create_test interesting_paths expected_hashes _ =
    let filter =
      DifferentialFilters.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.interesting_paths_filter
        interesting_paths
    in
    let filtered_report = filter report in
    assert_equal ~pp_diff:(pp_diff_of_int_list "Bug hash") expected_hashes
      (sorted_hashes_of_issues filtered_report)
  in
  [ ("test_interesting_paths_filter_with_none_interesting_paths", None, [1; 2; 4])
  ; ( "test_interesting_paths_filter_with_some_interesting_paths"
    , Some
        [ SourceFile.create ~warn_on_error:false "file_not_existing.java"
        ; SourceFile.create ~warn_on_error:false "file_4.java" ]
    , [4] )
  ; ( "test_interesting_paths_filter_with_some_interesting_paths_that_are_not_in_report"
    , Some
        [ SourceFile.create ~warn_on_error:false "file_not_existing.java"
        ; SourceFile.create ~warn_on_error:false "file_whatever.java" ]
    , [] ) ]
  |> List.map ~f:(fun (name, interesting_paths, expected_output) ->
         name >:: create_test interesting_paths expected_output )

let tests =
  "differential_filters_suite"
  >::: test_file_renamings_from_json @ test_file_renamings_find_previous
       @ test_relative_complements @ test_skip_anonymous_class_renamings
       @ test_resolve_infer_eradicate_conflict @ test_interesting_paths_filter
       @ [test_skip_duplicated_types_on_filenames; test_value_of_qualifier_tag]
