(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
open AggregatedSummaries
module F = Format

let test_package = Some "test.package"

let mock_summary id =
  (* For simplicity, mock summaries will be represented as a list with the single issue where id is encoded in description. *)
  let mock_issue =
    TypeErr.Condition_redundant
      { is_always_true= true
      ; loc= Location.dummy
      ; condition_descr= Some (Int.to_string id)
      ; nonnull_origin= TypeOrigin.New }
  in
  NullsafeSummary.{issues= [mock_issue]}


let get_mock_summary_id NullsafeSummary.{issues} =
  match issues with
  | [TypeErr.Condition_redundant {condition_descr= Some id}] ->
      Int.of_string id
  | _ ->
      assert_failure "Expected mock summary, got something else"


type expected_class_info =
  { name: string
  ; nested: expected_class_info list
  ; anm: (string * int list) list
        (* List of nested anonymous names together with list of their expected summary ids *)
  ; summary_ids: int list  (** sorted for convenience *) }

let assert_summaries_equal actual_summaries expected_ids class_name case_name =
  let actual_summaries_ids =
    actual_summaries |> List.map ~f:get_mock_summary_id |> List.sort ~compare:Int.compare
  in
  assert_equal expected_ids actual_summaries_ids
    ~msg:(F.sprintf "%s: summary ids for %s did not match" case_name class_name)


let assert_anonymous_equal actual expected_name_to_ids case_name =
  List.iter expected_name_to_ids ~f:(fun (anonymous_name, expected_summary_ids) ->
      let actual_summaries =
        JavaClassName.Map.find_opt
          (JavaClassName.make ~package:test_package ~classname:anonymous_name)
          actual
        |> IOption.if_none_eval ~f:(fun () ->
               assert_failure
                 (F.sprintf "%s: Did not find anonymous class info for %s" case_name anonymous_name) )
      in
      assert_summaries_equal actual_summaries expected_summary_ids anonymous_name case_name )


let rec assert_expected_list actual expected case_name =
  assert_equal (List.length actual) (List.length expected)
    ~msg:
      (Format.sprintf "%s: Expected list of %d, got %d" case_name (List.length expected)
         (List.length actual)) ;
  List.iter expected ~f:(fun {name; summary_ids; nested; anm} ->
      (* For each expected info find corresponding actual and compare *)
      let actual_info =
        List.find actual ~f:(fun actual_info ->
            JavaClassName.equal
              (ClassInfo.get_class_name actual_info)
              (JavaClassName.make ~package:test_package ~classname:name) )
        |> IOption.if_none_eval ~f:(fun () ->
               assert_failure (F.sprintf "%s: Did not find class info for %s" case_name name) )
      in
      assert_summaries_equal (ClassInfo.get_summaries actual_info) summary_ids name case_name ;
      assert_anonymous_equal (ClassInfo.get_nested_anonymous_summaries actual_info) anm case_name ;
      (* Now recursively check all children and match them with expected *)
      assert_expected_list
        (ClassInfo.get_nested_classes_info actual_info)
        nested
        (F.sprintf "%s:%s" case_name name) )


let aggregate list =
  List.map list ~f:(fun (classname, summary_id) ->
      (JavaClassName.make ~package:test_package ~classname, mock_summary summary_id) )
  |> AggregatedSummaries.aggregate


let single_class =
  "single_class"
  >:: fun _ ->
  assert_expected_list (aggregate []) [] "empty" ;
  assert_expected_list
    (aggregate [("A", 1)])
    [{name= "A"; summary_ids= [1]; anm= []; nested= []}]
    "single method" ;
  assert_expected_list
    (aggregate [("A", 1); ("A", 5); ("A", 2)])
    [{name= "A"; summary_ids= [1; 2; 5]; anm= []; nested= []}]
    "several methods"


let several_top_classes =
  "several_top_classes"
  >:: fun _ ->
  assert_expected_list
    (aggregate [("A", 1); ("B", 2)])
    [ {name= "A"; summary_ids= [1]; anm= []; nested= []}
    ; {name= "B"; summary_ids= [2]; anm= []; nested= []} ]
    "two simple classes" ;
  assert_expected_list
    (aggregate [("A", 1); ("B", 5); ("A", 2); ("C", 7); ("A", 8); ("B", 9)])
    [ {name= "A"; summary_ids= [1; 2; 8]; anm= []; nested= []}
    ; {name= "B"; summary_ids= [5; 9]; anm= []; nested= []}
    ; {name= "C"; summary_ids= [7]; anm= []; nested= []} ]
    "several classes"


let one_top_class_with_nested =
  "several_top_classes"
  >:: fun _ ->
  assert_expected_list
    (aggregate [("A$B", 1); ("A", 2)])
    [ { name= "A"
      ; summary_ids= [2]
      ; anm= []
      ; nested= [{name= "A$B"; summary_ids= [1]; anm= []; nested= []}] } ]
    "simple nested" ;
  assert_expected_list
    (aggregate [("A$B", 1); ("A", 2); ("A$B", 3); ("A", 4); ("A$C", 6); ("A$D", 7)])
    [ { name= "A"
      ; summary_ids= [2; 4]
      ; anm= []
      ; nested=
          [ {name= "A$B"; summary_ids= [1; 3]; anm= []; nested= []}
          ; {name= "A$C"; summary_ids= [6]; anm= []; nested= []}
          ; {name= "A$D"; summary_ids= [7]; anm= []; nested= []} ] } ]
    "simple nested, several summaries" ;
  assert_expected_list
    (aggregate [("A$B", 1)])
    [ { name= "A"
      ; summary_ids= []
      ; anm= []
      ; nested= [{name= "A$B"; summary_ids= [1]; anm= []; nested= []}] } ]
    "nested should also create ancestors even if they have no summaries" ;
  assert_expected_list
    (aggregate [("A$B$D$F", 1); ("A$B$C", 2)])
    [ { name= "A"
      ; summary_ids= []
      ; anm= []
      ; nested=
          [ { name= "A$B"
            ; summary_ids= []
            ; anm= []
            ; nested=
                [ {name= "A$B$C"; summary_ids= [2]; anm= []; nested= []}
                ; { name= "A$B$D"
                  ; summary_ids= []
                  ; anm= []
                  ; nested= [{name= "A$B$D$F"; summary_ids= [1]; anm= []; nested= []}] } ] } ] } ]
    "nested should also create ancestors even if they have no summaries"


let with_anonymous =
  "with_anonymous"
  >:: fun _ ->
  assert_expected_list
    (aggregate [("A$B$C$3$4", 1); ("A$5$6", 2); ("A$5", 3); ("A", 4); ("A$5$7", 5); ("A$5$6", 6)])
    [ { name= "A"
      ; summary_ids= [4]
      ; anm= [("A$5$6", [2; 6]); ("A$5", [3]); ("A$5$7", [5])]
      ; nested=
          [ { name= "A$B"
            ; summary_ids= []
            ; anm= []
            ; nested= [{name= "A$B$C"; summary_ids= []; anm= [("A$B$C$3$4", [1])]; nested= []}] } ]
      } ]
    "with_anonymous"


let several_top_classes_with_nested =
  "several_top_classes_with_nested"
  >:: fun _ ->
  assert_expected_list
    (aggregate [("B", 1); ("A", 2); ("C$D", 3); ("A$B$D", 4); ("A$B$D", 5); ("A", 6)])
    [ { name= "A"
      ; summary_ids= [2; 6]
      ; anm= []
      ; nested=
          [ { name= "A$B"
            ; summary_ids= []
            ; anm= []
            ; nested= [{name= "A$B$D"; summary_ids= [4; 5]; anm= []; nested= []}] } ] }
    ; {name= "B"; summary_ids= [1]; anm= []; nested= []}
    ; { name= "C"
      ; summary_ids= []
      ; anm= []
      ; nested= [{name= "C$D"; summary_ids= [3]; anm= []; nested= []}] } ]
    "several_top_classes_with_nested"


let test =
  "AggregatedSummariesTest"
  >::: [ single_class
       ; several_top_classes
       ; one_top_class_with_nested
       ; with_anonymous
       ; several_top_classes_with_nested ]
