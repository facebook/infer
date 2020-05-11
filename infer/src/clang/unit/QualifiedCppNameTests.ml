(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let test_fuzzy_match =
  let create_test fuzzy_qual_names qualifiers expected_match _ =
    let output =
      let qualified_name = QualifiedCppName.of_list qualifiers in
      let matcher = QualifiedCppName.Match.of_fuzzy_qual_names fuzzy_qual_names in
      QualifiedCppName.Match.match_qualifiers matcher qualified_name
    in
    assert_equal ~cmp:Bool.equal expected_match output
  in
  [ ("test_simple_match1", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["foo"; "baz"], true)
  ; ("test_simple_match2", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["foo"; "bar"; "baz"], true)
  ; ("test_simple_match3", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["goo"; "goo"], true)
  ; ("test_no_simple_match1", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["foo"; "bar"], false)
  ; ("test_no_simple_match2", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["goo"; "foo"], false)
  ; ("test_no_simple_match3", ["foo::bar::baz"; "foo::baz"; "goo::goo"], ["moo"], false)
  ; ( "test_no_simple_match4"
    , ["foo::bar::baz"; "foo::baz"; "goo::goo"]
    , ["foo"; "bar"; "baz"; "bad"]
    , false )
  ; ( "test_no_simple_match5"
    , ["foo::bar::baz"; "foo::baz"; "goo::goo"]
    , ["foo"; "bad"; "bar"; "baz"]
    , false )
  ; ( "test_template_match"
    , ["foo::bar::baz"]
    , ["foo"; "bar<goo::moo<int,std::string>,const X&>"; "baz<int>"]
    , true )
  ; ("test_std_direct_match", ["std::foo"], ["std"; "foo"], true)
  ; ("test_std_direct_no_match1", ["std::foo"], ["std"; "goo"], false)
  ; ("test_std_direct_no_match2", ["std::foo"], ["std"; "foo"; "bad"], false)
  ; ("test_std_direct_no_match3", ["std::foo"], ["stdBAD"; "foo"], false)
  ; ("test_std_no_fuzzy_match1", ["std::foo"], ["std"; "__1"; "foo"], false)
  ; ("test_std_no_fuzzy_match2", ["std::foo"], ["std"; "goo<int>"; "foo"], false)
  ; ("test_std_no_fuzzy_match3", ["std::foo"], ["std"; "goo<int>"; "foo<const X&>"], false)
  ; ("test_std_fuzzy_no_match1", ["std::foo"], ["std"; "__1"; "__2"; "foo"], false)
  ; ("test_std_fuzzy_no_match2", ["std::foo"], ["std"; "__1"; "foo"; "bad"], false) ]
  |> List.map ~f:(fun (name, fuzzy_qual_names, qualifiers, expected_output) ->
         name >:: create_test fuzzy_qual_names qualifiers expected_output )


let tests = "qualified_cpp_name_fuzzy_match" >::: test_fuzzy_match
