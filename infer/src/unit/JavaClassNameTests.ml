(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
open JavaClassName

let assert_equal_to classname ~expected_package ~expected_classname =
  assert_equal ~printer:JavaClassName.to_string classname
    (make ~package:expected_package ~classname:expected_classname)


let assert_some = function Some a -> a | None -> assert_failure "Expected Some, got None"

let assert_none = function Some _ -> assert_failure "Expected None, got Some" | None -> ()

let test_from_string =
  "test_from_string"
  >:: fun _ ->
  assert_equal_to
    (from_string "some.package.SomeClass")
    ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  assert_equal_to (from_string "SomeClass") ~expected_package:None ~expected_classname:"SomeClass" ;
  assert_equal_to
    (from_string "some.package.SomeClass$NestedClass")
    ~expected_package:(Some "some.package") ~expected_classname:"SomeClass$NestedClass" ;
  assert_equal_to
    (from_string "SomeClass$NestedClass")
    ~expected_package:None ~expected_classname:"SomeClass$NestedClass" ;
  (* anonymous classes *)
  assert_equal_to
    (from_string "some.package.SomeClass$1")
    ~expected_package:(Some "some.package") ~expected_classname:"SomeClass$1" ;
  (* anonymous classes can be of nested levels *)
  assert_equal_to
    (from_string "some.package.SomeClass$1$3")
    ~expected_package:(Some "some.package") ~expected_classname:"SomeClass$1$3" ;
  (* anonymous classes can be inside nested *)
  assert_equal_to
    (from_string "SomeClass$NestedClass$1$3")
    ~expected_package:None ~expected_classname:"SomeClass$NestedClass$1$3"


let test_anonymous =
  "test_anonymous"
  >:: fun _ ->
  (* If it is not an anonymous class, we expect this to return None *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass")
  |> assert_none ;
  get_user_defined_class_if_anonymous_inner (make ~package:None ~classname:"SomeClass")
  |> assert_none ;
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$SomeNestedClass$AgainNestedClass")
  |> assert_none ;
  (* If it is an anonymous class, we expect this to be detected *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$17")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  (* Can be several nested anonymous classes *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$17$23$1")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  (* Can be nested class with anonymous class *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$NestedClass$AgainNestedClass$17")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package")
       ~expected_classname:"SomeClass$NestedClass$AgainNestedClass" ;
  (* Can be nested class AND several nested anonymous classes *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$NestedClass$AgainNestedClass$17$23$1")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package")
       ~expected_classname:"SomeClass$NestedClass$AgainNestedClass" ;
  (* Some name inside of anonymous - this is still anonymous  *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$1$SomeName")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  (* Some names inside anonymous classes - should still return the innermost user defined *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"A$B$1$C$2")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"A$B" ;
  (* Pathological case - this is anonymous class on the outer level. Should return None because it is not inner. *)
  get_user_defined_class_if_anonymous_inner (make ~package:(Some "some.package") ~classname:"23")
  |> assert_none ;
  (* Pathological case - this is anonymous class on the outer level. Should return None because it is not inner. *)
  get_user_defined_class_if_anonymous_inner (make ~package:(Some "some.package") ~classname:"1$A")
  |> assert_none ;
  (* If it is a lambda class, we expect this to be detected *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:(Some "some.package") ~classname:"SomeClass$Lambda$_4_1")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  (* If package was empty, everything should still work *)
  get_user_defined_class_if_anonymous_inner
    (make ~package:None ~classname:"SomeClass$NestedClass$AgainNestedClass$17$23$1")
  |> assert_some
  |> assert_equal_to ~expected_package:None
       ~expected_classname:"SomeClass$NestedClass$AgainNestedClass"


let test_outer =
  "test_outer"
  >:: fun _ ->
  get_outer_class_name (make ~package:(Some "some.package") ~classname:"SomeClass") |> assert_none ;
  get_outer_class_name (make ~package:(Some "some.package") ~classname:"SomeClass$NestedClass")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package") ~expected_classname:"SomeClass" ;
  get_outer_class_name
    (make ~package:(Some "some.package") ~classname:"SomeClass$NestedClass$AnotherNested")
  |> assert_some
  |> assert_equal_to ~expected_package:(Some "some.package")
       ~expected_classname:"SomeClass$NestedClass"


let tests = "JavaClassNameTests" >::: [test_from_string; test_anonymous; test_outer]
