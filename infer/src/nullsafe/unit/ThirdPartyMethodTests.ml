(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open OUnit2
open ThirdPartyMethod

let assert_parse_ok input expected_output =
  let result = ThirdPartyMethod.parse input in
  match result with
  | Ok output ->
      (* Check that it was parsed to the expected result*)
      assert_equal output expected_output ~printer:(fun parse_result ->
          Pp.string_of_pp pp parse_result ) ;
      (* Check also that the canonical representation matches the original *)
      assert_equal (to_canonical_string output) input
  | Error error ->
      assert_failure
        (F.asprintf "Expected '%s' to be parsed, but got error %s instead" input
           (string_of_parsing_error error))


let assert_parse_bad input =
  let result = ThirdPartyMethod.parse input in
  match result with
  | Ok output ->
      assert_failure
        (F.asprintf "Expected '%s' to be NOT parsed, but was parsed as %a instead" input
           ThirdPartyMethod.pp output)
  | Error _ ->
      ()


let success_cases =
  "success_cases"
  >:: fun _ ->
  (* No params *)
  assert_parse_ok "a.b.C#foo()"
    {class_name= "a.b.C"; method_name= Method "foo"; params= []; ret_nullability= Nonnull} ;
  assert_parse_ok "a.b.C#foo() @Nullable"
    {class_name= "a.b.C"; method_name= Method "foo"; params= []; ret_nullability= Nullable} ;
  (* One param *)
  assert_parse_ok "a.b.C#foo(c.d.E)"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nonnull)]
    ; ret_nullability= Nonnull } ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E)"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nullable)]
    ; ret_nullability= Nonnull } ;
  assert_parse_ok "a.b.C#foo(c.d.E) @Nullable"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nonnull)]
    ; ret_nullability= Nullable } ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E) @Nullable"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nullable)]
    ; ret_nullability= Nullable } ;
  (* Many params *)
  assert_parse_ok "a.b.C#foo(c.d.E, a.b.C, x.y.Z)"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nonnull); ("a.b.C", Nonnull); ("x.y.Z", Nonnull)]
    ; ret_nullability= Nonnull } ;
  assert_parse_ok "a.b.C#foo(c.d.E, @Nullable a.b.C, x.y.Z)"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nonnull); ("a.b.C", Nullable); ("x.y.Z", Nonnull)]
    ; ret_nullability= Nonnull } ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E, a.b.C, @Nullable x.y.Z) @Nullable"
    { class_name= "a.b.C"
    ; method_name= Method "foo"
    ; params= [("c.d.E", Nullable); ("a.b.C", Nonnull); ("x.y.Z", Nullable)]
    ; ret_nullability= Nullable } ;
  (* Constructor *)
  assert_parse_ok "a.b.C#<init>(@Nullable c.d.E, a.b.C, x.y.Z) @Nullable"
    { class_name= "a.b.C"
    ; method_name= Constructor
    ; params= [("c.d.E", Nullable); ("a.b.C", Nonnull); ("x.y.Z", Nonnull)]
    ; ret_nullability= Nullable }


(* We intentionally don't test all bad cases.
   It is generally OK for nullsafe to allow something that is not really valid:
   We let other tools to make thorough linting.
   Also we don't test exact error type, because this is an implementation detail
   needed merely to simplify diagnostics
   *)
let bad_cases =
  "bad_cases"
  >:: fun _ ->
  assert_parse_bad "" ;
  assert_parse_bad "   " ;
  assert_parse_bad "blablabla" ;
  (* no # delimiter *)
  assert_parse_bad "a.b.C.f()" ;
  (* nested parenthesis *)
  assert_parse_bad "a.b.C#f(())" ;
  (* param names are not accepted *)
  assert_parse_bad "a.b.C#f(int param)" ;
  (* missed package for class *)
  assert_parse_bad "C#f()" ;
  (* Missed @ in annotation*)
  assert_parse_bad "a.b.C#f(Nullable a.b.C)" ;
  (* Extra spaces *)
  assert_parse_bad "a.b.C#f( a.b.C )" ;
  (* No space after comma *)
  assert_parse_bad "a.b.C#f(a.b.C,a.b.C)" ;
  (* Param names are not accepted *)
  assert_parse_bad "a.b.C#f(@Nullable int param)"


let test = "ThirdPartyMethodTests" >::: [success_cases; bad_cases]
