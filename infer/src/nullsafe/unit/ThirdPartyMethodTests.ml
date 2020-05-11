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
      assert_equal output expected_output ~printer:(fun parse_result ->
          Pp.string_of_pp pp_parse_result parse_result )
  | Error error ->
      assert_failure
        (F.asprintf "Expected '%s' to be parsed as %a, but got error %s instead" input
           ThirdPartyMethod.pp_parse_result expected_output (string_of_parsing_error error))


let assert_parse_bad input =
  let result = ThirdPartyMethod.parse input in
  match result with
  | Ok output ->
      assert_failure
        (F.asprintf "Expected '%s' to be NOT parsed, but was parsed as %a instead" input
           ThirdPartyMethod.pp_parse_result output)
  | Error _ ->
      ()


let success_cases =
  "success_cases"
  >:: fun _ ->
  (* No params *)
  assert_parse_ok "a.b.C#foo()"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= []}
    , {ret_nullability= Nonnull; param_nullability= []} ) ;
  assert_parse_ok "a.b.C#foo() @Nullable"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= []}
    , {ret_nullability= Nullable; param_nullability= []} ) ;
  (* One param *)
  assert_parse_ok "a.b.C#foo(c.d.E)"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"]}
    , {ret_nullability= Nonnull; param_nullability= [Nonnull]} ) ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E)"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"]}
    , {ret_nullability= Nonnull; param_nullability= [Nullable]} ) ;
  assert_parse_ok "a.b.C#foo(c.d.E) @Nullable"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"]}
    , {ret_nullability= Nullable; param_nullability= [Nonnull]} ) ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E) @Nullable"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"]}
    , {ret_nullability= Nullable; param_nullability= [Nullable]} ) ;
  (* Many params *)
  assert_parse_ok "a.b.C#foo(c.d.E, a.b.C, x.y.Z)"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"; "a.b.C"; "x.y.Z"]}
    , {ret_nullability= Nonnull; param_nullability= [Nonnull; Nonnull; Nonnull]} ) ;
  assert_parse_ok "a.b.C#foo(c.d.E, @Nullable a.b.C, x.y.Z)"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"; "a.b.C"; "x.y.Z"]}
    , {ret_nullability= Nonnull; param_nullability= [Nonnull; Nullable; Nonnull]} ) ;
  assert_parse_ok "a.b.C#foo(@Nullable c.d.E, a.b.C, @Nullable x.y.Z) @Nullable"
    ( {class_name= "a.b.C"; method_name= Method "foo"; param_types= ["c.d.E"; "a.b.C"; "x.y.Z"]}
    , {ret_nullability= Nullable; param_nullability= [Nullable; Nonnull; Nullable]} ) ;
  (* Constructor *)
  assert_parse_ok "a.b.C#<init>(@Nullable c.d.E, a.b.C, x.y.Z) @Nullable"
    ( {class_name= "a.b.C"; method_name= Constructor; param_types= ["c.d.E"; "a.b.C"; "x.y.Z"]}
    , {ret_nullability= Nullable; param_nullability= [Nullable; Nonnull; Nonnull]} )


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
