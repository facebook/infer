(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let parse input =
  let result = ThirdPartyMethod.parse input in
  match result with
  | Ok output ->
      F.print_string (Sexp.to_string (ThirdPartyMethod.sexp_of_t output)) ;
      let output_s = ThirdPartyMethod.to_canonical_string output in
      if not (String.equal output_s input) then
        F.printf
          "@\n\
           FAILED TEST: the canonical string of the parsed object is '%s', which does not match \
           the input string."
          output_s
  | Error error ->
      F.printf "error: %s" (ThirdPartyMethod.string_of_parsing_error error)


let%test_module "Third-party Method Tests OK Cases" =
  ( module struct
    (* No params *)

    let%expect_test "no params" =
      parse "a.b.C#foo()" ;
      [%expect {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nonnull)(params()))|}]

    let%expect_test _ =
      parse "a.b.C#foo() @Nullable" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nullable)(params()))|}]

    (* One param *)

    let%expect_test _ =
      parse "a.b.C#foo(c.d.E)" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nonnull)(params((c.d.E Nonnull))))|}]

    let%expect_test _ =
      parse "a.b.C#foo(@Nullable c.d.E)" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nonnull)(params((c.d.E Nullable))))|}]

    let%expect_test _ =
      parse "a.b.C#foo(c.d.E) @Nullable" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nullable)(params((c.d.E Nonnull))))|}]

    let%expect_test _ =
      parse "a.b.C#foo(@Nullable c.d.E) @Nullable" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nullable)(params((c.d.E Nullable))))|}]

    (* Many params *)

    let%expect_test _ =
      parse "a.b.C#foo(c.d.E, a.b.C, x.y.Z)" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nonnull)(params((c.d.E Nonnull)(a.b.C Nonnull)(x.y.Z Nonnull))))|}]

    let%expect_test _ =
      parse "a.b.C#foo(c.d.E, @Nullable a.b.C, x.y.Z)" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nonnull)(params((c.d.E Nonnull)(a.b.C Nullable)(x.y.Z Nonnull))))|}]

    let%expect_test _ =
      parse "a.b.C#foo(@Nullable c.d.E, a.b.C, @Nullable x.y.Z) @Nullable" ;
      [%expect
        {|((class_name a.b.C)(method_name(Method foo))(ret_nullability Nullable)(params((c.d.E Nullable)(a.b.C Nonnull)(x.y.Z Nullable))))|}]

    (* Constructor *)

    let%expect_test _ =
      parse "a.b.C#<init>(@Nullable c.d.E, a.b.C, x.y.Z) @Nullable" ;
      [%expect
        {|((class_name a.b.C)(method_name Constructor)(ret_nullability Nullable)(params((c.d.E Nullable)(a.b.C Nonnull)(x.y.Z Nonnull))))|}]
  end )

(* We intentionally don't test all bad cases.
   It is generally OK for nullsafe to allow something that is not really valid:
   We let other tools to make thorough linting.
   Also we don't test exact error type, because this is an implementation detail
   needed merely to simplify diagnostics
   *)
let%test_module "Third-party Method Tests Bad Cases" =
  ( module struct
    let%expect_test _ =
      parse "" ;
      [%expect {| error: Accepted format is <class>#<method>(<params>)[<return nullability>] |}]

    let%expect_test _ =
      parse "   " ;
      [%expect {| error: Accepted format is <class>#<method>(<params>)[<return nullability>] |}]

    let%expect_test _ =
      parse "blablabla" ;
      [%expect {|error: Accepted format is <class>#<method>(<params>)[<return nullability>]|}]

    let%expect_test "no # delimiter" =
      parse "a.b.C.f()" ;
      [%expect {|error: Accepted format is <class>#<method>(<params>)[<return nullability>]|}]

    let%expect_test "nested parenthesis" =
      parse "a.b.C#f(())" ;
      [%expect {|error: Accepted format is <class>#<method>(<params>)[<return nullability>]|}]

    let%expect_test "param names are not accepted" =
      parse "a.b.C#f(int param)" ;
      [%expect {|error: Each param should have form of [@Nullable] <fully qualified type name>|}]

    let%expect_test "missed package for class" =
      parse "C#f()" ;
      [%expect {|error: Class name should be fully qualified, including package name|}]

    let%expect_test "Missed @ in annotation" =
      parse "a.b.C#f(Nullable a.b.C)" ;
      [%expect {|error: Each param should have form of [@Nullable] <fully qualified type name>|}]

    let%expect_test "Extra spaces" =
      parse "a.b.C#f( a.b.C )" ;
      [%expect {|error: Each param should have form of [@Nullable] <fully qualified type name>|}]

    let%expect_test "No space after comma" =
      parse "a.b.C#f(a.b.C,a.b.C)" ;
      [%expect {|error: Params should be separated by a comma, followed by a single space|}]

    let%expect_test "Param names are not accepted" =
      parse "a.b.C#f(@Nullable int param)" ;
      [%expect {|error: Each param should have form of [@Nullable] <fully qualified type name>|}]
  end )
