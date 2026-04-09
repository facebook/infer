(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open TextualTestHelpers

let text =
  {|
       .source_language = "c"
       .source_file = "fake.c"

       define test1(): void {
         #start:
           ret null
       }

       |}


let%expect_test _ =
  parse_string_and_verify_keep_going text ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "c"

    .source_file = "fake.c"

    define test1() : void {
      #start:
          ret null

    }


    Veryfing the transformed module...
    verification succeeded |}]


let text =
  {|
       .source_language = "c"
       .source_file = "fake.c"

       define test1(): void {
         #start:
           ret test3()
       }

       define test2(): *int {
         #start:
           ret 0
       }
       |}


let%expect_test _ =
  parse_string_and_verify_keep_going text ;
  [%expect
    {|
    verification failed - 1 errors
    ------
    SIL: Consistency Error: function test3 which can be called with 0 arguments is not declared in dummy.sil at line 7, column 15
    |}]


let text =
  {|
       .source_language = "c"
       .source_file = "fake.c"

       define test1(): *int {
         #start:
           ret test2()
       }

       define test2(): *int {
         #start:
           ret 0
       }
       |}


let%expect_test _ =
  parse_string_and_verify_keep_going text ;
  [%expect
    {|
    verification succeeded - no warnings
    ------
    .source_language = "c"

    .source_file = "fake.c"

    define test1() : *int {
      #start:
          ret test2()

    }

    define test2() : *int {
      #start:
          ret 0

    }


    Veryfing the transformed module...
    verification succeeded
    |}]


(* Test that ~lenient:true recovers from BasicVerification errors.
   Without lenient, calling an undeclared function is a non-recoverable error.
   With lenient, the module is returned with warnings. *)
let text_basic_error =
  {|
       .source_language = "c"
       .source_file = "fake.c"

       define test_calls_undeclared(): void {
         #start:
           n0 = undeclared_function(1, 2)
           ret null
       }
       |}


let%expect_test "basic error without lenient fails" =
  parse_string_and_verify_keep_going text_basic_error ;
  [%expect
    {|
    verification failed - 1 errors
    ------
    SIL: Consistency Error: function undeclared_function which can be called with 2 arguments is not declared in dummy.sil at line 7, column 16
    |}]


let%expect_test "basic error with lenient recovers" =
  parse_string_and_verify_keep_going_lenient text_basic_error ;
  [%expect
    {|
    lenient verification succeeded - 1 warnings
    ------
    SIL: Consistency Error: function undeclared_function which can be called with 2 arguments is not declared in dummy.sil at line 7, column 16
    ------
    .source_language = "c"

    .source_file = "fake.c"

    define test_calls_undeclared() : void {
      #start:
          n0 = undeclared_function(1, 2)
          ret null

    }


    Verifying the filtered module...
    SIL: Consistency Error: function undeclared_function which can be called with 2 arguments is not declared in dummy.sil at line 7, column 16
    |}]
