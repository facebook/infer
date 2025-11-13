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
    SIL: Consistency Error: dummy.sil, line 7, column 15: function test3 which can be called with 0 arguments is not declared
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
