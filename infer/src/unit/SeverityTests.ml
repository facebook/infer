(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let order_tests _ =
  assert_equal (-1) (IssueType.compare_severity Info Advice) ;
  assert_equal (-1) (IssueType.compare_severity Advice Warning) ;
  assert_equal (-1) (IssueType.compare_severity Warning Error)


let tests = "severity_test_suite" >::: ["severity_order_tests" >:: order_tests]
