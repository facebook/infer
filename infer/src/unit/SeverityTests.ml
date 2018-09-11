(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let order_tests _ =
  assert_equal (-1) (Exceptions.compare_severity Exceptions.Like Exceptions.Info) ;
  assert_equal (-1) (Exceptions.compare_severity Exceptions.Info Exceptions.Advice) ;
  assert_equal (-1) (Exceptions.compare_severity Exceptions.Advice Exceptions.Warning) ;
  assert_equal (-1) (Exceptions.compare_severity Exceptions.Warning Exceptions.Error)


let tests = "severity_test_suite" >::: ["severity_order_tests" >:: order_tests]
