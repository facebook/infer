(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let tests =
  let open OUnit2 in
  let empty_string_test =
    let empty_string_test_ _ =
      assert_raises (Logging.InferUserError "Empty stack trace") (fun () -> Stacktrace.of_string "")
    in
    "empty_string" >:: empty_string_test_
  in
  let empty_trace_test =
    let empty_stack_trace_s = "Exception in thread \"main\" java.lang.NullPointerException" in
    let trace = Stacktrace.of_string empty_stack_trace_s in
    let empty_trace_test_ _ = assert_equal trace.frames [] in
    "empty_trace" >:: empty_trace_test_
  in
  let one_frame_trace_test =
    let one_frame_trace_test_s =
      "Exception in thread \"main\" java.lang.NullPointerException\n"
      ^ "\tat endtoend.java.checkers.crashcontext.MinimalCrashTest.main"
      ^ "(MinimalCrashTest.java:16)"
    in
    let trace = Stacktrace.of_string one_frame_trace_test_s in
    let expected =
      Stacktrace.make "java.lang.NullPointerException"
        [ Stacktrace.make_frame "endtoend.java.checkers.crashcontext.MinimalCrashTest" "main"
            "MinimalCrashTest.java" (Some 16) ]
    in
    let one_frame_trace_test_ _ = assert_equal trace expected in
    "one_frame_trace" >:: one_frame_trace_test_
  in
  let multi_frame_trace_test =
    let multi_frame_trace_test_s =
      "Exception in thread \"main\" java.lang.NullPointerException\n\t"
      ^ "at endtoend.java.checkers.crashcontext.MultiStackFrameCrashTest.bar"
      ^ "(MultiStackFrameCrashTest.java:16)\n"
      ^ "\tat endtoend.java.checkers.crashcontext.MultiStackFrameCrashTest.foo"
      ^ "(MultiStackFrameCrashTest.java:20)\n"
      ^ "\tat endtoend.java.checkers.crashcontext.MultiStackFrameCrashTest.main"
      ^ "(MultiStackFrameCrashTest.java:24)"
    in
    let trace = Stacktrace.of_string multi_frame_trace_test_s in
    let class_name = "endtoend.java.checkers.crashcontext.MultiStackFrameCrashTest" in
    let file_name = "MultiStackFrameCrashTest.java" in
    let expected =
      Stacktrace.make "java.lang.NullPointerException"
        [ Stacktrace.make_frame class_name "bar" file_name (Some 16)
        ; Stacktrace.make_frame class_name "foo" file_name (Some 20)
        ; Stacktrace.make_frame class_name "main" file_name (Some 24) ]
    in
    let multi_frame_trace_test_ _ = assert_equal trace expected in
    "multi_frame_trace_test" >:: multi_frame_trace_test_
  in
  let missing_line_info_test =
    let missing_line_info_test_s =
      "Exception in thread \"main\" java.lang.NullPointerException\n"
      ^ "\tat endtoend.java.checkers.crashcontext.MinimalCrashTest.main"
      ^ "(MinimalCrashTest.java)"
    in
    let trace = Stacktrace.of_string missing_line_info_test_s in
    let expected =
      Stacktrace.make "java.lang.NullPointerException"
        [ Stacktrace.make_frame "endtoend.java.checkers.crashcontext.MinimalCrashTest" "main"
            "MinimalCrashTest.java" None ]
    in
    let missing_line_info_test_ _ = assert_equal trace expected in
    "missing_line_info_test" >:: missing_line_info_test_
  in
  "all_tests_suite"
  >::: [ empty_string_test
       ; empty_trace_test
       ; one_frame_trace_test
       ; multi_frame_trace_test
       ; missing_line_info_test ]
