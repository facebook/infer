(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module UnixDiffTest = FileDiff.UnixDiff.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY

let test_unixdiff_process_raw_directives_with_valid_input =
  let create_test input expected _ =
    let found = UnixDiffTest.process_raw_directives input in
    let pp_diff fmt (expected, actual) =
      let expected_str = Format.asprintf "%a" (Pp.seq ~sep:"" UnixDiffTest.pp) expected in
      let actual_str = Format.asprintf "%a" (Pp.seq ~sep:"" UnixDiffTest.pp) actual in
      Format.fprintf fmt "Expected: '%s', found: '%s'" expected_str actual_str
    in
    assert_equal ~cmp:(List.equal UnixDiffTest.equal) ~pp_diff expected found
  in
  [ ("test_unixdiff_process_raw_directives_1", "UOOU", UnixDiffTest.[Unchanged; Old; Old; Unchanged])
  ; ("test_unixdiff_process_raw_directives_2", "", []) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_unixdiff_process_raw_directives_with_invalid_input =
  let create_test input expected_exception _ =
    let run () = UnixDiffTest.process_raw_directives input in
    assert_raises expected_exception run
  in
  [ ( "test_unixdiff_process_raw_directives_1"
    , "U OOU"
    , Logging.InferUserError "Unexpected char in input sequence. Failed parsing" )
  ; ( "test_unixdiff_process_raw_directives_2"
    , "UZ"
    , Logging.InferUserError "Unexpected char in input sequence. Failed parsing" )
  ; ( "test_unixdiff_process_raw_directives_3"
    , "UU "
    , Logging.InferUserError "Unexpected char in input sequence. Failed parsing" )
  ; ( "test_unixdiff_process_raw_directives_4"
    , " U"
    , Logging.InferUserError "Unexpected char in input sequence. Failed parsing" ) ]
  |> List.map ~f:(fun (name, test_input, expected_exception) ->
         name >:: create_test test_input expected_exception )


let test_unixdiff_pp =
  let create_test input expected _ =
    let found = Format.asprintf "%a" (Pp.seq ~sep:"" UnixDiffTest.pp) input in
    let pp_diff fmt (expected, actual) =
      Format.fprintf fmt "Expected: '%s', found: '%s'" expected actual
    in
    assert_equal ~cmp:String.equal ~pp_diff expected found
  in
  [ ("test_unixdiff_pp_1", UnixDiffTest.[Unchanged; Old; Old; Unchanged], "UOOU")
  ; ("test_unixdiff_pp_2", [], "") ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


module FileDiffTest = FileDiff.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY

let u length = List.init ~f:(fun _ -> UnixDiffTest.Unchanged) length

let n length = List.init ~f:(fun _ -> UnixDiffTest.New) length

let o length = List.init ~f:(fun _ -> UnixDiffTest.Old) length

let test_parse_directives_with_valid_input =
  let create_test input expected _ =
    let found = FileDiffTest.parse_directives input in
    let pp_diff fmt (expected, actual) =
      let expected_str = Format.asprintf "%a" (Pp.seq ~sep:", " Format.pp_print_int) expected in
      let actual_str = Format.asprintf "%a" (Pp.seq ~sep:", " Format.pp_print_int) actual in
      Format.fprintf fmt "Expected: '%s', found: '%s'" expected_str actual_str
    in
    assert_equal ~cmp:(List.equal Int.equal) ~pp_diff expected found
  in
  [ (*
  === test1 ===
  File1 and File2 are empty
  *)
    ("test_parse_directives_with_valid_input_1", [], [])
    (*
  === test2 ===
  File1     File2
  a         x
  b
  c
  d 
  *)
  ; ("test_parse_directives_with_valid_input_2", o 4 @ n 1, [1])
    (*
  === test3 ===
  File1     File2
  a         x
  v         b
  b         c
  c         Z
  *)
  ; ("test_parse_directives_with_valid_input_3", o 2 @ n 1 @ u 2 @ n 1, [1; 4])
    (*
  === test4 ===
  File1     File2
  a         w
  b         x
  c         y
            z
  *)
  ; ("test_parse_directives_with_valid_input_4", o 3 @ n 4, [1; 2; 3; 4])
    (*
  === test5 ===
  File1     File2
  a         a
  b         b
  c         c
  d         d
  e         e
            w
            x
            y
  *)
  ; ("test_parse_directives_with_valid_input_5", u 5 @ n 3, [6; 7; 8])
    (*
  === test6 ===
  File1     File2
  a         a
  b         b
  c         c
  d         d
  e         e
  f         i
  g
  h
  i
  *)
  ; ("test_parse_directives_with_valid_input_6", u 5 @ o 3 @ u 1, [6])
    (*
  === test7 ===
  File1     File2
  a
  b
  c
  *)
  ; ("test_parse_directives_with_valid_input_7", o 3, [1])
    (*
  === test8 ===
  File1     File2
  a         a
  b         d
  c         x
  d         y
  *)
  ; ("test_parse_directives_with_valid_input_8", u 1 @ o 2 @ u 1 @ n 2, [2; 3; 4])
    (*
  === test9 ===
  File1     File2
  a         d
  b         x
  c         y
  d         z
  *)
  ; ("test_parse_directives_with_valid_input_9", o 3 @ u 1 @ n 3, [1; 2; 3; 4])
    (*
  === test10 ===
  File1     File2
  a         a
  b         x
  c         d
  d         y
  e         z
  *)
  ; ("test_parse_directives_with_valid_input_10", u 1 @ o 2 @ n 1 @ u 1 @ o 1 @ n 2, [2; 4; 5])
    (*
  === test11 ===
  File1     File2
  a         a
  b         b
  c         x
  d         c
  e         y
  *)
  ; ("test_parse_directives_with_valid_input_11", u 2 @ n 1 @ u 1 @ o 2 @ n 1, [3; 5])
  ; ( "test_parse_directives_with_valid_input_12"
    , o 1 @ n 1 @ u 6 @ o 2 @ n 2 @ u 5 @ o 2 @ n 2 @ u 244 @ o 12 @ u 3 @ o 1 @ n 1 @ u 3
    , [1; 8; 9; 15; 16; 261; 264] )
    (*
    === test13 ===
    File1   File2
    a       a
    b       b
    c       c
    d       x
    e       y
            d
            e
    *)
  ; ("test_parse_directives_with_valid_input_13", u 3 @ n 2 @ u 2, [4; 5])
    (*
    === test14 ===
    File1   File2
    a       e
    b       w
    c       x
    d       y
    e       z
    *)
  ; ("test_parse_directives_with_valid_input_14", o 4 @ u 1 @ n 4, [1; 2; 3; 4; 5])
    (*
    === test15 ===
    File1   File2
    a       a
    b       x
    c
    d
    e
    *)
  ; ("test_parse_directives_with_valid_input_15", u 1 @ o 4 @ n 1, [2])
    (*
    === test16 ===
    File1   File2
    a       x
    b       e
    c
    d
    e
    *)
  ; ("test_parse_directives_with_valid_input_16", o 4 @ n 1 @ u 1, [1])
    (*
     === test17 ===
     File1      File2
     a          x
     v          b
     b          c
     c
    *)
  ; ("test_parse_directives_with_valid_input_17", o 2 @ n 1 @ u 2, [1]) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let test_parse_unix_diff_with_valid_input =
  let create_test input expected _ =
    let found = FileDiff.parse_unix_diff input in
    let pp_diff fmt (expected, actual) =
      let expected_str = Format.asprintf "%a" (Pp.seq ~sep:", " Format.pp_print_int) expected in
      let actual_str = Format.asprintf "%a" (Pp.seq ~sep:", " Format.pp_print_int) actual in
      Format.fprintf fmt "Expected: '%s', found: '%s'" expected_str actual_str
    in
    assert_equal ~cmp:(List.equal Int.equal) ~pp_diff expected found
  in
  [("test_parse_unix_diff_1", "OONUU", [1]); ("test_parse_unix_diff_2", "UOONUONN", [2; 4; 5])]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let tests =
  "filediff"
  >::: test_unixdiff_process_raw_directives_with_valid_input
       @ test_unixdiff_process_raw_directives_with_invalid_input @ test_unixdiff_pp
       @ test_parse_directives_with_valid_input @ test_parse_unix_diff_with_valid_input
