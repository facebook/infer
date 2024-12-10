(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 parsing} *)

let t parse_result = Format.printf "%a" Suppressions.pp_parse_result parse_result

let%expect_test "parsing empty file" =
  t @@ Suppressions.parse_lines [] ;
  [%expect {| Empty |}]


let%expect_test "parsing empty string" =
  t @@ Suppressions.parse_lines [""] ;
  [%expect {| Empty |}]


let%expect_test "parsing non-matching line" =
  t @@ Suppressions.parse_lines ["1+1 #hello"] ;
  [%expect {| Empty |}]


let%expect_test "parsing matching line" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1"] ;
  [%expect {| BUFFER_OVERRUN_L1: Blocks 1-2 |}]


let%expect_test "parsing matching line inside string gotcha" =
  t @@ Suppressions.parse_lines ["const char* s = \"@infer-ignore BUFFER_OVERRUN_L1,\";"] ;
  [%expect
    {|
    RESULT: BUFFER_OVERRUN_L1: Blocks 1-2

    ERRORS: "; not a valid issue_type / wildcard |}]


let%expect_test "parsing matching line no issue type" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore  "] ;
  [%expect {| Empty |}]


let%expect_test "parsing half-matching line no issue type" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore-all"] ;
  [%expect {| Empty |}]


let%expect_test "parsing matching line multiple issue types" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY"] ;
  [%expect {|
    BUFFER_OVERRUN_L1: Blocks 1-2
    PULSE_UNNECESSARY_COPY: Blocks 1-2 |}]


let%expect_test "parsing matching line multiple noise" =
  t
  @@ Suppressions.parse_lines
       ["1+1 // @infer-ignore BUFFER_OVERRUN_L1,,,, PULSE_UNNECESSARY_COPY,,,,,,,"] ;
  [%expect {|
    BUFFER_OVERRUN_L1: Blocks 1-2
    PULSE_UNNECESSARY_COPY: Blocks 1-2 |}]


let%expect_test "multi line block" =
  t
  @@ Suppressions.parse_lines
       ["// @infer-ignore BUFFER_OVERRUN_L1"; "1+1 // @infer-ignore ,PULSE_UNNECESSARY_COPY"] ;
  [%expect {|
    BUFFER_OVERRUN_L1: Blocks 1-3
    PULSE_UNNECESSARY_COPY: Blocks 1-3 |}]


let%expect_test "multiple blocks" =
  t
  @@ Suppressions.parse_lines
       ["// @infer-ignore BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"] ;
  [%expect {| BUFFER_OVERRUN_L1: Blocks 1-2, 3-4 |}]


let%expect_test "parsing matching line every" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"] ;
  [%expect {| BUFFER_OVERRUN_L1: Every |}]


let%expect_test "every overrides block" =
  t
  @@ Suppressions.parse_lines
       ["//@infer-ignore BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"] ;
  [%expect {| BUFFER_OVERRUN_L1: Every |}]


let%expect_test "block doesn't override every" =
  t
  @@ Suppressions.parse_lines
       ["//@infer-ignore-every BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"] ;
  [%expect {| BUFFER_OVERRUN_L1: Every |}]


let%expect_test "both ignore and ignore-every on single line" =
  t
  @@ Suppressions.parse_lines
       [ "1+1 // @infer-ignore-every BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY @infer-ignore \
          DEAD_STORE" ] ;
  [%expect
    {|
    RESULT: BUFFER_OVERRUN_L1: Every

    ERRORS: PULSE_UNNECESSARY_COPY @infer-ignore DEAD_STORE not a valid issue_type / wildcard
    ;
            Both @infer-ignore-every and @infer-ignore found in  line 2 |}]


let%expect_test "both ignore and ignore-every on single line every wins" =
  t
  @@ Suppressions.parse_lines
       [ "1+1 // @infer-ignore BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY @infer-ignore-every \
          DEAD_STORE" ] ;
  [%expect
    {|
    RESULT: DEAD_STORE: Every

    ERRORS: Both @infer-ignore-every and @infer-ignore found in  line 2 |}]


let%expect_test "simple wildcard" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every PULSE_UNNECESSARY_.*"] ;
  [%expect {| PULSE_UNNECESSARY_.*: Every |}]


let%expect_test "match everything wildcard is invalid" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every .*"] ;
  [%expect {|
    RESULT: Empty
    ERRORS: .* not a valid issue_type / wildcard |}]


let%expect_test "syntax error wildcard" =
  t @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every *"] ;
  [%expect {|
    RESULT: Empty
    ERRORS: * not a valid issue_type / wildcard |}]


(** {2 matching} *)

let no_error (x, errors) =
  assert (List.is_empty errors) ;
  x


let s1 = Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1"] |> no_error

let s2 =
  Suppressions.parse_lines
    ["//@infer-ignore ,PULSE_UNNECESSARY_COPY"; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"]
  |> no_error


let s_every = Suppressions.parse_lines ["1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"] |> no_error

let%test "matching suppression" =
  Suppressions.is_suppressed ~suppressions:s1 ~issue_type:"BUFFER_OVERRUN_L1" ~line:1


let%test "matching suppression, next line" =
  Suppressions.is_suppressed ~suppressions:s1 ~issue_type:"BUFFER_OVERRUN_L1" ~line:2


let%test "non matching suppression" =
  not @@ Suppressions.is_suppressed ~suppressions:s1 ~issue_type:"PULSE_UNNECESSARY_COPY" ~line:1


let%test "non matching suppression line" =
  not @@ Suppressions.is_suppressed ~suppressions:s1 ~issue_type:"BUFFER_OVERRUN_L1" ~line:4


let%test "matching suppression block" =
  Suppressions.is_suppressed ~suppressions:s2 ~issue_type:"BUFFER_OVERRUN_L1" ~line:2
  && Suppressions.is_suppressed ~suppressions:s2 ~issue_type:"PULSE_UNNECESSARY_COPY" ~line:1


let%test "matching suppression every" =
  Suppressions.is_suppressed ~suppressions:s_every ~issue_type:"BUFFER_OVERRUN_L1" ~line:1


let%test "matching suppression every large line" =
  Suppressions.is_suppressed ~suppressions:s_every ~issue_type:"BUFFER_OVERRUN_L1" ~line:1000


let s_wild =
  Suppressions.parse_lines ["1+1 // @infer-ignore-every PULSE_UNNECESSARY_.*"] |> no_error


let%test "matching suppression wildcard" =
  Suppressions.is_suppressed ~suppressions:s_wild ~issue_type:"PULSE_UNNECESSARY_COPY_ASSIGNMENT"
    ~line:1


let%test "non matching suppression wildcard" =
  not @@ Suppressions.is_suppressed ~suppressions:s_wild ~issue_type:"PULSE_UNNECESSARY" ~line:1
