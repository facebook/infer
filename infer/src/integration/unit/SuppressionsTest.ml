(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Map = String.Map

(** {2 parsing} *)

let%test "parsing empty file" = Map.is_empty @@ Suppressions.parse_lines []

let%test "parsing empty string" = Map.is_empty @@ Suppressions.parse_lines [""]

let%test "parsing non-matching line" = Map.is_empty @@ Suppressions.parse_lines ["1+1 #hello"]

let%test "parsing matching line" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1"])
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Blocks [{first= 1; last= 2}])


let%test "parsing matching line inside string gotcha" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines ["const char* s = \"@infer-ignore BUFFER_OVERRUN_L1,\";"])
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Blocks [{first= 1; last= 2}])


let%test "parsing matching line no issue type" =
  Map.is_empty @@ Suppressions.parse_lines ["1+1 // @infer-ignore  "]


let%test "parsing half-matching line no issue type" =
  Map.is_empty @@ Suppressions.parse_lines ["1+1 // @infer-ignore-all"]


let%test "parsing matching line multiple issue types" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY"])
    Map.(
      empty
      |> add_exn ~key:"BUFFER_OVERRUN_L1" ~data:(Suppressions.Span.Blocks [{first= 1; last= 2}])
      |> add_exn ~key:"PULSE_UNNECESSARY_COPY" ~data:(Suppressions.Span.Blocks [{first= 1; last= 2}]) )


let%test "parsing matching line multiple noise" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       ["1+1 // @infer-ignore BUFFER_OVERRUN_L1,,,, PULSE_UNNECESSARY_COPY,,,,,,,"] )
    Map.(
      empty
      |> add_exn ~key:"BUFFER_OVERRUN_L1" ~data:(Suppressions.Span.Blocks [{first= 1; last= 2}])
      |> add_exn ~key:"PULSE_UNNECESSARY_COPY" ~data:(Suppressions.Span.Blocks [{first= 1; last= 2}]) )


let%test "multi line block" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       ["// @infer-ignore BUFFER_OVERRUN_L1"; "1+1 // @infer-ignore ,PULSE_UNNECESSARY_COPY"] )
    Map.(
      empty
      |> add_exn ~key:"BUFFER_OVERRUN_L1" ~data:(Suppressions.Span.Blocks [{first= 1; last= 3}])
      |> add_exn ~key:"PULSE_UNNECESSARY_COPY" ~data:(Suppressions.Span.Blocks [{first= 1; last= 3}]) )


let%test "multiple blocks" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       ["// @infer-ignore BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"] )
    Map.(
      empty
      |> add_exn ~key:"BUFFER_OVERRUN_L1"
           ~data:(Suppressions.Span.Blocks [{first= 1; last= 2}; {first= 3; last= 4}]) )


let%test "parsing matching line every" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines ["1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"])
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Every)


let%test "every overrides block" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       ["//@infer-ignore BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"] )
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Every)


let%test "block doesn't override every" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       ["//@infer-ignore-every BUFFER_OVERRUN_L1"; ""; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"] )
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Every)


let%test "both ignore and ignore-every on single line" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       [ "1+1 // @infer-ignore-every BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY @infer-ignore \
          DEAD_STORE" ] )
    (* PULSE_UNNECESSARY_COPY @infer-ignore DEAD_STORE not a valid issue_type / wildcard *)
    (Map.singleton "BUFFER_OVERRUN_L1" @@ Suppressions.Span.Every)


let%test "both ignore and ignore-every on single line every wins" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines
       [ "1+1 // @infer-ignore BUFFER_OVERRUN_L1,PULSE_UNNECESSARY_COPY @infer-ignore-every \
          DEAD_STORE" ] )
    (Map.singleton "DEAD_STORE" @@ Suppressions.Span.Every)


let%test "simple wildcard" =
  Map.equal Suppressions.Span.equal
    (Suppressions.parse_lines ["1+1 // @infer-ignore-every PULSE_UNNECESSARY_.*"])
    (Map.singleton "PULSE_UNNECESSARY_.*" @@ Suppressions.Span.Every)


let%test "match everything wildcard is invalid" =
  Map.is_empty @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every .*"]


let%test "syntax error wildcard" =
  Map.is_empty @@ Suppressions.parse_lines ["1+1 // @infer-ignore-every *"]


(** {2 matching} *)

let s1 = Suppressions.parse_lines ["1+1 // @infer-ignore BUFFER_OVERRUN_L1"]

let s2 =
  Suppressions.parse_lines
    ["//@infer-ignore ,PULSE_UNNECESSARY_COPY"; "1+1 // @infer-ignore BUFFER_OVERRUN_L1"]


let s_every = Suppressions.parse_lines ["1+1 // @infer-ignore-every BUFFER_OVERRUN_L1"]

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


let s_wild = Suppressions.parse_lines ["1+1 // @infer-ignore-every PULSE_UNNECESSARY_.*"]

let%test "matching suppression wildcard" =
  Suppressions.is_suppressed ~suppressions:s_wild ~issue_type:"PULSE_UNNECESSARY_COPY_ASSIGNMENT"
    ~line:1


let%test "non matching suppression wildcard" =
  not @@ Suppressions.is_suppressed ~suppressions:s_wild ~issue_type:"PULSE_UNNECESSARY" ~line:1
