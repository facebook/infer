(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils
open Process

let () =
  assert_true "string_ends_with" (string_ends_with "foo" "o") ;
  assert_true "string_ends_with" (string_ends_with "foo" "") ;
  assert_false "string_ends_with" (string_ends_with "foo" "f")


let () =
  assert_true "string_starts_with" (string_starts_with "foo" "f") ;
  assert_true "string_starts_with" (string_starts_with "foo" "") ;
  assert_false "string_starts_with" (string_starts_with "foo" "o")


let () =
  assert_equal "string_split" (string_split ' ' "foo bla") ["foo"; "bla"] ;
  assert_equal "string_split" (string_split ' ' "foo") ["foo"] ;
  assert_equal "string_split" (string_split ' ' "") [""] ;
  assert_equal "string_split" (string_split ' ' " ") [""; ""] ;
  assert_equal "string_split" (string_split ' ' " foo bla ") [""; "foo"; "bla"; ""]


let () =
  assert_true "list_ends_with" (list_ends_with [1; 2; 3] [3]) ;
  assert_false "list_ends_with" (list_ends_with [1; 2; 3] [2])


let () =
  assert_true "list_starts_with" (list_starts_with [1; 2; 3] [1]) ;
  assert_false "list_starts_with" (list_starts_with [1; 2; 3] [2])


let () =
  let pid, ic =
    Process.fork (fun oc ->
        output_string oc "foo\nbar\n" ;
        true )
  in
  let lines = stream_to_list (line_stream_of_channel ic) in
  assert_true "line_stream_of_channel::wait" (Process.wait pid) ;
  assert_equal "line_stream_of_channel::result" lines ["foo"; "bar"]


let () =
  let lines = stream_to_list (stream_append (Stream.of_list ["foo"]) (Stream.of_list ["bar"])) in
  assert_equal "line_stream_of_channel::result" lines ["foo"; "bar"]


let () =
  let count = ref 0 in
  let f _ = incr count in
  let g = make_cached f in
  List.iter g [1; 2; 1; 3; 1] ;
  assert_equal "make_cached" !count 3


let () =
  let s = DisjointSet.create () in
  assert_equal "DisjointSet" (DisjointSet.find s 1) 1 ;
  assert_equal "DisjointSet" (DisjointSet.find s 2) 2 ;
  DisjointSet.union s 1 2 ;
  DisjointSet.union s 1 3 ;
  DisjointSet.union s 2 4 ;
  DisjointSet.union s 5 6 ;
  assert_equal "DisjointSet" (DisjointSet.find s 1) (DisjointSet.find s 2) ;
  assert_equal "DisjointSet" (DisjointSet.find s 1) (DisjointSet.find s 3) ;
  assert_equal "DisjointSet" (DisjointSet.find s 1) (DisjointSet.find s 4) ;
  assert_equal "DisjointSet" (DisjointSet.find s 1) (DisjointSet.find s 4) ;
  assert_equal "DisjointSet" (DisjointSet.find s 5) (DisjointSet.find s 6) ;
  assert_true "DisjointSet" (DisjointSet.find s 1 <> DisjointSet.find s 6) ;
  DisjointSet.union s 3 6 ;
  assert_true "DisjointSet" (DisjointSet.find s 1 = DisjointSet.find s 6) ;
  assert_equal "DisjointSet" (DisjointSet.find s 5) (DisjointSet.find s 6) ;
  let l = ref [] in
  DisjointSet.iter s (fun x y -> l := (y, x) :: !l) ;
  assert_equal "DisjointSetFinal" (List.sort compare !l |> List.map snd) [1; 2; 3; 4; 5; 6]


let () =
  let take13 = List.map (fun (x, y, z) -> (x, z)) in
  assert_equal "fix_arg_spec"
    (fix_arg_spec [] "bla" |> take13)
    [("-h", "     Display this list of options."); ("-help", "  "); ("--help", " ")] ;
  assert_equal "fix_arg_spec"
    (fix_arg_spec [("--foo", Arg.String ignore, "FOO fooling")] "bla" |> take13)
    [ ("--foo", "FOO fooling")
    ; ("-h", "       Display this list of options.")
    ; ("-help", "    ")
    ; ("--help", "   ") ]
