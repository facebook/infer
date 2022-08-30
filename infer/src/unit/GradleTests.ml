(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
open Gradle

let javac_data_eq {files= f1; opts= o1} {files= f2; opts= o2} =
  let string_list_eq = List.equal String.equal in
  string_list_eq f1 f2 && string_list_eq o1 o2


let test_parse line files opts =
  let p = String.concat ~sep:";" in
  let res = parse_gradle_line ~kotlin:true ~line in
  assert_equal ~cmp:javac_data_eq
    ~msg:
      (Printf.sprintf "f:[%s] <> [%s] || o:[%s] <> [%s]" (p res.files) (p files) (p res.opts)
         (p opts) )
    res {files; opts}


let tests_wrapper _test_ctxt =
  let tmpjava = Filename.temp_file "" ".java" in
  let tmpjavanoexist = "foo" ^/ tmpjava in
  let tmpnojava = Filename.temp_file "" "" in
  test_parse "" [] [""] ;
  test_parse ("-opt1 " ^ tmpjava) [tmpjava] ["-opt1"] ;
  test_parse ("-opt1 optval1 " ^ tmpjava) [tmpjava] ["-opt1"; "optval1"] ;
  test_parse
    ("-opt1 optval1 " ^ tmpjava ^ " " ^ tmpjavanoexist)
    [tmpjava ^ " " ^ tmpjavanoexist]
    ["-opt1"; "optval1"] ;
  test_parse ("-opt1 opt val1 " ^ tmpjava) [tmpjava] ["-opt1"; "opt val1"] ;
  test_parse ("-opt1 optval1 " ^ tmpjavanoexist) [tmpjavanoexist] ["-opt1"; "optval1"] ;
  test_parse "undef1 undef2" [] ["undef1"; "undef2"] ;
  test_parse
    ("-o " ^ tmpjava ^ " cls.class @" ^ tmpnojava)
    [tmpjava]
    ["-o"; "cls.class"; "@" ^ tmpnojava] ;
  test_parse ("-opt1 optval1 " ^ tmpjava ^ " cls.class") [tmpjava] ["-opt1"; "optval1 cls.class"] ;
  test_parse ("cls.class @" ^ tmpnojava ^ " b.txt") [] ["@" ^ tmpnojava ^ " b.txt"; "cls.class"] ;
  test_parse ("cls.class @" ^ tmpnojava ^ " @b.txt") [] ["@" ^ tmpnojava ^ " @b.txt"; "cls.class"] ;
  let rec biglist acc n l = if Int.equal n 0 then acc else biglist (l @ acc) (n - 1) l in
  let opts = biglist [] 100 ["-opt1"; "optval1"] in
  test_parse (String.concat ~sep:" " @@ (tmpjava :: opts)) [tmpjava] opts ;
  test_parse
    ("-d classes/java/main -s java/main " ^ tmpjava)
    [tmpjava]
    ["-d"; "classes/java/main"; "-s"; "java/main"] ;
  test_parse "-XDuseUnsharedTable=true -classpath '' -Xmaxerrs 1000" []
    ["-XDuseUnsharedTable=true"; "-classpath"; "''"; "-Xmaxerrs"; "1000"] ;
  test_parse "-XDuseUnsharedTable=true -classpath foo -Xmaxerrs 1000" []
    ["-XDuseUnsharedTable=true"; "-classpath"; "foo"; "-Xmaxerrs"; "1000"] ;
  let tmpkotlin = Filename.temp_file "" ".kt" in
  test_parse ("-opt1 " ^ tmpkotlin) [tmpkotlin] ["-opt1"] ;
  ()


let tests = "gradle_integration_suite" >:: tests_wrapper
