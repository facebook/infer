(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open Textual
open TextualTestHelpers

let text =
  {|
       .source_language = "hack"
       .source_file = "original.hack"

       .source_language = "java" // Won't have an effect

       define nothing(): void {
         #node0:
           ret null
       }
       |}


let%expect_test _ =
  let module_ = parse_module text in
  let attrs = module_.attrs in
  F.printf "%a" (Pp.seq ~sep:"\n" Attr.pp_with_loc) attrs ;
  [%expect
    {|
        .source_language = "hack" @[2:7]
        .source_file = "original.hack" @[3:7]
        .source_language = "java" @[5:7] |}] ;
  let lang = Option.value_exn (Module.lang module_) in
  F.printf "%s" (Lang.to_string lang) ;
  [%expect {| hack |}]


let show module_ = F.printf "%a" (Module.pp ~show_location:true) module_

let%expect_test _ =
  let module_ = parse_module text in
  show module_ ;
  [%expect
    {|
        .source_language = "hack" @[2:7]

        .source_file = "original.hack" @[3:7]

        .source_language = "java" @[5:7]

        define nothing() : void {
          #node0: @[8:9]
              ret null @[9:11]

        } @[10:8] |}]


let text =
  {|
       .source_language = "hack"

       declare HackMixed.foo(*HackMixed, int): int

       define foo(x: *HackMixed): int {
       #b0:
         n0:*HackMixed = load &x
         ret n0.HackMixed.foo(42)
       }
       |}


let%expect_test _ =
  let m = parse_module text in
  show m ;
  [%expect
    {|
        .source_language = "hack" @[2:7]

        declare HackMixed.foo(*HackMixed, int) : int

        define foo(x: *HackMixed) : int {
          #b0: @[7:7]
              n0:*HackMixed = load &x @[8:9]
              ret n0.HackMixed.foo(42) @[9:9]

        } @[10:8] |}]


let text =
  {|
       type A = {f1: int; f2: int}
       type B {f3: bool}
       type C extends A, B {f4: bool}
      |}


let%expect_test _ =
  let m = parse_module text in
  show m ;
  [%expect
    {|
        type A = {f1: int; f2: int}

        type B = {f3: bool}

        type C extends A, B = {f4: bool} |}]


let%expect_test "standalone ellipsis are OK" =
  let m =
    parse_module
      {|
           .source_language = "hack"
           declare todo(...): *Mixed
           declare foo(): *Mixed
           declare bar(int, float): *Mixed
           |}
  in
  show m ;
  [%expect
    {|
    .source_language = "hack" @[2:11]

    declare todo(...) : *Mixed

    declare foo() : *Mixed

    declare bar(int, float) : *Mixed |}]


let%expect_test "mixing regular formals and ellipsis is BAD" =
  parse_module_print_errors
    {|
             .source_language = "hack"
             declare foo(int, ...) : *HackMixed
             |} ;
  [%expect {| dummy.sil, line 3, column 30: SIL syntax error: unexpected token ... |}]


let%expect_test "numbers lexing" =
  let text =
    {|
         .source_language = "hack"
         define foo() : int {
         #entry:
           n0 = 12
           n1 = -42
           n2 = 1e1
           n3 = 2.
           n4 = 3.14
           n5 = 6.022137e+23
           n2 = -1e1
           n3 = -2.
           n4 = -3.14
           n5 = -6.022137e+23
           ret n1
         }
         |}
  in
  let m = parse_module text in
  show m ;
  [%expect
    {|
        .source_language = "hack" @[2:9]

        define foo() : int {
          #entry: @[4:9]
              n0 = 12 @[5:11]
              n1 = -42 @[6:11]
              n2 = 10. @[7:11]
              n3 = 2. @[8:11]
              n4 = 3.14 @[9:11]
              n5 = 6.022137e+23 @[10:11]
              n2 = -10. @[11:11]
              n3 = -2. @[12:11]
              n4 = -3.14 @[13:11]
              n5 = -6.022137e+23 @[14:11]
              ret n1 @[15:11]

        } @[16:10] |}]


let text =
  {|
       define f(declare: int) : int {
       #type:
       jmp type

       }
       |}


let%expect_test "keywords as idents" =
  let module_ = parse_module text |> TextualTransform.out_of_ssa in
  show module_ ;
  [%expect
    {|
          define f(declare: int) : int {
            #type: @[3:7]
                jmp type @[4:7]

          } @[6:8] |}]


let%expect_test "overloaded functions" =
  let text =
    {|
     .source_language = "hack"

     define f(a: int) : void {#b0: ret null }
     define f(a: int, b: bool) : void {#b0: ret null}

     define g(a: int, b: bool) : void {
     #b0:
       n0:int = load &a
       n1:bool = load &b
       n2 = f(n0)
       n3 = f(n0, n1)
       ret null
     }
     |}
  in
  let m = parse_module text in
  show m ;
  [%expect
    {|
    .source_language = "hack" @[2:5]

    define f(a: int) : void {
      #b0: @[4:30]
          ret null @[4:35]

    } @[4:45]

    define f(a: int, b: bool) : void {
      #b0: @[5:39]
          ret null @[5:44]

    } @[5:53]

    define g(a: int, b: bool) : void {
      #b0: @[8:5]
          n0:int = load &a @[9:7]
          n1:bool = load &b @[10:7]
          n2 = f(n0) @[11:7]
          n3 = f(n0, n1) @[12:7]
          ret null @[13:7]

    } @[14:6] |}]
