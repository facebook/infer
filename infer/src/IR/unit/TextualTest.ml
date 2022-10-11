(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual
module F = Format

let parse_module text =
  let source = SourceFile.create "dummy.sil" in
  match TextualParser.parse_string source text with
  | Ok m ->
      m
  | Error es ->
      List.iter es ~f:(fun e -> F.printf "%a" (TextualParser.pp_error source) e) ;
      raise (Failure "Couldn't parse a module")


let%test_module "parsing" =
  ( module struct
    let text =
      {|
       attribute source_language = "hack"
       attribute source_file = "original.hack"

       attribute source_language = "java" // Won't have an effect

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
        line 2, column 7: source_language = "hack"
        line 3, column 7: source_file = "original.hack"
        line 5, column 7: source_language = "java" |}] ;
      let lang = Option.value_exn (Module.lang module_) in
      F.printf "%s" (Lang.to_string lang) ;
      [%expect {| hack |}]

    let%expect_test _ =
      let module_ = parse_module text in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
         attribute source_language = "hack"

         attribute source_file = "original.hack"

         attribute source_language = "java"

         define nothing() : void {
           #node0:
               ret null

         } |}]

    let text =
      {|
       attribute source_language = "hack"

       declare HackMixed.foo(*HackMixed, int): int

       define foo(x: *HackMixed): int {
       #b0:
         n0:*HackMixed = load &x
         ret n0.HackMixed.foo(42)
       }
       |}


    let%expect_test _ =
      let m = parse_module text in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        attribute source_language = "hack"

        declare HackMixed.foo(*HackMixed, int) : int

        define foo(x: *HackMixed) : int {
          #b0:
              n0:*HackMixed = load &x
              ret n0.HackMixed.foo(42)

        } |}]

    let text =
      {|
       type A = {f1: int; f2: int}
       type B {f3: bool}
       type C extends A, B {f4: bool}
      |}


    let%expect_test _ =
      let m = parse_module text in
      F.printf "%a" Module.pp m ;
      [%expect
        {|
        type A = {f1: int; f2: int}

        type B = {f3: bool}

        type C extends A, B = {f4: bool} |}]
  end )

let%test_module "procnames" =
  ( module struct
    let%expect_test _ =
      let toplevel_proc =
        ProcDecl.
          { qualified_name=
              { enclosing_class= TopLevel
              ; name= {value= "toplevel"; loc= Location.known ~line:0 ~col:0} }
          ; formals_types= []
          ; result_type= Typ.Void }
      in
      let as_java = ProcDecl.to_sil Lang.Java toplevel_proc in
      let as_hack = ProcDecl.to_sil Lang.Hack toplevel_proc in
      F.printf "%a@\n" Procname.pp as_java ;
      F.printf "%a@\n" Procname.pp as_hack ;
      [%expect {|
        void $TOPLEVEL$CLASS$.toplevel()
        toplevel |}]
  end )

let%test_module "to_sil" =
  ( module struct
    let%expect_test _ =
      let no_lang = {|define nothing() : void { #node: ret null }|} in
      let m = parse_module no_lang in
      try Module.to_sil m |> ignore
      with ToSilTransformationError pp_msg ->
        pp_msg F.std_formatter () ;
        [%expect {| Missing or unsupported source_language attribute |}]
  end )

let%test_module "remove_internal_calls transformation" =
  ( module struct
    let input_text =
      {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n3 = __sil_mult_int(g3(n0), m(g1(n0), g2(n1)))
              n4 = m(n0, g3(n1))
              jmp lab1(g1(n3), g3(n0)), lab2(g2(n3), g3(n0))
          #lab1(n6: int, n7: int):
              n8 = __sil_mult_int(n6, n7)
              jmp lab
          #lab2(n10: int, n11: int):
              ret g3(m(n10, n11))
          #lab:
              throw g1(n8)
        }

        define empty() : void {
          #entry:
              ret null
        }|}


    let%expect_test _ =
      let module_ = parse_module input_text |> Transformation.remove_internal_calls in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n12 = g3(n0)
              n13 = g1(n0)
              n14 = g2(n1)
              n15 = m(n13, n14)
              n3 = __sil_mult_int(n12, n15)
              n16 = g3(n1)
              n4 = m(n0, n16)
              n17 = g1(n3)
              n18 = g3(n0)
              n19 = g2(n3)
              n20 = g3(n0)
              jmp lab1(n17, n18), lab2(n19, n20)

          #lab1(n6: int, n7: int):
              n8 = __sil_mult_int(n6, n7)
              jmp lab

          #lab2(n10: int, n11: int):
              n21 = m(n10, n11)
              n22 = g3(n21)
              ret n22

          #lab:
              n23 = g1(n8)
              throw n23

        }

        define empty() : void {
          #entry:
              ret null

        } |}]
  end )

let%test_module "let_propagation transformation" =
  ( module struct
    let input_text =
      {|

        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              n3 = __sil_mult_int(n0, n1)
              n4 = __sil_neg(n3, n0)
              jmp lab(n4)
          #lab(n5: int):
              n6 = __sil_neg(n1)
              n7 = __sil_plusa(n6, n3)
              n8 = 42 // dead
              ret n7
        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> Transformation.let_propagation in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              jmp lab(__sil_neg(__sil_mult_int(n0, n1), n0))

          #lab(n5: int):
              ret __sil_plusa(__sil_neg(n1), __sil_mult_int(n0, n1))

        } |}]
  end )

let%test_module "out-of-ssa transformation" =
  ( module struct
    let input_text =
      {|
          define f(x: int, y: int) : int {
            #entry:
                n0:int = load &x
                n1:int = load &y
                jmp lab1(n0, n1), lab3(n1, __sil_mult_int(n1, n0))

            #lab1(n2: int, n3: int):
                jmp lab2(n3, n2)

            #lab2(n4: int, n5: int):
                ret __sil_plusa(n4, n5)

            #lab3(n6: int, n7: int):
                jmp lab2(n6, n7)

        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> Transformation.out_of_ssa in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
          define f(x: int, y: int) : int {
            #entry:
                n0:int = load &x
                n1:int = load &y
                store &__SSA2 <- n0:int
                store &__SSA3 <- n1:int
                store &__SSA6 <- n1:int
                store &__SSA7 <- __sil_mult_int(n1, n0):int
                jmp lab1, lab3

            #lab1:
                n2:int = load &__SSA2
                n3:int = load &__SSA3
                store &__SSA4 <- n3:int
                store &__SSA5 <- n2:int
                jmp lab2

            #lab2:
                n4:int = load &__SSA4
                n5:int = load &__SSA5
                ret __sil_plusa(n4, n5)

            #lab3:
                n6:int = load &__SSA6
                n7:int = load &__SSA7
                store &__SSA4 <- n6:int
                store &__SSA5 <- n7:int
                jmp lab2

          } |}]
  end )
