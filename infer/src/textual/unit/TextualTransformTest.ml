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

let%test_module "remove_internal_calls transformation" =
  ( module struct
    let input_text =
      {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

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
              throw g4(n8)
        }

        define empty() : void {
          #entry:
              ret null
        }|}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.remove_internal_calls in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

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
              n23 = g4(n8)
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
              n4 = __sil_minusa(n3, n0)
              jmp lab(n4)
          #lab(n5: int):
              n6 = __sil_neg(n1)
              n7 = __sil_plusa(n6, n3)
              n8 = 42 // dead
              ret n7
        } |}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.let_propagation in
      F.printf "%a" Module.pp module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry:
              n0:int = load &x
              n1:int = load &y
              jmp lab(__sil_minusa(__sil_mult_int(n0, n1), n0))

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
      let module_ = parse_module input_text |> TextualTransform.out_of_ssa in
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
