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

let show module_ = F.printf "%a" (Module.pp ~show_location:true) module_

(* Inspired by the following Python program:

   def f(x, y, z, t):
      return (x and y) or (z and t)
*)
let python_inspired_text =
  {|
        define f(x: int, y: int, z: int, t: int) : int {
          #b0:
              n0:int = load &x
              if n0 then jmp b1 else jmp b2

          #b1:
              n2:int = load &y
              if n2 then jmp b4(n2) else jmp b2

          #b2:
              n5:int = load &z
              if n5 then jmp b5 else jmp b4(n5)

          #b5:
              n8:int = load &t
              jmp b4(n8)

          #b4(n9: int):
              ret n9

        }
        |}


let%test_module "remove_effects_in_subexprs transformation" =
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
              n4 = m([&x:int], g3([&y]))
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
        }

        type cell = { value:int; next: *cell }

        define next(l: *cell) : *cell {
          #entry:
             ret l->cell.next
        }
    |}


    let%expect_test _ =
      let lang = Lang.Hack in
      let module_, _ = parse_module input_text |> remove_effects_in_subexprs lang in
      show module_ ;
      [%expect
        {|
        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry: @[13:10]
              n0:int = load &x @[14:14]
              n1:int = load &y @[15:14]
              n12 = g3(n0) @[16:14]
              n13 = g1(n0) @[16:14]
              n14 = g2(n1) @[16:14]
              n15 = m(n13, n14) @[16:14]
              n3 = __sil_mult_int(n12, n15) @[16:14]
              n16:int = load &x @[17:14]
              n17:int = load &y @[17:14]
              n18 = g3(n17) @[17:14]
              n4 = m(n16, n18) @[17:14]
              n19 = g1(n3) @[18:14]
              n20 = g3(n0) @[18:14]
              n21 = g2(n3) @[18:14]
              n22 = g3(n0) @[18:14]
              jmp lab1(n19, n20), lab2(n21, n22) @[18:14]

          #lab1(n6: int, n7: int): @[19:10]
              n8 = __sil_mult_int(n6, n7) @[20:14]
              jmp lab @[21:14]

          #lab2(n10: int, n11: int): @[22:10]
              n23 = m(n10, n11) @[23:14]
              n24 = g3(n23) @[23:14]
              ret n24 @[23:14]

          #lab: @[24:10]
              n25 = g4(n8) @[25:14]
              throw n25 @[25:14]

        } @[26:9]

        define empty() : void {
          #entry: @[29:10]
              ret null @[30:14]

        } @[31:9]

        type cell = {value: int; next: *cell}

        define next(l: *cell) : *cell {
          #entry: @[36:10]
              n0:*cell = load &l @[37:13]
              n1:*cell = load n0.cell.next @[37:13]
              ret n1 @[37:13]

        } @[38:9] |}]
  end )


let%test_module "remove_if_terminator transformation" =
  ( module struct
    let input_text =
      {|
        define f(b1: int, b2: int, b3: int, b4: int, b5: int) : int {
          #entry:
              n1 : int = load &b1
              n2 : int = load &b2
              n3 : int = load &b3
              n4 : int = load &b4
              n5 : int = load &b5
              if n1 && n2 && n3 then jmp lab1 else jmp lab2
          #lab1:
              ret 1
          #lab2:
              if n2 || n1 && n3 then jmp lab3
              else jmp lab4
          #lab3:
              if n1 && (n2 || (n3 && (n4 || n5))) then jmp lab4 else jmp lab5
          #lab4:
              ret 2
          #lab5:
              ret 3
        }

        define g(b1: int, b2: int, b3: int) : int {
          #entry:
              n1 : int = load &b1
              n2 : int = load &b2
              n3 : int = load &b3
              if (n1 || n2) && n3 then jmp lab1 else jmp if2
          #lab1:
              ret 1
          #if2: // we test the situation where the generated label may already exists
              ret 2
        }

        define h(b1: int, b2: int, b3: int) : int {
          #entry:
              n1 : int = load &b1
              n2 : int = load &b2
              n3 : int = load &b3
              if n1 && n2 && n3 then ret 1
              else if n2 || n1 && n3 then ret 2
              else ret 3
        }

        define if_lparen_test(b1: int, b2: int) : int {
          #entry:
              n1 : int = load &b1
              n2 : int = load &b2
              if (n1 || n2) then ret 1 else jmp lab1
          #lab1:
              if (n1 && n2) then ret 1 else jmp lab2
          #lab2:
              if (n1) && n2 then ret 1 else jmp lab3
          #lab3:
              if ((n1)) && n2 then ret 1 else jmp lab4
          #lab4:
              if ((n1 && n2)) || n1 then ret 1 else jmp lab5
          #lab5:
              if ((n1 || n2)) && (n1) then ret 1 else ret 2
        }|}


    let%expect_test _ =
      let module_ = parse_module input_text |> TextualTransform.remove_if_terminator in
      show module_ ;
      [%expect
        {|
        define f(b1: int, b2: int, b3: int, b4: int, b5: int) : int {
          #entry: @[3:10]
              n1:int = load &b1 @[4:14]
              n2:int = load &b2 @[5:14]
              n3:int = load &b3 @[6:14]
              n4:int = load &b4 @[7:14]
              n5:int = load &b5 @[8:14]
              jmp lab1, if0, if1, if2 @[9:14]

          #if0: @[9:14]
              prune __sil_lnot(n1) @[9:14]
              jmp lab2 @[9:14]

          #if1: @[9:14]
              prune __sil_lnot(n2) @[9:14]
              jmp lab2 @[9:14]

          #if2: @[9:14]
              prune __sil_lnot(n3) @[9:14]
              jmp lab2 @[9:14]

          #lab1: @[10:10]
              prune n1 @[9:14]
              prune n2 @[9:14]
              prune n3 @[9:14]
              ret 1 @[11:14]

          #lab2: @[12:10]
              jmp if5, if6, if3, if4 @[13:14]

          #if5: @[13:14]
              prune n2 @[13:14]
              jmp lab3 @[13:14]

          #if6: @[13:14]
              prune n1 @[13:14]
              prune n3 @[13:14]
              jmp lab3 @[13:14]

          #if3: @[13:14]
              prune __sil_lnot(n2) @[13:14]
              prune __sil_lnot(n1) @[13:14]
              jmp lab4 @[13:14]

          #if4: @[13:14]
              prune __sil_lnot(n2) @[13:14]
              prune __sil_lnot(n3) @[13:14]
              jmp lab4 @[13:14]

          #lab3: @[15:10]
              jmp if10, if11, if12, if7, if8, if9 @[16:14]

          #if10: @[16:14]
              prune n1 @[16:14]
              prune n2 @[16:14]
              jmp lab4 @[16:14]

          #if11: @[16:14]
              prune n1 @[16:14]
              prune n3 @[16:14]
              prune n4 @[16:14]
              jmp lab4 @[16:14]

          #if12: @[16:14]
              prune n1 @[16:14]
              prune n3 @[16:14]
              prune n5 @[16:14]
              jmp lab4 @[16:14]

          #if7: @[16:14]
              prune __sil_lnot(n1) @[16:14]
              jmp lab5 @[16:14]

          #if8: @[16:14]
              prune __sil_lnot(n2) @[16:14]
              prune __sil_lnot(n3) @[16:14]
              jmp lab5 @[16:14]

          #if9: @[16:14]
              prune __sil_lnot(n2) @[16:14]
              prune __sil_lnot(n4) @[16:14]
              prune __sil_lnot(n5) @[16:14]
              jmp lab5 @[16:14]

          #lab4: @[17:10]
              ret 2 @[18:14]

          #lab5: @[19:10]
              ret 3 @[20:14]

        } @[21:9]

        define g(b1: int, b2: int, b3: int) : int {
          #entry: @[24:10]
              n1:int = load &b1 @[25:14]
              n2:int = load &b2 @[26:14]
              n3:int = load &b3 @[27:14]
              jmp if3, if4, if0, if1 @[28:14]

          #if3: @[28:14]
              prune n1 @[28:14]
              prune n3 @[28:14]
              jmp lab1 @[28:14]

          #if4: @[28:14]
              prune n2 @[28:14]
              prune n3 @[28:14]
              jmp lab1 @[28:14]

          #if0: @[28:14]
              prune __sil_lnot(n1) @[28:14]
              prune __sil_lnot(n2) @[28:14]
              jmp if2 @[28:14]

          #if1: @[28:14]
              prune __sil_lnot(n3) @[28:14]
              jmp if2 @[28:14]

          #lab1: @[29:10]
              ret 1 @[30:14]

          #if2: @[31:10]
              ret 2 @[32:14]

        } @[33:9]

        define h(b1: int, b2: int, b3: int) : int {
          #entry: @[36:10]
              n1:int = load &b1 @[37:14]
              n2:int = load &b2 @[38:14]
              n3:int = load &b3 @[39:14]
              jmp if7, if4, if5, if6 @[40:14]

          #if7: @[40:14]
              prune n1 @[40:14]
              prune n2 @[40:14]
              prune n3 @[40:14]
              ret 1 @[40:14]

          #if4: @[40:14]
              prune __sil_lnot(n1) @[40:14]
              jmp if2, if3, if0, if1 @[40:14]

          #if5: @[40:14]
              prune __sil_lnot(n2) @[40:14]
              jmp if2, if3, if0, if1 @[40:14]

          #if6: @[40:14]
              prune __sil_lnot(n3) @[40:14]
              jmp if2, if3, if0, if1 @[40:14]

          #if2: @[40:14]
              prune n2 @[40:14]
              ret 2 @[40:14]

          #if3: @[40:14]
              prune n1 @[40:14]
              prune n3 @[40:14]
              ret 2 @[40:14]

          #if0: @[40:14]
              prune __sil_lnot(n2) @[40:14]
              prune __sil_lnot(n1) @[40:14]
              ret 3 @[40:14]

          #if1: @[40:14]
              prune __sil_lnot(n2) @[40:14]
              prune __sil_lnot(n3) @[40:14]
              ret 3 @[40:14]

        } @[43:9]

        define if_lparen_test(b1: int, b2: int) : int {
          #entry: @[46:10]
              n1:int = load &b1 @[47:14]
              n2:int = load &b2 @[48:14]
              jmp if0, if1, lab1 @[49:14]

          #if0: @[49:14]
              prune n1 @[49:14]
              ret 1 @[49:14]

          #if1: @[49:14]
              prune n2 @[49:14]
              ret 1 @[49:14]

          #lab1: @[50:10]
              prune __sil_lnot(n1) @[49:14]
              prune __sil_lnot(n2) @[49:14]
              jmp if4, if2, if3 @[51:14]

          #if4: @[51:14]
              prune n1 @[51:14]
              prune n2 @[51:14]
              ret 1 @[51:14]

          #if2: @[51:14]
              prune __sil_lnot(n1) @[51:14]
              jmp lab2 @[51:14]

          #if3: @[51:14]
              prune __sil_lnot(n2) @[51:14]
              jmp lab2 @[51:14]

          #lab2: @[52:10]
              jmp if7, if5, if6 @[53:14]

          #if7: @[53:14]
              prune n1 @[53:14]
              prune n2 @[53:14]
              ret 1 @[53:14]

          #if5: @[53:14]
              prune __sil_lnot(n1) @[53:14]
              jmp lab3 @[53:14]

          #if6: @[53:14]
              prune __sil_lnot(n2) @[53:14]
              jmp lab3 @[53:14]

          #lab3: @[54:10]
              jmp if10, if8, if9 @[55:14]

          #if10: @[55:14]
              prune n1 @[55:14]
              prune n2 @[55:14]
              ret 1 @[55:14]

          #if8: @[55:14]
              prune __sil_lnot(n1) @[55:14]
              jmp lab4 @[55:14]

          #if9: @[55:14]
              prune __sil_lnot(n2) @[55:14]
              jmp lab4 @[55:14]

          #lab4: @[56:10]
              jmp if13, if14, if11, if12 @[57:14]

          #if13: @[57:14]
              prune n1 @[57:14]
              prune n2 @[57:14]
              ret 1 @[57:14]

          #if14: @[57:14]
              prune n1 @[57:14]
              ret 1 @[57:14]

          #if11: @[57:14]
              prune __sil_lnot(n1) @[57:14]
              prune __sil_lnot(n1) @[57:14]
              jmp lab5 @[57:14]

          #if12: @[57:14]
              prune __sil_lnot(n2) @[57:14]
              prune __sil_lnot(n1) @[57:14]
              jmp lab5 @[57:14]

          #lab5: @[58:10]
              jmp if17, if18, if15, if16 @[59:14]

          #if17: @[59:14]
              prune n1 @[59:14]
              prune n1 @[59:14]
              ret 1 @[59:14]

          #if18: @[59:14]
              prune n2 @[59:14]
              prune n1 @[59:14]
              ret 1 @[59:14]

          #if15: @[59:14]
              prune __sil_lnot(n1) @[59:14]
              prune __sil_lnot(n2) @[59:14]
              ret 2 @[59:14]

          #if16: @[59:14]
              prune __sil_lnot(n1) @[59:14]
              ret 2 @[59:14]

        } @[60:9] |}]


    let%expect_test _ =
      let module_ = parse_module python_inspired_text |> TextualTransform.remove_if_terminator in
      show module_ ;
      [%expect
        {|
          define f(x: int, y: int, z: int, t: int) : int {
            #b0: @[3:10]
                n0:int = load &x @[4:14]
                jmp b1, if0 @[5:14]

            #if0: @[5:14]
                prune __sil_lnot(n0) @[5:14]
                jmp b2 @[5:14]

            #b1: @[7:10]
                prune n0 @[5:14]
                n2:int = load &y @[8:14]
                jmp if2, if1 @[9:14]

            #if2: @[9:14]
                prune n2 @[9:14]
                jmp b4(n2) @[9:14]

            #if1: @[9:14]
                prune __sil_lnot(n2) @[9:14]
                jmp b2 @[9:14]

            #b2: @[11:10]
                n5:int = load &z @[12:14]
                jmp b5, if3 @[13:14]

            #if3: @[13:14]
                prune __sil_lnot(n5) @[13:14]
                jmp b4(n5) @[13:14]

            #b5: @[15:10]
                prune n5 @[13:14]
                n8:int = load &t @[16:14]
                jmp b4(n8) @[17:14]

            #b4(n9: int): @[19:10]
                ret n9 @[20:14]

          } @[22:9] |}]
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
      show module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry: @[4:10]
              n0:int = load &x @[5:14]
              n1:int = load &y @[6:14]
              jmp lab(__sil_minusa(__sil_mult_int(n0, n1), n0)) @[9:14]

          #lab(n5: int): @[10:10]
              ret __sil_plusa(__sil_neg(n1), __sil_mult_int(n0, n1)) @[14:14]

        } @[15:9] |}]
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
      show module_ ;
      [%expect
        {|
        define f(x: int, y: int) : int {
          #entry: @[3:12]
              n0:int = load &x @[4:16]
              n1:int = load &y @[5:16]
              store &__SSA2 <- n0:int @?
              store &__SSA3 <- n1:int @?
              store &__SSA6 <- n1:int @?
              store &__SSA7 <- __sil_mult_int(n1, n0):int @?
              jmp lab1, lab3 @[6:16]

          #lab1: @[8:12]
              n2:int = load &__SSA2 @?
              n3:int = load &__SSA3 @?
              store &__SSA4 <- n3:int @?
              store &__SSA5 <- n2:int @?
              jmp lab2 @[9:16]

          #lab2: @[11:12]
              n4:int = load &__SSA4 @?
              n5:int = load &__SSA5 @?
              ret __sil_plusa(n4, n5) @[12:16]

          #lab3: @[14:12]
              n6:int = load &__SSA6 @?
              n7:int = load &__SSA7 @?
              store &__SSA4 <- n6:int @?
              store &__SSA5 <- n7:int @?
              jmp lab2 @[15:16]

        } @[17:9] |}]


    let%expect_test _ =
      let module_ =
        parse_module python_inspired_text |> TextualTransform.remove_if_terminator
        |> TextualTransform.out_of_ssa
      in
      show module_ ;
      [%expect
        {|
        define f(x: int, y: int, z: int, t: int) : int {
          #b0: @[3:10]
              n0:int = load &x @[4:14]
              jmp b1, if0 @[5:14]

          #if0: @[5:14]
              prune __sil_lnot(n0) @[5:14]
              jmp b2 @[5:14]

          #b1: @[7:10]
              prune n0 @[5:14]
              n2:int = load &y @[8:14]
              jmp if2, if1 @[9:14]

          #if2: @[9:14]
              prune n2 @[9:14]
              store &__SSA9 <- n2:int @?
              jmp b4 @[9:14]

          #if1: @[9:14]
              prune __sil_lnot(n2) @[9:14]
              jmp b2 @[9:14]

          #b2: @[11:10]
              n5:int = load &z @[12:14]
              jmp b5, if3 @[13:14]

          #if3: @[13:14]
              prune __sil_lnot(n5) @[13:14]
              store &__SSA9 <- n5:int @?
              jmp b4 @[13:14]

          #b5: @[15:10]
              prune n5 @[13:14]
              n8:int = load &t @[16:14]
              store &__SSA9 <- n8:int @?
              jmp b4 @[17:14]

          #b4: @[19:10]
              n9:int = load &__SSA9 @?
              ret n9 @[20:14]

        } @[22:9] |}]
  end )


let%expect_test "closures" =
  let source =
    {|
        .source_language = "hack"

        define C.add(x: int, y: int, z: int, u: float, v: string) : int {
          #entry:
            ret __sil_plusa(x, __sil_plusa(y, z))
        }

        define D.foo(x: int) : void {
          local y: *HackMixed
          #entry:
            n0 = fun (p1, p2, p3) -> C.add(x, 1, p1, p2, p3)
            store &y <- n0: *HackMixed
            n1 = y(x, 1.0, null)
            n2 = n0(x, 2.0, null)
            ret __sil_plusa(n1, n2)
        }
    |}
  in
  let module_ = parse_module source in
  show module_ ;
  [%expect
    {|
      .source_language = "hack" @[2:8]

      define C.add(x: int, y: int, z: int, u: float, v: string) : int {
        #entry: @[5:10]
            ret __sil_plusa([&x:int], __sil_plusa([&y:int], [&z:int])) @[6:12]

      } @[7:9]

      define D.foo(x: int) : void {
        local y: *HackMixed
        #entry: @[11:10]
            n0 = fun (p1, p2, p3) -> C.add([&x:int], 1, p1, p2, p3) @[12:12]
            store &y <- n0:*HackMixed @[13:12]
            n1 = [&y:*HackMixed]([&x:int], 1., null) @[14:12]
            n2 = n0([&x:int], 2., null) @[15:12]
            ret __sil_plusa(n1, n2) @[16:12]

      } @[17:9] |}] ;
  let module_, _ = remove_effects_in_subexprs Lang.Hack module_ in
  show module_ ;
  [%expect
    {|
      .source_language = "hack" @[2:8]

      type .final PyClosure<dummy:0> = {x: int; y: int}

      define .closure_wrapper PyClosure<dummy:0>.call(__this: *PyClosure<dummy:0>, p1: int, p2: float, p3: string) : int {
        #entry: @[12:12]
            n0:*PyClosure<dummy:0> = load &__this @[12:12]
            n1:int = load n0.?.x @[12:12]
            n2:*PyClosure<dummy:0> = load &__this @[12:12]
            n3:int = load n2.?.y @[12:12]
            n4:int = load &p1 @[12:12]
            n5:float = load &p2 @[12:12]
            n6:string = load &p3 @[12:12]
            n7 = C.add(n1, n3, n4, n5, n6) @[12:12]
            ret n7 @[12:12]

      } @[12:12]

      define C.add(x: int, y: int, z: int, u: float, v: string) : int {
        #entry: @[5:10]
            n0:int = load &x @[6:12]
            n1:int = load &y @[6:12]
            n2:int = load &z @[6:12]
            ret __sil_plusa(n0, __sil_plusa(n1, n2)) @[6:12]

      } @[7:9]

      define D.foo(x: int) : void {
        local y: *HackMixed
        #entry: @[11:10]
            n3:int = load &x @[12:12]
            n4 = __sil_allocate(<PyClosure<dummy:0>>) @[12:12]
            store n4.?.x <- n3:int @[12:12]
            store n4.?.y <- 1:int @[12:12]
            n0 = n4 @[12:12]
            store &y <- n0:*HackMixed @[13:12]
            n6:*HackMixed = load &y @[14:12]
            n7:int = load &x @[14:12]
            n8 = n6.?.call(n7, 1., null) @[14:12]
            n1 = n8 @[14:12]
            n9:int = load &x @[15:12]
            n10 = n0.?.call(n9, 2., null) @[15:12]
            n2 = n10 @[15:12]
            ret __sil_plusa(n1, n2) @[16:12]

      } @[17:9] |}] ;
  type_check module_ ;
  [%expect {|
      verification succeeded |}]
