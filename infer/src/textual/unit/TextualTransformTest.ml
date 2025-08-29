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
       .source_language = "python"
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
       .source_language = "python"
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
        .source_language = "python" @[2:7]

        declare g1(int) : int

        declare g2(int) : int

        declare g3(int) : int

        declare g4(int) : *int

        declare m(int, int) : int

        define f(x: int, y: int) : int {
          #entry: @[14:10]
              n0:int = load &x @[15:14]
              n1:int = load &y @[16:14]
              n12 = g3(n0) @[17:14]
              n13 = g1(n0) @[17:14]
              n14 = g2(n1) @[17:14]
              n15 = m(n13, n14) @[17:14]
              n3 = __sil_mult_int(n12, n15) @[17:14]
              n16:int = load &x @[18:14]
              n17:int = load &y @[18:14]
              n18 = g3(n17) @[18:14]
              n4 = m(n16, n18) @[18:14]
              n19 = g1(n3) @[19:14]
              n20 = g3(n0) @[19:14]
              n21 = g2(n3) @[19:14]
              n22 = g3(n0) @[19:14]
              jmp lab1(n19, n20), lab2(n21, n22) @[19:14]

          #lab1(n6: int, n7: int): @[20:10]
              n8 = __sil_mult_int(n6, n7) @[21:14]
              jmp lab @[22:14]

          #lab2(n10: int, n11: int): @[23:10]
              n23 = m(n10, n11) @[24:14]
              n24 = g3(n23) @[24:14]
              ret n24 @[24:14]

          #lab: @[25:10]
              n25 = g4(n8) @[26:14]
              throw n25 @[26:14]

        } @[27:9]

        define empty() : void {
          #entry: @[30:10]
              ret null @[31:14]

        } @[32:9]

        type cell = {value: int; next: *cell}

        define next(l: *cell) : *cell {
          #entry: @[37:10]
              n0:*cell = load &l @[38:13]
              n1:*cell = load n0.cell.next @[38:13]
              ret n1 @[38:13]

        } @[39:9]
        |}]
  end )


let%test_module "remove_if_terminator transformation" =
  ( module struct
    let input_text =
      {|
        .source_language = "python"
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
      let module_ = parse_module input_text |> TextualTransform.remove_if_exp_and_terminator in
      show module_ ;
      [%expect
        {|
        .source_language = "python" @[2:8]

        define f(b1: int, b2: int, b3: int, b4: int, b5: int) : int {
          #entry: @[4:10]
              n1:int = load &b1 @[5:14]
              n2:int = load &b2 @[6:14]
              n3:int = load &b3 @[7:14]
              n4:int = load &b4 @[8:14]
              n5:int = load &b5 @[9:14]
              jmp lab1, if0, if1, if2 @[10:14]

          #if0: @[10:14]
              prune __sil_lnot(n1) @[10:14]
              jmp lab2 @[10:14]

          #if1: @[10:14]
              prune __sil_lnot(n2) @[10:14]
              jmp lab2 @[10:14]

          #if2: @[10:14]
              prune __sil_lnot(n3) @[10:14]
              jmp lab2 @[10:14]

          #lab1: @[11:10]
              prune n1 @[10:14]
              prune n2 @[10:14]
              prune n3 @[10:14]
              ret 1 @[12:14]

          #lab2: @[13:10]
              jmp if5, if6, if3, if4 @[14:14]

          #if5: @[14:14]
              prune n2 @[14:14]
              jmp lab3 @[14:14]

          #if6: @[14:14]
              prune n1 @[14:14]
              prune n3 @[14:14]
              jmp lab3 @[14:14]

          #if3: @[14:14]
              prune __sil_lnot(n2) @[14:14]
              prune __sil_lnot(n1) @[14:14]
              jmp lab4 @[14:14]

          #if4: @[14:14]
              prune __sil_lnot(n2) @[14:14]
              prune __sil_lnot(n3) @[14:14]
              jmp lab4 @[14:14]

          #lab3: @[16:10]
              jmp if10, if11, if12, if7, if8, if9 @[17:14]

          #if10: @[17:14]
              prune n1 @[17:14]
              prune n2 @[17:14]
              jmp lab4 @[17:14]

          #if11: @[17:14]
              prune n1 @[17:14]
              prune n3 @[17:14]
              prune n4 @[17:14]
              jmp lab4 @[17:14]

          #if12: @[17:14]
              prune n1 @[17:14]
              prune n3 @[17:14]
              prune n5 @[17:14]
              jmp lab4 @[17:14]

          #if7: @[17:14]
              prune __sil_lnot(n1) @[17:14]
              jmp lab5 @[17:14]

          #if8: @[17:14]
              prune __sil_lnot(n2) @[17:14]
              prune __sil_lnot(n3) @[17:14]
              jmp lab5 @[17:14]

          #if9: @[17:14]
              prune __sil_lnot(n2) @[17:14]
              prune __sil_lnot(n4) @[17:14]
              prune __sil_lnot(n5) @[17:14]
              jmp lab5 @[17:14]

          #lab4: @[18:10]
              ret 2 @[19:14]

          #lab5: @[20:10]
              ret 3 @[21:14]

        } @[22:9]

        define g(b1: int, b2: int, b3: int) : int {
          #entry: @[25:10]
              n1:int = load &b1 @[26:14]
              n2:int = load &b2 @[27:14]
              n3:int = load &b3 @[28:14]
              jmp if3, if4, if0, if1 @[29:14]

          #if3: @[29:14]
              prune n1 @[29:14]
              prune n3 @[29:14]
              jmp lab1 @[29:14]

          #if4: @[29:14]
              prune n2 @[29:14]
              prune n3 @[29:14]
              jmp lab1 @[29:14]

          #if0: @[29:14]
              prune __sil_lnot(n1) @[29:14]
              prune __sil_lnot(n2) @[29:14]
              jmp if2 @[29:14]

          #if1: @[29:14]
              prune __sil_lnot(n3) @[29:14]
              jmp if2 @[29:14]

          #lab1: @[30:10]
              ret 1 @[31:14]

          #if2: @[32:10]
              ret 2 @[33:14]

        } @[34:9]

        define h(b1: int, b2: int, b3: int) : int {
          #entry: @[37:10]
              n1:int = load &b1 @[38:14]
              n2:int = load &b2 @[39:14]
              n3:int = load &b3 @[40:14]
              jmp if7, if4, if5, if6 @[41:14]

          #if7: @[41:14]
              prune n1 @[41:14]
              prune n2 @[41:14]
              prune n3 @[41:14]
              ret 1 @[41:14]

          #if4: @[41:14]
              prune __sil_lnot(n1) @[41:14]
              jmp if2, if3, if0, if1 @[41:14]

          #if5: @[41:14]
              prune __sil_lnot(n2) @[41:14]
              jmp if2, if3, if0, if1 @[41:14]

          #if6: @[41:14]
              prune __sil_lnot(n3) @[41:14]
              jmp if2, if3, if0, if1 @[41:14]

          #if2: @[41:14]
              prune n2 @[41:14]
              ret 2 @[41:14]

          #if3: @[41:14]
              prune n1 @[41:14]
              prune n3 @[41:14]
              ret 2 @[41:14]

          #if0: @[41:14]
              prune __sil_lnot(n2) @[41:14]
              prune __sil_lnot(n1) @[41:14]
              ret 3 @[41:14]

          #if1: @[41:14]
              prune __sil_lnot(n2) @[41:14]
              prune __sil_lnot(n3) @[41:14]
              ret 3 @[41:14]

        } @[44:9]

        define if_lparen_test(b1: int, b2: int) : int {
          #entry: @[47:10]
              n1:int = load &b1 @[48:14]
              n2:int = load &b2 @[49:14]
              jmp if0, if1, lab1 @[50:14]

          #if0: @[50:14]
              prune n1 @[50:14]
              ret 1 @[50:14]

          #if1: @[50:14]
              prune n2 @[50:14]
              ret 1 @[50:14]

          #lab1: @[51:10]
              prune __sil_lnot(n1) @[50:14]
              prune __sil_lnot(n2) @[50:14]
              jmp if4, if2, if3 @[52:14]

          #if4: @[52:14]
              prune n1 @[52:14]
              prune n2 @[52:14]
              ret 1 @[52:14]

          #if2: @[52:14]
              prune __sil_lnot(n1) @[52:14]
              jmp lab2 @[52:14]

          #if3: @[52:14]
              prune __sil_lnot(n2) @[52:14]
              jmp lab2 @[52:14]

          #lab2: @[53:10]
              jmp if7, if5, if6 @[54:14]

          #if7: @[54:14]
              prune n1 @[54:14]
              prune n2 @[54:14]
              ret 1 @[54:14]

          #if5: @[54:14]
              prune __sil_lnot(n1) @[54:14]
              jmp lab3 @[54:14]

          #if6: @[54:14]
              prune __sil_lnot(n2) @[54:14]
              jmp lab3 @[54:14]

          #lab3: @[55:10]
              jmp if10, if8, if9 @[56:14]

          #if10: @[56:14]
              prune n1 @[56:14]
              prune n2 @[56:14]
              ret 1 @[56:14]

          #if8: @[56:14]
              prune __sil_lnot(n1) @[56:14]
              jmp lab4 @[56:14]

          #if9: @[56:14]
              prune __sil_lnot(n2) @[56:14]
              jmp lab4 @[56:14]

          #lab4: @[57:10]
              jmp if13, if14, if11, if12 @[58:14]

          #if13: @[58:14]
              prune n1 @[58:14]
              prune n2 @[58:14]
              ret 1 @[58:14]

          #if14: @[58:14]
              prune n1 @[58:14]
              ret 1 @[58:14]

          #if11: @[58:14]
              prune __sil_lnot(n1) @[58:14]
              prune __sil_lnot(n1) @[58:14]
              jmp lab5 @[58:14]

          #if12: @[58:14]
              prune __sil_lnot(n2) @[58:14]
              prune __sil_lnot(n1) @[58:14]
              jmp lab5 @[58:14]

          #lab5: @[59:10]
              jmp if17, if18, if15, if16 @[60:14]

          #if17: @[60:14]
              prune n1 @[60:14]
              prune n1 @[60:14]
              ret 1 @[60:14]

          #if18: @[60:14]
              prune n2 @[60:14]
              prune n1 @[60:14]
              ret 1 @[60:14]

          #if15: @[60:14]
              prune __sil_lnot(n1) @[60:14]
              prune __sil_lnot(n2) @[60:14]
              ret 2 @[60:14]

          #if16: @[60:14]
              prune __sil_lnot(n1) @[60:14]
              ret 2 @[60:14]

        } @[61:9] |}]


    let%expect_test _ =
      let module_ =
        parse_module python_inspired_text |> TextualTransform.remove_if_exp_and_terminator
      in
      show module_ ;
      [%expect
        {|
        .source_language = "python" @[2:7]

        define f(x: int, y: int, z: int, t: int) : int {
          #b0: @[4:10]
              n0:int = load &x @[5:14]
              jmp b1, if0 @[6:14]

          #if0: @[6:14]
              prune __sil_lnot(n0) @[6:14]
              jmp b2 @[6:14]

          #b1: @[8:10]
              prune n0 @[6:14]
              n2:int = load &y @[9:14]
              jmp if2, if1 @[10:14]

          #if2: @[10:14]
              prune n2 @[10:14]
              jmp b4(n2) @[10:14]

          #if1: @[10:14]
              prune __sil_lnot(n2) @[10:14]
              jmp b2 @[10:14]

          #b2: @[12:10]
              n5:int = load &z @[13:14]
              jmp b5, if3 @[14:14]

          #if3: @[14:14]
              prune __sil_lnot(n5) @[14:14]
              jmp b4(n5) @[14:14]

          #b5: @[16:10]
              prune n5 @[14:14]
              n8:int = load &t @[17:14]
              jmp b4(n8) @[18:14]

          #b4(n9: int): @[20:10]
              ret n9 @[21:14]

        } @[23:9] |}]
  end )


let%test_module "let_propagation transformation" =
  ( module struct
    let input_text =
      {|
        .source_language = "python"
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
        .source_language = "python" @[2:8]

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
          .source_language = "python"
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
        .source_language = "python" @[2:10]

        define f(x: int, y: int) : int {
          #entry: @[4:12]
              n0:int = load &x @[5:16]
              n1:int = load &y @[6:16]
              store &__SSA2 <- n0:int @?
              store &__SSA3 <- n1:int @?
              store &__SSA6 <- n1:int @?
              store &__SSA7 <- __sil_mult_int(n1, n0):int @?
              jmp lab1, lab3 @[7:16]

          #lab1: @[9:12]
              n2:int = load &__SSA2 @?
              n3:int = load &__SSA3 @?
              store &__SSA4 <- n3:int @?
              store &__SSA5 <- n2:int @?
              jmp lab2 @[10:16]

          #lab2: @[12:12]
              n4:int = load &__SSA4 @?
              n5:int = load &__SSA5 @?
              ret __sil_plusa(n4, n5) @[13:16]

          #lab3: @[15:12]
              n6:int = load &__SSA6 @?
              n7:int = load &__SSA7 @?
              store &__SSA4 <- n6:int @?
              store &__SSA5 <- n7:int @?
              jmp lab2 @[16:16]

        } @[18:9] |}]


    let%expect_test _ =
      let module_ =
        parse_module python_inspired_text |> TextualTransform.remove_if_exp_and_terminator
        |> TextualTransform.out_of_ssa
      in
      show module_ ;
      [%expect
        {|
        .source_language = "python" @[2:7]

        define f(x: int, y: int, z: int, t: int) : int {
          #b0: @[4:10]
              n0:int = load &x @[5:14]
              jmp b1, if0 @[6:14]

          #if0: @[6:14]
              prune __sil_lnot(n0) @[6:14]
              jmp b2 @[6:14]

          #b1: @[8:10]
              prune n0 @[6:14]
              n2:int = load &y @[9:14]
              jmp if2, if1 @[10:14]

          #if2: @[10:14]
              prune n2 @[10:14]
              store &__SSA9 <- n2:int @?
              jmp b4 @[10:14]

          #if1: @[10:14]
              prune __sil_lnot(n2) @[10:14]
              jmp b2 @[10:14]

          #b2: @[12:10]
              n5:int = load &z @[13:14]
              jmp b5, if3 @[14:14]

          #if3: @[14:14]
              prune __sil_lnot(n5) @[14:14]
              store &__SSA9 <- n5:int @?
              jmp b4 @[14:14]

          #b5: @[16:10]
              prune n5 @[14:14]
              n8:int = load &t @[17:14]
              store &__SSA9 <- n8:int @?
              jmp b4 @[18:14]

          #b4: @[20:10]
              n9:int = load &__SSA9 @?
              ret n9 @[21:14]

        } @[23:9] |}]
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

    } @[17:9]
    |}] ;
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

    } @[17:9]
    |}] ;
  type_check module_ ;
  [%expect {|
      verification succeeded |}]


let%expect_test "get_class_ts" =
  let source =
    {|
        .source_language = "hack"

        declare TODO_hhbc_ClassGetTS(...) : *HackMixed

        define BuilderTesterSimplified$static.createReifiedSimplified($this: .notnull *BuilderTesterSimplified$static, $arg: .notnull *HackInt) : .const_type = "HH\this::TB" *HackMixed {
            local $builder_cls: *void, $0: *void, $1: *void, $2: *void
            #b0:
                n0 = $builtins.hack_new_dict($builtins.hack_string("kind"), $builtins.hack_int(102), $builtins.hack_string("root_name"), $builtins.hack_string("HH\\this"), $builtins.hack_string("access_list"), $builtins.hhbc_new_vec($builtins.hack_string("TB")))
                n1 = $builtins.hhbc_new_vec()
                n2 = $builtins.hhbc_combine_and_resolve_type_struct(n0)
                n3 = TODO_hhbc_ClassGetTS(n2)
                store &$builder_cls <- n3:*HackMixed
                n4:*HackMixed = load &$builder_cls
                n5 = $builtins.hhbc_class_get_c(n4)
                ret null
        }
    |}
  in
  let module_ = parse_module source |> TextualTransform.ClassGetTS.transform in
  show module_ ;
  [%expect
    {|
    .source_language = "hack" @[2:8]

    declare TODO_hhbc_ClassGetTS(...) : *HackMixed

    define BuilderTesterSimplified$static.createReifiedSimplified($this: .notnull *BuilderTesterSimplified$static, $arg: .notnull *HackInt) : .const_type = "HH\this::TB" *HackMixed {
      local $builder_cls: *void, $0: *void, $1: *void, $2: *void
      #b0: @[8:12]
          n0 = $builtins.hack_new_dict($builtins.hack_string("kind"), $builtins.hack_int(102), $builtins.hack_string("root_name"), $builtins.hack_string("HH\\this"), $builtins.hack_string("access_list"), $builtins.hhbc_new_vec($builtins.hack_string("TB"))) @[9:16]
          n1 = $builtins.hhbc_new_vec() @[10:16]
          n2 = $builtins.hhbc_combine_and_resolve_type_struct(n0) @[11:16]
          n3 = $builtins.hack_get_class_from_type("HH\\this", "TB") @[12:16]
          store &$builder_cls <- n3:*HackMixed @[13:16]
          n4:*HackMixed = load &$builder_cls @[14:16]
          n5 = $builtins.hhbc_class_get_c(n4) @[15:16]
          ret null @[16:16]

    } @[17:9] |}]


let%expect_test "toplevel if expression" =
  let source =
    {|
        .source_language = "hack"

        define main($a: int, $b: int): int {
            #b0:
                n0 = (if $b then 0 else $a)
                n1 = (if $a then 2 else n0)
                ret n0
        }
    |}
  in
  let module_ = parse_module source |> TextualTransform.remove_if_exp_and_terminator in
  show module_ ;
  [%expect
    {|
    .source_language = "hack" @[2:8]

    define main($a: int, $b: int) : int {
      #b0: @[5:12]
          jmp if_exp1, if_exp2 @[6:16]

      #if_exp2: @[6:16]
          prune __sil_lnot([&$b:int]) @[6:16]
          jmp if_exp0([&$a:int]) @[6:16]

      #if_exp1: @[6:16]
          prune [&$b:int] @[6:16]
          jmp if_exp0(0) @[6:16]

      #if_exp0(n0: *void): @[6:16]
          jmp if_exp4, if_exp5 @[7:16]

      #if_exp5: @[7:16]
          prune __sil_lnot([&$a:int]) @[7:16]
          jmp if_exp3(n0) @[7:16]

      #if_exp4: @[7:16]
          prune [&$a:int] @[7:16]
          jmp if_exp3(2) @[7:16]

      #if_exp3(n1: *void): @[7:16]
          ret n0 @[8:16]

    } @[9:9]
    |}]


let%expect_test "if expression in subexpr" =
  let source =
    {|
        .source_language = "hack"

        declare f(int): void

        declare g(int): int

        define main($a: int, $b: int): void {
            #b0:
                n0 = f((if g($b) then g(0) else g($a)))
                n1 = f((if g($a) then g(2) else g($b)))
                ret null
        }


        define if_terminal($a: int, $b: int): int {
            #b0:
                if g($b) && g($a) then ret 0 else ret 1
        }
    |}
  in
  let module_, _ = parse_module source |> remove_effects_in_subexprs Hack in
  show module_ ;
  [%expect
    {|
    .source_language = "hack" @[2:8]

    declare f(int) : void

    declare g(int) : int

    define main($a: int, $b: int) : void {
      #b0: @[9:12]
          n2:int = load &$b @[10:16]
          n3 = g(n2) @[10:16]
          jmp if_exp1, if_exp2 @[10:16]

      #if_exp2: @[10:16]
          prune __sil_lnot(n3) @[10:16]
          n8:int = load &$a @[10:16]
          n9 = g(n8) @[10:16]
          jmp if_exp0(n9) @[10:16]

      #if_exp1: @[10:16]
          prune n3 @[10:16]
          n10 = g(0) @[10:16]
          jmp if_exp0(n10) @[10:16]

      #if_exp0(n4: *void): @[10:16]
          n0 = f(n4) @[10:16]
          n5:int = load &$a @[11:16]
          n6 = g(n5) @[11:16]
          jmp if_exp4, if_exp5 @[11:16]

      #if_exp5: @[11:16]
          prune __sil_lnot(n6) @[11:16]
          n11:int = load &$b @[11:16]
          n12 = g(n11) @[11:16]
          jmp if_exp3(n12) @[11:16]

      #if_exp4: @[11:16]
          prune n6 @[11:16]
          n13 = g(2) @[11:16]
          jmp if_exp3(n13) @[11:16]

      #if_exp3(n7: *void): @[11:16]
          n1 = f(n7) @[11:16]
          ret null @[12:16]

    } @[13:9]

    define if_terminal($a: int, $b: int) : int {
      #b0: @[17:12]
          n0:int = load &$b @[18:16]
          n1 = g(n0) @[18:16]
          jmp if2, if0, if1 @[18:16]

      #if2: @[18:16]
          prune n1 @[18:16]
          n2:int = load &$a @[18:16]
          n3 = g(n2) @[18:16]
          prune n3 @[18:16]
          ret 0 @[18:16]

      #if0: @[18:16]
          prune __sil_lnot(n1) @[18:16]
          ret 1 @[18:16]

      #if1: @[18:16]
          n4:int = load &$a @[18:16]
          n5 = g(n4) @[18:16]
          prune __sil_lnot(n5) @[18:16]
          ret 1 @[18:16]

    } @[19:9]
    |}]
