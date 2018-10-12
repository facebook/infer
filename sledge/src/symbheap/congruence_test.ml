(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    open Congruence

    let printf pp = Format.printf "@.%a@." pp
    let of_eqs = List.fold ~init:true_ ~f:(fun r (a, b) -> and_eq r a b)
    let mem_eq x y r = entails r (and_eq true_ x y)
    let i8 = Typ.integer ~bits:8
    let i64 = Typ.integer ~bits:64
    let ( ! ) i = Exp.integer (Z.of_int i)
    let ( + ) = Exp.add
    let ( - ) = Exp.sub
    let f = Exp.convert ~dst:i64 ~src:i8
    let g = Exp.xor
    let wrt = Var.Set.empty
    let t_, wrt = Var.fresh "t" ~wrt
    let u_, wrt = Var.fresh "u" ~wrt
    let v_, wrt = Var.fresh "v" ~wrt
    let w_, wrt = Var.fresh "w" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, _ = Var.fresh "z" ~wrt
    let t = Exp.var t_
    let u = Exp.var u_
    let v = Exp.var v_
    let w = Exp.var w_
    let x = Exp.var x_
    let y = Exp.var y_
    let z = Exp.var z_
    let f1 = of_eqs [(!0, !1)]

    let%test _ = is_false f1
    let%test _ = Map.is_empty (classes f1)
    let%test _ = is_false (merge f1 !1 !1)

    let f2 = of_eqs [(x, x + !1)]

    let%test _ = is_false f2

    let f3 = of_eqs [(x + !0, x + !1)]

    let%test _ = is_false f3

    let f4 = of_eqs [(x, y); (x + !0, y + !1)]

    let%test _ = is_false f4

    let t1 = of_eqs [(!1, !1)]

    let%test _ = is_true t1

    let t2 = of_eqs [(x, x)]

    let%test _ = is_true t2
    let%test _ = is_false (and_ f3 t2)
    let%test _ = is_false (and_ t2 f3)

    let r0 = true_

    let%expect_test _ =
      printf pp r0 ;
      [%expect {| {sat= true; rep= []; lkp= []; cls= []; use= []} |}]

    let%expect_test _ = printf pp_classes r0 ; [%expect {| |}]
    let%test _ = difference r0 (f x) (f x) |> Poly.equal (Some (Z.of_int 0))
    let%test _ = difference r0 !4 !3 |> Poly.equal (Some (Z.of_int 1))

    let r1 = of_eqs [(x, y)]

    let%test _ = not (Map.is_empty (classes r1))

    let%expect_test _ =
      printf pp r1 ;
      [%expect
        {|
          {sat= true;
           rep= [[%x_5 ↦ %y_6]];
           lkp= [];
           cls= [[%y_6 ↦ {%y_6, %x_5}]];
           use= []} |}]

    let%expect_test _ = printf pp_classes r1 ; [%expect {| %y_6 = %x_5 |}]
    let%test _ = mem_eq x y r1

    let r2 = of_eqs [(x, y); (f x, y); (f y, z)]

    let%expect_test _ =
      printf pp r2 ;
      [%expect
        {|
          {sat= true;
           rep= [[%x_5 ↦ %y_6]; [%z_7 ↦ %y_6]];
           lkp= [];
           cls= [[%y_6 ↦ {%z_7, %y_6, %x_5}]];
           use= []} |}]

    let%expect_test _ =
      printf pp_classes r2 ; [%expect {| %y_6 = %z_7 = %x_5 |}]

    let%test _ = mem_eq x z r2
    let%test _ = mem_eq x y (or_ r1 r2)
    let%test _ = entails (or_ r1 r2) r1
    let%test _ = not (mem_eq x z (or_ r1 r2))
    let%test _ = mem_eq x z (or_ f1 r2)
    let%test _ = mem_eq x z (or_ r2 f3)
    let%test _ = mem_eq (f x) (f z) r2
    let%test _ = mem_eq (g x y) (g z y) r2

    let%test _ =
      mem_eq w z (rename r2 Var.Subst.(extend empty ~replace:x_ ~with_:w_))

    let%test _ =
      r2 == rename r2 Var.Subst.(extend empty ~replace:w_ ~with_:x_)

    let%test _ =
      mem_eq v w
        (rename r2
           Var.Subst.(
             empty
             |> extend ~replace:x_ ~with_:v_
             |> extend ~replace:y_ ~with_:w_))

    let%test _ = difference (or_ r1 r2) x z |> Poly.equal None

    let r3 = of_eqs [(g y z, w); (v, w); (g y w, t); (x, v); (x, u); (u, z)]

    let%expect_test _ =
      printf pp r3 ;
      [%expect
        {|
      {sat= true;
       rep= [[%t_1 ↦ %w_4];
             [%u_2 ↦ %w_4];
             [%v_3 ↦ %w_4];
             [%x_5 ↦ %w_4];
             [%z_7 ↦ %w_4];
             [(%y_6 xor %w_4) ↦ %w_4];
             [(%y_6 xor %z_7) ↦ %w_4]];
       lkp= [[(%y_6 xor %w_4) ↦ (%y_6 xor %w_4)];
             [(%y_6 xor %z_7) ↦ (%y_6 xor %z_7)];
             [(xor %y_6) ↦ (xor %y_6)]];
       cls= [[%w_4
              ↦ {(%y_6 xor %w_4), %t_1, %z_7, %u_2, %x_5, %v_3, %w_4,
                 (%y_6 xor %z_7)}]];
       use= [[%w_4 ↦ {(%y_6 xor %w_4)}];
             [%y_6 ↦ {(xor %y_6)}];
             [(xor %y_6) ↦ {(%y_6 xor %w_4), (%y_6 xor %z_7)}]]} |}]

    let%test _ = mem_eq t z r3
    let%test _ = mem_eq x z r3
    let%test _ = mem_eq x z (and_ r2 r3)

    let r4 = of_eqs [(w + !2, x - !3); (x - !5, y + !7); (y, z - !4)]

    let%test _ = mem_eq x (w + !5) r4
    let%test _ = difference r4 x w |> Poly.equal (Some (Z.of_int 5))

    let r5 = of_eqs [(x, y); (g w x, y); (g w y, f z)]

    let%test _ =
      Set.equal (fv r5) (Set.of_list (module Var) [w_; x_; y_; z_])

    let r6 = of_eqs [(x, !1); (!1, y)]

    let%test _ = mem_eq x y r6

    let r7 = of_eqs [(v, x); (w, z); (y, z)]

    let%test _ = normalize (merge r7 x z) w |> Exp.equal z

    let%test _ =
      normalize (merge ~prefer:(fun e ~over:f -> Exp.compare f e) r7 x z) w
      |> Exp.equal x

    let%test _ = mem_eq (g w x) (g w z) (of_eqs [(g w x, g y z); (x, z)])
    let%test _ = mem_eq (g w x) (g w z) (of_eqs [(g w x, g y w); (x, z)])
  end )
