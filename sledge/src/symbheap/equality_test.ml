(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    open Equality

    let () = Trace.init ~margin:68 ~config:Trace.none ()

    (* let () = Trace.init ~margin:160 ~config:all () *)
    let printf pp = Format.printf "@\n%a@." pp
    let pp = printf pp
    let pp_classes = printf pp_classes
    let of_eqs = List.fold ~init:true_ ~f:(fun r (a, b) -> and_eq a b r)
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( + ) = Term.add
    let ( - ) = Term.sub
    let ( * ) = Term.mul
    let f = Term.unsigned 8
    let g = Term.rem
    let wrt = Var.Set.empty
    let t_, wrt = Var.fresh "t" ~wrt
    let u_, wrt = Var.fresh "u" ~wrt
    let v_, wrt = Var.fresh "v" ~wrt
    let w_, wrt = Var.fresh "w" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, _ = Var.fresh "z" ~wrt
    let t = Term.var t_
    let u = Term.var u_
    let v = Term.var v_
    let w = Term.var w_
    let x = Term.var x_
    let y = Term.var y_
    let z = Term.var z_
    let f1 = of_eqs [(!0, !1)]

    let%test _ = is_false f1
    let%expect_test _ = pp f1 ; [%expect {| {sat= false; rep= []} |}]
    let%test _ = is_false (and_eq !1 !1 f1)

    let f2 = of_eqs [(x, x + !1)]

    let%test _ = is_false f2
    let%expect_test _ = pp f2 ; [%expect {| {sat= false; rep= []} |}]

    let f3 = of_eqs [(x + !0, x + !1)]

    let%test _ = is_false f3
    let%expect_test _ = pp f3 ; [%expect {| {sat= false; rep= []} |}]

    let f4 = of_eqs [(x, y); (x + !0, y + !1)]

    let%test _ = is_false f4

    let%expect_test _ =
      pp f4 ; [%expect {| {sat= false; rep= [[%y_6 ↦ %x_5]]} |}]

    let t1 = of_eqs [(!1, !1)]

    let%test _ = is_true t1

    let t2 = of_eqs [(x, x)]

    let%test _ = is_true t2
    let%test _ = is_false (and_ f3 t2)
    let%test _ = is_false (and_ t2 f3)

    let r0 = true_

    let%expect_test _ = pp r0 ; [%expect {| {sat= true; rep= []} |}]
    let%expect_test _ = pp_classes r0 ; [%expect {| |}]
    let%test _ = difference r0 (f x) (f x) |> Poly.equal (Some (Z.of_int 0))
    let%test _ = difference r0 !4 !3 |> Poly.equal (Some (Z.of_int 1))

    let r1 = of_eqs [(x, y)]

    let%expect_test _ =
      pp_classes r1 ;
      pp r1 ;
      [%expect
        {|
          %x_5 = %y_6

          {sat= true; rep= [[%y_6 ↦ %x_5]]} |}]

    let%test _ = entails_eq r1 x y

    let r2 = of_eqs [(x, y); (f x, y); (f y, z)]

    let%expect_test _ =
      pp_classes r2 ;
      pp r2 ;
      [%expect
        {|
          %x_5 = %y_6 = %z_7 = ((u8) %x_5)

          {sat= true;
           rep= [[%y_6 ↦ %x_5]; [%z_7 ↦ %x_5]; [((u8) %x_5) ↦ %x_5]]} |}]

    let%test _ = entails_eq r2 x z
    let%test _ = entails_eq (or_ r1 r2) x y
    let%test _ = not (entails_eq (or_ r1 r2) x z)
    let%test _ = entails_eq (or_ f1 r2) x z
    let%test _ = entails_eq (or_ r2 f3) x z
    let%test _ = entails_eq r2 (f y) y
    let%test _ = entails_eq r2 (f x) (f z)
    let%test _ = entails_eq r2 (g x y) (g z y)
    let%test _ = difference (or_ r1 r2) x z |> Poly.equal None

    let r3 = of_eqs [(g y z, w); (v, w); (g y w, t); (x, v); (x, u); (u, z)]

    let%expect_test _ =
      pp_classes r3 ;
      pp r3 ;
      [%expect
        {|
      %t_1 = %u_2 = %v_3 = %w_4 = %x_5 = %z_7 = (%y_6 rem %v_3)
      = (%y_6 rem %z_7)

      {sat= true;
       rep= [[%u_2 ↦ %t_1];
             [%v_3 ↦ %t_1];
             [%w_4 ↦ %t_1];
             [%x_5 ↦ %t_1];
             [%z_7 ↦ %t_1];
             [(%y_6 rem %v_3) ↦ %t_1];
             [(%y_6 rem %z_7) ↦ %t_1]]} |}]

    let%test _ = entails_eq r3 t z
    let%test _ = entails_eq r3 x z
    let%test _ = entails_eq (and_ r2 r3) x z

    let r4 = of_eqs [(w + !2, x - !3); (x - !5, y + !7); (y, z - !4)]

    let%expect_test _ =
      pp_classes r4 ;
      pp r4 ;
      [%expect
        {|
      (%z_7 + -4) = %y_6 ∧ (%z_7 + 3) = %w_4
      ∧ (%z_7 + 8) = %x_5

      {sat= true;
       rep= [[%w_4 ↦ (%z_7 + 3)];
             [%x_5 ↦ (%z_7 + 8)];
             [%y_6 ↦ (%z_7 + -4)]]} |}]

    let%test _ = entails_eq r4 x (w + !5)
    let%test _ = difference r4 x w |> Poly.equal (Some (Z.of_int 5))

    let r5 = of_eqs [(x, y); (g w x, y); (g w y, f z)]

    let%test _ =
      Set.equal (fv r5) (Set.of_list (module Var) [w_; x_; y_; z_])

    let r6 = of_eqs [(x, !1); (!1, y)]

    let%expect_test _ =
      pp_classes r6 ;
      pp r6 ;
      [%expect
        {|
      1 = %x_5 = %y_6

      {sat= true; rep= [[%x_5 ↦ 1]; [%y_6 ↦ 1]]} |}]

    let%test _ = entails_eq r6 x y

    let r7 = of_eqs [(v, x); (w, z); (y, z)]

    let%expect_test _ =
      pp_classes r7 ;
      pp r7 ;
      pp (and_eq x z r7) ;
      pp_classes (and_eq x z r7) ;
      [%expect
        {|
      %v_3 = %x_5
      ∧ %w_4 = %y_6 = %z_7

      {sat= true; rep= [[%x_5 ↦ %v_3]; [%y_6 ↦ %w_4]; [%z_7 ↦ %w_4]]}

      {sat= true;
       rep= [[%w_4 ↦ %v_3]; [%x_5 ↦ %v_3]; [%y_6 ↦ %v_3]; [%z_7 ↦ %v_3]]}

      %v_3 = %w_4 = %x_5 = %y_6 = %z_7 |}]

    let%expect_test _ =
      printf (List.pp " , " Term.pp) (Equality.class_of r7 t) ;
      printf (List.pp " , " Term.pp) (Equality.class_of r7 x) ;
      printf (List.pp " , " Term.pp) (Equality.class_of r7 z) ;
      [%expect
        {|
        %t_1

        %v_3 , %x_5

        %w_4 , %z_7 , %y_6 |}]

    let r7' = and_eq x z r7

    let%expect_test _ =
      pp_classes r7' ;
      pp r7' ;
      [%expect
        {|
      %v_3 = %w_4 = %x_5 = %y_6 = %z_7

      {sat= true;
       rep= [[%w_4 ↦ %v_3]; [%x_5 ↦ %v_3]; [%y_6 ↦ %v_3]; [%z_7 ↦ %v_3]]} |}]

    let%test _ = normalize r7' w |> Term.equal v

    let%test _ =
      entails_eq (of_eqs [(g w x, g y z); (x, z)]) (g w x) (g w z)

    let%test _ =
      entails_eq (of_eqs [(g w x, g y w); (x, z)]) (g w x) (g w z)

    let r8 = of_eqs [(x + !42, (!3 * y) + (!13 * z)); (!13 * z, x)]

    let%expect_test _ =
      pp_classes r8 ;
      pp r8 ;
      [%expect
        {|
      (13 × %z_7) = %x_5
      ∧ 14 = %y_6

      {sat= true; rep= [[%x_5 ↦ (13 × %z_7)]; [%y_6 ↦ 14]]} |}]

    let%test _ = entails_eq r8 y !14

    let r9 = of_eqs [(x, z - !16)]

    let%expect_test _ =
      pp_classes r9 ;
      pp r9 ;
      [%expect
        {|
       (%z_7 + -16) = %x_5

       {sat= true; rep= [[%x_5 ↦ (%z_7 + -16)]]} |}]

    let%test _ = difference r9 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let r10 = of_eqs [(!16, z - x)]

    let%expect_test _ =
      pp_classes r10 ;
      pp r10 ;
      Format.printf "@.%a@." Term.pp (z - (x + !8)) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (z - (x + !8))) ;
      Format.printf "@.%a@." Term.pp (x + !8 - z) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (x + !8 - z)) ;
      [%expect
        {|
      (%z_7 + -16) = %x_5

      {sat= true; rep= [[%x_5 ↦ (%z_7 + -16)]]}

      (-1 × %x_5 + %z_7 + -8)

      8

      (%x_5 + -1 × %z_7 + 8)

      -8 |}]

    let%test _ = difference r10 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let%test _ =
      difference r10 (x + !8) z |> Poly.equal (Some (Z.of_int (-8)))

    let r11 = of_eqs [(!16, z - x); (x + !8 - z, z - !16 + !8 - z)]

    let%expect_test _ =
      pp_classes r11 ; [%expect {|
        (%z_7 + -16) = %x_5 |}]

    let r12 = of_eqs [(!16, z - x); (x + !8 - z, z + !16 + !8 - z)]

    let%expect_test _ = pp_classes r12 ; [%expect {| (%z_7 + -16) = %x_5 |}]

    let r13 = of_eqs [(Term.eq x !2, y); (Term.dq x !2, z); (y, z)]

    let%expect_test _ =
      pp r13 ;
      [%expect
        {|
      {sat= true;
       rep= [[%z_7 ↦ %y_6]; [(%x_5 = 2) ↦ %y_6]; [(%x_5 ≠ 2) ↦ %y_6]]} |}]

    let%test _ = not (is_false r13) (* incomplete *)

    let a = Term.dq x !0
    let r14 = of_eqs [(a, a); (x, !1)]

    let%expect_test _ =
      pp r14 ; [%expect {| {sat= true; rep= [[%x_5 ↦ 1]]} |}]

    let%test _ = entails_eq r14 a Term.true_

    let b = Term.dq y !0
    let r14 = of_eqs [(a, b); (x, !1)]

    let%expect_test _ =
      pp r14 ;
      [%expect
        {|
          {sat= true; rep= [[%x_5 ↦ 1]; [(%y_6 ≠ 0) ↦ -1]]} |}]

    let%test _ = entails_eq r14 a Term.true_
    let%test _ = entails_eq r14 b Term.true_

    let b = Term.dq x !0
    let r15 = of_eqs [(b, b); (x, !1)]

    let%expect_test _ =
      pp r15 ; [%expect {|
          {sat= true; rep= [[%x_5 ↦ 1]]} |}]

    let%test _ = entails_eq r15 b (Term.signed 1 !1)
    let%test _ = entails_eq r15 (Term.unsigned 1 b) !1
  end )
