(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    open Congruence

    (* let () = Trace.init ~margin:160 ~config:all () *)

    let () = Trace.init ~margin:68 ~config:none ()
    let printf pp = Format.printf "@\n%a@." pp
    let pp = printf pp
    let pp_classes = printf pp_classes
    let of_eqs = List.fold ~init:true_ ~f:(fun r (a, b) -> and_eq r a b)
    let mem_eq x y r = Exp.equal (normalize r x) (normalize r y)
    let i8 = Typ.integer ~bits:8
    let i64 = Typ.integer ~bits:64
    let ( ! ) i = Exp.integer (Z.of_int i) Typ.siz
    let ( + ) = Exp.add Typ.siz
    let ( - ) = Exp.sub Typ.siz
    let ( * ) = Exp.mul Typ.siz
    let f = Exp.convert ~dst:i64 ~src:i8
    let g = Exp.rem
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

    let%expect_test _ =
      pp f1 ;
      [%expect {| {sat= false; rep= []; lkp= []; cls= []; use= []} |}]

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
      pp r0 ;
      [%expect {| {sat= true; rep= []; lkp= []; cls= []; use= []} |}]

    let%expect_test _ = pp_classes r0 ; [%expect {| |}]
    let%test _ = difference r0 (f x) (f x) |> Poly.equal (Some (Z.of_int 0))
    let%test _ = difference r0 !4 !3 |> Poly.equal (Some (Z.of_int 1))

    let r1 = of_eqs [(x, y)]

    let%expect_test _ =
      pp_classes r1 ;
      pp r1 ;
      [%expect
        {|
          %y_6 = %x_5

          {sat= true;
           rep= [[%x_5 ↦ %y_6]];
           lkp= [];
           cls= [[%y_6 ↦ {%x_5, %y_6}]];
           use= []} |}]

    let%test _ = not (Map.is_empty (classes r1))
    let%test _ = mem_eq x y r1

    let r2 = of_eqs [(x, y); (f x, y); (f y, z)]

    let%expect_test _ =
      pp_classes r2 ;
      pp r2 ;
      [%expect
        {|
          %y_6 = ((i64)(i8) %x_5) = ((i64)(i8) %y_6) = %x_5 = %z_7

          {sat= true;
           rep= [[((i64)(i8) %x_5) ↦ %y_6];
                 [((i64)(i8) %y_6) ↦ %y_6];
                 [%x_5 ↦ %y_6];
                 [%z_7 ↦ %y_6]];
           lkp= [[((i64)(i8) %y_6) ↦ ((i64)(i8) %x_5)]];
           cls= [[%y_6
                  ↦ {((i64)(i8) %x_5), ((i64)(i8) %y_6), %x_5, %y_6, %z_7}]];
           use= [[%y_6 ↦ {((i64)(i8) %x_5)}];
                 [(i64)(i8) ↦ {((i64)(i8) %x_5)}]]} |}]

    let r2' = extend r2 (f z)

    let%expect_test _ =
      pp_classes r2' ;
      pp r2' ;
      [%expect
        {|
          %y_6 = ((i64)(i8) %x_5) = ((i64)(i8) %y_6) = ((i64)(i8) %z_7)
          = %x_5 = %z_7

          {sat= true;
           rep= [[((i64)(i8) %x_5) ↦ %y_6];
                 [((i64)(i8) %y_6) ↦ %y_6];
                 [((i64)(i8) %z_7) ↦ %y_6];
                 [%x_5 ↦ %y_6];
                 [%z_7 ↦ %y_6]];
           lkp= [[((i64)(i8) %y_6) ↦ ((i64)(i8) %x_5)]];
           cls= [[%y_6
                  ↦ {((i64)(i8) %x_5), ((i64)(i8) %y_6), ((i64)(i8) %z_7),
                     %x_5, %y_6, %z_7}]];
           use= [[%y_6 ↦ {((i64)(i8) %x_5)}];
                 [(i64)(i8) ↦ {((i64)(i8) %x_5)}]]} |}]

    let%test _ = mem_eq x z r2
    let%test _ = mem_eq x y (or_ r1 r2)
    let%test _ = entails (or_ r1 r2) r1
    let%test _ = not (mem_eq x z (or_ r1 r2))
    let%test _ = mem_eq x z (or_ f1 r2)
    let%test _ = mem_eq x z (or_ r2 f3)
    let%test _ = mem_eq (f y) y r2
    let%test _ = mem_eq (f x) (f z) r2'
    let%test _ = not (mem_eq (f x) (f z) r2)
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
      pp_classes r3 ;
      pp r3 ;
      [%expect
        {|
      ∧ %w_4 = (%y_6 rem %w_4) = (%y_6 rem %z_7) = %t_1 = %u_2 = %v_3
        = %x_5 = %z_7

      {sat= true;
       rep= [[(%y_6 rem %w_4) ↦ %w_4];
             [(%y_6 rem %z_7) ↦ %w_4];
             [(rem %y_6) ↦ ];
             [%t_1 ↦ %w_4];
             [%u_2 ↦ %w_4];
             [%v_3 ↦ %w_4];
             [%x_5 ↦ %w_4];
             [%z_7 ↦ %w_4]];
       lkp= [[(%y_6 rem %w_4) ↦ ]; [(%y_6 rem %z_7) ↦ ]; [(rem %y_6) ↦ ]];
       cls= [[(rem %y_6) ↦ {(rem %y_6)}];
             [%w_4
              ↦ {(%y_6 rem %w_4), (%y_6 rem %z_7), %t_1, %u_2, %v_3,
                 %w_4, %x_5, %z_7}]];
       use= [[(rem %y_6) ↦ {(%y_6 rem %w_4), (%y_6 rem %z_7)}];
             [%w_4 ↦ {(%y_6 rem %w_4)}];
             [%y_6 ↦ {(rem %y_6)}];
             [rem ↦ {(rem %y_6)}]]} |}]

    let%test _ = mem_eq t z r3
    let%test _ = mem_eq x z r3
    let%test _ = mem_eq x z (and_ r2 r3)

    let r4 = of_eqs [(w + !2, x - !3); (x - !5, y + !7); (y, z - !4)]

    let%expect_test _ =
      pp_classes r4 ;
      pp r4 ;
      [%expect
        {|
      %y_6 = (%w_4 + -7) = (%x_5 + -12) = (%z_7 + -4)

      {sat= true;
       rep= [[%w_4 ↦ (%y_6 + 7)];
             [%x_5 ↦ (%y_6 + 12)];
             [%z_7 ↦ (%y_6 + 4)]];
       lkp= [];
       cls= [[%y_6 ↦ {(%w_4 + -7), (%x_5 + -12), (%z_7 + -4), %y_6}]];
       use= []} |}]

    let%test _ = mem_eq x (w + !5) r4
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

      {sat= true;
       rep= [[%x_5 ↦ 1]; [%y_6 ↦ 1]];
       lkp= [];
       cls= [[1 ↦ {%x_5, %y_6, 1}]];
       use= []} |}]

    let%test _ = mem_eq x y r6

    let r7 = of_eqs [(v, x); (w, z); (y, z)]

    let%expect_test _ =
      pp_classes r7 ;
      pp r7 ;
      pp (merge ~prefer:(fun e ~over:f -> Exp.compare e f) r7 x z) ;
      pp (merge ~prefer:(fun e ~over:f -> Exp.compare f e) r7 x z) ;
      [%expect
        {|
      %x_5 = %v_3
      ∧ %z_7 = %w_4 = %y_6

      {sat= true;
       rep= [[%v_3 ↦ %x_5]; [%w_4 ↦ %z_7]; [%y_6 ↦ %z_7]];
       lkp= [];
       cls= [[%x_5 ↦ {%v_3, %x_5}]; [%z_7 ↦ {%w_4, %y_6, %z_7}]];
       use= []}

      {sat= true;
       rep= [[%v_3 ↦ %z_7]; [%w_4 ↦ %z_7]; [%x_5 ↦ %z_7]; [%y_6 ↦ %z_7]];
       lkp= [];
       cls= [[%z_7 ↦ {%v_3, %w_4, %x_5, %y_6, %z_7}]];
       use= []}

      {sat= true;
       rep= [[%v_3 ↦ %x_5]; [%w_4 ↦ %x_5]; [%y_6 ↦ %x_5]; [%z_7 ↦ %x_5]];
       lkp= [];
       cls= [[%x_5 ↦ {%v_3, %w_4, %x_5, %y_6, %z_7}]];
       use= []} |}]

    let%test _ = normalize (merge r7 x z) w |> Exp.equal z

    let%test _ =
      let prefer e ~over:f = Exp.compare e f in
      prefer z ~over:x > 0

    let%test _ =
      let prefer e ~over:f = Exp.compare e f in
      prefer z ~over:x > 0
      (* so x should not be the rep *)
      && normalize (merge ~prefer r7 x z) w |> Exp.equal z

    let%test _ =
      let prefer e ~over:f = Exp.compare f e in
      prefer x ~over:z > 0
      (* so z should not be the rep *)
      && normalize (merge ~prefer r7 x z) w |> Exp.equal x

    let%test _ =
      mem_eq (g w x) (g w z)
        (extend (of_eqs [(g w x, g y z); (x, z)]) (g w z))

    let%test _ =
      mem_eq (g w x) (g w z)
        (extend (of_eqs [(g w x, g y w); (x, z)]) (g w z))

    let r8 = of_eqs [(x + !42, (!3 * y) + (!13 * z)); (!13 * z, x)]

    let%expect_test _ =
      pp_classes r8 ;
      pp r8 ;
      [%expect
        {|
      (3 × %y_6 + 13 × %z_7) = (%x_5 + 42) = (13 × %z_7 + 42)

      {sat= true;
       rep= [[(3 × %y_6 + 13 × %z_7) ↦ ];
             [(13 × %z_7) ↦ (3 × %y_6 + 13 × %z_7 + -42)];
             [%x_5 ↦ (3 × %y_6 + 13 × %z_7 + -42)]];
       lkp= [[(3 × %y_6 + 13 × %z_7) ↦ ]; [(13 × %z_7) ↦ ]];
       cls= [[(3 × %y_6 + 13 × %z_7)
              ↦ {(%x_5 + 42), (3 × %y_6 + 13 × %z_7), (13 × %z_7 + 42)}]];
       use= [[%y_6 ↦ {(3 × %y_6 + 13 × %z_7)}];
             [%z_7 ↦ {(13 × %z_7), (3 × %y_6 + 13 × %z_7)}];
             [+ ↦ {(13 × %z_7), (3 × %y_6 + 13 × %z_7)}]]} |}]

    (* (* incomplete *)
     * let%test _ = mem_eq y !14 r8 *)

    let r9 = of_eqs [(x, z - !16)]

    let%expect_test _ =
      pp_classes r9 ;
      pp r9 ;
      [%expect
        {|
       %z_7 = (%x_5 + 16)

       {sat= true;
        rep= [[%x_5 ↦ (%z_7 + -16)]];
        lkp= [];
        cls= [[%z_7 ↦ {(%x_5 + 16), %z_7}]];
        use= []} |}]

    let%test _ = difference r9 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let r10 = of_eqs [(!16, z - x)]

    let%expect_test _ =
      pp_classes r10 ;
      pp r10 ;
      Format.printf "@.%a@." Exp.pp (z - (x + !8)) ;
      Format.printf "@.%a@." Exp.pp (normalize r10 (z - (x + !8))) ;
      Format.printf "@.%a@." Exp.pp (x + !8 - z) ;
      Format.printf "@.%a@." Exp.pp (normalize r10 (x + !8 - z)) ;
      [%expect
        {|
      16 = (-1 × %x_5 + %z_7)

      {sat= true;
       rep= [[(-1 × %x_5 + %z_7) ↦ 16]];
       lkp= [[(-1 × %x_5 + %z_7) ↦ ]];
       cls= [[16 ↦ {(-1 × %x_5 + %z_7), 16}]];
       use= [[%x_5 ↦ {(-1 × %x_5 + %z_7)}];
             [%z_7 ↦ {(-1 × %x_5 + %z_7)}];
             [+ ↦ {(-1 × %x_5 + %z_7)}]]}

      (-1 × %x_5 + %z_7 + -8)

      8

      (%x_5 + -1 × %z_7 + 8)

      (%x_5 + -1 × %z_7 + 8) |}]

    let%test _ = difference r10 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    (* (* incomplete *)
     * let%test _ =
     *   difference r10 (x + !8) z |> Poly.equal (Some (Z.of_int (-8))) *)

    let r11 = of_eqs [(!16, z - x); (x + !8 - z, z - !16 + !8 - z)]

    let%expect_test _ =
      pp_classes r11 ;
      [%expect
        {|
        -16 = (%x_5 + -1 × %z_7)
        ∧ 16 = (-1 × %x_5 + %z_7) |}]

    let r12 = of_eqs [(!16, z - x); (x + !8 - z, z + !16 + !8 - z)]

    let%expect_test _ =
      pp_classes r12 ;
      [%expect {| 16 = (-1 × %x_5 + %z_7) = (%x_5 + -1 × %z_7) |}]
  end )
