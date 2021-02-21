(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Sledge

let%test_module _ =
  ( module struct
    open Fol
    open Context

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160
     *     ~config:
     *       (Result.get_ok
     *          (Trace.parse
     *             "+Fol+Context-Context.canon-Context.canon_f-Context.norm"))
     *     () *)

    [@@@warning "-32"]

    let printf pp = Format.printf "@\n%a@." pp
    let pp_raw = printf pp_raw
    let pp = Format.printf "@\n@[<hv>  %a@]@." pp
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( + ) = Term.add
    let ( - ) = Term.sub
    let ( * ) i e = Term.mulq (Q.of_int i) e
    let wrt = Var.Set.empty
    let t_, wrt = Var.fresh "t" ~wrt
    let u_, wrt = Var.fresh "u" ~wrt
    let v_, wrt = Var.fresh "v" ~wrt
    let w_, wrt = Var.fresh "w" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, wrt = Var.fresh "z" ~wrt
    let t = Term.var t_
    let u = Term.var u_
    let v = Term.var v_
    let w = Term.var w_
    let x = Term.var x_
    let y = Term.var y_
    let z = Term.var z_
    let f x = Term.apply (Uninterp "f") [|x|]
    let g x y = Term.apply (Uninterp "g") [|x; y|]

    let of_eqs l =
      List.fold
        ~f:(fun (a, b) (us, r) -> add us (Formula.eq a b) r)
        l (wrt, empty)
      |> snd

    let add_eq a b r = add wrt (Formula.eq a b) r |> snd
    let union r s = union wrt r s |> snd
    let inter r s = inter wrt r s |> snd
    let implies_eq r a b = implies r (Formula.eq a b)
    let difference x e f = Term.get_z (normalize x (Term.sub e f))

    (** tests *)

    let f1 = of_eqs [(!0, !1)]

    let%test _ = is_unsat f1

    let%expect_test _ =
      pp_raw f1 ;
      [%expect {| { sat= false; rep= []; cls= []; use= [] } |}]

    let%test _ = is_unsat (add_eq !1 !1 f1)

    let f2 = of_eqs [(x, x + !1)]

    let%test _ = is_unsat f2

    let%expect_test _ =
      pp_raw f2 ;
      [%expect {| { sat= false; rep= []; cls= []; use= [] } |}]

    let f3 = of_eqs [(x + !0, x + !1)]

    let%test _ = is_unsat f3

    let%expect_test _ =
      pp_raw f3 ;
      [%expect {| { sat= false; rep= []; cls= []; use= [] } |}]

    let f4 = of_eqs [(x, y); (x + !0, y + !1)]

    let%test _ = is_unsat f4

    let%expect_test _ =
      pp_raw f4 ;
      [%expect
        {|
          { sat= false;
            rep= [[%x_5 ↦ ]; [%y_6 ↦ %x_5]];
            cls= [[%x_5 ↦ {%y_6}]];
            use= [] } |}]

    let t1 = of_eqs [(!1, !1)]

    let%test _ = is_empty t1

    let t2 = of_eqs [(x, x)]

    let%test _ = is_empty t2
    let%test _ = is_unsat (union f3 t2)
    let%test _ = is_unsat (union t2 f3)

    let r0 = empty

    let%expect_test _ =
      pp_raw r0 ;
      [%expect {| { sat= true; rep= []; cls= []; use= [] } |}]

    let%expect_test _ =
      pp r0 ;
      [%expect {| tt |}]

    let%test _ = difference r0 (f x) (f x) |> Poly.equal (Some (Z.of_int 0))
    let%test _ = difference r0 !4 !3 |> Poly.equal (Some (Z.of_int 1))

    let r1 = of_eqs [(x, y)]

    let%expect_test _ =
      pp r1 ;
      pp_raw r1 ;
      [%expect
        {|
    
        %x_5 = %y_6
    
      { sat= true;
        rep= [[%x_5 ↦ ]; [%y_6 ↦ %x_5]];
        cls= [[%x_5 ↦ {%y_6}]];
        use= [] } |}]

    let%test _ = implies_eq r1 x y

    let r2 = of_eqs [(x, y); (f x, y); (f y, z)]

    let%expect_test _ =
      pp r2 ;
      pp_raw r2 ;
      [%expect
        {|
        %x_5 = %y_6 = %z_7 = f(%x_5)
    
      { sat= true;
        rep= [[%x_5 ↦ ]; [%y_6 ↦ %x_5]; [%z_7 ↦ %x_5]; [f(%x_5) ↦ %x_5]];
        cls= [[%x_5 ↦ {%y_6, %z_7, f(%x_5)}]];
        use= [[%x_5 ↦ f(%x_5)]] } |}]

    let%test _ = implies_eq r2 x z
    let%test _ = implies_eq (inter r1 r2) x y
    let%test _ = not (implies_eq (inter r1 r2) x z)
    let%test _ = difference (inter r1 r2) x z |> Poly.equal None
    let%test _ = implies_eq (inter f1 r2) x z
    let%test _ = implies_eq (inter r2 f3) x z
    let%test _ = implies_eq r2 (f y) y
    let%test _ = implies_eq r2 (f x) (f z)
    let%test _ = implies_eq r2 (g x y) (g z y)

    let%expect_test _ =
      let r = of_eqs [(w, y); (y, z)] in
      let s = of_eqs [(x, y); (y, z)] in
      let rs = inter r s in
      pp_raw r ;
      pp_raw s ;
      pp_raw rs ;
      [%expect
        {|
        { sat= true;
          rep= [[%w_4 ↦ ]; [%y_6 ↦ %w_4]; [%z_7 ↦ %w_4]];
          cls= [[%w_4 ↦ {%y_6, %z_7}]];
          use= [] }
    
        { sat= true;
          rep= [[%x_5 ↦ ]; [%y_6 ↦ %x_5]; [%z_7 ↦ %x_5]];
          cls= [[%x_5 ↦ {%y_6, %z_7}]];
          use= [] }
    
        { sat= true;
          rep= [[%y_6 ↦ ]; [%z_7 ↦ %y_6]];
          cls= [[%y_6 ↦ {%z_7}]];
          use= [] } |}]

    let%test _ =
      let r = of_eqs [(w, y); (y, z)] in
      let s = of_eqs [(x, y); (y, z)] in
      let rs = inter r s in
      implies_eq rs y z

    let r3 = of_eqs [(g y z, w); (v, w); (g y w, t); (x, v); (x, u); (u, z)]

    let%expect_test _ =
      pp r3 ;
      pp_raw r3 ;
      [%expect
        {|
        %t_1 = %u_2 = %v_3 = %w_4 = %x_5 = %z_7 = g(%y_6, %t_1)
        = g(%y_6, %u_2) = g(%y_6, %v_3) = g(%y_6, %z_7)
    
      { sat= true;
        rep= [[%t_1 ↦ ];
              [%u_2 ↦ %t_1];
              [%v_3 ↦ %t_1];
              [%w_4 ↦ %t_1];
              [%x_5 ↦ %t_1];
              [%y_6 ↦ ];
              [%z_7 ↦ %t_1];
              [g(%y_6, %t_1) ↦ %t_1];
              [g(%y_6, %u_2) ↦ %t_1];
              [g(%y_6, %v_3) ↦ %t_1];
              [g(%y_6, %z_7) ↦ %t_1]];
        cls= [[%t_1
               ↦ {%u_2, %v_3, %w_4, %x_5, %z_7, g(%y_6, %t_1),
                  g(%y_6, %u_2), g(%y_6, %v_3), g(%y_6, %z_7)}]];
        use= [[%t_1 ↦ g(%y_6, %t_1)];
              [%u_2 ↦ g(%y_6, %u_2)];
              [%v_3 ↦ g(%y_6, %v_3)];
              [%y_6 ↦ g(%y_6, %t_1), g(%y_6, %u_2), g(%y_6, %v_3),
               g(%y_6, %z_7)];
              [%z_7 ↦ g(%y_6, %z_7)]] } |}]

    let%test _ = implies_eq r3 t z
    let%test _ = implies_eq r3 x z
    let%test _ = implies_eq (union r2 r3) x z

    let r4 = of_eqs [(w + !2, x - !3); (x - !5, y + !7); (y, z - !4)]

    let%expect_test _ =
      pp r4 ;
      pp_raw r4 ;
      [%expect
        {|
        (-4 + %z_7) = %y_6 ∧ (3 + %z_7) = %w_4 ∧ (8 + %z_7) = %x_5
    
      { sat= true;
        rep= [[%w_4 ↦ (3 + %z_7)];
              [%x_5 ↦ (8 + %z_7)];
              [%y_6 ↦ (-4 + %z_7)];
              [%z_7 ↦ ]];
        cls= [[(-4 + %z_7) ↦ {%y_6}];
              [(3 + %z_7) ↦ {%w_4}];
              [(8 + %z_7) ↦ {%x_5}]];
        use= [[%z_7 ↦ (-4 + %z_7), (3 + %z_7), (8 + %z_7)]] } |}]

    let%test _ = implies_eq r4 x (w + !5)
    let%test _ = difference r4 x w |> Poly.equal (Some (Z.of_int 5))

    let r5 = of_eqs [(x, y); (g w x, y); (g w y, f z)]

    let%test _ = Var.Set.equal (fv r5) (Var.Set.of_list [w_; x_; y_; z_])

    let r6 = of_eqs [(x, !1); (!1, y)]

    let%expect_test _ =
      pp r6 ;
      pp_raw r6 ;
      [%expect
        {|
        1 = %x_5 = %y_6
    
      { sat= true;
        rep= [[%x_5 ↦ 1]; [%y_6 ↦ 1]];
        cls= [[1 ↦ {%x_5, %y_6}]];
        use= [] } |}]

    let%test _ = implies_eq r6 x y

    let r7 = of_eqs [(v, x); (w, z); (y, z)]

    let%expect_test _ =
      pp r7 ;
      pp_raw r7 ;
      [%expect
        {|
          %v_3 = %x_5 ∧ %w_4 = %y_6 = %z_7
    
        { sat= true;
          rep= [[%v_3 ↦ ];
                [%w_4 ↦ ];
                [%x_5 ↦ %v_3];
                [%y_6 ↦ %w_4];
                [%z_7 ↦ %w_4]];
          cls= [[%v_3 ↦ {%x_5}]; [%w_4 ↦ {%y_6, %z_7}]];
          use= [] } |}]

    let r7' = add_eq x z r7

    let%expect_test _ =
      pp r7' ;
      pp_raw r7' ;
      [%expect
        {|
        %v_3 = %w_4 = %x_5 = %y_6 = %z_7
    
      { sat= true;
        rep= [[%v_3 ↦ ];
              [%w_4 ↦ %v_3];
              [%x_5 ↦ %v_3];
              [%y_6 ↦ %v_3];
              [%z_7 ↦ %v_3]];
        cls= [[%v_3 ↦ {%w_4, %x_5, %y_6, %z_7}]];
        use= [] } |}]

    let%test _ = normalize r7' w |> Term.equal v

    let%test _ =
      implies_eq (of_eqs [(g w x, g y z); (x, z)]) (g w x) (g w z)

    let%test _ =
      implies_eq (of_eqs [(g w x, g y w); (x, z)]) (g w x) (g w z)

    let r8 = of_eqs [(x + !42, (3 * y) + (13 * z)); (13 * z, x)]

    let%expect_test _ =
      pp r8 ;
      pp_raw r8 ;
      [%expect
        {|
        14 = %y_6 ∧ 13×%z_7 = %x_5
    
      { sat= true;
        rep= [[%x_5 ↦ 13×%z_7]; [%y_6 ↦ 14]; [%z_7 ↦ ]];
        cls= [[14 ↦ {%y_6}]; [13×%z_7 ↦ {%x_5}]];
        use= [[%z_7 ↦ 13×%z_7]] } |}]

    let%test _ = implies_eq r8 y !14

    let r9 = of_eqs [(x, z - !16)]

    let%expect_test _ =
      pp_raw r9 ;
      pp_raw r9 ;
      [%expect
        {|
      { sat= true;
        rep= [[%x_5 ↦ (-16 + %z_7)]; [%z_7 ↦ ]];
        cls= [[(-16 + %z_7) ↦ {%x_5}]];
        use= [[%z_7 ↦ (-16 + %z_7)]] }
    
      { sat= true;
        rep= [[%x_5 ↦ (-16 + %z_7)]; [%z_7 ↦ ]];
        cls= [[(-16 + %z_7) ↦ {%x_5}]];
        use= [[%z_7 ↦ (-16 + %z_7)]] } |}]

    let%test _ = difference r9 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let r10 = of_eqs [(!16, z - x)]

    let%expect_test _ =
      pp_raw r10 ;
      pp_raw r10 ;
      Format.printf "@.%a@." Term.pp (z - (x + !8)) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (z - (x + !8))) ;
      Format.printf "@.%a@." Term.pp (x + !8 - z) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (x + !8 - z)) ;
      [%expect
        {|
        { sat= true;
          rep= [[%x_5 ↦ (-16 + %z_7)]; [%z_7 ↦ ]];
          cls= [[(-16 + %z_7) ↦ {%x_5}]];
          use= [[%z_7 ↦ (-16 + %z_7)]] }
    
        { sat= true;
          rep= [[%x_5 ↦ (-16 + %z_7)]; [%z_7 ↦ ]];
          cls= [[(-16 + %z_7) ↦ {%x_5}]];
          use= [[%z_7 ↦ (-16 + %z_7)]] }
    
        (-8 + -1×%x_5 + %z_7)
    
        8
    
        (8 + %x_5 + -1×%z_7)
    
        -8 |}]

    let%test _ = difference r10 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let%test _ =
      difference r10 (x + !8) z |> Poly.equal (Some (Z.of_int (-8)))

    let r11 = of_eqs [(!16, z - x); (x + !8 - z, z - !16 + !8 - z)]

    let%expect_test _ =
      pp r11 ;
      [%expect {| (-16 + %z_7) = %x_5 |}]

    let r12 = of_eqs [(!16, z - x); (x + !8 - z, z + !16 + !8 - z)]

    let%expect_test _ =
      pp r12 ;
      [%expect {| (-16 + %z_7) = %x_5 |}]

    let r13 =
      of_eqs
        [ (Formula.inject (Formula.eq x !2), y)
        ; (Formula.inject (Formula.dq x !2), z)
        ; (y, z) ]

    let%expect_test _ =
      pp_raw r13 ;
      [%expect
        {|
          { sat= true;
            rep= [[%y_6 ↦ ]; [%z_7 ↦ %y_6]];
            cls= [[%y_6 ↦ {%z_7}]];
            use= [] } |}]

    let%test _ = not (is_unsat r13) (* incomplete *)

    let a = Formula.inject (Formula.dq x !0)
    let r14 = of_eqs [(a, a); (x, !1)]

    let%expect_test _ =
      pp_raw r14 ;
      [%expect
        {|
          { sat= true; rep= [[%x_5 ↦ 1]]; cls= [[1 ↦ {%x_5}]]; use= [] } |}]

    let%test _ = implies_eq r14 a (Formula.inject Formula.tt)

    let b = Formula.inject (Formula.dq y !0)
    let r14 = of_eqs [(a, b); (x, !1)]

    let%expect_test _ =
      pp_raw r14 ;
      [%expect
        {|
          { sat= true; rep= [[%x_5 ↦ 1]]; cls= [[1 ↦ {%x_5}]]; use= [] } |}]

    let%test _ = implies_eq r14 a (Formula.inject Formula.tt)
    (* incomplete *)
    let%test _ = not (implies_eq r14 b (Formula.inject Formula.tt))

    let b = Formula.inject (Formula.dq x !0)
    let r15 = of_eqs [(b, b); (x, !1)]

    let%expect_test _ =
      pp_raw r15 ;
      [%expect
        {|
          { sat= true; rep= [[%x_5 ↦ 1]]; cls= [[1 ↦ {%x_5}]]; use= [] } |}]

    (* f(x−1)−1=x+1, f(y)+1=y−1, y+1=x ⊢ false *)
    let r16 =
      of_eqs [(f (x - !1) - !1, x + !1); (f y + !1, y - !1); (y + !1, x)]

    let%expect_test _ =
      pp_raw r16 ;
      [%expect
        {|
      { sat= false;
        rep= [[%x_5 ↦ (1 + %y_6)];
              [%y_6 ↦ ];
              [f(%y_6) ↦ (-2 + %y_6)];
              [f((-1 + %x_5)) ↦ (3 + %y_6)]];
        cls= [[(-2 + %y_6) ↦ {f(%y_6)}];
              [(1 + %y_6) ↦ {%x_5}];
              [(3 + %y_6) ↦ {f((-1 + %x_5))}]];
        use= [[%x_5 ↦ f((-1 + %x_5))];
              [%y_6 ↦ (-2 + %y_6), (1 + %y_6), (3 + %y_6), f(%y_6)]] } |}]

    let%test _ = is_unsat r16

    (* f(x) = x, f(y) = y − 1, y = x ⊢ false *)
    let r17 = of_eqs [(f x, x); (f y, y - !1); (y, x)]

    let%expect_test _ =
      pp_raw r17 ;
      [%expect
        {|
      { sat= false;
        rep= [[%x_5 ↦ ];
              [%y_6 ↦ %x_5];
              [f(%x_5) ↦ %x_5];
              [f(%y_6) ↦ (-1 + %x_5)]];
        cls= [[%x_5 ↦ {%y_6, f(%x_5)}]; [(-1 + %x_5) ↦ {f(%y_6)}]];
        use= [[%x_5 ↦ (-1 + %x_5), f(%x_5)]; [%y_6 ↦ f(%y_6)]] } |}]

    let%test _ = is_unsat r17

    let%expect_test _ =
      let r18 = of_eqs [(f x, x); (f y, y - !1)] in
      pp_raw r18 ;
      pp r18 ;
      [%expect
        {|
        { sat= true;
          rep= [[%x_5 ↦ ];
                [%y_6 ↦ ];
                [f(%x_5) ↦ %x_5];
                [f(%y_6) ↦ (-1 + %y_6)]];
          cls= [[%x_5 ↦ {f(%x_5)}]; [(-1 + %y_6) ↦ {f(%y_6)}]];
          use= [[%x_5 ↦ f(%x_5)]; [%y_6 ↦ (-1 + %y_6), f(%y_6)]] }
    
          %x_5 = f(%x_5) ∧ (-1 + %y_6) = f(%y_6) |}]

    let r19 = of_eqs [(x, y + z); (x, !0); (y, !0)]

    let%expect_test _ =
      pp_raw r19 ;
      [%expect
        {|
          { sat= true;
            rep= [[%x_5 ↦ 0]; [%y_6 ↦ 0]; [%z_7 ↦ 0]];
            cls= [[0 ↦ {%x_5, %y_6, %z_7}]];
            use= [] } |}]

    let%test _ = implies_eq r19 x !0
    let%test _ = implies_eq r19 y !0
    let%test _ = implies_eq r19 z !0

    let r20 = of_eqs [(f x, t); (x, y); (f y, u)]

    let%expect_test _ =
      pp r20 ;
      pp_raw r20 ;
      [%expect
        {|
          %t_1 = %u_2 = f(%x_5) ∧ %x_5 = %y_6

        { sat= true;
          rep= [[%t_1 ↦ ];
                [%u_2 ↦ %t_1];
                [%x_5 ↦ ];
                [%y_6 ↦ %x_5];
                [f(%x_5) ↦ %t_1]];
          cls= [[%t_1 ↦ {%u_2, f(%x_5)}]; [%x_5 ↦ {%y_6}]];
          use= [[%x_5 ↦ f(%x_5)]] } |}]

    (* f(x) = t; x = y; f(y) = u ⊢ t = u *)
    let%test _ = implies_eq r20 t u

    let%expect_test _ =
      let f =
        Formula.(
          andN
            [ eq x y
            ; cond ~cnd:(eq x !0) ~pos:(eq z !2) ~neg:(eq z !3)
            ; or_ (eq w !4) (eq w !5) ])
      in
      printf Formula.pp f ;
      printf Formula.pp
        (Formula.orN (Iter.to_list (Iter.map ~f:snd3 (Context.dnf f)))) ;
      [%expect
        {|
        ((%x_5 = %y_6) ∧ ((5 = %w_4) ∨ (4 = %w_4))
          ∧ ((0 = %x_5) ? (2 = %z_7) : (3 = %z_7)))
    
        (((%x_5 = %y_6) ∧ (0 = %x_5) ∧ (5 = %w_4) ∧ (2 = %z_7))
          ∨ ((%x_5 = %y_6) ∧ (0 = %x_5) ∧ (4 = %w_4) ∧ (2 = %z_7))
          ∨ ((%x_5 = %y_6) ∧ (5 = %w_4) ∧ (3 = %z_7) ∧ (0 ≠ %x_5))
          ∨ ((%x_5 = %y_6) ∧ (4 = %w_4) ∧ (3 = %z_7) ∧ (0 ≠ %x_5))) |}]

    let%test "unsigned boolean overflow" =
      Formula.equal Formula.tt
        (Llair_to_Fol.formula
           Llair.(
             Exp.uge
               (Exp.integer Typ.bool Z.minus_one)
               (Exp.signed 1 (Exp.integer Typ.siz Z.one) ~to_:Typ.bool)))

    let pp_exp e =
      Format.printf "@\nexp= %a; term= %a@." Llair.Exp.pp e Term.pp
        (Llair_to_Fol.term e)

    let ( !! ) i = Llair.(Exp.integer Typ.siz (Z.of_int i))

    let%expect_test _ =
      pp_exp Llair.(Exp.signed 1 !!1 ~to_:Typ.bool) ;
      [%expect {| exp= ((i1)(s1) 1); term= -1 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.unsigned 1 !!(-1) ~to_:Typ.byt) ;
      [%expect {| exp= ((i8)(u1) -1); term= 1 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.signed 8 !!(-1) ~to_:Typ.int) ;
      [%expect {| exp= ((i32)(s8) -1); term= -1 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.unsigned 8 !!(-1) ~to_:Typ.int) ;
      [%expect {| exp= ((i32)(u8) -1); term= 255 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.signed 8 !!255 ~to_:Typ.byt) ;
      [%expect {| exp= ((i8)(s8) 255); term= -1 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.signed 7 !!255 ~to_:Typ.byt) ;
      [%expect {| exp= ((i8)(s7) 255); term= -1 |}]

    let%expect_test _ =
      pp_exp Llair.(Exp.unsigned 7 !!255 ~to_:Typ.byt) ;
      [%expect {| exp= ((i8)(u7) 255); term= 127 |}]

    let%expect_test _ =
      pp_exp
        Llair.(
          Exp.uge
            (Exp.integer Typ.bool Z.minus_one)
            (Exp.signed 1 !!1 ~to_:Typ.bool)) ;
      [%expect {| exp= (-1 u≥ ((i1)(s1) 1)); term= tt |}]
  end )
