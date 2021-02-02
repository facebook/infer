(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol

let%test_module _ =
  ( module struct
    open Context

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160
     *     ~config:
     *       (Result.get_ok
     *          (Trace.parse
     *             "+Context-Context.canon-Context.canon_f-Context.norm-Context.find_extend_"))
     *     () *)

    [@@@warning "-32"]

    let printf pp = Format.printf "@\n%a@." pp
    let pp = printf Context.pp_raw
    let pp_classes = Format.printf "@\n@[<hv>  %a@]@." Context.pp
    let ( ! ) i = Term.integer (Z.of_int i)
    let g x y = Term.apply (Uninterp "g") [|x; y|]
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

    let of_eqs l =
      List.fold
        ~f:(fun (a, b) (us, r) -> add us (Formula.eq a b) r)
        l (wrt, empty)
      |> snd

    let implies_eq r a b = implies r (Formula.eq a b)

    (* tests *)

    let r3 = of_eqs [(g y z, w); (v, w); (g y w, t); (x, v); (x, u); (u, z)]

    let%expect_test _ =
      pp_classes r3 ;
      pp r3 ;
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

    let b = Formula.inject (Formula.dq x !0)
    let r15 = of_eqs [(b, b); (x, !1)]

    let%expect_test _ =
      pp r15 ;
      [%expect
        {|
          { sat= true; rep= [[%x_5 ↦ 1]]; cls= [[1 ↦ {%x_5}]]; use= [] } |}]

    let%test _ = implies_eq r15 (Term.neg b) (Term.apply (Signed 1) [|!1|])
    let%test _ = implies_eq r15 (Term.apply (Unsigned 1) [|b|]) !1

    let%expect_test _ =
      replay
        {|(Dnf
            (Eq (Sized (seq (Var (id 1) (name a))) (siz (Z 8)))
              (Concat
                ((Sized (seq (Var (id 3) (name c))) (siz (Z 4)))
                  (Sized (seq (Var (id 2) (name b))) (siz (Z 4)))))))|} ;
      [%expect {| |}]

    let%expect_test _ =
      replay
        {|(Union
            ((Var (id 1) (name a)) (Var (id 2) (name a)) (Var (id 3) (name a))
              (Var (id 6) (name l)) (Var (id 7) (name a1)))
            ((xs ()) (sat true)
              (rep
                (((Var (id 7) (name a1)) (Var (id 2) (name a)))
                  ((Var (id 2) (name a)) (Var (id 2) (name a)))))
              (cls (((Var (id 2) (name a)) ((Var (id 7) (name a1)))))) (use ())
              (pnd ()))
            ((xs ()) (sat true)
              (rep
                (((Var (id 7) (name a1)) (Var (id 7) (name a1)))
                  ((Var (id 3) (name a))
                    (Concat
                      ((Sized (seq (Var (id 1) (name a))) (siz (Z 8)))
                        (Sized (seq (Var (id 7) (name a1))) (siz (Z 8))))))
                  ((Var (id 1) (name a)) (Var (id 1) (name a)))))
              (cls
                (((Concat
                    ((Sized (seq (Var (id 1) (name a))) (siz (Z 8)))
                      (Sized (seq (Var (id 7) (name a1))) (siz (Z 8)))))
                   ((Var (id 3) (name a))))))
              (use
                (((Var (id 7) (name a1))
                   ((Concat
                      ((Sized (seq (Var (id 1) (name a))) (siz (Z 8)))
                        (Sized (seq (Var (id 7) (name a1))) (siz (Z 8)))))))
                  ((Var (id 1) (name a))
                    ((Concat
                       ((Sized (seq (Var (id 1) (name a))) (siz (Z 8)))
                         (Sized (seq (Var (id 7) (name a1))) (siz (Z 8)))))))))
              (pnd ())))|} ;
      [%expect {| |}]

    let%expect_test _ =
      replay
        {|(Union
            ((Var (id 0) (name 1)) (Var (id 0) (name 4)) (Var (id 0) (name 6))
              (Var (id 0) (name 8)) (Var (id 0) (name freturn))
              (Var (id 1) (name freturn)))
            ((xs ()) (sat true)
              (rep
                (((Apply (Signed 8) ((Var (id 0) (name 8))))
                   (Var (id 0) (name freturn)))
                  ((Var (id 0) (name freturn)) (Var (id 0) (name freturn)))
                  ((Var (id 0) (name 8)) (Var (id 0) (name 8)))))
              (cls
                (((Var (id 0) (name freturn))
                   ((Apply (Signed 8) ((Var (id 0) (name 8))))))))
              (use
                (((Var (id 0) (name 8)) ((Apply (Signed 8) ((Var (id 0) (name 8))))))))
              (pnd ()))
            ((xs ()) (sat true)
              (rep
                (((Apply (Uninterp .str) ()) (Var (id 0) (name 6)))
                  ((Var (id 0) (name 8)) (Z 73))
                  ((Var (id 0) (name 6)) (Var (id 0) (name 6)))))
              (cls
                (((Z 73) ((Var (id 0) (name 8))))
                  ((Var (id 0) (name 6)) ((Apply (Uninterp .str) ())))))
              (use ()) (pnd ())))|} ;
      [%expect {||}]

    let%expect_test _ =
      replay
        {|(Apply_and_elim
            ((Var (id 0) (name 12)) (Var (id 0) (name 16)) (Var (id 0) (name 19))
              (Var (id 0) (name 22)) (Var (id 0) (name 23)) (Var (id 0) (name 26))
              (Var (id 0) (name 29)) (Var (id 0) (name 33)) (Var (id 0) (name 35))
              (Var (id 0) (name 39)) (Var (id 0) (name 4)) (Var (id 0) (name 40))
              (Var (id 0) (name 41)) (Var (id 0) (name 47)) (Var (id 0) (name 5))
              (Var (id 0) (name 52)) (Var (id 0) (name 8)) (Var (id 1) (name m))
              (Var (id 2) (name a)) (Var (id 6) (name b)))
            ((Var (id 1) (name m)) (Var (id 2) (name a)) (Var (id 6) (name b)))
            (((Var (id 6) (name b)) (Var (id 2) (name a)))
              ((Var (id 1) (name m))
                (Apply (Signed 32)
                  ((Apply (Signed 32) ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))))))
            ((xs ()) (sat true)
              (rep
                (((Apply (Signed 32)
                    ((Apply (Signed 32) ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))))
                   (Var (id 1) (name m)))
                  ((Apply (Signed 32) ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))
                    (Apply (Signed 32) ((Arith (((((Var (id 0) (name 23)) 1)) 4))))))
                  ((Apply (Signed 32) ((Var (id 0) (name 5))))
                    (Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1))))
                  ((Var (id 6) (name b)) (Var (id 2) (name a)))
                  ((Var (id 2) (name a)) (Var (id 2) (name a)))
                  ((Var (id 1) (name m)) (Var (id 1) (name m)))
                  ((Var (id 0) (name 8))
                    (Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1))))
                  ((Var (id 0) (name 52)) (Var (id 0) (name 26)))
                  ((Var (id 0) (name 5)) (Var (id 0) (name 5)))
                  ((Var (id 0) (name 35)) (Var (id 0) (name 23)))
                  ((Var (id 0) (name 33)) (Z 0))
                  ((Var (id 0) (name 26)) (Var (id 0) (name 26)))
                  ((Var (id 0) (name 23)) (Var (id 0) (name 23)))
                  ((Var (id 0) (name 22)) (Var (id 0) (name 22)))
                  ((Var (id 0) (name 19))
                    (Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1))))
                  ((Var (id 0) (name 16))
                    (Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1))))))
              (cls
                (((Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1)))
                   ((Var (id 0) (name 16)) (Var (id 0) (name 8))
                     (Var (id 0) (name 19))
                     (Apply (Signed 32) ((Var (id 0) (name 5))))))
                  ((Z 0) ((Var (id 0) (name 33))))
                  ((Var (id 2) (name a)) ((Var (id 6) (name b))))
                  ((Var (id 1) (name m))
                    ((Apply (Signed 32)
                       ((Apply (Signed 32)
                          ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))))))
                  ((Var (id 0) (name 26)) ((Var (id 0) (name 52))))
                  ((Var (id 0) (name 23)) ((Var (id 0) (name 35))))))
              (use
                (((Apply (Signed 32) ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))
                   ((Apply (Signed 32)
                      ((Apply (Signed 32)
                         ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))))))
                  ((Var (id 0) (name 5))
                    ((Apply (Signed 32) ((Var (id 0) (name 5))))))
                  ((Var (id 0) (name 23))
                    ((Arith ((() -1) ((((Var (id 0) (name 23)) 1)) 1)))
                      (Apply (Signed 32)
                        ((Arith (((((Var (id 0) (name 23)) 1)) 4)))))))))
              (pnd ())))|} ;
      [%expect {| |}]

    let%expect_test _ =
      replay
        {|(Add () (Eq (Var (id 2) (name u)) (Var (id 5) (name x)))
            ((xs ()) (sat true)
              (rep
                (((Apply (Uninterp g) ((Var (id 6) (name y)) (Var (id 7) (name z))))
                   (Var (id 3) (name v)))
                  ((Apply (Uninterp g) ((Var (id 6) (name y)) (Var (id 3) (name v))))
                    (Var (id 1) (name t)))
                  ((Var (id 7) (name z)) (Var (id 7) (name z)))
                  ((Var (id 6) (name y)) (Var (id 6) (name y)))
                  ((Var (id 5) (name x)) (Var (id 3) (name v)))
                  ((Var (id 4) (name w)) (Var (id 3) (name v)))
                  ((Var (id 3) (name v)) (Var (id 3) (name v)))
                  ((Var (id 1) (name t)) (Var (id 1) (name t)))))
              (cls
                (((Var (id 3) (name v))
                   ((Var (id 5) (name x))
                     (Apply (Uninterp g)
                       ((Var (id 6) (name y)) (Var (id 7) (name z))))
                     (Var (id 4) (name w))))
                  ((Var (id 1) (name t))
                    ((Apply (Uninterp g)
                       ((Var (id 6) (name y)) (Var (id 3) (name v))))))))
              (use
                (((Var (id 7) (name z))
                   ((Apply (Uninterp g)
                      ((Var (id 6) (name y)) (Var (id 7) (name z))))))
                  ((Var (id 6) (name y))
                    ((Apply (Uninterp g)
                       ((Var (id 6) (name y)) (Var (id 3) (name v))))
                      (Apply (Uninterp g)
                        ((Var (id 6) (name y)) (Var (id 7) (name z))))))
                  ((Var (id 3) (name v))
                    ((Apply (Uninterp g)
                       ((Var (id 6) (name y)) (Var (id 3) (name v))))))))
              (pnd ())))|} ;
      [%expect {| |}]

    (* let%expect_test _ =
     *   replay
     *     {||} ;
     *   [%expect {| |}] *)
  end )
