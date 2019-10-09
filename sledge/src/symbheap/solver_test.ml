(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    (* let () = Trace.init ~margin:160 ~config:all () *)

    let () =
      Trace.init ~margin:68
        ~config:(Result.ok_exn (Trace.parse "+Solver.infer_frame"))
        ()

    let infer_frame p xs q =
      Solver.infer_frame p (Var.Set.of_list xs) q
      |> (ignore : Sh.t option -> _)

    let check_frame p xs q =
      Solver.infer_frame p (Var.Set.of_list xs) q
      |> fun r -> assert (Option.is_some r)

    let ( ! ) i = Term.integer (Z.of_int i)
    let ( + ) = Term.add
    let ( - ) = Term.sub
    let ( * ) = Term.mul
    let wrt = Var.Set.empty
    let a_, wrt = Var.fresh "a" ~wrt
    let a2_, wrt = Var.fresh "a" ~wrt
    let a3_, wrt = Var.fresh "a" ~wrt
    let b_, wrt = Var.fresh "b" ~wrt
    let k_, wrt = Var.fresh "k" ~wrt
    let l_, wrt = Var.fresh "l" ~wrt
    let l2_, wrt = Var.fresh "l" ~wrt
    let m_, wrt = Var.fresh "m" ~wrt
    let n_, _ = Var.fresh "n" ~wrt
    let a = Term.var a_
    let a2 = Term.var a2_
    let a3 = Term.var a3_
    let b = Term.var b_
    let k = Term.var k_
    let l = Term.var l_
    let l2 = Term.var l2_
    let m = Term.var m_
    let n = Term.var n_

    let%expect_test _ =
      check_frame Sh.emp [] Sh.emp ;
      [%expect
        {|
        ( infer_frame:   emp \-   emp
        ) infer_frame:   emp |}]

    let%expect_test _ =
      check_frame (Sh.false_ Var.Set.empty) [] Sh.emp ;
      [%expect
        {|
        ( infer_frame:   false \-   emp
        ) infer_frame:   false |}]

    let%expect_test _ =
      check_frame Sh.emp [n_; m_] (Sh.and_ (Term.eq m n) Sh.emp) ;
      [%expect
        {|
        ( infer_frame:   emp \- ∃ %m_8, %n_9 .   %m_8 = %n_9 ∧ emp
        ) infer_frame:   emp |}]

    let%expect_test _ =
      check_frame
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a})
        [] Sh.emp ;
      [%expect
        {|
        ( infer_frame:   %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩ \-   emp
        ) infer_frame:   %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩ |}]

    let%expect_test _ =
      check_frame
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a})
        []
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a}) ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩
          \-   %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩
        ) infer_frame:   emp |}]

    let%expect_test _ =
      let common = Sh.seg {loc= l2; bas= b; len= !10; siz= !10; arr= a2} in
      let seg1 = Sh.seg {loc= l; bas= b; len= !10; siz= !10; arr= a} in
      let minued = Sh.star common seg1 in
      let subtrahend =
        Sh.and_ (Term.eq m n)
          (Sh.exists
             (Var.Set.of_list [m_])
             (Sh.extend_us (Var.Set.of_list [m_]) common))
      in
      infer_frame minued [n_; m_] subtrahend ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %b_4, 10 )-> ⟨10,%a_1⟩ * %l_7 -[ %b_4, 10 )-> ⟨10,%a_2⟩
          \- ∃ %m_8, %n_9 .
            ∃ %m_10 .   %m_8 = %n_9 ∧ %l_7 -[ %b_4, 10 )-> ⟨10,%a_2⟩
        ) infer_frame: ∃ %m_10 .   %l_6 -[ %b_4, 10 )-> ⟨10,%a_1⟩ |}]

    let%expect_test _ =
      check_frame
        (Sh.star
           (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a})
           (Sh.seg {loc= l2; bas= b; len= m; siz= n; arr= a2}))
        []
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a}) ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩
          * %l_7 -[ %b_4, %m_8 )-> ⟨%n_9,%a_2⟩
          \-   %l_6 -[ %b_4, %m_8 )-> ⟨%n_9,%a_1⟩
        ) infer_frame:   %l_7 -[ %b_4, %m_8 )-> ⟨%n_9,%a_2⟩ |}]

    let%expect_test _ =
      check_frame
        (Sh.star
           (Sh.seg {loc= l; bas= l; len= !16; siz= !8; arr= a})
           (Sh.seg {loc= l + !8; bas= l; len= !16; siz= !8; arr= a2}))
        [a3_]
        (Sh.seg {loc= l; bas= l; len= !16; siz= !16; arr= a3}) ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %l_6, 16 )-> ⟨8,%a_1⟩
          * (%l_6 + 8) -[ %l_6, 16 )-> ⟨8,%a_2⟩
          \- ∃ %a_3 .
              %l_6 -[)-> ⟨16,%a_3⟩
        ) infer_frame:
          ∃ %a1_7 .   %a_2 = %a1_7 ∧ ⟨16,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a_2⟩ ∧ emp |}]

    let%expect_test _ =
      check_frame
        (Sh.star
           (Sh.seg {loc= l; bas= l; len= !16; siz= !8; arr= a})
           (Sh.seg {loc= l + !8; bas= l; len= !16; siz= !8; arr= a2}))
        [a3_; m_]
        (Sh.seg {loc= l; bas= l; len= m; siz= !16; arr= a3}) ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %l_6, 16 )-> ⟨8,%a_1⟩
          * (%l_6 + 8) -[ %l_6, 16 )-> ⟨8,%a_2⟩
          \- ∃ %a_3, %m_8 .
              %l_6 -[ %l_6, %m_8 )-> ⟨16,%a_3⟩
        ) infer_frame:
          ∃ %a1_9 .
            %a_2 = %a1_9
          ∧ ⟨16,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a_2⟩
          ∧ 16 = %m_8
          ∧ emp |}]

    let%expect_test _ =
      check_frame
        (Sh.star
           (Sh.seg {loc= l; bas= l; len= !16; siz= !8; arr= a})
           (Sh.seg {loc= l + !8; bas= l; len= !16; siz= !8; arr= a2}))
        [a3_; m_]
        (Sh.seg {loc= l; bas= l; len= m; siz= m; arr= a3}) ;
      [%expect
        {|
        ( infer_frame:
            %l_6 -[ %l_6, 16 )-> ⟨8,%a_1⟩
          * (%l_6 + 8) -[ %l_6, 16 )-> ⟨8,%a_2⟩
          \- ∃ %a_3, %m_8 .
              %l_6 -[)-> ⟨%m_8,%a_3⟩
        ) infer_frame:
          ∃ %a1_9 .
            %a_2 = %a1_9
          ∧ ⟨16,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a_2⟩
          ∧ 16 = %m_8
          ∧ emp |}]

    let%expect_test _ =
      check_frame
        (Sh.star
           (Sh.seg {loc= k; bas= k; len= !16; siz= !32; arr= a})
           (Sh.seg {loc= l; bas= l; len= !8; siz= !8; arr= !16}))
        [a2_; m_; n_]
        (Sh.star
           (Sh.seg {loc= l; bas= l; len= !8; siz= !8; arr= n})
           (Sh.seg {loc= k; bas= k; len= m; siz= n; arr= a2})) ;
      [%expect
        {|
        ( infer_frame:
            %k_5 -[ %k_5, 16 )-> ⟨32,%a_1⟩ * %l_6 -[)-> ⟨8,16⟩
          \- ∃ %a_2, %m_8, %n_9 .
              %k_5 -[ %k_5, %m_8 )-> ⟨%n_9,%a_2⟩ * %l_6 -[)-> ⟨8,%n_9⟩
        ) infer_frame:
          ∃ %a0_10, %a1_11 .
            %a_2 = %a0_10
          ∧ ⟨32,%a_1⟩ = ⟨%n_9,%a0_10⟩^⟨16,%a1_11⟩
          ∧ 16 = %m_8 = %n_9
          ∧ (%k_5 + 16) -[ %k_5, 16 )-> ⟨16,%a1_11⟩ |}]

    (* Incompleteness: changing the order of the subtrahend leads to
       different existential instantiation, which fails *)
    let%expect_test _ =
      infer_frame
        (Sh.star
           (Sh.seg {loc= k; bas= k; len= !16; siz= !32; arr= a})
           (Sh.seg {loc= l; bas= l; len= !8; siz= !8; arr= !16}))
        [a2_; m_; n_]
        (Sh.star
           (Sh.seg {loc= k; bas= k; len= m; siz= n; arr= a2})
           (Sh.seg {loc= l; bas= l; len= !8; siz= !8; arr= n})) ;
      [%expect
        {|
        ( infer_frame:
            %k_5 -[ %k_5, 16 )-> ⟨32,%a_1⟩ * %l_6 -[)-> ⟨8,16⟩
          \- ∃ %a_2, %m_8, %n_9 .
              %k_5 -[ %k_5, %m_8 )-> ⟨%n_9,%a_2⟩ * %l_6 -[)-> ⟨8,%n_9⟩
        ) infer_frame: |}]

    let seg_split_symbolically =
      Sh.star
        (Sh.seg {loc= l; bas= l; len= !16; siz= !8 * n; arr= a2})
        (Sh.seg
           { loc= l + (!8 * n)
           ; bas= l
           ; len= !16
           ; siz= !16 - (!8 * n)
           ; arr= a3 })

    let%expect_test _ =
      check_frame
        (Sh.and_
           Term.(or_ (or_ (eq n !0) (eq n !1)) (eq n !2))
           seg_split_symbolically)
        [m_; a_]
        (Sh.seg {loc= l; bas= l; len= m; siz= m; arr= a}) ;
      [%expect
        {|
        ( infer_frame:
            (%l_6 + 8 × %n_9) -[ %l_6, 16 )-> ⟨(-8 × %n_9 + 16),%a_3⟩
          * %l_6 -[ %l_6, 16 )-> ⟨(8 × %n_9),%a_2⟩
          * ( (  2 = %n_9 ∧ emp)
            ∨ (  0 = %n_9 ∧ emp)
            ∨ (  1 = %n_9 ∧ emp)
            )
          \- ∃ %a_1, %m_8 .
              %l_6 -[)-> ⟨%m_8,%a_1⟩
        ) infer_frame:
            ( (  %a_1 = %a_2
               ∧ 2 = %n_9
               ∧ 16 = %m_8
               ∧ (%l_6 + 16) -[ %l_6, 16 )-> ⟨0,%a_3⟩)
            ∨ (∃ %a1_10 .
                 %a_3 = %a1_10
               ∧ ⟨16,%a_1⟩ = ⟨8,%a_2⟩^⟨8,%a_3⟩
               ∧ 1 = %n_9
               ∧ 16 = %m_8
               ∧ emp)
            ∨ (∃ %a1_10 .
                 %a_3 = %a1_10
               ∧ ⟨16,%a_1⟩ = ⟨0,%a_2⟩^⟨16,%a_3⟩
               ∧ 0 = %n_9
               ∧ 16 = %m_8
               ∧ emp)
            ) |}]

    (* Incompleteness: equivalent to above but using ≤ instead of ∨ *)
    let%expect_test _ =
      infer_frame
        (Sh.and_ (Term.le n !2) seg_split_symbolically)
        [m_; a_]
        (Sh.seg {loc= l; bas= l; len= m; siz= m; arr= a}) ;
      [%expect
        {|
        ( infer_frame:
            (%n_9 ≤ 2)
          ∧ (%l_6 + 8 × %n_9) -[ %l_6, 16 )-> ⟨(-8 × %n_9 + 16),%a_3⟩
          * %l_6 -[ %l_6, 16 )-> ⟨(8 × %n_9),%a_2⟩
          \- ∃ %a_1, %m_8 .
              %l_6 -[)-> ⟨%m_8,%a_1⟩
        ) infer_frame: |}]
  end )
