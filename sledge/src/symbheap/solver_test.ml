(*
 * Copyright (c) 2019-present, Facebook, Inc.
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

    let check_frame p xs q =
      Solver.infer_frame p (Var.Set.of_list xs) q
      |> fun r -> assert (Option.is_some r)

    let ( ! ) i = Exp.integer (Z.of_int i) Typ.siz
    let ( + ) = Exp.add Typ.ptr
    let wrt = Var.Set.empty
    let a_, wrt = Var.fresh "a" ~wrt
    let a2_, wrt = Var.fresh "a" ~wrt
    let a3_, wrt = Var.fresh "a" ~wrt
    let b_, wrt = Var.fresh "b" ~wrt
    let l_, wrt = Var.fresh "l" ~wrt
    let l2_, wrt = Var.fresh "l" ~wrt
    let m_, wrt = Var.fresh "m" ~wrt
    let n_, _ = Var.fresh "n" ~wrt
    let a = Exp.var a_
    let a2 = Exp.var a2_
    let a3 = Exp.var a3_
    let b = Exp.var b_
    let l = Exp.var l_
    let l2 = Exp.var l2_
    let m = Exp.var m_
    let n = Exp.var n_

    let%expect_test _ =
      check_frame Sh.emp [] Sh.emp ;
      [%expect
        {|
        ( infer_frame: emp \- emp
        ) infer_frame: emp |}]

    let%expect_test _ =
      check_frame (Sh.false_ Var.Set.empty) [] Sh.emp ;
      [%expect
        {|
        ( infer_frame: emp * false \- emp
        ) infer_frame: emp * false |}]

    let%expect_test _ =
      check_frame
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a})
        [] Sh.emp ;
      [%expect
        {|
        ( infer_frame:   %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩ \- emp
        ) infer_frame:   %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩ |}]

    let%expect_test _ =
      check_frame
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a})
        []
        (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a}) ;
      [%expect
        {|
        ( infer_frame:
            %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩
          \-   %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩
        ) infer_frame: emp |}]

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
            %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩
          * %l_6 -[ %b_4, %m_7 )-> ⟨%n_8,%a_2⟩
          \-   %l_5 -[ %b_4, %m_7 )-> ⟨%n_8,%a_1⟩
        ) infer_frame:   %l_6 -[ %b_4, %m_7 )-> ⟨%n_8,%a_2⟩ |}]

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
            (%l_5 + 8) -[ %l_5, 16 )-> ⟨8,%a_2⟩
          * %l_5 -[ %l_5, 16 )-> ⟨8,%a_1⟩
          \- ∃ %a_3 .
              %l_5 -[)-> ⟨16,%a_3⟩
        ) infer_frame:
          ∃ %a1_6 .   ⟨16,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a1_6⟩ ∧ %a_2 = %a1_6 ∧ emp |}]

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
            (%l_5 + 8) -[ %l_5, 16 )-> ⟨8,%a_2⟩
          * %l_5 -[ %l_5, 16 )-> ⟨8,%a_1⟩
          \- ∃ %a_3, %m_7 .
              %l_5 -[ %l_5, %m_7 )-> ⟨16,%a_3⟩
        ) infer_frame:
          ∃ %a1_8 .
            ⟨16,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a1_8⟩
          ∧ %a_2 = %a1_8
          ∧ 16 = %m_7
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
            (%l_5 + 8) -[ %l_5, 16 )-> ⟨8,%a_2⟩
          * %l_5 -[ %l_5, 16 )-> ⟨8,%a_1⟩
          \- ∃ %a_3, %m_7 .
              %l_5 -[)-> ⟨%m_7,%a_3⟩
        ) infer_frame:
          ∃ %a1_8 .
            ⟨%m_7,%a_3⟩ = ⟨8,%a_1⟩^⟨8,%a1_8⟩
          ∧ %a_2 = %a1_8
          ∧ 16 = %m_7
          ∧ emp |}]
  end )
