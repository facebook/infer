(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Sledge
open Fol

let%test_module _ =
  ( module struct
    open Sh

    let () = Dbg.init ~margin:68 ()

    (* let () =
     *   Dbg.init ~margin:160
     *     ~config:
     *       (Dbg.parse
     *          "+Sh.simplify+Sh.simplify_+Sh.norm+Sh.and_subst+Context.solve_and_elim+Context.partition_valid+Context.solve_for_vars+Context.apply_and_elim+Context.apply_subst+Context.elim" )
     *     () *)

    [@@@warning "-unused-value-declaration"]

    let pp = Format.printf "@\n%a@." pp
    let pp_raw = Format.printf "@\n%a@." pp_raw
    let pp_djn = Format.printf "@\n%a@." pp_djn
    let ( ~$ ) = Var.Set.of_list
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( + ) = Term.add
    let ( - ) = Term.sub
    let ( = ) = Formula.eq
    let f x = Term.apply (Uninterp "f") [|x|]
    let wrt = Var.Set.empty
    let a_, wrt = Var.fresh "a" ~wrt
    let b_, wrt = Var.fresh "b" ~wrt
    let c_, wrt = Var.fresh "c" ~wrt
    let d_, wrt = Var.fresh "d" ~wrt
    let e_, wrt = Var.fresh "e" ~wrt
    let m_, wrt = Var.fresh "m" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, wrt = Var.fresh "z" ~wrt
    let _ = wrt
    let a = Term.var a_
    let b = Term.var b_
    let c = Term.var c_
    let d = Term.var d_
    let e = Term.var e_
    let m = Term.var m_
    let x = Term.var x_
    let y = Term.var y_
    let z = Term.var z_

    let eq_concat (siz, seq) xs =
      let ys, len =
        Array.fold_map xs Term.zero ~f:(fun (siz, seq) len ->
            ({Term.siz; seq}, Term.add siz len) )
      in
      Formula.and_ (Formula.eq siz len) (Formula.eq seq (Term.concat ys))

    let of_eqs l =
      List.fold ~f:(fun (a, b) q -> and_ (Formula.eq a b) q) l emp

    let%expect_test _ =
      pp
        (star
           (seg {loc= x; bas= x; len= !16; siz= !8; cnt= a})
           (seg {loc= x + !8; bas= x; len= !16; siz= !8; cnt= b}) ) ;
      [%expect {|
          %x_7 -[)-> ⟨8,%a_1⟩^⟨8,%b_2⟩ |}]

    let%expect_test _ =
      let p = exists ~$[x_] (extend_us ~$[x_] emp) in
      let q = pure (x = !0) in
      pp p ;
      pp q ;
      pp (star p q) ;
      [%expect
        {|
        ∃ %x_7 .   emp
    
          0 = %x_7 ∧ emp
    
          0 = %x_7 ∧ emp |}]

    let%expect_test _ =
      let q =
        or_
          (pure (x = !0))
          (exists ~$[x_]
             (or_
                (and_ (x = !1) (pure (y = !1)))
                (exists ~$[x_] (pure (x = !2))) ) )
      in
      pp q ;
      pp_djn (dnf q) ;
      [%expect
        {|
          ( (  0 = %x_7 ∧ emp) ∨ (  ( (  emp) ∨ (  1 = %y_8 ∧ emp) )) )
    
        ( (  0 = %x_7 ∧ (0 = %x_7) ∧ emp)
        ∨ (∃ %x_7, %x_8 .   2 = %x_8 ∧ (2 = %x_8) ∧ emp)
        ∨ (∃ %x_7 .   1 = %x_7 = %y_8 ∧ ((1 = %x_7) ∧ (1 = %y_8)) ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        exists ~$[x_]
          (or_
             (pure (x = !0))
             (exists ~$[x_]
                (or_
                   (and_ (x = !1) (pure (y = !1)))
                   (exists ~$[x_] (pure (x = !2))) ) ) )
      in
      pp q ;
      pp_djn (dnf q) ;
      [%expect
        {|
          ( (  emp) ∨ (  ( (  emp) ∨ (  1 = %y_8 ∧ emp) )) )
    
        ( (∃ %x_7 .   0 = %x_7 ∧ (0 = %x_7) ∧ emp)
        ∨ (∃ %x_7, %x_9, %x_10 .   2 = %x_10 ∧ (2 = %x_10) ∧ emp)
        ∨ (∃ %x_7, %x_9 .
             1 = %y_8 = %x_9 ∧ ((1 = %y_8) ∧ (1 = %x_9))
           ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        exists ~$[x_]
          (or_
             (pure (x = !0))
             (exists ~$[x_]
                (or_
                   (and_ (x = !1) (pure (y = !1)))
                   (exists ~$[x_] (pure (x = !2))) ) ) )
      in
      pp q ;
      pp (simplify q) ;
      [%expect
        {|
        ( (  emp) ∨ (  ( (  emp) ∨ (  1 = %y_8 ∧ emp) )) )
    
        ( (  emp) ∨ (  emp) ∨ (  1 = %y_8 ∧ emp) ) |}]

    let%expect_test _ =
      let q = exists ~$[x_] (of_eqs [(f x, x); (f y, y - !1)]) in
      pp q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %x_7 .   %x_7 = f(%x_7) ∧ (%y_8 - 1) = f(%y_8) ∧ emp
    
          (%y_8 - 1) = f(%y_8) ∧ ((f(%y_8) + 1) = %y_8) ∧ emp
    
          (%y_8 - 1) = f(%y_8) ∧ emp |}]

    let%expect_test _ =
      let q =
        exists
          ~$[a_; c_; d_; e_]
          (star
             (pure (eq_concat (!16, e) [|(!8, a); (!8, d)|]))
             (or_
                (pure (Formula.dq x !0))
                (exists (Var.Set.of_list [b_])
                   (pure (eq_concat (!8, a) [|(!4, c); (!4, b)|])) ) ) )
      in
      pp_raw q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %a_1, %c_3, %d_4, %e_5 .
          (⟨8,%a_1⟩^⟨8,%d_4⟩) = %e_5 ∧ (%e_5 = (⟨8,%a_1⟩^⟨8,%d_4⟩))
        ∧ emp
        * ( (∃ %b_2 .
               (⟨4,%c_3⟩^⟨4,%b_2⟩) = %a_1 ∧ (%a_1 = (⟨4,%c_3⟩^⟨4,%b_2⟩))
             ∧ emp)
          ∨ (  tt ∧ (0 ≠ %x_7) ∧ emp)
          )
    
          tt ∧ tt
        ∧ emp
        * ( (  tt ∧ tt ∧ emp) ∨ (  tt ∧ (0 ≠ %x_7) ∧ emp) )
    
          ( (  emp) ∨ (  (0 ≠ %x_7) ∧ emp) ) |}]

    let%expect_test _ =
      let q =
        exists
          ~$[b_; m_]
          (star
             (seg {loc= x; bas= b; len= m; siz= !8; cnt= !0})
             (or_ (of_eqs [(b, y); (m, c)]) (of_eqs [(b, z); (m, c)])) )
      in
      pp_raw q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %b_2, %m_6 .
          tt ∧ tt
        ∧ %x_7 -[ %b_2, %m_6 )-> ⟨8,0⟩
        * ( (  %b_2 = %y_8 ∧ %c_3 = %m_6
             ∧ ((%b_2 = %y_8) ∧ (%c_3 = %m_6))
             ∧ emp)
          ∨ (  %b_2 = %z_9 ∧ %c_3 = %m_6
             ∧ ((%b_2 = %z_9) ∧ (%c_3 = %m_6))
             ∧ emp)
          )
    
        ∃ %b_2 .
          tt ∧ tt
        ∧ %x_7 -[ %b_2, %c_3 )-> ⟨8,0⟩
        * ( (  %b_2 = %y_8 ∧ (%b_2 = %y_8) ∧ emp)
          ∨ (  %b_2 = %z_9 ∧ (%b_2 = %z_9) ∧ emp)
          )
    
        ∃ %b_2 .
          %x_7 -[ %b_2, %c_3 )-> ⟨8,0⟩
        * ( (  %b_2 = %y_8 ∧ emp) ∨ (  %b_2 = %z_9 ∧ emp) ) |}]

    (* ( Sh.freshen_xs:
     *   {%1!1, %2!2, %3!3, %4!4, %5!5, %7!6, %8!7, %9!8, %10!9, %12!10,
     *    %13!11, %15!12, %16!13, %17!14, %18!15, %19!16, %20!17, %30!18,
     *    %freturn!19, %4!25, %0!27, %5!28, %.0!30, %freturn!31, %0!32,
     *    %5!33, %6!34, %.0!35, %freturn!36, %0!40, %5!41, %9!42, %.0!43,
     *    %freturn!44, %0!45, %5!46, %9!47, %.0!48, %freturn!49, %0!56,
     *    %2!57, %5!58, %6!59, %.frozen!60, %.frozen1!61, %7!62,
     *    %.decomposed!63, %11!64, %13!65, %19!66, %freturn_77,
     *    %freturn_78, %freturn_79, %b_80, %m_81, %a_82}
     *   ∃ %17!14 .
     *     ( (∃ %b_83 .
     *          %4!4 = %5!5 = %0!27 = %0!32 = %0!40 = %0!45 = %0!56
     *        ∧ %10!9 = %13!11
     *        ∧ %b_80 = %3!3 = %15!12
     *        ∧ %a_82 = %b_83
     *        ∧ 16 = %m_81
     *        ∧ (%8!7 - 8) = %2!2 = %7!6
     *        ∧ (%b_80 + 8) = %16!13
     *        ∧ ((100 > (u32)((%5!28 - 1))) ∧ (0 ≠ %4!4))
     *        ∧ %1!1 -[)-> ⟨4,%10!9⟩
     *        * %4!4
     *            -[)->
     *            ⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩
     *            ^⟨1,0⟩^⟨3,_⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,5⟩^⟨4,5⟩
     *            ^⟨4,5⟩
     *        * %9!8 -[)-> ⟨4,1⟩
     *        * %12!10 -[)-> ⟨4,2⟩
     *        * %4!25 -[)-> ⟨4,4⟩
     *        * (%b_80 + 8) -[ %b_80, 16 )-> ⟨8,%4!25⟩
     *        * %a_82 -[)-> ⟨4,3⟩
     *        * (%8!7 - 8) -[)-> ⟨8,%9!8⟩^⟨8,%12!10⟩)
     *     ∨ (∃ %m_84 .
     *          %4!4 = %5!5 = %0!27 = %0!32 = %0!40 = %0!45 = %0!56
     *        ∧ %10!9 = %13!11
     *        ∧ %b_80 = %3!3 = %15!12
     *        ∧ %a_82 = %m_84
     *        ∧ 16 = %m_81
     *        ∧ (%8!7 - 8) = %2!2 = %7!6
     *        ∧ (%b_80 + 8) = %16!13
     *        ∧ ((100 > (u32)((%5!28 - 1))) ∧ (0 ≠ %4!4))
     *        ∧ %1!1 -[)-> ⟨4,%10!9⟩
     *        * %4!4
     *            -[)->
     *            ⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩
     *            ^⟨1,0⟩^⟨3,_⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,5⟩^⟨4,5⟩
     *            ^⟨4,5⟩
     *        * %9!8 -[)-> ⟨4,1⟩
     *        * %12!10 -[)-> ⟨4,2⟩
     *        * %4!25 -[)-> ⟨4,4⟩
     *        * (%b_80 + 8) -[ %b_80, 16 )-> ⟨8,%4!25⟩
     *        * %a_82 -[)-> ⟨4,3⟩
     *        * (%8!7 - 8) -[)-> ⟨8,%9!8⟩^⟨8,%12!10⟩)
     *     )
     * ) Sh.freshen_xs: [%17!14 ↦ %17_83]
     *   ∃ %17_83, %b_84 .
     *     %4!4 = %5!5 = %0!27 = %0!32 = %0!40 = %0!45 = %0!56
     *   ∧ %10!9 = %13!11
     *   ∧ %b_80 = %3!3 = %15!12
     *   ∧ %a_82 = %b_84
     *   ∧ 16 = %m_81
     *   ∧ (%8!7 - 8) = %2!2 = %7!6
     *   ∧ (%b_80 + 8) = %16!13
     *   ∧ ((100 > (u32)((%5!28 - 1))) ∧ (0 ≠ %4!4))
     *   ∧ %1!1 -[)-> ⟨4,%10!9⟩
     *   * %4!4
     *       -[)->
     *       ⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩^⟨1,0⟩
     *       ^⟨3,_⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,0⟩^⟨4,5⟩^⟨4,5⟩^⟨4,5⟩
     *   * %9!8 -[)-> ⟨4,1⟩
     *   * %12!10 -[)-> ⟨4,2⟩
     *   * %4!25 -[)-> ⟨4,4⟩
     *   * (%b_80 + 8) -[ %b_80, 16 )-> ⟨8,%4!25⟩
     *   * %a_82 -[)-> ⟨4,3⟩
     *   * (%8!7 - 8) -[)-> ⟨8,%9!8⟩^⟨8,%12!10⟩ *)
    let%expect_test _ =
      replay
        {|
        (Freshen_xs
         ((heap ())
          (djns
           ((((heap
               (((loc (Trm (Var (id -4611686018427387902) (name 1)))) (bas (Trm (Var (id -4611686018427387902) (name 1)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Var (id -4611686018427387894) (name 10)))))
                ((loc (Trm (Var (id -4611686018427387899) (name 4)))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Var (id -4611686018427387897) (name 7)))) (bas (Trm (Var (id -4611686018427387901) (name 2)))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387895) (name 9)))))
                ((loc (Trm (Var (id -4611686018427387896) (name 8)))) (bas (Trm (Var (id -4611686018427387901) (name 2)))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387893) (name 12)))))
                ((loc (Trm (Var (id -4611686018427387895) (name 9)))) (bas (Trm (Var (id -4611686018427387895) (name 9)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 1))))
                ((loc (Trm (Var (id -4611686018427387893) (name 12)))) (bas (Trm (Var (id -4611686018427387893) (name 12)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 2))))
                ((loc (Trm (Var (id -4611686018427387890) (name 16)))) (bas (Trm (Arith ((() -8) ((((Var (id -4611686018427387890) (name 16)) 1)) 1))))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387878) (name 4)))))
                ((loc (Trm (Var (id -4611686018427387878) (name 4)))) (bas (Trm (Var (id -4611686018427387878) (name 4)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 4))))
                ((loc (Trm (Var (id 83) (name b)))) (bas (Trm (Var (id 83) (name b)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 3))))
                ((loc (Trm (Arith ((() 4) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 8) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 12) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 16) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 20) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 21) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 22) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 23) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 24) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 25) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 3))) (cnt (Trm (Var (id 71) (name a1)))))
                ((loc (Trm (Arith ((() 28) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 32) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 36) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 40) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 44) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 48) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))
                ((loc (Trm (Arith ((() 52) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))
                ((loc (Trm (Arith ((() 56) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))))
              (djns ())
              (pure
               (And
                (pos
                 ((Eq (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387897) (name 7))) (Eq (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387891) (name 15)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387876) (name 0)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387871) (name 0))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387863) (name 0)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387858) (name 0))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387847) (name 0)))
                  (Eq (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387892) (name 13))) (Eq (Var (id 82) (name a)) (Var (id 83) (name b))) (Eq0 (Arith ((() -16) ((((Var (id 81) (name m)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387901) (name 2)) 1)) -1) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387900) (name 3)) 1)) -1) ((((Var (id -4611686018427387890) (name 16)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387890) (name 16)) 1)) 1) ((((Var (id 80) (name b)) 1)) -1))))
                  (Pos (Arith ((() 100) ((((Apply (Unsigned 32) ((Arith ((() -1) ((((Var (id -4611686018427387875) (name 5)) 1)) 1))))) 1)) -1))))))
                (neg ((Eq0 (Var (id -4611686018427387899) (name 4)))))))
              (xs ((Var (id 71) (name a1)) (Var (id 83) (name b))))
              (us
               ((Var (id -4611686018427387902) (name 1)) (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))
                (Var (id -4611686018427387897) (name 7)) (Var (id -4611686018427387896) (name 8)) (Var (id -4611686018427387895) (name 9)) (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387893) (name 12))
                (Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387891) (name 15)) (Var (id -4611686018427387890) (name 16)) (Var (id -4611686018427387889) (name 17)) (Var (id -4611686018427387888) (name 18))
                (Var (id -4611686018427387887) (name 19)) (Var (id -4611686018427387886) (name 20)) (Var (id -4611686018427387885) (name 30)) (Var (id -4611686018427387884) (name freturn)) (Var (id -4611686018427387878) (name 4))
                (Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387875) (name 5)) (Var (id -4611686018427387873) (name .0)) (Var (id -4611686018427387872) (name freturn)) (Var (id -4611686018427387871) (name 0))
                (Var (id -4611686018427387870) (name 5)) (Var (id -4611686018427387869) (name 6)) (Var (id -4611686018427387868) (name .0)) (Var (id -4611686018427387867) (name freturn)) (Var (id -4611686018427387863) (name 0))
                (Var (id -4611686018427387862) (name 5)) (Var (id -4611686018427387861) (name 9)) (Var (id -4611686018427387860) (name .0)) (Var (id -4611686018427387859) (name freturn)) (Var (id -4611686018427387858) (name 0))
                (Var (id -4611686018427387857) (name 5)) (Var (id -4611686018427387856) (name 9)) (Var (id -4611686018427387855) (name .0)) (Var (id -4611686018427387854) (name freturn)) (Var (id -4611686018427387847) (name 0))
                (Var (id -4611686018427387846) (name 2)) (Var (id -4611686018427387845) (name 5)) (Var (id -4611686018427387844) (name 6)) (Var (id -4611686018427387843) (name .frozen)) (Var (id -4611686018427387842) (name .frozen1))
                (Var (id -4611686018427387841) (name 7)) (Var (id -4611686018427387840) (name .decomposed)) (Var (id -4611686018427387839) (name 11)) (Var (id -4611686018427387838) (name 13)) (Var (id -4611686018427387837) (name 19))
                (Var (id 77) (name freturn)) (Var (id 78) (name freturn)) (Var (id 79) (name freturn)) (Var (id 80) (name b)) (Var (id 81) (name m)) (Var (id 82) (name a))))
              (ctx
               ((xs ()) (sat true)
                (rep
                 (((Var (id -4611686018427387901) (name 2)) (Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1)))) ((Var (id -4611686018427387900) (name 3)) (Var (id 80) (name b)))
                  ((Var (id -4611686018427387898) (name 5)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387897) (name 7)) (Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))
                  ((Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387894) (name 10))) ((Var (id -4611686018427387891) (name 15)) (Var (id 80) (name b)))
                  ((Var (id -4611686018427387890) (name 16)) (Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1)))) ((Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387899) (name 4)))
                  ((Var (id -4611686018427387871) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387863) (name 0)) (Var (id -4611686018427387899) (name 4)))
                  ((Var (id -4611686018427387858) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387847) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id 81) (name m)) (Z 16))
                  ((Var (id 83) (name b)) (Var (id 82) (name a)))))
                (cls
                 (((Var (id -4611686018427387899) (name 4))
                   ((Var (id -4611686018427387898) (name 5)) (Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387871) (name 0)) (Var (id -4611686018427387863) (name 0)) (Var (id -4611686018427387858) (name 0))
                    (Var (id -4611686018427387847) (name 0))))
                  ((Var (id -4611686018427387894) (name 10)) ((Var (id -4611686018427387892) (name 13)))) ((Var (id 80) (name b)) ((Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387891) (name 15))))
                  ((Var (id 82) (name a)) ((Var (id 83) (name b)))) ((Z 16) ((Var (id 81) (name m))))
                  ((Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))) ((Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387897) (name 7))))
                  ((Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1))) ((Var (id -4611686018427387890) (name 16))))))
                (use (((Var (id -4611686018427387896) (name 8)) ((Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))) ((Var (id 80) (name b)) ((Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1))))))) (pnd ()))))
             ((heap
               (((loc (Trm (Var (id -4611686018427387902) (name 1)))) (bas (Trm (Var (id -4611686018427387902) (name 1)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Var (id -4611686018427387894) (name 10)))))
                ((loc (Trm (Var (id -4611686018427387899) (name 4)))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Var (id -4611686018427387897) (name 7)))) (bas (Trm (Var (id -4611686018427387901) (name 2)))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387895) (name 9)))))
                ((loc (Trm (Var (id -4611686018427387896) (name 8)))) (bas (Trm (Var (id -4611686018427387901) (name 2)))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387893) (name 12)))))
                ((loc (Trm (Var (id -4611686018427387895) (name 9)))) (bas (Trm (Var (id -4611686018427387895) (name 9)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 1))))
                ((loc (Trm (Var (id -4611686018427387893) (name 12)))) (bas (Trm (Var (id -4611686018427387893) (name 12)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 2))))
                ((loc (Trm (Var (id -4611686018427387890) (name 16)))) (bas (Trm (Arith ((() -8) ((((Var (id -4611686018427387890) (name 16)) 1)) 1))))) (len (Trm (Z 16))) (siz (Trm (Z 8))) (cnt (Trm (Var (id -4611686018427387878) (name 4)))))
                ((loc (Trm (Var (id -4611686018427387878) (name 4)))) (bas (Trm (Var (id -4611686018427387878) (name 4)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 4))))
                ((loc (Trm (Var (id 84) (name m)))) (bas (Trm (Var (id 84) (name m)))) (len (Trm (Z 4))) (siz (Trm (Z 4))) (cnt (Trm (Z 3))))
                ((loc (Trm (Arith ((() 4) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 8) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 12) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 16) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 20) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 21) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 22) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 23) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 24) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 1))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 25) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 3))) (cnt (Trm (Var (id 71) (name a1)))))
                ((loc (Trm (Arith ((() 28) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 32) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 36) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 40) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 44) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 0))))
                ((loc (Trm (Arith ((() 48) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))
                ((loc (Trm (Arith ((() 52) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))
                ((loc (Trm (Arith ((() 56) ((((Var (id -4611686018427387899) (name 4)) 1)) 1))))) (bas (Trm (Var (id -4611686018427387899) (name 4)))) (len (Trm (Z 60))) (siz (Trm (Z 4))) (cnt (Trm (Z 5))))))
              (djns ())
              (pure
               (And
                (pos
                 ((Eq (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387897) (name 7))) (Eq (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387891) (name 15)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387876) (name 0)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387871) (name 0))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387863) (name 0)))
                  (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387858) (name 0))) (Eq (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387847) (name 0)))
                  (Eq (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387892) (name 13))) (Eq (Var (id 82) (name a)) (Var (id 84) (name m))) (Eq0 (Arith ((() -16) ((((Var (id 81) (name m)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387901) (name 2)) 1)) -1) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387900) (name 3)) 1)) -1) ((((Var (id -4611686018427387890) (name 16)) 1)) 1))))
                  (Eq0 (Arith ((() -8) ((((Var (id -4611686018427387890) (name 16)) 1)) 1) ((((Var (id 80) (name b)) 1)) -1))))
                  (Pos (Arith ((() 100) ((((Apply (Unsigned 32) ((Arith ((() -1) ((((Var (id -4611686018427387875) (name 5)) 1)) 1))))) 1)) -1))))))
                (neg ((Eq0 (Var (id -4611686018427387899) (name 4)))))))
              (xs ((Var (id 71) (name a1)) (Var (id 84) (name m))))
              (us
               ((Var (id -4611686018427387902) (name 1)) (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))
                (Var (id -4611686018427387897) (name 7)) (Var (id -4611686018427387896) (name 8)) (Var (id -4611686018427387895) (name 9)) (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387893) (name 12))
                (Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387891) (name 15)) (Var (id -4611686018427387890) (name 16)) (Var (id -4611686018427387889) (name 17)) (Var (id -4611686018427387888) (name 18))
                (Var (id -4611686018427387887) (name 19)) (Var (id -4611686018427387886) (name 20)) (Var (id -4611686018427387885) (name 30)) (Var (id -4611686018427387884) (name freturn)) (Var (id -4611686018427387878) (name 4))
                (Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387875) (name 5)) (Var (id -4611686018427387873) (name .0)) (Var (id -4611686018427387872) (name freturn)) (Var (id -4611686018427387871) (name 0))
                (Var (id -4611686018427387870) (name 5)) (Var (id -4611686018427387869) (name 6)) (Var (id -4611686018427387868) (name .0)) (Var (id -4611686018427387867) (name freturn)) (Var (id -4611686018427387863) (name 0))
                (Var (id -4611686018427387862) (name 5)) (Var (id -4611686018427387861) (name 9)) (Var (id -4611686018427387860) (name .0)) (Var (id -4611686018427387859) (name freturn)) (Var (id -4611686018427387858) (name 0))
                (Var (id -4611686018427387857) (name 5)) (Var (id -4611686018427387856) (name 9)) (Var (id -4611686018427387855) (name .0)) (Var (id -4611686018427387854) (name freturn)) (Var (id -4611686018427387847) (name 0))
                (Var (id -4611686018427387846) (name 2)) (Var (id -4611686018427387845) (name 5)) (Var (id -4611686018427387844) (name 6)) (Var (id -4611686018427387843) (name .frozen)) (Var (id -4611686018427387842) (name .frozen1))
                (Var (id -4611686018427387841) (name 7)) (Var (id -4611686018427387840) (name .decomposed)) (Var (id -4611686018427387839) (name 11)) (Var (id -4611686018427387838) (name 13)) (Var (id -4611686018427387837) (name 19))
                (Var (id 77) (name freturn)) (Var (id 78) (name freturn)) (Var (id 79) (name freturn)) (Var (id 80) (name b)) (Var (id 81) (name m)) (Var (id 82) (name a))))
              (ctx
               ((xs ()) (sat true)
                (rep
                 (((Var (id -4611686018427387901) (name 2)) (Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1)))) ((Var (id -4611686018427387900) (name 3)) (Var (id 80) (name b)))
                  ((Var (id -4611686018427387898) (name 5)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387897) (name 7)) (Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))
                  ((Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387894) (name 10))) ((Var (id -4611686018427387891) (name 15)) (Var (id 80) (name b)))
                  ((Var (id -4611686018427387890) (name 16)) (Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1)))) ((Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387899) (name 4)))
                  ((Var (id -4611686018427387871) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387863) (name 0)) (Var (id -4611686018427387899) (name 4)))
                  ((Var (id -4611686018427387858) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id -4611686018427387847) (name 0)) (Var (id -4611686018427387899) (name 4))) ((Var (id 81) (name m)) (Z 16))
                  ((Var (id 84) (name m)) (Var (id 82) (name a)))))
                (cls
                 (((Var (id -4611686018427387899) (name 4))
                   ((Var (id -4611686018427387898) (name 5)) (Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387871) (name 0)) (Var (id -4611686018427387863) (name 0)) (Var (id -4611686018427387858) (name 0))
                    (Var (id -4611686018427387847) (name 0))))
                  ((Var (id -4611686018427387894) (name 10)) ((Var (id -4611686018427387892) (name 13)))) ((Var (id 80) (name b)) ((Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387891) (name 15))))
                  ((Var (id 82) (name a)) ((Var (id 84) (name m)))) ((Z 16) ((Var (id 81) (name m))))
                  ((Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))) ((Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387897) (name 7))))
                  ((Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1))) ((Var (id -4611686018427387890) (name 16))))))
                (use (((Var (id -4611686018427387896) (name 8)) ((Arith ((() -8) ((((Var (id -4611686018427387896) (name 8)) 1)) 1))))) ((Var (id 80) (name b)) ((Arith ((() 8) ((((Var (id 80) (name b)) 1)) 1))))))) (pnd ())))))))
          (pure Tt) (xs ((Var (id -4611686018427387889) (name 17))))
          (us
           ((Var (id -4611686018427387902) (name 1)) (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))
            (Var (id -4611686018427387897) (name 7)) (Var (id -4611686018427387896) (name 8)) (Var (id -4611686018427387895) (name 9)) (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387893) (name 12))
            (Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387891) (name 15)) (Var (id -4611686018427387890) (name 16)) (Var (id -4611686018427387888) (name 18)) (Var (id -4611686018427387887) (name 19))
            (Var (id -4611686018427387886) (name 20)) (Var (id -4611686018427387885) (name 30)) (Var (id -4611686018427387884) (name freturn)) (Var (id -4611686018427387878) (name 4)) (Var (id -4611686018427387876) (name 0))
            (Var (id -4611686018427387875) (name 5)) (Var (id -4611686018427387873) (name .0)) (Var (id -4611686018427387872) (name freturn)) (Var (id -4611686018427387871) (name 0)) (Var (id -4611686018427387870) (name 5))
            (Var (id -4611686018427387869) (name 6)) (Var (id -4611686018427387868) (name .0)) (Var (id -4611686018427387867) (name freturn)) (Var (id -4611686018427387863) (name 0)) (Var (id -4611686018427387862) (name 5))
            (Var (id -4611686018427387861) (name 9)) (Var (id -4611686018427387860) (name .0)) (Var (id -4611686018427387859) (name freturn)) (Var (id -4611686018427387858) (name 0)) (Var (id -4611686018427387857) (name 5))
            (Var (id -4611686018427387856) (name 9)) (Var (id -4611686018427387855) (name .0)) (Var (id -4611686018427387854) (name freturn)) (Var (id -4611686018427387847) (name 0)) (Var (id -4611686018427387846) (name 2))
            (Var (id -4611686018427387845) (name 5)) (Var (id -4611686018427387844) (name 6)) (Var (id -4611686018427387843) (name .frozen)) (Var (id -4611686018427387842) (name .frozen1)) (Var (id -4611686018427387841) (name 7))
            (Var (id -4611686018427387840) (name .decomposed)) (Var (id -4611686018427387839) (name 11)) (Var (id -4611686018427387838) (name 13)) (Var (id -4611686018427387837) (name 19)) (Var (id 77) (name freturn)) (Var (id 78) (name freturn))
            (Var (id 79) (name freturn)) (Var (id 80) (name b)) (Var (id 81) (name m)) (Var (id 82) (name a))))
          (ctx ((xs ()) (sat true) (rep ()) (cls ()) (use ()) (pnd ()))))
         ((Var (id -4611686018427387902) (name 1)) (Var (id -4611686018427387901) (name 2)) (Var (id -4611686018427387900) (name 3)) (Var (id -4611686018427387899) (name 4)) (Var (id -4611686018427387898) (name 5))
          (Var (id -4611686018427387897) (name 7)) (Var (id -4611686018427387896) (name 8)) (Var (id -4611686018427387895) (name 9)) (Var (id -4611686018427387894) (name 10)) (Var (id -4611686018427387893) (name 12))
          (Var (id -4611686018427387892) (name 13)) (Var (id -4611686018427387891) (name 15)) (Var (id -4611686018427387890) (name 16)) (Var (id -4611686018427387889) (name 17)) (Var (id -4611686018427387888) (name 18))
          (Var (id -4611686018427387887) (name 19)) (Var (id -4611686018427387886) (name 20)) (Var (id -4611686018427387885) (name 30)) (Var (id -4611686018427387884) (name freturn)) (Var (id -4611686018427387878) (name 4))
          (Var (id -4611686018427387876) (name 0)) (Var (id -4611686018427387875) (name 5)) (Var (id -4611686018427387873) (name .0)) (Var (id -4611686018427387872) (name freturn)) (Var (id -4611686018427387871) (name 0))
          (Var (id -4611686018427387870) (name 5)) (Var (id -4611686018427387869) (name 6)) (Var (id -4611686018427387868) (name .0)) (Var (id -4611686018427387867) (name freturn)) (Var (id -4611686018427387863) (name 0))
          (Var (id -4611686018427387862) (name 5)) (Var (id -4611686018427387861) (name 9)) (Var (id -4611686018427387860) (name .0)) (Var (id -4611686018427387859) (name freturn)) (Var (id -4611686018427387858) (name 0))
          (Var (id -4611686018427387857) (name 5)) (Var (id -4611686018427387856) (name 9)) (Var (id -4611686018427387855) (name .0)) (Var (id -4611686018427387854) (name freturn)) (Var (id -4611686018427387847) (name 0))
          (Var (id -4611686018427387846) (name 2)) (Var (id -4611686018427387845) (name 5)) (Var (id -4611686018427387844) (name 6)) (Var (id -4611686018427387843) (name .frozen)) (Var (id -4611686018427387842) (name .frozen1))
          (Var (id -4611686018427387841) (name 7)) (Var (id -4611686018427387840) (name .decomposed)) (Var (id -4611686018427387839) (name 11)) (Var (id -4611686018427387838) (name 13)) (Var (id -4611686018427387837) (name 19))
          (Var (id 77) (name freturn)) (Var (id 78) (name freturn)) (Var (id 79) (name freturn)) (Var (id 80) (name b)) (Var (id 81) (name m)) (Var (id 82) (name a))))
      |} ;
      [%expect {| |}]

    (* ( Sh.freshen_xs:
     *   {%a_2, %b_4, %l_7, %m_8, %n_9}
     *   ∃ %m_8 .   %l_7 -[ %b_4, 10 )-> ⟨10,%a_2⟩
     * ) Sh.freshen_xs: [%m_8 ↦ %m_10]
     *   ∃ %m_10 .   %l_7 -[ %b_4, 10 )-> ⟨10,%a_2⟩ *)
    let%expect_test _ =
      replay
        {|
        (Freshen_xs
          ((heap
            (((loc (Trm (Var (id 7) (name l))))
              (bas (Trm (Var (id 4) (name b)))) (len (Trm (Z 10)))
              (siz (Trm (Z 10))) (cnt (Trm (Var (id 2) (name a)))))))
           (djns ()) (pure Tt) (xs ((Var (id 8) (name m))))
           (us
            ((Var (id 2) (name a)) (Var (id 4) (name b))
             (Var (id 7) (name l))))
           (ctx ((xs ()) (sat true) (rep ()) (cls ()) (use ()) (pnd ()))))
          ((Var (id 2) (name a)) (Var (id 4) (name b)) (Var (id 7) (name l))
           (Var (id 8) (name m)) (Var (id 9) (name n))))
        |} ;
      [%expect {| |}]
  end )
