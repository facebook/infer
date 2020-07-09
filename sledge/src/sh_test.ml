(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol

let%test_module _ =
  ( module struct
    open Sh

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160 ~config:(Result.ok_exn (Trace.parse "+Sh")) ()
     *
     * [@@@warning "-32"] *)

    let pp = Format.printf "@\n%a@." pp
    let pp_raw = Format.printf "@\n%a@." pp_raw
    let pp_djn = Format.printf "@\n%a@." pp_djn
    let ( ~$ ) = Var.Set.of_list
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( - ) = Term.sub
    let ( = ) = Formula.eq
    let f = Term.splat (* any uninterpreted function *)

    let wrt = Var.Set.empty
    let a_, wrt = Var.fresh "a" ~wrt
    let b_, wrt = Var.fresh "b" ~wrt
    let c_, wrt = Var.fresh "c" ~wrt
    let d_, wrt = Var.fresh "d" ~wrt
    let e_, wrt = Var.fresh "e" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let _ = wrt
    let a = Term.var a_
    let b = Term.var b_
    let c = Term.var c_
    let d = Term.var d_
    let e = Term.var e_
    let x = Term.var x_
    let y = Term.var y_

    let eq_concat (siz, seq) ms =
      Formula.eq (Term.sized ~siz ~seq)
        (Term.concat
           (Array.map ~f:(fun (siz, seq) -> Term.sized ~siz ~seq) ms))

    let of_eqs l =
      List.fold ~init:emp ~f:(fun q (a, b) -> and_ (Formula.eq a b) q) l

    let%expect_test _ =
      let p = exists ~$[x_] (extend_us ~$[x_] emp) in
      let q = pure (x = !0) in
      pp p ;
      pp q ;
      pp (star p q) ;
      [%expect
        {|
        ∃ %x_6 .   emp
    
          0 = %x_6 ∧ emp
    
          0 = %x_6 ∧ emp |}]

    let%expect_test _ =
      let q =
        or_
          (pure (x = !0))
          (exists
             ~$[x_]
             (or_
                (and_ (x = !1) (pure (y = !1)))
                (exists ~$[x_] (pure (x = !2)))))
      in
      pp q ;
      pp_djn (dnf q) ;
      [%expect
        {|
          ( (  0 = %x_6 ∧ emp)
          ∨ (  ( (  1 = _ = %y_7 ∧ emp) ∨ (  2 = _ ∧ emp) ))
          )
    
        ( (∃ %x_6, %x_7 .   2 = %x_7 ∧ (%x_7 = 2) ∧ emp)
        ∨ (∃ %x_6 .   1 = %x_6 = %y_7 ∧ ((%x_6 = 1) ∧ (%y_7 = 1)) ∧ emp)
        ∨ (  0 = %x_6 ∧ (%x_6 = 0) ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        exists
          ~$[x_]
          (or_
             (pure (x = !0))
             (exists
                ~$[x_]
                (or_
                   (and_ (x = !1) (pure (y = !1)))
                   (exists ~$[x_] (pure (x = !2))))))
      in
      pp q ;
      pp_djn (dnf q) ;
      [%expect
        {|
          ( (  0 = _ ∧ emp)
          ∨ (  ( (  1 = _ = %y_7 ∧ emp) ∨ (  2 = _ ∧ emp) ))
          )
    
        ( (∃ %x_6, %x_8, %x_9 .   2 = %x_9 ∧ (%x_9 = 2) ∧ emp)
        ∨ (∃ %x_6, %x_8 .
             1 = %y_7 = %x_8
           ∧ ((%x_8 = 1) ∧ (%y_7 = 1))
           ∧ emp)
        ∨ (∃ %x_6 .   0 = %x_6 ∧ (%x_6 = 0) ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        exists
          ~$[x_]
          (or_
             (pure (x = !0))
             (exists
                ~$[x_]
                (or_
                   (and_ (x = !1) (pure (y = !1)))
                   (exists ~$[x_] (pure (x = !2))))))
      in
      pp q ;
      pp (simplify q) ;
      [%expect
        {|
        ( (  0 = _ ∧ emp)
        ∨ (  ( (  1 = _ = %y_7 ∧ emp) ∨ (  2 = _ ∧ emp) ))
        )
    
        ( (  1 = %y_7 ∧ emp) ∨ (  emp) ∨ (  emp) ) |}]

    let%expect_test _ =
      let q = exists ~$[x_] (of_eqs [(f x, x); (f y, y - !1)]) in
      pp q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %x_6 .   %x_6 = %x_6^ ∧ ((-1 × 1) + (1 × %y_7)) = %y_7^ ∧ emp
    
          ((-1 × 1) + (1 × %y_7)) = %y_7^
        ∧ (%y_7^ = ((-1 × 1) + (1 × %y_7)))
        ∧ emp

          ((-1 × 1) + (1 × %y_7)) = %y_7^ ∧ emp |}]

    let%expect_test _ =
      let q =
        exists
          ~$[a_; c_; d_; e_]
          (star
             (pure (eq_concat (!16, e) [|(!8, a); (!8, d)|]))
             (or_
                (pure (Formula.dq x !0))
                (exists
                   (Var.Set.of_list [b_])
                   (pure (eq_concat (!8, a) [|(!4, c); (!4, b)|])))))
      in
      pp_raw q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %a_1, %c_3, %d_4, %e_5 .
          (⟨8,%a_1⟩^⟨8,%d_4⟩) = %e_5
        ∧ ((⟨16,%e_5⟩ = (⟨8,%a_1⟩^⟨8,%d_4⟩)) ∧ tt)
        ∧ emp
        * ( (  (%x_6 ≠ 0) ∧ emp)
          ∨ (∃ %b_2 .
               (⟨4,%c_3⟩^⟨4,%b_2⟩) = %a_1
             ∧ (⟨8,%a_1⟩ = (⟨4,%c_3⟩^⟨4,%b_2⟩))
             ∧ emp)
          )

          tt ∧ emp * ( (  tt ∧ emp) ∨ (  (%x_6 ≠ 0) ∧ emp) )

          ( (  emp) ∨ (  (%x_6 ≠ 0) ∧ emp) ) |}]
  end )
