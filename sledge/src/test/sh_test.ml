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

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160
     *     ~config:
     *       (Result.get_ok
     *          (Trace.parse
     *             "+Sh.simplify+Sh.simplify_+Sh.norm+Sh.and_subst+Context.solve_and_elim+Context.partition_valid+Context.solve_for_vars+Context.apply_and_elim+Context.apply_subst+Context.elim"))
     *     () *)

    [@@@warning "-32"]

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

    let eq_concat (siz, seq) ms =
      Formula.eq (Term.sized ~siz ~seq)
        (Term.concat
           (Array.map ~f:(fun (siz, seq) -> Term.sized ~siz ~seq) ms))

    let of_eqs l =
      List.fold ~f:(fun (a, b) q -> and_ (Formula.eq a b) q) l emp

    let%expect_test _ =
      pp
        (star
           (seg {loc= x; bas= x; len= !16; siz= !8; cnt= a})
           (seg {loc= x + !8; bas= x; len= !16; siz= !8; cnt= b})) ;
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
          ( (  0 = %x_7 ∧ emp) ∨ (  ( (  1 = %y_8 ∧ emp) ∨ (  emp) )) )
    
        ( (∃ %x_7, %x_8 .   2 = %x_8 ∧ (2 = %x_8) ∧ emp)
        ∨ (∃ %x_7 .   1 = %x_7 = %y_8 ∧ ((1 = %x_7) ∧ (1 = %y_8)) ∧ emp)
        ∨ (  0 = %x_7 ∧ (0 = %x_7) ∧ emp)
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
          ( (  emp) ∨ (  ( (  1 = %y_8 ∧ emp) ∨ (  emp) )) )
    
        ( (∃ %x_7, %x_9, %x_10 .   2 = %x_10 ∧ (2 = %x_10) ∧ emp)
        ∨ (∃ %x_7, %x_9 .
             1 = %y_8 = %x_9 ∧ ((1 = %y_8) ∧ (1 = %x_9))
           ∧ emp)
        ∨ (∃ %x_7 .   0 = %x_7 ∧ (0 = %x_7) ∧ emp)
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
        ( (  emp) ∨ (  ( (  1 = %y_8 ∧ emp) ∨ (  emp) )) )
    
        ( (  emp) ∨ (  1 = %y_8 ∧ emp) ∨ (  emp) ) |}]

    let%expect_test _ =
      let q = exists ~$[x_] (of_eqs [(f x, x); (f y, y - !1)]) in
      pp q ;
      let q' = simplify q in
      pp_raw q' ;
      pp q' ;
      [%expect
        {|
        ∃ %x_7 .   %x_7 = f(%x_7) ∧ (-1 + %y_8) = f(%y_8) ∧ emp
    
          (-1 + %y_8) = f(%y_8) ∧ ((1 + f(%y_8)) = %y_8) ∧ emp
    
          (-1 + %y_8) = f(%y_8) ∧ emp |}]

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
          (⟨8,%a_1⟩^⟨8,%d_4⟩) = %e_5 ∧ (⟨16,%e_5⟩ = (⟨8,%a_1⟩^⟨8,%d_4⟩))
        ∧ emp
        * ( (  tt ∧ (0 ≠ %x_7) ∧ emp)
          ∨ (∃ %b_2 .
               (⟨4,%c_3⟩^⟨4,%b_2⟩) = %a_1
             ∧ (⟨8,%a_1⟩ = (⟨4,%c_3⟩^⟨4,%b_2⟩))
             ∧ emp)
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
             (or_ (of_eqs [(b, y); (m, c)]) (of_eqs [(b, z); (m, c)])))
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
        * ( (  %b_2 = %z_9 ∧ (%b_2 = %z_9) ∧ emp)
          ∨ (  %b_2 = %y_8 ∧ (%b_2 = %y_8) ∧ emp)
          )
    
        ∃ %b_2 .
          %x_7 -[ %b_2, %c_3 )-> ⟨8,0⟩
        * ( (  %b_2 = %z_9 ∧ emp) ∨ (  %b_2 = %y_8 ∧ emp) ) |}]
  end )
