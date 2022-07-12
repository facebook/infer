(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Sledge
open Fol
open Symbolic_heap

let%test_module _ =
  ( module struct
    open Xsh

    let () = Dbg.init ~margin:68 ()

    (* let () =
     *   Dbg.init ~margin:160
     *     ~config:
     *       (Dbg.parse_exn
     *          "+Sh.simplify+Sh.simplify_+Sh.norm+Sh.and_subst+Context.solve_and_elim+Context.partition_valid+Context.solve_for_vars+Context.apply_and_elim+Context.apply_subst+Context.elim" )
     *     () *)

    [@@@warning "-unused-value-declaration"]

    let pp = Format.printf "@\n%a@." pp
    let pp_raw = Format.printf "@\n%a@." pp_raw
    let pp_djn = Format.printf "@\n%a@." pp_djn
    let ( ~$ ) = Var.Set.of_list
    let i n = Term.integer (Z.of_int n)
    let ( + ) = Term.add
    let ( - ) = Term.sub
    let ( = ) = Formula.eq
    let f x = Term.apply (Uninterp "f") [|x|]
    let vx = ref Var.Context.empty

    let var name =
      let x_ = Var.Fresh.var name vx in
      (x_, Term.var x_)

    let a_, a = var "a"
    let b_, b = var "b"
    let c_, c = var "c"
    let d_, d = var "d"
    let e_, e = var "e"
    let m_, m = var "m"
    let x_, x = var "x"
    let y_, y = var "y"
    let z_, z = var "z"

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
           (seg {loc= x; bas= x; len= i 16; siz= i 8; cnt= a})
           (seg {loc= x + i 8; bas= x; len= i 16; siz= i 8; cnt= b}) ) ;
      [%expect {|
          %x_7 -[)-> ⟨8,%a_1⟩^⟨8,%b_2⟩ |}]

    let%expect_test _ =
      let p = exists ~$[x_] (extend_us ~$[x_] emp) in
      let q = pure (x = i 0) in
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
          (pure (x = i 0))
          (exists ~$[x_]
             (or_
                (and_ (x = i 1) (pure (y = i 1)))
                (exists ~$[x_] (pure (x = i 2))) ) )
      in
      pp q ;
      pp_djn (Xsh.Set.of_iter (dnf q)) ;
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
             (pure (x = i 0))
             (exists ~$[x_]
                (or_
                   (and_ (x = i 1) (pure (y = i 1)))
                   (exists ~$[x_] (pure (x = i 2))) ) ) )
      in
      pp q ;
      pp_djn (Xsh.Set.of_iter (dnf q)) ;
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
             (pure (x = i 0))
             (exists ~$[x_]
                (or_
                   (and_ (x = i 1) (pure (y = i 1)))
                   (exists ~$[x_] (pure (x = i 2))) ) ) )
      in
      pp q ;
      pp (simplify q) ;
      [%expect
        {|
        ( (  emp) ∨ (  ( (  emp) ∨ (  1 = %y_8 ∧ emp) )) )
    
        ( (  emp) ∨ (  emp) ∨ (  1 = %y_8 ∧ emp) ) |}]

    let%expect_test _ =
      let q = exists ~$[x_] (of_eqs [(f x, x); (f y, y - i 1)]) in
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
             (pure (eq_concat (i 16, e) [|(i 8, a); (i 8, d)|]))
             (or_
                (pure (Formula.dq x (i 0)))
                (exists (Var.Set.of_list [b_])
                   (pure (eq_concat (i 8, a) [|(i 4, c); (i 4, b)|])) ) ) )
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
             (seg {loc= x; bas= b; len= m; siz= i 8; cnt= i 0})
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
  end )
