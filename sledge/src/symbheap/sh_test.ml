(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    (* let () = Trace.init ~margin:68 ~config:all () *)

    let () = Trace.init ~margin:68 ~config:Trace.none ()
    let pp = Format.printf "@\n%a@." Sh.pp
    let pp_djn = Format.printf "@\n%a@." Sh.pp_djn
    let ( ~$ ) = Var.Set.of_list
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( = ) = Term.eq
    let wrt = Var.Set.empty
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, _ = Var.fresh "y" ~wrt
    let x = Term.var x_
    let y = Term.var y_

    let%expect_test _ =
      let p = Sh.(exists ~$[x_] (extend_us ~$[x_] emp)) in
      let q = Sh.(pure (x = !0)) in
      pp p ;
      pp q ;
      pp (Sh.star p q) ;
      [%expect
        {|
        ∃ %x_1 .   emp

          0 = %x_1 ∧ emp

          0 = %x_1 ∧ emp |}]

    let%expect_test _ =
      let q =
        Sh.(
          or_
            (pure (x = !0))
            (exists
               ~$[x_]
               (or_
                  (and_ (x = !1) (pure (y = !1)))
                  (exists ~$[x_] (pure (x = !2))))))
      in
      pp q ;
      pp_djn (Sh.dnf q) ;
      [%expect
        {|
          ( (  0 = %x_1 ∧ emp)
          ∨ (∃ %x_1 .  ( (  1 = %x_1 = %y_2 ∧ emp)
                       ∨ (∃ %x_1 .  2 = %x_1 ∧ emp)
                       ))
          )

        ( (∃ %x_1, %x_2 .   2 = %x_2 ∧ emp)
        ∨ (∃ %x_1 .   1 = %x_1 = %y_2 ∧ emp)
        ∨ (  0 = %x_1 ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        Sh.(
          exists
            ~$[x_]
            (or_
               (pure (x = !0))
               (exists
                  ~$[x_]
                  (or_
                     (and_ (x = !1) (pure (y = !1)))
                     (exists ~$[x_] (pure (x = !2)))))))
      in
      pp q ;
      pp_djn (Sh.dnf q) ;
      [%expect
        {|
        ∃ %x_1 .
          ( (  0 = %x_1 ∧ emp)
          ∨ (∃ %x_1 .  ( (  1 = %x_1 = %y_2 ∧ emp)
                       ∨ (∃ %x_1 .  2 = %x_1 ∧ emp)
                       ))
          )

        ( (∃ %x_1, %x_3, %x_4 .   2 = %x_4 ∧ emp)
        ∨ (∃ %x_1, %x_3 .   1 = %y_2 = %x_3 ∧ emp)
        ∨ (∃ %x_1 .   0 = %x_1 ∧ emp)
        ) |}]

    let%expect_test _ =
      let q =
        Sh.(
          exists
            ~$[x_]
            (or_
               (pure (x = !0))
               (exists
                  ~$[x_]
                  (or_
                     (and_ (x = !1) (pure (y = !1)))
                     (exists ~$[x_] (pure (x = !2)))))))
      in
      pp q ;
      pp (Sh.simplify q) ;
      [%expect
        {|
        ∃ %x_1 .
          ( (  0 = %x_1 ∧ emp)
          ∨ (∃ %x_1 .  ( (  1 = %x_1 = %y_2 ∧ emp)
                       ∨ (∃ %x_1 .  2 = %x_1 ∧ emp)
                       ))
          )

          ( (  emp) ∨ (  ( (  emp) ∨ (  emp) )) ) |}]
  end )
