(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    (* let () = Trace.init ~margin:68 ~config:all () *)
    let () = Trace.init ~margin:68 ~config:none ()
    let pp = Format.printf "@\n%a@." Term.pp
    let char = Typ.integer ~bits:8
    let ( ! ) i = Term.integer (Z.of_int i) char
    let ( + ) = Term.add char
    let ( - ) = Term.sub char
    let ( * ) = Term.mul char
    let ( = ) = Term.eq
    let ( != ) = Term.dq
    let ( > ) = Term.gt
    let ( >= ) = Term.ge
    let ( < ) = Term.lt
    let ( <= ) = Term.le
    let ( && ) = Term.and_
    let ( || ) = Term.or_
    let ( ~~ ) = Term.not_ Typ.bool
    let wrt = Var.Set.empty
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, _ = Var.fresh "z" ~wrt
    let y = Term.var y_
    let z = Term.var z_

    let%test "booleans distinct" =
      Term.is_false
        (Term.eq
           (Term.integer Z.minus_one Typ.bool)
           (Term.integer Z.zero Typ.bool))

    let%test "unsigned booleans distinct" =
      Term.is_false
        (Term.eq
           (Term.integer Z.one Typ.bool)
           (Term.integer Z.zero Typ.bool))

    let%test "boolean overflow" =
      Term.is_true
        (Term.eq
           (Term.integer Z.minus_one Typ.bool)
           (Term.integer Z.one Typ.bool))

    let%test "unsigned boolean overflow" =
      Term.is_true
        (Term.uge
           (Term.integer Z.minus_one Typ.bool)
           (Term.integer Z.one Typ.bool))

    let%expect_test _ =
      pp (!42 + !13) ;
      [%expect {| 55 |}]

    let%expect_test _ =
      pp (!(-128) && !127) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!(-128) || !127) ;
      [%expect {| -1 |}]

    let%expect_test _ =
      pp (z + !42 + !13) ;
      [%expect {| (%z_2 + 55) |}]

    let%expect_test _ =
      pp (z + !42 + !(-42)) ;
      [%expect {| %z_2 |}]

    let%expect_test _ =
      pp (z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (y * z * y) ;
      [%expect {| (%y_1^2 × %z_2) |}]

    let%expect_test _ =
      pp ((!2 * z * z) + (!3 * z) + !4) ;
      [%expect {| (2 × (%z_2^2) + 3 × %z_2 + 4) |}]

    let%expect_test _ =
      pp
        ( !1 + (!2 * z) + (!3 * y)
        + (!4 * z * z)
        + (!5 * y * y)
        + (!6 * z * y)
        + (!7 * y * z * y)
        + (!8 * z * y * z)
        + (!9 * z * z * z) ) ;
      [%expect
        {|
         (6 × (%y_1 × %z_2) + 8 × (%y_1 × %z_2^2) + 5 × (%y_1^2)
           + 7 × (%y_1^2 × %z_2) + 4 × (%z_2^2) + 9 × (%z_2^3) + 3 × %y_1
           + 2 × %z_2 + 1) |}]

    let%expect_test _ =
      pp (!0 * z * y) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!1 * z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (!7 * z * (!2 * y)) ;
      [%expect {| (14 × (%y_1 × %z_2)) |}]

    let%expect_test _ =
      pp (!13 + (!42 * z)) ;
      [%expect {| (42 × %z_2 + 13) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42) ;
      [%expect {| (13 × %z_2 + 42) |}]

    let%expect_test _ =
      pp ((!2 * z) - !3 + ((!(-2) * z) + !3)) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!3 * y) + (!13 * z) + !42) ;
      [%expect {| (3 × %y_1 + 13 × %z_2 + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y)) ;
      [%expect {| (3 × %y_1 + 13 × %z_2 + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!2 * z)) ;
      [%expect {| (3 × %y_1 + 15 × %z_2 + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!(-13) * z)) ;
      [%expect {| (3 × %y_1 + 42) |}]

    let%expect_test _ =
      pp (z + !42 + ((!3 * y) + (!(-1) * z))) ;
      [%expect {| (3 × %y_1 + 42) |}]

    let%expect_test _ =
      pp (!(-1) * (z + (!(-1) * y))) ;
      [%expect {| (%y_1 + -1 × %z_2) |}]

    let%expect_test _ =
      pp (((!3 * y) + !2) * (!4 + (!5 * z))) ;
      [%expect {| (15 × (%y_1 × %z_2) + 12 × %y_1 + 10 × %z_2 + 8) |}]

    let%expect_test _ =
      pp (((!2 * z) - !3 + ((!(-2) * z) + !3)) * (!4 + (!5 * z))) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 - ((!3 * y) + (!13 * z))) ;
      [%expect {| (-3 × %y_1 + 42) |}]

    let%expect_test _ =
      pp (z = y) ;
      [%expect {| (%z_2 = %y_1) |}]

    let%expect_test _ =
      pp (z = z) ;
      [%expect {| -1 |}]

    let%expect_test _ =
      pp (z != z) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!1 = !0) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!3 * y = z = Term.bool true) ;
      [%expect {| ((3 × %y_1) = %z_2) |}]

    let%expect_test _ =
      pp (Term.bool true = (!3 * y = z)) ;
      [%expect {| ((3 × %y_1) = %z_2) |}]

    let%expect_test _ =
      pp (!3 * y = z = Term.bool false) ;
      [%expect {| ((3 × %y_1) ≠ %z_2) |}]

    let%expect_test _ =
      pp (Term.bool false = (!3 * y = z)) ;
      [%expect {| ((3 × %y_1) ≠ %z_2) |}]

    let%expect_test _ =
      pp (y - (!(-3) * y) + !4) ;
      [%expect {| (4 × %y_1 + 4) |}]

    let%expect_test _ =
      pp ((!(-3) * y) + !4 - y) ;
      [%expect {| (-4 × %y_1 + 4) |}]

    let%expect_test _ =
      pp (y = (!(-3) * y) + !4) ;
      [%expect {| (%y_1 = (-3 × %y_1 + 4)) |}]

    let%expect_test _ =
      pp ((!(-3) * y) + !4 = y) ;
      [%expect {| ((-3 × %y_1 + 4) = %y_1) |}]

    let%expect_test _ =
      pp (Term.sub Typ.bool (Term.bool true) (z = !4)) ;
      [%expect {| (-1 × (%z_2 = 4) + -1) |}]

    let%expect_test _ =
      pp (Term.add Typ.bool (Term.bool true) (z = !4) = (z = !4)) ;
      [%expect {| (((%z_2 = 4) + -1) = (%z_2 = 4)) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 = (!3 * y) + (!13 * z)) ;
      [%expect {| ((13 × %z_2 + 42) = (3 × %y_1 + 13 × %z_2)) |}]

    let%expect_test _ =
      pp ((!13 * z) + !(-42) = (!3 * y) + (!13 * z)) ;
      [%expect {| ((13 × %z_2 + -42) = (3 × %y_1 + 13 × %z_2)) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| ((13 × %z_2 + 42) = (-3 × %y_1 + 13 × %z_2)) |}]

    let%expect_test _ =
      pp ((!10 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| ((10 × %z_2 + 42) = (-3 × %y_1 + 13 × %z_2)) |}]

    let%expect_test _ =
      pp ~~((!13 * z) + !(-42) != (!3 * y) + (!13 * z)) ;
      [%expect {| ((13 × %z_2 + -42) = (3 × %y_1 + 13 × %z_2)) |}]

    let%expect_test _ =
      pp ~~(y > !2 && z <= !3) ;
      [%expect {| ((%y_1 ≤ 2) || (%z_2 > 3)) |}]

    let%expect_test _ =
      pp ~~(y >= !2 || z < !3) ;
      [%expect {| ((%y_1 < 2) && (%z_2 ≥ 3)) |}]

    let%expect_test _ =
      pp Term.(eq z null) ;
      pp Term.(eq null z) ;
      pp Term.(dq (eq null z) (bool false)) ;
      [%expect
        {|
        (%z_2 = null)

        (null = %z_2)

        (null = %z_2) |}]

    let%expect_test _ =
      let z1 = z + !1 in
      let z1_2 = z1 * z1 in
      pp z1_2 ;
      pp (z1_2 * z1_2) ;
      [%expect
        {|
        ((%z_2^2) + 2 × %z_2 + 1)

        (6 × (%z_2^2) + 4 × (%z_2^3) + (%z_2^4) + 4 × %z_2 + 1) |}]
  end )
