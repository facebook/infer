(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    let () = Trace.init ~margin:68 ~config:none ()
    let pp = Format.printf "%t%a@." (fun _ -> Trace.flush ()) Exp.pp
    let char = Typ.integer ~bits:8
    let ( ! ) i = Exp.integer (Z.of_int i) char
    let ( + ) = Exp.add char
    let ( - ) = Exp.sub char
    let ( * ) = Exp.mul char
    let ( = ) = Exp.eq
    let ( != ) = Exp.dq
    let ( > ) = Exp.gt
    let ( >= ) = Exp.ge
    let ( < ) = Exp.lt
    let ( <= ) = Exp.le
    let ( && ) = Exp.and_
    let ( || ) = Exp.or_
    let ( ~~ ) = Exp.not_ Typ.bool
    let wrt = Var.Set.empty
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, _ = Var.fresh "z" ~wrt
    let y = Exp.var y_
    let z = Exp.var z_

    let%test "booleans distinct" =
      Exp.is_false
        (Exp.eq
           (Exp.integer Z.minus_one Typ.bool)
           (Exp.integer Z.zero Typ.bool))

    let%test "unsigned booleans distinct" =
      Exp.is_false
        (Exp.eq (Exp.integer Z.one Typ.bool) (Exp.integer Z.zero Typ.bool))

    let%test "boolean overflow" =
      Exp.is_true
        (Exp.eq
           (Exp.integer Z.minus_one Typ.bool)
           (Exp.integer Z.one Typ.bool))

    let%test "unsigned boolean overflow" =
      Exp.is_true
        (Exp.uge
           (Exp.integer Z.minus_one Typ.bool)
           (Exp.integer Z.one Typ.bool))

    let%expect_test _ =
      pp (!42 + !13) ;
      [%expect {| 55 |}]

    let%expect_test _ =
      pp (!(-128) && !127) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!(-128) || !127) ;
      [%expect {| -1 |}]

    let%test "monomial coefficient must be toplevel" =
      match !7 * z * (!2 * y) with
      | App {op= App {op= Mul _}; arg= Integer _} -> true
      | _ -> false

    let%test "polynomial constant must be toplevel" =
      match (!13 * z) + !42 + (!3 * y) with
      | App {op= App {op= Add _}; arg= Integer _} -> true
      | _ -> false

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
      [%expect {| (%y_1 × %y_1 × %z_2) |}]

    let%expect_test _ =
      pp ((!2 * z * z) + (!3 * z) + !4) ;
      [%expect {| ((2 × %z_2 × %z_2) + (3 × %z_2) + 4) |}]

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
         ((7 × %y_1 × %y_1 × %z_2) + (8 × %y_1 × %z_2 × %z_2)
          + (9 × %z_2 × %z_2 × %z_2) + (5 × %y_1 × %y_1) + (6 × %y_1 × %z_2)
          + (4 × %z_2 × %z_2) + (3 × %y_1) + (2 × %z_2) + 1) |}]

    let%expect_test _ =
      pp (!0 * z * y) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!1 * z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (!7 * z * (!2 * y)) ;
      [%expect {| (14 × %y_1 × %z_2) |}]

    let%expect_test _ =
      pp (!13 + (!42 * z)) ;
      [%expect {| ((42 × %z_2) + 13) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42) ;
      [%expect {| ((13 × %z_2) + 42) |}]

    let%expect_test _ =
      pp ((!2 * z) - !3 + ((!(-2) * z) + !3)) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!3 * y) + (!13 * z) + !42) ;
      [%expect {| ((3 × %y_1) + (13 × %z_2) + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y)) ;
      [%expect {| ((3 × %y_1) + (13 × %z_2) + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!2 * z)) ;
      [%expect {| ((3 × %y_1) + (15 × %z_2) + 42) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!(-13) * z)) ;
      [%expect {| ((3 × %y_1) + 42) |}]

    let%expect_test _ =
      pp (z + !42 + ((!3 * y) + (!(-1) * z))) ;
      [%expect {| ((3 × %y_1) + 42) |}]

    let%expect_test _ =
      pp (!(-1) * (z + (!(-1) * y))) ;
      [%expect {| (%y_1 + (-1 × %z_2)) |}]

    let%expect_test _ =
      pp (((!3 * y) + !2) * (!4 + (!5 * z))) ;
      [%expect
        {| ((15 × %y_1 × %z_2) + (12 × %y_1) + (10 × %z_2) + 8) |}]

    let%expect_test _ =
      pp (((!2 * z) - !3 + ((!(-2) * z) + !3)) * (!4 + (!5 * z))) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 - ((!3 * y) + (!13 * z))) ;
      [%expect {| ((-3 × %y_1) + 42) |}]

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
      pp (!3 * y = z = Exp.bool true) ;
      [%expect {| (%z_2 = (3 × %y_1)) |}]

    let%expect_test _ =
      pp (Exp.bool true = (!3 * y = z)) ;
      [%expect {| (%z_2 = (3 × %y_1)) |}]

    let%expect_test _ =
      pp (!3 * y = z = Exp.bool false) ;
      [%expect {| (%z_2 ≠ (3 × %y_1)) |}]

    let%expect_test _ =
      pp (Exp.bool false = (!3 * y = z)) ;
      [%expect {| (%z_2 ≠ (3 × %y_1)) |}]

    let%expect_test _ =
      pp (y - (!(-3) * y) + !4) ;
      [%expect {| ((4 × %y_1) + 4) |}]

    let%expect_test _ =
      pp ((!(-3) * y) + !4 - y) ;
      [%expect {| ((-4 × %y_1) + 4) |}]

    let%expect_test _ =
      pp (y = (!(-3) * y) + !4) ;
      [%expect {| (4 = (4 × %y_1)) |}]

    let%expect_test _ =
      pp ((!(-3) * y) + !4 = y) ;
      [%expect {| (4 = (4 × %y_1)) |}]

    let%expect_test _ =
      pp (Exp.sub Typ.bool (Exp.bool true) (z = !4)) ;
      [%expect {| ((4 = %z_2) + -1) |}]

    let%expect_test _ =
      pp (Exp.add Typ.bool (Exp.bool true) (z = !4) = (z = !4)) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 = (!3 * y) + (!13 * z)) ;
      [%expect {| (42 = (3 × %y_1)) |}]

    let%expect_test _ =
      pp ((!13 * z) + !(-42) = (!3 * y) + (!13 * z)) ;
      [%expect {| (42 = (-3 × %y_1)) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| (42 = (-3 × %y_1)) |}]

    let%expect_test _ =
      pp ((!10 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| (42 = ((-3 × %y_1) + (3 × %z_2))) |}]

    let%expect_test _ =
      pp ~~((!13 * z) + !(-42) != (!3 * y) + (!13 * z)) ;
      [%expect {| (42 = (-3 × %y_1)) |}]

    let%expect_test _ =
      pp ~~(y > !2 && z <= !3) ;
      [%expect {| ((%z_2 > 3) || (%y_1 <= 2)) |}]

    let%expect_test _ =
      pp ~~(y >= !2 || z < !3) ;
      [%expect {| ((%z_2 >= 3) && (%y_1 < 2)) |}]

    let%expect_test _ =
      pp Exp.(eq z null) ;
      pp Exp.(eq null z) ;
      pp Exp.(dq (eq null z) (bool false)) ;
      [%expect
        {|
        (null = %z_2)

        (null = %z_2)

        (null = %z_2) |}]
  end )
