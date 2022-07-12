(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol
module T = Term
module F = Formula

(* [@@@warning "-unused-value-declaration"] *)

let%test_module _ =
  ( module struct
    let () = Dbg.init ~margin:68 ()

    (* let () = Dbg.init ~margin:68 ~config:Dbg.all () *)

    let pp = Format.printf "@\n%a@." T.pp
    let ppf = Format.printf "@\n%a@." F.pp
    let i n = Term.integer (Z.of_int n)
    let ( + ) = T.add
    let ( - ) = T.sub
    let ( * ) = T.mul
    let ( = ) = F.eq
    let ( != ) = F.dq
    let ( < ) = F.lt
    let ( <= ) = F.le
    let ( && ) = F.and_
    let ( || ) = F.or_
    let ( ~~ ) = F.not_
    let wrt = Var.Set.empty
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, _ = Var.fresh "z" ~wrt
    let y = T.var y_
    let z = T.var z_

    let%test "booleans distinct" = F.equal F.ff (F.iff F.tt F.ff)
    let%test "u1 values distinct" = F.equal F.ff (T.one = T.zero)

    let%test "boolean overflow" =
      F.equal F.tt (i (-1) = T.apply (Signed 1) [|i 1|])

    let%test _ = F.equal F.tt (T.one = T.apply (Unsigned 1) [|i (-1)|])

    let%expect_test _ =
      pp (i 42 + i 13) ;
      [%expect {| 55 |}]

    let%expect_test _ =
      pp (T.apply BitAnd [|i (-128); i 127|]) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (T.apply BitOr [|i (-128); i 127|]) ;
      [%expect {| -1 |}]

    let%expect_test _ =
      pp (z + i 42 + i 13) ;
      [%expect {| (%z_2 + 55) |}]

    let%expect_test _ =
      pp (z + i 42 + i (-42)) ;
      [%expect {| %z_2 |}]

    let%expect_test _ =
      pp (z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (y * z * y) ;
      [%expect {| (%y_1^2 × %z_2) |}]

    let%expect_test _ =
      pp ((i 2 * z * z) + (i 3 * z) + i 4) ;
      [%expect {| (3×%z_2 + 2×%z_2^2 + 4) |}]

    let%expect_test _ =
      pp
        ( i 1
        + (i 2 * z)
        + (i 3 * y)
        + (i 4 * z * z)
        + (i 5 * y * y)
        + (i 6 * z * y)
        + (i 7 * y * z * y)
        + (i 8 * z * y * z)
        + (i 9 * z * z * z) ) ;
      [%expect
        {|
         (3×%y_1 + 6×(%y_1 × %z_2) + 8×(%y_1 × %z_2^2) + 5×%y_1^2
           + 7×(%y_1^2 × %z_2) + 2×%z_2 + 4×%z_2^2 + 9×%z_2^3 + 1) |}]

    let%expect_test _ =
      pp (i 0 * z * y) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (i 1 * z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (i 7 * z * (i 2 * y)) ;
      [%expect {| 14×(%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (i 13 + (i 42 * z)) ;
      [%expect {| (42×%z_2 + 13) |}]

    let%expect_test _ =
      pp ((i 13 * z) + i 42) ;
      [%expect {| (13×%z_2 + 42) |}]

    let%expect_test _ =
      pp ((i 2 * z) - i 3 + ((i (-2) * z) + i 3)) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((i 3 * y) + (i 13 * z) + i 42) ;
      [%expect {| (3×%y_1 + 13×%z_2 + 42) |}]

    let%expect_test _ =
      pp ((i 13 * z) + i 42 + (i 3 * y)) ;
      [%expect {| (3×%y_1 + 13×%z_2 + 42) |}]

    let%expect_test _ =
      pp ((i 13 * z) + i 42 + (i 3 * y) + (i 2 * z)) ;
      [%expect {| (3×%y_1 + 15×%z_2 + 42) |}]

    let%expect_test _ =
      pp ((i 13 * z) + i 42 + (i 3 * y) + (i (-13) * z)) ;
      [%expect {| (3×%y_1 + 42) |}]

    let%expect_test _ =
      pp (z + i 42 + ((i 3 * y) + (i (-1) * z))) ;
      [%expect {| (3×%y_1 + 42) |}]

    let%expect_test _ =
      pp (i (-1) * (z + (i (-1) * y))) ;
      [%expect {| (%y_1 + -1×%z_2) |}]

    let%expect_test _ =
      pp (((i 3 * y) + i 2) * (i 4 + (i 5 * z))) ;
      [%expect {| ((3×%y_1 + 2) × (5×%z_2 + 4)) |}]

    let%expect_test _ =
      pp (((i 2 * z) - i 3 + ((i (-2) * z) + i 3)) * (i 4 + (i 5 * z))) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((i 13 * z) + i 42 - ((i 3 * y) + (i 13 * z))) ;
      [%expect {| (-3×%y_1 + 42) |}]

    let%expect_test _ =
      ppf (z = y) ;
      [%expect {| (%y_1 = %z_2) |}]

    let%expect_test _ =
      ppf (z = z) ;
      [%expect {| tt |}]

    let%expect_test _ =
      ppf (z != z) ;
      [%expect {| ff |}]

    let%expect_test _ =
      ppf (i 1 = i 0) ;
      [%expect {| ff |}]

    let%expect_test _ =
      ppf (F.iff (i 3 * y = z) F.tt) ;
      [%expect {| (3×%y_1 = %z_2) |}]

    let%expect_test _ =
      ppf (F.iff F.tt (i 3 * y = z)) ;
      [%expect {| (3×%y_1 = %z_2) |}]

    let%expect_test _ =
      ppf (F.iff (i 3 * y = z) F.ff) ;
      [%expect {| (3×%y_1 ≠ %z_2) |}]

    let%expect_test _ =
      ppf (F.iff F.ff (i 3 * y = z)) ;
      [%expect {| (3×%y_1 ≠ %z_2) |}]

    let%expect_test _ =
      pp (y - (i (-3) * y) + i 4) ;
      [%expect {| (4×%y_1 + 4) |}]

    let%expect_test _ =
      pp ((i (-3) * y) + i 4 - y) ;
      [%expect {| (-4×%y_1 + 4) |}]

    let%expect_test _ =
      ppf (y = (i (-3) * y) + i 4) ;
      [%expect {| (4 = 4×%y_1) |}]

    let%expect_test _ =
      ppf ((i (-3) * y) + i 4 = y) ;
      [%expect {| (4 = 4×%y_1) |}]

    let%expect_test _ =
      pp (T.sub (F.inject F.tt) (F.inject (z = i 4))) ;
      [%expect {| ((4 = %z_2) ? 0 : 1) |}]

    let%expect_test _ =
      pp (T.add (F.inject F.tt) (F.inject (F.iff (z = i 4) (z = i 4)))) ;
      [%expect {| 2 |}]

    let%expect_test _ =
      ppf ((i 13 * z) + i 42 = (i 3 * y) + (i 13 * z)) ;
      [%expect {| (42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((i 13 * z) + i (-42) = (i 3 * y) + (i 13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((i 13 * z) + i 42 = (i (-3) * y) + (i 13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((i 10 * z) + i 42 = (i (-3) * y) + (i 13 * z)) ;
      [%expect {| ((3×%y_1 + 42) = 3×%z_2) |}]

    let%expect_test _ =
      ppf ~~((i 13 * z) + i (-42) != (i 3 * y) + (i 13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ~~(i 2 < y && z <= i 3) ;
      [%expect {| ((3 < %z_2) ∨ (2 ≥ %y_1)) |}]

    let%expect_test _ =
      ppf ~~(i 2 <= y || z < i 3) ;
      [%expect {| ((2 > %y_1) ∧ (3 ≤ %z_2)) |}]

    let%expect_test _ =
      ppf (F.eq z T.zero) ;
      ppf (F.eq T.zero z) ;
      ppf (F.xor (F.eq T.zero z) F.ff) ;
      [%expect
        {|
        (0 = %z_2)

        (0 = %z_2)

        (0 = %z_2) |}]

    let%expect_test _ =
      let z1 = z + i 1 in
      let z1_2 = z1 * z1 in
      pp z1_2 ;
      pp (z1_2 * z1_2) ;
      [%expect {|
        (%z_2 + 1)^2

        (%z_2 + 1)^4 |}]
  end )
