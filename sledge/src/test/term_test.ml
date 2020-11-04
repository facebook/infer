(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol
module T = Term
module F = Formula

(* [@@@warning "-32"] *)

let%test_module _ =
  ( module struct
    let () = Trace.init ~margin:68 ()

    (* let () = Trace.init ~margin:68 ~config:Trace.all () *)

    let pp = Format.printf "@\n%a@." T.pp
    let ppf = Format.printf "@\n%a@." F.pp
    let ( ! ) i = T.integer (Z.of_int i)
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
      F.equal F.tt (!(-1) = T.apply (Signed 1) [|!1|])

    let%test _ = F.equal F.tt (T.one = T.apply (Unsigned 1) [|!(-1)|])

    let%expect_test _ =
      pp (!42 + !13) ;
      [%expect {| 55 |}]

    let%expect_test _ =
      pp (T.apply BitAnd [|!(-128); !127|]) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (T.apply BitOr [|!(-128); !127|]) ;
      [%expect {| -1 |}]

    let%expect_test _ =
      pp (z + !42 + !13) ;
      [%expect {| (55 + %z_2) |}]

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
      [%expect {| (4 + 3×%z_2 + 2×%z_2^2) |}]

    let%expect_test _ =
      pp
        ( !1
        + (!2 * z)
        + (!3 * y)
        + (!4 * z * z)
        + (!5 * y * y)
        + (!6 * z * y)
        + (!7 * y * z * y)
        + (!8 * z * y * z)
        + (!9 * z * z * z) ) ;
      [%expect
        {|
         (1 + 3×%y_1 + 6×(%y_1 × %z_2) + 8×(%y_1 × %z_2^2) + 5×%y_1^2
           + 7×(%y_1^2 × %z_2) + 2×%z_2 + 4×%z_2^2 + 9×%z_2^3) |}]

    let%expect_test _ =
      pp (!0 * z * y) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp (!1 * z * y) ;
      [%expect {| (%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (!7 * z * (!2 * y)) ;
      [%expect {| 14×(%y_1 × %z_2) |}]

    let%expect_test _ =
      pp (!13 + (!42 * z)) ;
      [%expect {| (13 + 42×%z_2) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42) ;
      [%expect {| (42 + 13×%z_2) |}]

    let%expect_test _ =
      pp ((!2 * z) - !3 + ((!(-2) * z) + !3)) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!3 * y) + (!13 * z) + !42) ;
      [%expect {| (42 + 3×%y_1 + 13×%z_2) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y)) ;
      [%expect {| (42 + 3×%y_1 + 13×%z_2) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!2 * z)) ;
      [%expect {| (42 + 3×%y_1 + 15×%z_2) |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 + (!3 * y) + (!(-13) * z)) ;
      [%expect {| (42 + 3×%y_1) |}]

    let%expect_test _ =
      pp (z + !42 + ((!3 * y) + (!(-1) * z))) ;
      [%expect {| (42 + 3×%y_1) |}]

    let%expect_test _ =
      pp (!(-1) * (z + (!(-1) * y))) ;
      [%expect {| (%y_1 + -1×%z_2) |}]

    let%expect_test _ =
      pp (((!3 * y) + !2) * (!4 + (!5 * z))) ;
      [%expect {| ((2 + 3×%y_1) × (4 + 5×%z_2)) |}]

    let%expect_test _ =
      pp (((!2 * z) - !3 + ((!(-2) * z) + !3)) * (!4 + (!5 * z))) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pp ((!13 * z) + !42 - ((!3 * y) + (!13 * z))) ;
      [%expect {| (42 + -3×%y_1) |}]

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
      ppf (!1 = !0) ;
      [%expect {| ff |}]

    let%expect_test _ =
      ppf (F.iff (!3 * y = z) F.tt) ;
      [%expect {| (3×%y_1 = %z_2) |}]

    let%expect_test _ =
      ppf (F.iff F.tt (!3 * y = z)) ;
      [%expect {| (3×%y_1 = %z_2) |}]

    let%expect_test _ =
      ppf (F.iff (!3 * y = z) F.ff) ;
      [%expect {| (3×%y_1 ≠ %z_2) |}]

    let%expect_test _ =
      ppf (F.iff F.ff (!3 * y = z)) ;
      [%expect {| (3×%y_1 ≠ %z_2) |}]

    let%expect_test _ =
      pp (y - (!(-3) * y) + !4) ;
      [%expect {| (4 + 4×%y_1) |}]

    let%expect_test _ =
      pp ((!(-3) * y) + !4 - y) ;
      [%expect {| (4 + -4×%y_1) |}]

    let%expect_test _ =
      ppf (y = (!(-3) * y) + !4) ;
      [%expect {| (4 = 4×%y_1) |}]

    let%expect_test _ =
      ppf ((!(-3) * y) + !4 = y) ;
      [%expect {| (4 = 4×%y_1) |}]

    let%expect_test _ =
      pp (T.sub (F.inject F.tt) (F.inject (z = !4))) ;
      [%expect {| ((4 = %z_2) ? 0 : 1) |}]

    let%expect_test _ =
      pp (T.add (F.inject F.tt) (F.inject (F.iff (z = !4) (z = !4)))) ;
      [%expect {| 2 |}]

    let%expect_test _ =
      ppf ((!13 * z) + !42 = (!3 * y) + (!13 * z)) ;
      [%expect {| (42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((!13 * z) + !(-42) = (!3 * y) + (!13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((!13 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ((!10 * z) + !42 = (!(-3) * y) + (!13 * z)) ;
      [%expect {| ((42 + 3×%y_1) = 3×%z_2) |}]

    let%expect_test _ =
      ppf ~~((!13 * z) + !(-42) != (!3 * y) + (!13 * z)) ;
      [%expect {| (-42 = 3×%y_1) |}]

    let%expect_test _ =
      ppf ~~(!2 < y && z <= !3) ;
      [%expect {| ((3 < %z_2) ∨ (2 ≥ %y_1)) |}]

    let%expect_test _ =
      ppf ~~(!2 <= y || z < !3) ;
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
      let z1 = z + !1 in
      let z1_2 = z1 * z1 in
      pp z1_2 ;
      pp (z1_2 * z1_2) ;
      [%expect {|
        (1 + %z_2)^2

        (1 + %z_2)^4 |}]
  end )
