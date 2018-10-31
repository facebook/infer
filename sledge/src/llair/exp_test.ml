(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    let pf = Format.printf "%t%a@." (fun _ -> Trace.flush ()) Exp.pp
    let char = Typ.integer ~bits:8
    let ( ! ) i = Exp.integer (Z.of_int i) char
    let ( + ) = Exp.add
    let ( && ) = Exp.and_
    let ( || ) = Exp.or_

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
      pf (!42 + !13) ;
      [%expect {| 55 |}]

    let%expect_test _ =
      pf (!(-128) && !127) ;
      [%expect {| 0 |}]

    let%expect_test _ =
      pf (!(-128) || !127) ;
      [%expect {| -1 |}]
  end )
