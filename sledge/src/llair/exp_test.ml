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

    open Exp

    let pp e = Format.printf "@\n{desc= %a; term= %a}@." pp e Term.pp e.term
    let ( ! ) i = integer Typ.siz (Z.of_int i)

    let%expect_test _ =
      pp (signed 1 !1 ~to_:Typ.bool) ;
      [%expect {| {desc= ((i1)(s1) 1); term= -1} |}]

    let%expect_test _ =
      pp (unsigned 1 !(-1) ~to_:Typ.byt) ;
      [%expect {| {desc= ((i8)(u1) -1); term= 1} |}]

    let%expect_test _ =
      pp (signed 8 !(-1) ~to_:Typ.int) ;
      [%expect {| {desc= ((i32)(s8) -1); term= -1} |}]

    let%expect_test _ =
      pp (unsigned 8 !(-1) ~to_:Typ.int) ;
      [%expect {| {desc= ((i32)(u8) -1); term= 255} |}]

    let%expect_test _ =
      pp (signed 8 !255 ~to_:Typ.byt) ;
      [%expect {| {desc= ((i8)(s8) 255); term= -1} |}]

    let%expect_test _ =
      pp (signed 7 !255 ~to_:Typ.byt) ;
      [%expect {| {desc= ((i8)(s7) 255); term= -1} |}]

    let%expect_test _ =
      pp (unsigned 7 !255 ~to_:Typ.byt) ;
      [%expect {| {desc= ((i8)(u7) 255); term= 127} |}]

    let%expect_test _ =
      pp (uge (integer Typ.bool Z.minus_one) (signed 1 !1 ~to_:Typ.bool)) ;
      [%expect {| {desc= (-1 u≥ ((i1)(s1) 1)); term= -1} |}]
  end )
