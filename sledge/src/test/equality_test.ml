(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ses

let%test_module _ =
  ( module struct
    open Equality

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160
     *     ~config:(Result.get_ok (Trace.parse "+Equality"))
     *     ()
     *
     * [@@@warning "-32"] *)

    let printf pp = Format.printf "@\n%a@." pp
    let pp = printf pp
    let pp_classes = Format.printf "@\n@[<hv>  %a@]@." pp_classes
    let ( ! ) i = Term.integer (Z.of_int i)
    let g = Term.rem
    let wrt = Var.Set.empty
    let t_, wrt = Var.fresh "t" ~wrt
    let u_, wrt = Var.fresh "u" ~wrt
    let v_, wrt = Var.fresh "v" ~wrt
    let w_, wrt = Var.fresh "w" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, wrt = Var.fresh "z" ~wrt
    let t = Term.var t_
    let u = Term.var u_
    let v = Term.var v_
    let w = Term.var w_
    let x = Term.var x_
    let y = Term.var y_
    let z = Term.var z_

    let of_eqs l =
      List.fold ~init:(wrt, true_)
        ~f:(fun (us, r) (a, b) -> and_eq us a b r)
        l
      |> snd

    let implies_eq r a b = implies r (Term.eq a b)

    (* tests *)

    let r3 = of_eqs [(g y z, w); (v, w); (g y w, t); (x, v); (x, u); (u, z)]

    let%expect_test _ =
      pp_classes r3 ;
      pp r3 ;
      [%expect
        {|
        %t_1 = %u_2 = %v_3 = %w_4 = %x_5 = %z_7 = (%y_6 rem %t_1)
        = (%y_6 rem %t_1)

      {sat= true;
       rep= [[%t_1 ↦ ];
             [%u_2 ↦ %t_1];
             [%v_3 ↦ %t_1];
             [%w_4 ↦ %t_1];
             [%x_5 ↦ %t_1];
             [%y_6 ↦ ];
             [%z_7 ↦ %t_1];
             [(%y_6 rem %v_3) ↦ %t_1];
             [(%y_6 rem %z_7) ↦ %t_1];
             [-1 ↦ ];
             [0 ↦ ]]} |}]

    let%test _ = implies_eq r3 t z

    let b = Term.dq x !0
    let r15 = of_eqs [(b, b); (x, !1)]

    let%expect_test _ =
      pp r15 ;
      [%expect
        {|
          {sat= true; rep= [[%x_5 ↦ 1]; [(%x_5 ≠ 0) ↦ -1]; [-1 ↦ ]; [0 ↦ ]]} |}]

    let%test _ = implies_eq r15 b (Term.signed 1 !1)
    let%test _ = implies_eq r15 (Term.unsigned 1 b) !1
  end )
