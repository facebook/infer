(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let%test_module _ =
  ( module struct
    open Fol
    open Context

    let () = Trace.init ~margin:68 ()

    (* let () =
     *   Trace.init ~margin:160
     *     ~config:(Result.ok_exn (Trace.parse "+Fol"))
     *     ()
     *
     * [@@@warning "-32"] *)

    let printf pp = Format.printf "@\n%a@." pp
    let pp_raw = printf pp_raw
    let pp = Format.printf "@\n@[<hv>  %a@]@." pp
    let ( ! ) i = Term.integer (Z.of_int i)
    let ( + ) = Term.add
    let ( - ) = Term.sub

    (* let ( * ) i e = Term.mulq (Q.of_int i) e *)
    let wrt = Var.Set.empty
    let t_, wrt = Var.fresh "t" ~wrt

    (* let u_, wrt = Var.fresh "u" ~wrt *)
    (* let v_, wrt = Var.fresh "v" ~wrt *)
    let w_, wrt = Var.fresh "w" ~wrt
    let x_, wrt = Var.fresh "x" ~wrt
    let y_, wrt = Var.fresh "y" ~wrt
    let z_, wrt = Var.fresh "z" ~wrt
    let t = Term.var t_

    (* let u = Term.var u_ *)
    (* let v = Term.var v_ *)
    let w = Term.var w_
    let x = Term.var x_
    let y = Term.var y_
    let z = Term.var z_
    let f = Term.mul t

    (* let g = Term.mul u *)

    let of_eqs l =
      List.fold ~init:(wrt, empty)
        ~f:(fun (us, r) (a, b) -> add us (Formula.eq a b) r)
        l
      |> snd

    (* let and_eq a b r = and_formula wrt (Formula.eq a b) r |> snd *)
    (* let and_ r s = and_ wrt r s |> snd *)
    let or_ r s = interN wrt [r; s] |> snd
    let difference x e f = Term.d_int (Context.normalize x (Term.sub e f))
    let r0 = empty

    let%test _ = difference r0 (f x) (f x) |> Poly.equal (Some (Z.of_int 0))
    let%test _ = difference r0 !4 !3 |> Poly.equal (Some (Z.of_int 1))

    let r1 = of_eqs [(x, y)]
    let r2 = of_eqs [(x, y); (f x, y); (f y, z)]

    let%test _ = difference (or_ r1 r2) x z |> Poly.equal None

    let r4 = of_eqs [(w + !2, x - !3); (x - !5, y + !7); (y, z - !4)]

    let%test _ = difference r4 x w |> Poly.equal (Some (Z.of_int 5))

    let r9 = of_eqs [(x, z - !16)]

    let%expect_test _ =
      pp r9 ;
      pp_raw r9 ;
      [%expect
        {|
        (-16 + %z_5) = %x_3
    
      {sat= true;
       rep= [[%x_3 ↦ (%z_5 + -16)]; [%z_5 ↦ ]; [-1 ↦ ]; [0 ↦ ]]} |}]

    let%test _ = difference r9 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let r10 = of_eqs [(!16, z - x)]

    let%expect_test _ =
      pp r10 ;
      pp_raw r10 ;
      Format.printf "@.%a@." Term.pp (z - (x + !8)) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (z - (x + !8))) ;
      Format.printf "@.%a@." Term.pp (x + !8 - z) ;
      Format.printf "@.%a@." Term.pp (normalize r10 (x + !8 - z)) ;
      [%expect
        {|
          (-16 + %z_5) = %x_3
    
        {sat= true;
         rep= [[%x_3 ↦ (%z_5 + -16)]; [%z_5 ↦ ]; [-1 ↦ ]; [0 ↦ ]]}

        (%z_5 - (%x_3 + 8))

        8

        ((%x_3 + 8) - %z_5)

        -8 |}]

    let%test _ = difference r10 z (x + !8) |> Poly.equal (Some (Z.of_int 8))

    let%test _ =
      difference r10 (x + !8) z |> Poly.equal (Some (Z.of_int (-8)))
  end )
