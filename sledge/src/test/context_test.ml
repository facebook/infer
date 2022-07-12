(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol

let%test_module _ =
  ( module struct
    open Context

    let () = Dbg.init ~margin:68 ()

    (* let () =
     *   Dbg.init ~margin:160
     *     ~config:
     *       (Dbg.parse_exn
     *          "+Context-Context.canon-Context.canon_f-Context.norm-Context.find_extend_" )
     *     () *)

    [@@@warning "-unused-value-declaration"]

    let vx = ref Var.Set.empty

    let var name =
      let x_, wrt = Var.fresh name ~wrt:!vx in
      vx := wrt ;
      (x_, Term.var x_)

    let of_eqs l =
      List.fold
        ~f:(fun (a, b) (us, r) -> add us (Formula.eq a b) r)
        l (!vx, empty)
      |> snd

    let implies_eq r a b = implies r (Formula.eq a b)
    let printf pp = Format.printf "@\n%a@." pp
    let pp = printf Context.pp_raw
    let i n = Term.integer (Z.of_int n)
    let x_, x = var "x"

    (* tests *)

    let b = Formula.inject (Formula.dq x (i 0))
    let r15 = of_eqs [(b, b); (x, i 1)]

    let%expect_test _ =
      pp r15 ;
      [%expect
        {|
          { sat= true; rep= [[%x_1 ↦ 1]]; cls= [[1 ↦ {%x_1}]]; use= [] } |}]

    let%test _ = implies_eq r15 (Term.neg b) (Term.apply (Signed 1) [|i 1|])
    let%test _ = implies_eq r15 (Term.apply (Unsigned 1) [|b|]) (i 1)

    (* let%expect_test _ =
     *   replay
     *     {||} ;
     *   [%expect {| |}] *)
  end )
