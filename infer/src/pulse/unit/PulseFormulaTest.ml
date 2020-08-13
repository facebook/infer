(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module Formula = PulseFormula

let%test_module _ =
  ( module struct
    let normalize phi = Formula.normalize phi |> F.printf "%a" Formula.pp

    (** shorthand for defining formulas easily *)

    let i i = Formula.Term.of_intlit (IntLit.of_int i)

    let ( + ) t1 t2 = Formula.Term.of_binop (PlusA None) t1 t2

    let ( - ) t1 t2 = Formula.Term.of_binop (MinusA None) t1 t2

    let ( = ) t1 t2 = Formula.mk_equal t1 t2

    let ( < ) t1 t2 = Formula.mk_less_than t1 t2

    let ( && ) phi1 phi2 = Formula.aand phi1 phi2

    let x = Formula.Term.of_absval (AbstractValue.mk_fresh ())

    let y = Formula.Term.of_absval (AbstractValue.mk_fresh ())

    let z = Formula.Term.of_absval (AbstractValue.mk_fresh ())

    let%expect_test _ =
      normalize (x + i 1 - i 1 < x) ;
      [%expect "\n        [ &&\n        {((v1)+(1))+(-1) < v1}]"]

    let%expect_test _ =
      normalize (x + (y - x) < y) ;
      [%expect "\n        [ &&\n        {(v1)+((v2)+(-(v1))) < v2}]"]

    let%expect_test _ =
      normalize (x = y && y = z && z = i 0 && x = i 1) ;
      [%expect "\n        false"]
  end )
