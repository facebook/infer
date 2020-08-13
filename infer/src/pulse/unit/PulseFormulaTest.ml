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

    let simplify ~keep phi =
      Formula.simplify ~keep:(AbstractValue.Set.of_list keep) phi |> F.printf "%a" Formula.pp


    (** shorthand for defining formulas easily *)

    let i i = Formula.Term.of_intlit (IntLit.of_int i)

    let ( + ) t1 t2 = Formula.Term.of_binop (PlusA None) t1 t2

    let ( - ) t1 t2 = Formula.Term.of_binop (MinusA None) t1 t2

    let ( = ) t1 t2 = Formula.mk_equal t1 t2

    let ( < ) t1 t2 = Formula.mk_less_than t1 t2

    let ( && ) phi1 phi2 = Formula.aand phi1 phi2

    let x_var = AbstractValue.mk_fresh ()

    let x = Formula.Term.of_absval x_var

    let y_var = AbstractValue.mk_fresh ()

    let y = Formula.Term.of_absval y_var

    let z_var = AbstractValue.mk_fresh ()

    let z = Formula.Term.of_absval z_var

    let w = Formula.Term.of_absval (AbstractValue.mk_fresh ())

    let v = Formula.Term.of_absval (AbstractValue.mk_fresh ())

    let%expect_test _ =
      normalize (x + i 1 - i 1 < x) ;
      [%expect {|
        [ &&
        {((v1)+(1))+(-1) < v1}]|}]

    let%expect_test _ =
      normalize (x + (y - x) < y) ;
      [%expect {|
        [ &&
        {(v1)+((v2)+(-(v1))) < v2}]|}]

    let%expect_test _ =
      normalize (x = y && y = z && z = i 0 && x = i 1) ;
      [%expect {|false|}]

    (* should be false (x = w + (y+1) -> 1 = w + z -> 1 = 0) but we don't go and normalize sub-terms
       in the existing relation when adding new equalities to the relation *)
    let%expect_test _ =
      normalize (x = w + y + i 1 && y + i 1 = z && x = i 1 && w + z = i 0) ;
      [%expect {|
[0=(v4)+(v3)∧1=v1=((v4)+(v2))+(1)∧v3=(v2)+(1) &&
]|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = i 0 && y = i 1 && z = i 2 && w = i 3) ;
      [%expect {|
[0=v1∧1=v2∧2=v3∧3=v4 &&
]|}]

    let%expect_test _ =
      simplify ~keep:[y_var] (x = y + i 1 && x = i 0) ;
      [%expect {|
[0=v1=(v2)+(1) &&
]|}]

    (* should keep most of this or realize that [w = z] hence this boils down to [z+1 = 0] *)
    let%expect_test _ =
      simplify ~keep:[y_var; z_var] (x = y + z && w = x - y && v = w + i 1 && v = i 0) ;
      [%expect {|
[0=v5=(v4)+(1)∧v1=(v2)+(v3)∧v4=(v1)+(-(v2)) &&
]|}]
  end )
