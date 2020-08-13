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
    (** shorthand for defining formulas easily *)

    let i i = Formula.Term.of_intlit (IntLit.of_int i)

    let ( + ) t1 t2 = Formula.Term.of_binop (PlusA None) t1 t2

    let ( - ) t1 t2 = Formula.Term.of_binop (MinusA None) t1 t2

    let ( = ) t1 t2 = Formula.mk_equal t1 t2

    let ( < ) t1 t2 = Formula.mk_less_than t1 t2

    let ( && ) phi1 phi2 = Formula.aand phi1 phi2

    let var_names = Caml.Hashtbl.create 4

    let mk_var name =
      let v = AbstractValue.mk_fresh () in
      Caml.Hashtbl.add var_names v name ;
      v


    let x_var = mk_var "x"

    let x = Formula.Term.of_absval x_var

    let y_var = mk_var "y"

    let y = Formula.Term.of_absval y_var

    let z_var = mk_var "z"

    let z = Formula.Term.of_absval z_var

    let w = Formula.Term.of_absval (mk_var "w")

    let v = Formula.Term.of_absval (mk_var "v")

    (** utilities for writing tests *)

    let pp_var fmt v =
      match Caml.Hashtbl.find_opt var_names v with
      | Some name ->
          F.pp_print_string fmt name
      | None ->
          AbstractValue.pp fmt v


    let formula_pp = Formula.pp_with_pp_var pp_var

    let normalize phi = Formula.normalize phi |> F.printf "%a" formula_pp

    let simplify ~keep phi =
      Formula.simplify ~keep:(AbstractValue.Set.of_list keep) phi |> F.printf "%a" formula_pp


    (** the actual tests *)

    let%expect_test _ =
      normalize (x + i 1 - i 1 < x) ;
      [%expect {|
        [true && {(x+1)+(-1) < x}]|}]

    let%expect_test _ =
      normalize (x + (y - x) < y) ;
      [%expect {|
        [true && {x+(y-x) < y}]|}]

    let%expect_test _ =
      normalize (x = y && y = z && z = i 0 && x = i 1) ;
      [%expect {|false|}]

    (* should be false (x = w + (y+1) -> 1 = w + z -> 1 = 0) but we don't go and normalize sub-terms
       in the existing relation when adding new equalities to the relation *)
    let%expect_test _ =
      normalize (x = w + y + i 1 && y + i 1 = z && x = i 1 && w + z = i 0) ;
      [%expect {|
[0=(w+z) ∧ 1=x=((w+y)+1) ∧ z=(y+1) && true]|}]

    let%expect_test _ =
      normalize (Formula.Term.of_binop Ne x y = i 0 && x = i 0 && y = i 1) ;
      [%expect {|
        [0=x=(x≠y) ∧ 1=y && true]|}]

    let%expect_test _ =
      normalize (Formula.Term.of_binop Eq x y = i 0 && x = i 0 && y = i 1) ;
      [%expect {|
        [0=x=(x=y) ∧ 1=y && true]|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = i 0 && y = i 1 && z = i 2 && w = i 3) ;
      [%expect {|
[0=x ∧ 1=y ∧ 2=z ∧ 3=w && true]|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = y + i 1 && x = i 0) ;
      [%expect {|
[0=x=(y+1) && true]|}]

    let%expect_test _ =
      simplify ~keep:[y_var] (x = y + i 1 && x = i 0) ;
      [%expect {|
[0=x=(y+1) && true]|}]

    (* should keep most of this or realize that [w = z] hence this boils down to [z+1 = 0] *)
    let%expect_test _ =
      simplify ~keep:[y_var; z_var] (x = y + z && w = x - y && v = w + i 1 && v = i 0) ;
      [%expect {|
[0=v=(w+1) ∧ x=(y+z) ∧ w=(x-y) && true]|}]
  end )
