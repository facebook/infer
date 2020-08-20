(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue
open PulseFormula
open SatUnsatMonad

let%test_module _ =
  ( module struct
    (** {2 Utilities for defining formulas easily}

        We want to be able to write something close to [x + y - 42 < z], but the API of
        {!PulseFormula} doesn't support building arbitrary formulas or even arbitrary terms.
        Instead, we have to introduce intermediate variables for each sub-expression (this
        corresponds to how the reste of Pulse interacts with the arithmetic layer too, so it's good
        that we follow this constraint here too).

        The definitions here make this transparent by passing the formula around. For example,
        building [x+y] takes in a formula [phi] and returns [(phi ∧ v123 = x+y, v123)], i.e. a
        pair of the formula with a new intermediate equality and the resulting intermediate
        variable. This allows us to chain operations: [x+y-42] is a function that takes a formula,
        passes it to [x+y] returning [(phi',v123)] as we saw with [phi' = phi ∧ v123 = x+y],
        passes it to "42", which here is also a function returning [(phi',42)] (note the unchanged
        [phi']), then finally returns [(phi ∧ v123 = x+y ∧ v234 = v123-42, v234)].

        This is convoluted, especially as each step may also return [Unsat] even during "term"
        construction, but as a result the tests themselves should be straightforward to understand. *)

    (** a literal integer leaves the formula unchanged and returns a [LiteralOperand] *)
    let i i phi = Sat (phi, LiteralOperand (IntLit.of_int i))

    (** similarly as for literals; this is not used directly in tests so the name is a bit more
        descriptive *)
    let op_of_var x phi = Sat (phi, AbstractValueOperand x)

    let of_binop bop f1 f2 phi =
      let* phi, op1 = f1 phi in
      let* phi, op2 = f2 phi in
      let v = Var.mk_fresh () in
      let+ phi = and_equal_binop v bop op1 op2 phi in
      (phi, AbstractValueOperand v)


    let ( + ) f1 f2 phi = of_binop (PlusA None) f1 f2 phi

    let ( - ) f1 f2 phi = of_binop (MinusA None) f1 f2 phi

    let ( = ) f1 f2 phi =
      let* phi, op1 = f1 phi in
      let* phi, op2 = f2 phi in
      and_equal op1 op2 phi


    let ( < ) f1 f2 phi =
      let* phi, op1 = f1 phi in
      let* phi, op2 = f2 phi in
      and_less_than op1 op2 phi


    let ( && ) f1 f2 phi = f1 phi >>= f2

    (* we remember a mapping [Var.t -> string] to print more readable results that mention the
       user-defined variables by their readable names instead of [v123] *)
    let var_names = Caml.Hashtbl.create 4

    let mk_var name =
      let v = AbstractValue.mk_fresh () in
      Caml.Hashtbl.add var_names v name ;
      v


    let x_var = mk_var "x"

    let x = op_of_var x_var

    let y_var = mk_var "y"

    let y = op_of_var y_var

    let z_var = mk_var "z"

    let z = op_of_var z_var

    let w = op_of_var (mk_var "w")

    let v = op_of_var (mk_var "v")

    (** reset to this state before each test so that variable id's remain stable when tests are
        added in the future *)
    let init_vars_state = AbstractValue.State.get ()

    let pp_var fmt v =
      match Caml.Hashtbl.find_opt var_names v with
      | Some name ->
          F.pp_print_string fmt name
      | None ->
          AbstractValue.pp fmt v


    let normalized_pp fmt = function
      | Unsat ->
          F.pp_print_string fmt "unsat"
      | Sat phi ->
          pp_with_pp_var pp_var fmt phi


    let test ~f phi =
      AbstractValue.State.set init_vars_state ;
      phi ttrue >>= f |> F.printf "%a" normalized_pp


    let normalize phi = test ~f:normalize phi

    let simplify ~keep phi = test ~f:(simplify ~keep:(AbstractValue.Set.of_list keep)) phi

    (** the actual tests *)

    let%expect_test _ =
      normalize (x < y) ;
      [%expect {|true (no var=var) && true (no linear) && {x < y}|}]

    let%expect_test _ =
      normalize (x + i 1 - i 1 < x) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      normalize (x + (y - x) < y) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      normalize (x = y && y = z && z = i 0 && x = i 1) ;
      [%expect {|unsat|}]

    (* should be false (x = w + (y+1) -> 1 = w + z -> 1 = 0)  *)
    let%expect_test _ =
      normalize (x = w + y + i 1 && y + i 1 = z && x = i 1 && w + z = i 0) ;
      [%expect {|unsat|}]

    (* same as above but atoms are given in the opposite order *)
    let%expect_test _ =
      normalize (w + z = i 0 && x = i 1 && y + i 1 = z && x = w + y + i 1) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      normalize (of_binop Ne x y = i 0 && x = i 0 && y = i 1) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      normalize (of_binop Eq x y = i 0 && x = i 0 && y = i 1) ;
      [%expect {|true (no var=var) && x = 0 ∧ y = 1 ∧ v6 = 0 && true (no atoms)|}]

    let%expect_test _ =
      normalize (x = i 0 && x < i 0) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = i 0 && y = i 1 && z = i 2 && w = i 3) ;
      [%expect {|true (no var=var) && x = 0 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = y + i 1 && x = i 0) ;
      [%expect {|x=v6 && x = 0 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[y_var] (x = y + i 1 && x = i 0) ;
      [%expect {|true (no var=var) && y = -1 && true (no atoms)|}]

    (* should keep most of this or realize that [w = z] hence this boils down to [z+1 = 0] *)
    let%expect_test _ =
      simplify ~keep:[y_var; z_var] (x = y + z && w = x - y && v = w + i 1 && v = i 0) ;
      [%expect {|x=v6 ∧ z=w=v7 && x = y -1 ∧ z = -1 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + z && w + x + y = i 0 && v = w + i 1) ;
      [%expect
        {|
        x=v6 ∧ v=v9
        &&
        x = 1/2·z + -1/2·w ∧ y = -1/2·z + -1/2·w ∧ v = w +1 ∧ v7 = 1/2·z + 1/2·w
        &&
        true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + i 4 && x = w && y = z) ;
      [%expect {|x=w=v6 ∧ y=z && x = y +4 && true (no atoms)|}]
  end )
