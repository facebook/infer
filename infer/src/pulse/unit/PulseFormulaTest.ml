(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue
open PulseFormula
open PulseSatUnsat.Import

(** {2 Utilities for defining formulas easily}

    We want to be able to write something close to [x + y - 42 < z], but the API of {!PulseFormula}
    doesn't support building arbitrary formulas or even arbitrary terms. Instead, we have to
    introduce intermediate variables for each sub-expression (this corresponds to how the rest of
    Pulse interacts with the arithmetic layer, so it's good that we follow this convention here
    too).

    The definitions here make this transparent by passing the formula around. For example, building
    [x+y] takes in a formula [phi] and returns [(phi ∧ v123 = x+y, v123)], i.e. a pair of the
    formula with a new intermediate equality and the resulting intermediate variable. This allows us
    to chain operations: [x+y-42] is a function that takes a formula, passes it to [x+y] returning
    [(phi',v123)] as we saw with [phi' = phi ∧ v123 = x+y], passes it to "42", which here is also
    a function returning [(phi',42)] (note the unchanged [phi']), then finally returns
    [(phi ∧ v123 = x+y ∧ v234 = v123-42, v234)].

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
  let+ phi, _new_eqs = and_equal_binop v bop op1 op2 phi in
  (phi, AbstractValueOperand v)


let ( + ) f1 f2 phi = of_binop (PlusA None) f1 f2 phi

let ( - ) f1 f2 phi = of_binop (MinusA None) f1 f2 phi

let ( * ) f1 f2 phi = of_binop (Mult None) f1 f2 phi

let ( / ) f1 f2 phi = of_binop Div f1 f2 phi

let ( & ) f1 f2 phi = of_binop BAnd f1 f2 phi

let ( mod ) f1 f2 phi = of_binop Mod f1 f2 phi

let ( = ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_equal op1 op2 phi >>| fst


let ( < ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_less_than op1 op2 phi >>| fst


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

let w_var = mk_var "w"

let w = op_of_var w_var

let v = op_of_var (mk_var "v")

(** reset to this state before each test so that variable id's remain stable when tests are added in
    the future *)
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
  phi ttrue >>= f >>| fst |> F.printf "%a" normalized_pp


let dummy_tenv = Tenv.create ()

let dummy_get_dynamic_type _ = None

let normalize phi = test ~f:(normalize dummy_tenv ~get_dynamic_type:dummy_get_dynamic_type) phi

let simplify ~keep phi =
  test
    ~f:
      (simplify dummy_tenv ~get_dynamic_type:dummy_get_dynamic_type
         ~keep:(AbstractValue.Set.of_list keep))
    phi


let%test_module "normalization" =
  ( module struct
    let%expect_test _ =
      normalize (x < y) ;
      [%expect
        {|
        known=true (no var=var) && true (no linear) && {[x + -y] < 0}, pruned=true (no atoms),
        both=true (no var=var) && true (no linear) && {[x + -y] < 0}|}]

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
      [%expect
        {|
        known=true (no var=var) && x = 0 ∧ y = 1 ∧ v6 = 0 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && x = 0 ∧ y = 1 ∧ v6 = 0 && true (no atoms)|}]

    let%expect_test _ =
      normalize (x = i 0 && x < i 0) ;
      [%expect {|unsat|}]

    let%expect_test _ =
      normalize (x + y < x + y) ;
      [%expect {|unsat|}]

    let%expect_test "nonlinear arithmetic" =
      normalize (z * (x + (v * y) + i 1) / w = i 0) ;
      [%expect
        {|
        known=true (no var=var)
              &&
              x = -v6 + v8 -1 ∧ v7 = v8 -1 ∧ v10 = 0
              &&
              {0 = [v9]÷[w]}∧{[v6] = [v]×[y]}∧{[v9] = [z]×[v8]},
        pruned=true (no atoms),
        both=true (no var=var)
             &&
             x = -v6 + v8 -1 ∧ v7 = v8 -1 ∧ v10 = 0
             &&
             {0 = [v9]÷[w]}∧{[v6] = [v]×[y]}∧{[v9] = [z]×[v8]} |}]

    (* check that this becomes all linear equalities *)
    let%expect_test _ =
      normalize (i 12 * (x + (i 3 * y) + i 1) / i 7 = i 0) ;
      [%expect
        {|
        known=true (no var=var)
              &&
              x = -v6 -1 ∧ y = 1/3·v6 ∧ v7 = -1 ∧ v8 = 0 ∧ v9 = 0 ∧ v10 = 0
              &&
              true (no atoms),
        pruned=true (no atoms),
        both=true (no var=var)
             &&
             x = -v6 -1 ∧ y = 1/3·v6 ∧ v7 = -1 ∧ v8 = 0 ∧ v9 = 0 ∧ v10 = 0
             &&
             true (no atoms)|}]

    (* check that this becomes all linear equalities thanks to constant propagation *)
    let%expect_test _ =
      normalize (z * (x + (v * y) + i 1) / w = i 0 && z = i 12 && v = i 3 && w = i 7) ;
      [%expect
        {|
        known=true (no var=var)
              &&
              x = -v6 -1 ∧ y = 1/3·v6 ∧ z = 12 ∧ w = 7 ∧ v = 3 ∧ v7 = -1
               ∧ v8 = 0 ∧ v9 = 0 ∧ v10 = 0
              &&
              true (no atoms),
        pruned=true (no atoms),
        both=true (no var=var)
             &&
             x = -v6 -1 ∧ y = 1/3·v6 ∧ z = 12 ∧ w = 7 ∧ v = 3 ∧ v7 = -1
              ∧ v8 = 0 ∧ v9 = 0 ∧ v10 = 0
             &&
             true (no atoms)|}]
  end )

let%test_module "variable elimination" =
  ( module struct
    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y) ;
      [%expect
        {|
        known=x=y && true (no linear) && true (no atoms), pruned=true (no atoms),
        both=x=y && true (no linear) && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = i 0 && y = i 1 && z = i 2 && w = i 3) ;
      [%expect
        {|
        known=true (no var=var) && x = 0 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && x = 0 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var] (x = y + i 1 && x = i 0) ;
      [%expect
        {|
          known=true (no var=var) && x = 0 && true (no atoms), pruned=true (no atoms),
          both=true (no var=var) && x = 0 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[y_var] (x = y + i 1 && x = i 0) ;
      [%expect
        {|
        known=true (no var=var) && y = -1 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && y = -1 && true (no atoms)|}]

    (* should keep most of this or realize that [w = z] hence this boils down to [z+1 = 0] *)
    let%expect_test _ =
      simplify ~keep:[y_var; z_var] (x = y + z && w = x - y && v = w + i 1 && v = i 0) ;
      [%expect
        {|
        known=true (no var=var) && x = y -1 ∧ z = -1 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && x = y -1 ∧ z = -1 && true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + z && w + x + y = i 0 && v = w + i 1) ;
      [%expect
        {|
          known=true (no var=var)
                &&
                x = -v + v7 +1 ∧ y = -v7 ∧ z = -v + 2·v7 +1 ∧ w = v -1
                &&
                true (no atoms),
          pruned=true (no atoms),
          both=true (no var=var)
               &&
               x = -v + v7 +1 ∧ y = -v7 ∧ z = -v + 2·v7 +1 ∧ w = v -1
               &&
               true (no atoms)|}]

    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + i 4 && x = w && y = z) ;
      [%expect
        {|
        known=true (no var=var) && x = y +4 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && x = y +4 && true (no atoms)|}]
  end )

let%test_module "non-linear simplifications" =
  ( module struct
    let%expect_test "zero propagation" =
      simplify ~keep:[w_var] (((i 0 / (x * z)) & v) * v mod y = w) ;
      [%expect
        {|
        known=true (no var=var) && w = 0 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && w = 0 && true (no atoms)|}]

    let%expect_test "constant propagation: bitshift" =
      simplify ~keep:[x_var] (of_binop Shiftlt (of_binop Shiftrt (i 0b111) (i 2)) (i 2) = x) ;
      [%expect
        {|
        known=true (no var=var) && x = 4 && true (no atoms), pruned=true (no atoms),
        both=true (no var=var) && x = 4 && true (no atoms)|}]

    let%expect_test "non-linear becomes linear" =
      normalize (w = (i 2 * z) - i 3 && z = x * y && y = i 2) ;
      [%expect
        {|
          known=z=v8 ∧ w=v7 && x = 1/4·v6 ∧ y = 2 ∧ z = 1/2·v6 ∧ w = v6 -3 && true (no atoms),
          pruned=true (no atoms),
          both=z=v8 ∧ w=v7 && x = 1/4·v6 ∧ y = 2 ∧ z = 1/2·v6 ∧ w = v6 -3 && true (no atoms)|}]
  end )
