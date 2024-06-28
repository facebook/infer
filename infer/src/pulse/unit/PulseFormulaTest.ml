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
    [(phi',v123)] as we saw with [phi' = phi ∧ v123 = x+y], passes it to "42", which here is also a
    function returning [(phi',42)] (note the unchanged [phi']), then finally returns
    [(phi ∧ v123 = x+y ∧ v234 = v123-42, v234)].

    This is convoluted, especially as each step may also return [Unsat] even during "term"
    construction, but as a result the tests themselves should be straightforward to understand. *)

(** a literal integer leaves the formula unchanged and returns a [ConstOperand] *)
let i i phi = Sat (phi, ConstOperand (Cint (IntLit.of_int i)))

(** a literal string leaves the formula unchanged and returns a [ConstOperand] *)
let s s phi = Sat (phi, ConstOperand (Cstr s))

(** similarly as for literals; this is not used directly in tests so the name is a bit more
    descriptive *)
let op_of_var x phi = Sat (phi, AbstractValueOperand x)

let of_binop bop f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  let v = Var.mk_fresh () in
  let+ phi, _new_eqs = and_equal_binop v bop op1 op2 phi in
  (phi, AbstractValueOperand v)


(* the following are shorthand notations that are generally useful to keep around *)
[@@@warning "-unused-value-declaration"]

let instanceof typ x_var y_var phi =
  let+ phi, _new_eqs = and_equal_instanceof y_var x_var typ ~nullable:false phi in
  phi


let is_int x phi =
  let+ phi, _new_eqs = and_is_int x phi in
  phi


let ( + ) f1 f2 phi = of_binop (PlusA None) f1 f2 phi

let ( - ) f1 f2 phi = of_binop (MinusA None) f1 f2 phi

let ( * ) f1 f2 phi = of_binop (Mult None) f1 f2 phi

let ( / ) f1 f2 phi = of_binop DivI f1 f2 phi

let ( & ) f1 f2 phi = of_binop BAnd f1 f2 phi

let ( mod ) f1 f2 phi = of_binop Mod f1 f2 phi

let ( ^ ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  let v = Var.mk_fresh () in
  let+ phi, _new_eqs = and_equal_string_concat v op1 op2 phi in
  (phi, AbstractValueOperand v)


let eq f1 f2 phi = of_binop Eq f1 f2 phi

let ne f1 f2 phi = of_binop Ne f1 f2 phi

let ge f1 f2 phi = of_binop Ge f1 f2 phi

let gt f1 f2 phi = of_binop Gt f1 f2 phi

let lt f1 f2 phi = of_binop Lt f1 f2 phi

let and_ f1 f2 phi = of_binop LAnd f1 f2 phi

let or_ f1 f2 phi = of_binop LOr f1 f2 phi

let ( = ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_equal op1 op2 phi >>| fst


let ( =. ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  prune_binop ~negated:false Binop.Eq op1 op2 phi >>| fst


let ( <> ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_not_equal op1 op2 phi >>| fst


let ( < ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_less_than op1 op2 phi >>| fst


let ( > ) f1 f2 phi = ( < ) f2 f1 phi

let ( <= ) f1 f2 phi =
  let* phi, op1 = f1 phi in
  let* phi, op2 = f2 phi in
  and_less_equal op1 op2 phi >>| fst


let ( >= ) f1 f2 phi = ( <= ) f2 f1 phi

let ( && ) f1 f2 phi = f1 phi >>= f2

[@@@warning "+unused-value-declaration"]
(* end of shorthand notations *)

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

let dummy_tenv = Tenv.create ()

let () = PulseContext.set_tenv_global_for_testing dummy_tenv

(* save the global state now after all named variables have been declared *)
let global_state = AnalysisGlobalState.save ()

let pp_var fmt v =
  match Caml.Hashtbl.find_opt var_names v with
  | Some name ->
      F.pp_print_string fmt name
  | None ->
      AbstractValue.pp fmt v


let test ~f phi init_phi =
  (* reset global state before each test so that variable id's remain stable when tests are added in
      the future *)
  AnalysisGlobalState.restore global_state ;
  let phi = phi init_phi in
  F.printf "Formula:@\n  @[<2>%a@]@\n" (SatUnsat.pp (pp_with_pp_var pp_var)) phi ;
  let phi' = phi >>= f in
  F.printf "Result: " ;
  if SatUnsat.equal equal phi phi' then F.printf "same"
  else F.printf "changed@\n  @[<2>%a@]" (SatUnsat.pp (pp_with_pp_var pp_var)) phi'


let nil_typ = Typ.mk (Tstruct (ErlangType Nil))

let cons_typ = Typ.mk (Tstruct (ErlangType Cons))

let normalize_with phi init_phi = test ~f:(fun phi -> Sat phi) phi init_phi

let normalize phi = normalize_with phi ttrue

let normalize_with_all_types_Nil phi =
  match
    SatUnsat.list_fold [x_var; y_var; z_var; w_var] ~init:ttrue ~f:(fun phi v ->
        let* phi, _ = and_dynamic_type v nil_typ phi in
        Sat phi )
  with
  | Unsat ->
      Logging.die InternalError "Failed to initialise test phi"
  | Sat init_phi ->
      normalize_with phi init_phi


let () = Language.curr_language := Language.Erlang

let simplify ~keep phi =
  let keep = AbstractValue.Set.of_list keep in
  test phi
    ~f:(fun phi ->
      (* keep variables as if they were in the pre-condition, which makes [simplify] keeps the most
         facts (eg atoms in [pruned] may be discarded if their variables are not in the pre) *)
      simplify ~precondition_vocabulary:keep ~keep phi >>| fst3 )
    ttrue


(* These instanceof tests now normalize at construction time *)
let%test_module "normalization" =
  ( module struct
    let%expect_test _ =
      normalize_with_all_types_Nil
        (instanceof nil_typ x_var z_var && instanceof nil_typ y_var w_var && z = i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize_with_all_types_Nil
        (instanceof nil_typ x_var z_var && instanceof nil_typ y_var w_var && w = i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize_with_all_types_Nil
        (instanceof cons_typ x_var y_var && instanceof nil_typ x_var y_var) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (x < y) ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: linear_eqs: x = y -a1 -1 && term_eqs: [y -a1 -1]=x
        Result: same|}]


    let%expect_test _ =
      normalize (x + i 1 - i 1 < x) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (i 1 = i 1) ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: (empty)
        Result: same|}]


    let%expect_test _ =
      normalize (x + (y - x) < y) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (x = y && y = z && z = i 0 && x = i 1) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    (* should be false (x = w + (y+1) -> 1 = w + z -> 1 = 0)  *)
    let%expect_test _ =
      normalize (x = w + y + i 1 && y + i 1 = z && x = i 1 && w + z = i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    (* same as above but atoms are given in the opposite order *)
    let%expect_test _ =
      normalize (w + z = i 0 && x = i 1 && y + i 1 = z && x = w + y + i 1) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (of_binop Ne x y = i 0 && x = i 0 && y = i 1) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (of_binop Ne x y = i 0 && x = i 0 && y = i 12) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (of_binop Eq x y = i 0 && x = i 0 && y = i 1) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6
               && linear_eqs: x = 0 ∧ y = 1
               && term_eqs: 0=x∧1=y
               && intervals: x=0 ∧ y=1
        Result: same|}]


    let%expect_test _ =
      normalize (x = i 0 && x < i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (x + y < x + y) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "nonlinear arithmetic" =
      normalize (z * (x + (v * y) + i 1) / w = i 0) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: linear_eqs: x = -v6 +v8 -1 ∧ v7 = v8 -1 ∧ v10 = 0
               && term_eqs: 0=v10∧[-v6 +v8 -1]=x∧[v8 -1]=v7∧(z×v8)=v9∧(v×y)=v6∧(v9÷w)=v10
               && intervals: v10=0
        Result: same|}]


    (* check that this becomes all linear equalities *)
    let%expect_test _ =
      normalize (i 12 * (x + (i 3 * y) + i 1) / i 1 = i 0) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: v8=v9=v10
               && linear_eqs: x = -v6 -1 ∧ y = 1/3·v6 ∧ v7 = -1 ∧ v8 = 0
               && term_eqs: (-1)=v7∧0=v8∧[-v6 -1]=x∧[1/3·v6]=y
               && intervals: v8=0
        Result: same|}]


    (* check that this becomes all linear equalities thanks to constant propagation *)
    let%expect_test _ =
      normalize (z * (x + (v * y) + i 1) / w = i 0 && z = i 12 && v = i 3 && w = i 1) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: v8=v9=v10
               && linear_eqs: x = -v6 -1 ∧ y = 1/3·v6 ∧ z = 12 ∧ w = 1
                               ∧ v = 3 ∧ v7 = -1 ∧ v8 = 0
               && term_eqs: (-1)=v7∧0=v8∧1=w∧3=v∧12=z∧[-v6 -1]=x∧[1/3·v6]=y
               && intervals: z=12 ∧ w=1 ∧ v=3 ∧ v8=0
        Result: same|}]


    (* expected: [is_int(x)] and [is_int(y)] get simplified away, [is_int(z)] is kept around *)
    let%expect_test _ =
      normalize
        (is_int x_var && x + x = i 4 && is_int y_var && y = i (-42) && is_int z_var && z = x + w) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: z=v7
               && linear_eqs: x = 2 ∧ y = -42 ∧ z = w +2 ∧ v6 = 4
               && term_eqs: (-42)=y∧2=x∧4=v6∧[w +2]=z
               && intervals: y=-42 ∧ v6=4
               && atoms: {is_int([w +2]) = 1}
        Result: same
|}]


    let%expect_test _ =
      normalize (is_int x_var && x + x = i 5) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]
  end )


let%test_module "variable elimination" =
  ( module struct
    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y) ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: var_eqs: x=y
        Result: same|}]


    let%expect_test _ =
      simplify ~keep:[x_var] (x = i 0 && y = i 1 && z = i 2 && w = i 3) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: linear_eqs: x = 0 ∧ y = 1 ∧ z = 2 ∧ w = 3
               && term_eqs: 0=x∧1=y∧2=z∧3=w
               && intervals: x=0 ∧ y=1 ∧ z=2 ∧ w=3
        Result: changed
          conditions: (empty) phi: term_eqs: 0=x|}]


    let%expect_test _ =
      simplify ~keep:[x_var] (x = y + i 1 && x = i 0) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6 && linear_eqs: x = 0 ∧ y = -1 && term_eqs: (-1)=y∧0=x && intervals: x=0
        Result: changed
          conditions: (empty) phi: term_eqs: 0=x|}]


    let%expect_test _ =
      simplify ~keep:[y_var] (x = y + i 1 && x = i 0) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6 && linear_eqs: x = 0 ∧ y = -1 && term_eqs: (-1)=y∧0=x && intervals: x=0
        Result: changed
          conditions: (empty) phi: term_eqs: (-1)=y|}]


    (* should keep most of this or realize that [w = z] hence this boils down to [z+1 = 0] *)
    let%expect_test _ =
      simplify ~keep:[y_var; z_var] (x = y + z && w = x - y && v = w + i 1 && v = i 0) ;
      [%expect
        {|
          Formula:
            conditions: (empty)
            phi: var_eqs: x=v6 ∧ z=w=v7 ∧ v=v8
                 && linear_eqs: x = y -1 ∧ z = -1 ∧ v = 0
                 && term_eqs: (-1)=z∧0=v∧[y -1]=x
                 && intervals: v=0
          Result: changed
            conditions: (empty) phi: term_eqs: (-1)=z∧[y -1]=x|}]


    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + z && w + x + y = i 0 && v = w + i 1) ;
      [%expect
        {|
          Formula:
            conditions: (empty)
            phi: var_eqs: x=v6 ∧ v=v9
                 && linear_eqs: x = -v +v7 +1 ∧ y = -v7 ∧ z = -v +2·v7 +1 ∧ w = v -1 ∧ v8 = 0
                 && term_eqs: 0=v8∧[v -1]=w∧[-v7]=y∧[-v +v7 +1]=x∧[-v +2·v7 +1]=z
                 && intervals: v8=0
          Result: changed
            conditions: (empty) phi: term_eqs: [v -1]=w∧[-v7]=y∧[-v +v7 +1]=x∧[-v +2·v7 +1]=z|}]


    let%expect_test _ =
      simplify ~keep:[x_var; y_var] (x = y + i 4 && x = w && y = z) ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: var_eqs: x=w=v6 ∧ y=z && linear_eqs: x = y +4 && term_eqs: [y +4]=x
        Result: changed
          conditions: (empty) phi: term_eqs: [y +4]=x|}]
  end )


let%test_module "non-linear simplifications" =
  ( module struct
    let%expect_test "zero propagation" =
      simplify ~keep:[w_var] (((i 0 / (x * z)) & v) * v mod y = w) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: w=v7=v8=v9=v10 && linear_eqs: w = 0 && term_eqs: 0=w∧(x×z)=v6 && intervals: w=0
        Result: changed
          conditions: (empty) phi: term_eqs: 0=w|}]


    let%expect_test "constant propagation: bitshift" =
      simplify ~keep:[x_var] (of_binop Shiftlt (of_binop Shiftrt (i 0b111) (i 2)) (i 2) = x) ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: var_eqs: x=v7 && linear_eqs: x = 4 ∧ v6 = 1 && term_eqs: 1=v6∧4=x
        Result: changed
          conditions: (empty) phi: term_eqs: 4=x|}]


    let%expect_test "non-linear becomes linear" =
      normalize (w = (i 2 * z) - i 3 && z = x * y && y = i 2) ;
      [%expect
        {|
          Formula:
            conditions: (empty)
            phi: var_eqs: z=v8 ∧ w=v7
                 && linear_eqs: x = 1/4·v6 ∧ y = 2 ∧ z = 1/2·v6 ∧ w = v6 -3
                 && term_eqs: 2=y∧[v6 -3]=w∧[1/4·v6]=x∧[1/2·v6]=z
                 && intervals: y=2
          Result: same|}]
  end )


let%test_module "inequalities" =
  ( module struct
    let%expect_test "simple contradiction" =
      normalize (x < i 0 && x >= i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "simple contradiction" =
      normalize (x < y && x >= y) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "add to tableau with pivot" =
      normalize (x >= i 0 && y >= i 0 && z >= i 0 && x + y >= i 2 && z - y <= i (-3)) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: a3=z ∧ a2=y ∧ a1=x
               && linear_eqs: a2 = a3 +a5 +3 ∧ a1 = -a3 +a4 -a5 -1 ∧ v6 = a4 +2 ∧ v7 = -a5 -3
               && term_eqs: [-a5 -3]=v7∧[-a3 +a4 -a5 -1]=a1∧[a4 +2]=v6∧[a3 +a5 +3]=a2
               && intervals: a3≥0 ∧ a2≥0 ∧ a1≥0 ∧ v6≥2 ∧ v7≤-3
        Result: same |}]


    let%expect_test "add to tableau with pivot then unsat" =
      normalize (x >= i 0 && y >= i 0 && z >= i 0 && x + y >= i 2 && z - y <= i (-3) && y < i 1) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "contradiction using pivot" =
      normalize (x >= i 0 && y >= i 0 && z >= i 0 && x + y <= i 2 && y - z >= i 3) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "constant propagation to tableau" =
      normalize (x < i 34 && y < i 2 * x && x = i 32 && y = i 64) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "tableau simplified away by constant propagation" =
      normalize (x < i 34 && y <= i 2 * x && x = i 32 && y = i 64) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: y=v6
               && linear_eqs: a2 = 0 ∧ a1 = 1 ∧ x = 32 ∧ y = 64
               && term_eqs: 0=a2∧1=a1∧32=x∧64=y
               && intervals: a2=0 ∧ x=32 ∧ y=64
        Result: same|}]


    let%expect_test "(negated) inequality followed by pruned equality" =
      normalize (lt x (i 2) = i 0 && x =. i 2) ;
      [%expect
        {|
         Formula:
           conditions: {x = 2}
           phi: var_eqs: a1=v6
                && linear_eqs: a1 = 0 ∧ x = 2
                && term_eqs: 0=a1∧2=x
                && intervals: a1=0 ∧ x=2
         Result: same |}]
  end )


let%test_module "intervals" =
  ( module struct
    (* rationals cannot detect the contradiction but intervals do integer reasoning *)
    let%expect_test "integer equality in concrete interval" =
      normalize (x >= i 0 && x < i 3 && x <> i 0 && x <> i 1 && x <> i 2) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    (* same as above but we stop earlier to see that intervals infer that [x = 2] *)
    let%expect_test "integer equality consequence" =
      simplify ~keep:[x_var] (x >= i 0 && x < i 3 && x <> i 0 && x <> i 1) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: a1=x
               && linear_eqs: a2 = 0 ∧ a1 = 2
               && term_eqs: 0=a2∧2=a1
               && intervals: a2=0 ∧ a1=2
        Result: changed
          conditions: (empty) phi: term_eqs: 2=x|}]


    let%expect_test "interval intersection" =
      normalize (x >= i 0 && x < i 3 && x <> i 0 && y >= i 2 && y < i 10 && x = y) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: a3=a2 ∧ a1=x=y
               && linear_eqs: a4 = 7 ∧ a3 = 0 ∧ a1 = 2
               && term_eqs: 0=a3∧2=a1∧7=a4
               && intervals: a3=0 ∧ a1=2
        Result: same |}]
  end )


let%test_module "conjunctive normal form" =
  ( module struct
    let%expect_test _ =
      normalize (and_ (ge x (i 0)) (lt x (i 0)) = i 1) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    (* same as above with <> 0 instead of = 1 *)
    let%expect_test _ =
      normalize (and_ (ge x (i 0)) (lt x (i 0)) <> i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test "¬ (x ≠ 0 ∨ x > 0 ∨ x < 0) <=> x = 0" =
      normalize (or_ (ne x (i 0)) (or_ (gt x (i 0)) (lt x (i 0))) = i 0) ;
      [%expect
        {|
          Formula:
            conditions: (empty)
            phi: var_eqs: x=v6=v7=v8=v9=v10 && linear_eqs: x = 0 && term_eqs: 0=x && intervals: x=0
          Result: same |}]


    let%expect_test "UNSAT: ¬ (x = 0 ∨ x > 0 ∨ x < 0)" =
      normalize (or_ (eq x (i 0)) (or_ (gt x (i 0)) (lt x (i 0))) = i 0) ;
      [%expect {|
        Formula:
          unsat
        Result: same|}]


    let%expect_test _ =
      normalize (and_ (ge x (i 0)) (gt x (i 0)) <> i 0) ;
      [%expect
        {|
          Formula:
            conditions: (empty)
            phi: var_eqs: v6=v7
                 && linear_eqs: x = a1 +1 ∧ v6 = 1
                 && term_eqs: 1=v6∧[a1 +1]=x∧(0<x)=v6∧(0≤x)=v6
                 && intervals: v8≠0
                 && atoms: {v8 ≠ 0}
          Result: same|}]
  end )


let%test_module "non-numerical constants" =
  ( module struct
    let%expect_test _ =
      normalize (x = s "hello world") ;
      [%expect
        {|
        Formula:
          conditions: (empty) phi: const_eqs: x="hello world" && term_eqs: "hello world"=x
        Result: same |}]


    let%expect_test _ =
      normalize (x = s "hello" && x = s "world") ;
      [%expect {|
        Formula:
          unsat
        Result: same |}]


    let%expect_test _ =
      normalize (x = s "hello" ^ s " " ^ s "world" && y = s "hello world" && x = y) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=y=v7
               && const_eqs: x="hello world" ∧ v6=" world"
               && term_eqs: " world"=v6∧"hello world"=x
        Result: same |}]


    let%expect_test _ =
      normalize (x = s "hello" ^ s "world" && y = s "no match" && x = y) ;
      [%expect {|
        Formula:
          unsat
        Result: same |}]


    let%expect_test _ =
      normalize (y = s "hello" && z = s "world" && x = y ^ z) ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6
               && const_eqs: x="helloworld" ∧ y="hello" ∧ z="world"
               && term_eqs: "hello"=y∧"helloworld"=x∧"world"=z
        Result: same |}]


    let%expect_test _ =
      normalize (x = y ^ z && y = s "hello" && z = s "world") ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6
               && const_eqs: x="helloworld" ∧ y="hello" ∧ z="world"
               && term_eqs: "hello"=y∧"world"=z
        Result: same |}]


    let%expect_test _ =
      normalize (x = y ^ z && y = s "hello" && z = s "world" && x = y) ;
      [%expect {|
        Formula:
          unsat
        Result: same |}]


    let%expect_test _ =
      normalize (x = y ^ z && y = s "hello" && x = s "prefix cannot match") ;
      [%expect
        {|
        Formula:
          conditions: (empty)
          phi: var_eqs: x=v6
               && const_eqs: x="prefix cannot match" ∧ y="hello"
               && term_eqs: "hello"=y∧"prefix cannot match"=x∧("hello"^z)=x
        Result: same |}]
  end )
