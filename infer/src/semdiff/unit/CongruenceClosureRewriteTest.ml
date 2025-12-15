(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open CongruenceClosureSolver
open CongruenceClosureRewrite

let st = ref (init ~debug:false)

let restart () = st := init ~debug:false

let pp_subst fmt = pp_subst !st fmt

let pp_nested_term atom = (pp_nested_term !st) atom

let test_pattern_e_matching ?(debug = false) pattern atom =
  F.printf "e-matching pattern %a with term %a:@." Pattern.pp pattern pp_nested_term atom ;
  let substs = Pattern.e_match ~debug !st pattern atom in
  if List.is_empty substs then F.printf "--> no substitution found@."
  else List.iteri substs ~f:(fun i subst -> F.printf "--> subst #%d: %a@." i pp_subst subst) ;
  F.print_newline ()


let mk_term header args = mk_term !st ~header ~args

let mk_const header = mk_term header []

let merge atom term =
  let pp_term fmt = function
    | Atom atom ->
        pp_nested_term fmt atom
    | App _ ->
        F.pp_print_string fmt "?"
  in
  F.printf "merging %a and %a...@." pp_nested_term atom pp_term term ;
  merge !st atom term


let test_to_term_conversion pattern subst =
  let atom = Pattern.to_term !st subst pattern in
  F.printf "@[<hv>pattern %a@ becomes %a@]@." Pattern.pp pattern pp_nested_term atom ;
  atom


let var str = Pattern.Var (Var.of_string str)

let apply name args = Pattern.Term {header= Header.of_string name; args}

let const header = apply header []

let%expect_test "e-matching singleton" =
  restart () ;
  let one = mk_const "1" in
  let zero = mk_const "0" in
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t1 = mk_term "mult" [zero; x] in
  let t2 = mk_term "plus" [one; t1; y] in
  let t3 = mk_term "mult" [y; zero] in
  let pattern = apply "mult" [const "0"; var "X"] in
  test_pattern_e_matching pattern t1 ;
  test_pattern_e_matching pattern t2 ;
  test_pattern_e_matching pattern t3 ;
  [%expect
    {|
    e-matching pattern (mult 0 ?X) with term (mult 0 x):
    --> subst #0: {X: x}

    e-matching pattern (mult 0 ?X) with term (plus 1 (mult 0 x) y):
    --> no substitution found

    e-matching pattern (mult 0 ?X) with term (mult y 0):
    --> no substitution found
    |}]


let%expect_test "e-matching multiple" =
  restart () ;
  let zero = mk_const "0" in
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t1 = mk_term "plus" [y; x] in
  let t2 = mk_term "plus" [x; y] in
  let t3 = mk_term "mult" [zero; t2] in
  merge t1 (Atom t2) ;
  let pattern = apply "mult" [const "0"; apply "plus" [var "U"; var "V"]] in
  test_pattern_e_matching pattern t3 ;
  [%expect
    {|
    merging (plus y x) and (plus x y)...
    e-matching pattern (mult 0 (plus ?U ?V)) with term (mult 0 (plus x y)):
    --> subst #0: {U: x,V: y}
    --> subst #1: {U: y,V: x}
    |}]


let%expect_test "pattern -> term" =
  restart () ;
  let zero = mk_const "0" in
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t1 = mk_term "plus" [y; x] in
  let t2 = mk_term "mult" [zero; t1] in
  let expected =
    mk_term "mult" [mk_term "div" [mk_const "2"; x]; mk_term "plus" [t1; mk_const "1"; zero; t2]]
  in
  let vX = Var.of_string "X" in
  let vU = Var.of_string "U" in
  let vV = Var.of_string "V" in
  let vW = Var.of_string "W" in
  let pattern =
    apply "mult" [apply "div" [const "2"; Var vX]; apply "plus" [Var vU; const "1"; Var vV; Var vW]]
  in
  let subst = mk_subst [(vX, x); (vU, t1); (vV, zero); (vW, t2)] in
  let obtained = test_to_term_conversion pattern subst in
  F.printf "expected == obtained? %b@." (CC.is_equiv !st expected obtained) ;
  [%expect
    {|
    pattern (mult (div 2 ?X) (plus ?U 1 ?V ?W))
    becomes (mult (div 2 x) (plus (plus y x) 1 0 (mult 0 (plus y x))))
    expected == obtained? true
    |}]


let%expect_test "rewriting" =
  restart () ;
  let x = mk_const "x" in
  let y = mk_const "y" in
  let z = mk_const "z" in
  let t1 = mk_term "mult" [x; mk_term "plus" [y; z]] in
  let t2 = mk_term "plus" [mk_term "mult" [x; y]; mk_term "mult" [x; z]] in
  let distribute_rule : Rule.t =
    { lhs= apply "mult" [var "X"; apply "plus" [var "Y"; var "Z"]]
    ; rhs= apply "plus" [apply "mult" [var "X"; var "Y"]; apply "mult" [var "X"; var "Z"]] }
  in
  F.printf "t1 := %a@." pp_nested_term t1 ;
  F.printf "t2 := %a@." pp_nested_term t2 ;
  F.printf "t1 == t2? %b@." (CC.is_equiv !st t1 t2) ;
  F.printf "applying rule %a...@." Rule.pp distribute_rule ;
  let _ = Rule.apply ~debug:true !st distribute_rule t1 in
  F.printf "t1 == t2? %b@." (CC.is_equiv !st t1 t2) ;
  [%expect
    {|
    t1 := (mult x (plus y z))
    t2 := (plus (mult x y) (mult x z))
    t1 == t2? false
    applying rule (mult ?X (plus ?Y ?Z)) ==> (plus (mult ?X ?Y) (mult ?X ?Z))...
    subst #0 = {X: x,Y: y,Z: z}
    rhs_term = (plus (mult x y) (mult x z))
    t1 == t2? true
    |}]
