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

let test_pattern ?(debug = false) pattern atom =
  F.printf "e-matching pattern %a with term %a:@." Pattern.pp pattern pp_nested_term atom ;
  let substs = Pattern.e_match ~debug !st pattern atom in
  if List.is_empty substs then F.printf "--> no substitution found@."
  else List.iteri substs ~f:(fun i subst -> F.printf "--> subst #%d: %a@." i pp_subst subst) ;
  F.print_newline ()


let mk_atom value = mk_atom !st value

let mk_term header args = mk_term !st ~header ~args

let merge atom term =
  let pp_term fmt = function
    | Atom atom ->
        pp_nested_term fmt atom
    | App _ ->
        F.pp_print_string fmt "?"
  in
  F.printf "merging %a and %a...@." pp_nested_term atom pp_term term ;
  merge !st atom term


let var str = Pattern.Var (Var.of_string str)

let apply name args = Pattern.Term {header= Header.of_string name; args}

let const header = apply header []

let%expect_test "e-matching singleton" =
  restart () ;
  let one = mk_atom "1" in
  let zero = mk_atom "0" in
  let x = mk_atom "x" in
  let y = mk_atom "y" in
  let t1 = mk_term "mult" [zero; x] in
  let t2 = mk_term "plus" [one; t1; y] in
  let t3 = mk_term "mult" [y; zero] in
  let pattern = apply "mult" [const "0"; var "X"] in
  test_pattern pattern t1 ;
  test_pattern pattern t2 ;
  test_pattern pattern t3 ;
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
  let zero = mk_atom "0" in
  let x = mk_atom "x" in
  let y = mk_atom "y" in
  let t1 = mk_term "plus" [y; x] in
  let t2 = mk_term "plus" [x; y] in
  let t3 = mk_term "mult" [zero; t2] in
  merge t1 (Atom t2) ;
  let pattern = apply "mult" [const "0"; apply "plus" [var "U"; var "V"]] in
  test_pattern pattern t3 ;
  [%expect
    {|
    merging (plus y x) and (plus x y)...
    e-matching pattern (mult 0 (plus ?U ?V)) with term (mult 0 (plus x y)):
    --> subst #0: {U: x,V: y}
    --> subst #1: {U: y,V: x}
    |}]
