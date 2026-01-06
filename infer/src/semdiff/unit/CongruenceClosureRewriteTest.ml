(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open CongruenceClosureSolver
open CongruenceClosureRewrite
open CongruenceClosureRewrite.TestOnly

let st = ref (init ~debug:false)

let restart () = st := init ~debug:false

let pp_subst fmt = pp_subst !st fmt

let pp_nested_term atom = (pp_nested_term !st) atom

let test_e_match_at ?(debug = false) pattern atom =
  F.printf "e-matching pattern %a with term %a:@." Pattern.pp pattern pp_nested_term atom ;
  let substs = e_match_pattern_at ~debug !st pattern atom in
  if List.is_empty substs then F.printf "--> no substitution found@."
  else List.iteri substs ~f:(fun i subst -> F.printf "--> subst #%d: %a@." i pp_subst subst) ;
  F.print_newline ()


let test_e_match ?(debug = false) pattern =
  F.printf "e-matching pattern %a:@." Pattern.pp pattern ;
  e_match_pattern ~debug !st pattern ~f:(fun atom subst ->
      F.printf "--> term %a@." pp_nested_term atom ;
      F.printf "    subst %a@." pp_subst subst )


let mk_term header args =
  let header = mk_header !st header in
  mk_term !st header args


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
  let atom = pattern_to_term !st subst pattern in
  F.printf "@[<hv>pattern %a@ becomes %a@]@." Pattern.pp pattern pp_nested_term atom ;
  atom


let var str = Pattern.Var (Var.of_string str)

let apply name args = Pattern.Term {header= mk_header !st name; args}

let const header = apply header []

let mk_ellipsis header arg : Pattern.ellipsis = {header= mk_header !st header; arg}

let parse_pattern_with_error str =
  match parse_pattern !st str with
  | Ok _ ->
      F.printf "no error"
  | Error err ->
      F.printf "%a" pp_parse_error err


let parse_rule_with_error str =
  match parse_rule !st str with
  | Ok _ ->
      F.printf "no error"
  | Error err ->
      F.printf "%a" pp_parse_error err


let parse_pattern str =
  match parse_pattern !st str with
  | Ok pattern ->
      pattern
  | Error err ->
      L.die UserError "parse error %a" pp_parse_error err


let parse_rule str =
  match parse_rule !st str with
  | Ok pattern ->
      pattern
  | Error err ->
      L.die UserError "parse error %a" pp_parse_error err


let pp_vars fmt vars =
  F.fprintf fmt "@[<hv>{" ;
  List.iteri vars ~f:(fun i var ->
      Var.pp fmt var ;
      if i > 0 then F.fprintf fmt "@ " ) ;
  F.fprintf fmt "@]}"


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
  test_e_match_at pattern t1 ;
  test_e_match_at pattern t2 ;
  test_e_match_at pattern t3 ;
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
  test_e_match_at pattern t3 ;
  [%expect
    {|
    merging (plus y x) and (plus x y)...
    e-matching pattern (mult 0 (plus ?U ?V)) with term (mult 0 (plus x y)):
    --> subst #0: {U: x,V: y}
    --> subst #1: {U: y,V: x}
    |}]


let%expect_test "e-matching search" =
  restart () ;
  let zero = mk_const "0" in
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t1 = mk_term "plus" [y; x] in
  let t2 = mk_term "plus" [x; y] in
  let t3 = mk_term "plus" [t1; t2] in
  let t4 = mk_term "plus" [zero; t3] in
  let t5 = mk_term "start" [t4] in
  merge t1 (Atom t2) ;
  let pattern = apply "plus" [var "U"; apply "plus" [var "V"; var "W"]] in
  F.printf "t5 := %a@." pp_nested_term t5 ;
  test_e_match pattern ;
  [%expect
    {|
    merging (plus y x) and (plus x y)...
    t5 := (start (plus 0 (plus (plus y x) (plus x y))))
    e-matching pattern (plus ?U (plus ?V ?W)):
    --> term (plus (plus y x) (plus x y))
        subst {U: (plus ... ...),V: x,W: y}
    --> term (plus (plus y x) (plus x y))
        subst {U: (plus ... ...),V: y,W: x}
    --> term (plus 0 (plus (plus y x) (plus x y)))
        subst {U: 0,V: (plus ... ...),W: (plus ... ...)}
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


let%expect_test "rewriting at specific atom" =
  restart () ;
  let x = mk_const "x" in
  let y = mk_const "y" in
  let z = mk_const "z" in
  let t1 = mk_term "mult" [x; mk_term "plus" [y; z]] in
  let t2 = mk_term "plus" [mk_term "mult" [x; y]; mk_term "mult" [x; z]] in
  let distribute_rule : Rule.t =
    parse_rule "(mult ?X (plus ?Y ?Z)) ==> (plus (mult ?X ?Y) (mult ?X ?Z))"
  in
  F.printf "t1 := %a@." pp_nested_term t1 ;
  F.printf "t2 := %a@." pp_nested_term t2 ;
  F.printf "t1 == t2? %b@." (CC.is_equiv !st t1 t2) ;
  F.printf "applying rule %a...@." Rule.pp distribute_rule ;
  let _ = apply_rule_at ~debug:true !st distribute_rule t1 in
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


let%expect_test "rewriting one round" =
  restart () ;
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t = mk_term "g" [mk_term "f" [mk_term "g" [y]]] in
  merge x (Atom t) ;
  F.printf "0: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  let rule1 : Rule.t = parse_rule "(g (g ?X)) ==> ?X" in
  let rule2 : Rule.t = parse_rule "(f ?X) ==> ?X" in
  let rules = [rule1; rule2] in
  let updates = rewrite_rules_once ~debug:true !st rules in
  F.printf "%d updates@." updates ;
  F.printf "1: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  let updates = rewrite_rules_once ~debug:true !st rules in
  F.printf "%d updates@." updates ;
  F.printf "2: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  let updates = rewrite_rules_once ~debug:true !st rules in
  F.printf "%d updates@." updates ;
  F.printf "3: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  [%expect
    {|
    merging x and (g (f (g y)))...
    0: x == y? false
    rewriting atom (f (g y)) with rule (f ?X) ==> ?X and subst {X: (g ...)}
    1 updates
    1: x == y? false
    rewriting atom (g (f (g y))) with rule (g (g ?X)) ==> ?X and subst {X: y}
    1 updates
    2: x == y? true
    0 updates
    3: x == y? true
    |}]


let%expect_test "full rewrite" =
  restart () ;
  let x = mk_const "x" in
  let y = mk_const "y" in
  let t = mk_term "g" [mk_term "f" [mk_term "g" [y]]] in
  merge x (Atom t) ;
  F.printf "0: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  let rule1 : Rule.t = parse_rule "(g (g ?X)) ==> ?X" in
  let rule2 : Rule.t = parse_rule "(f ?X) ==> ?X" in
  let rules = [rule1; rule2] in
  let rounds = Rule.full_rewrite !st rules in
  F.printf "%d rounds@." rounds ;
  F.printf "1: %a == %a? %b@." Atom.pp x Atom.pp y (CC.is_equiv !st x y) ;
  [%expect
    {|
    merging x and (g (f (g y)))...
    0: x == y? false
    3 rounds
    1: x == y? true
    |}]


let%expect_test "parse pattern" =
  restart () ;
  let pattern1 =
    parse_pattern
      {|
         (Type
           (arg1 ?V1)
           (arg2 List)
           (arg3 (Exp (Const 1) (Const 2) (Str "quoted"))))
  |}
  in
  F.printf "pattern1 = %a@." Pattern.pp pattern1 ;
  F.printf "  with vars = %a@." pp_vars (Pattern.vars pattern1) ;
  [%expect
    {|
    pattern1 = (Type (arg1 ?V1) (arg2 List) (arg3 (Exp (Const 1) (Const 2) (Str "quoted"))))
      with vars = {V1}
    |}]


let%expect_test "parse pattern error 1" =
  restart () ;
  parse_pattern_with_error
    {|
         (Type
           (arg1 ?V1)
           (arg2 List)
           (arg3 (Exp (Const 1) (Const 2) (Str "quoted")))) (oops)
  |} ;
  [%expect
    {|
    paser error at line 4, col 60:
               (arg3 (Exp (Const 1) (Const 2) (Str "quoted")))) (oops)
                                                                ^
    --> end of buffer expected
    |}]


let%expect_test "parse pattern error 2" =
  restart () ;
  parse_pattern_with_error
    {|
         (Type
           (arg1 ?V1)
           (arg2 List)
           (arg3 (Exp (Const 1) (Const 2) (Str "quoted"))
  |} ;
  [%expect
    {|
    paser error at line 4, col 57:
               (arg3 (Exp (Const 1) (Const 2) (Str "quoted"))
                                                             ^
    --> end of buffer unexpected
    |}]


let%expect_test "parse rule error 1" =
  restart () ;
  parse_rule_with_error {|
         (Type
           (arg1 ?V1)
           (arg2 List))

  |} ;
  [%expect
    {|
    paser error at line 3, col 23:
               (arg2 List))
                           ^
    --> arrow "==>" expected
    |}]


let%expect_test "parse rule error 2" =
  restart () ;
  parse_rule_with_error {|
         (Type
           (arg1 ?V1)
           (arg2 List)) =>

  |} ;
  [%expect
    {|
    paser error at line 3, col 24:
               (arg2 List)) =>
                            ^
    --> arrow "==>" expected
    |}]


let test_e_match_ellipsis_at ellipsis atom =
  F.printf "ellipsis = %a@.@." Pattern.pp_ellipsis ellipsis ;
  TestOnly.e_match_ellipsis_at !st ellipsis atom
  |> List.iter ~f:(fun atom2 -> F.printf "%a --> %a@." pp_nested_term atom pp_nested_term atom2)


let%expect_test "e-matching ellipsis" =
  restart () ;
  let zero = mk_const "0" in
  let one = mk_const "1" in
  let two = mk_const "2" in
  let null = mk_const "null" in
  let list args = mk_term "list" args in
  let ellipsis = mk_ellipsis "list" (const "null") in
  let l1 = list [zero; null; one; two] in
  test_e_match_ellipsis_at ellipsis l1 ;
  let l2 = list [two; null; one; null; zero; null] in
  test_e_match_ellipsis_at ellipsis l2 ;
  merge l1 (Atom l2) ;
  test_e_match_ellipsis_at ellipsis l2 ;
  [%expect
    {|
    ellipsis = (list ... null ...)

    (list 0 null 1 2) --> (list 0 1 2)
    ellipsis = (list ... null ...)

    (list 2 null 1 null 0 null) --> (list 2 1 0)
    merging (list 0 null 1 2) and (list 2 null 1 null 0 null)...
    ellipsis = (list ... null ...)

    (list 2 null 1 null 0 null) --> (list 0 1 2)
    (list 2 null 1 null 0 null) --> (list 2 1 0)
    |}]
