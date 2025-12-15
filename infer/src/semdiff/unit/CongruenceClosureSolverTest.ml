(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open CongruenceClosureSolver

let () = Random.init 0

let st = ref (init ~debug:false)

let restart () = st := init ~debug:false

let mk_term header args = mk_term !st ~header ~args

let mk_const header = mk_term header []

let merge atom term = merge !st atom term

let repr atom = representative !st atom

let show_stats () = show_stats !st

let show_term_roots header =
  F.printf "roots[%s] = @[<hv>{" header ;
  let first = ref true in
  iter_term_roots !st (mk_header !st header) ~f:(fun atom ->
      if not !first then F.printf ",@ " ;
      F.printf "%a" (pp_nested_term !st) atom ;
      first := false ) ;
  F.printf "}@]@."


let pp_nested_term atom = F.printf "%a@." (pp_nested_term !st) atom

let equiv_atoms atom =
  let l = equiv_atoms !st atom in
  let repr = repr atom in
  F.printf "%a: {%a} (repr=%a)\n" Atom.pp atom (Pp.comma_seq Atom.pp) l Atom.pp repr


let%expect_test "" =
  restart () ;
  let a = mk_const "a" in
  let b = mk_const "b" in
  let c = mk_const "c" in
  let d = mk_const "d" in
  let e = mk_const "e" in
  let g = mk_const "g" in
  let h = mk_const "h" in
  merge d (App (g, h)) ;
  merge d (Atom c) ;
  merge a (App (g, d)) ;
  merge c (Atom e) ;
  merge b (Atom e) ;
  merge h (Atom b) ;
  show_stats () ;
  equiv_atoms a ;
  equiv_atoms b ;
  equiv_atoms c ;
  equiv_atoms d ;
  equiv_atoms e ;
  equiv_atoms g ;
  equiv_atoms h ;
  [%expect
    {|
    size=7
    max_depth=3
    a: {h,b,d,c,e,a} (repr=a)
    b: {b} (repr=a)
    c: {d,c} (repr=a)
    d: {d} (repr=a)
    e: {h,b,d,c,e} (repr=a)
    g: {g} (repr=g)
    h: {h} (repr=a)
    |}]


let%expect_test "mk_term" =
  restart () ;
  let a = mk_const "area" in
  let b = mk_const "band" in
  let c = mk_const "card" in
  let d = mk_const "door" in
  let e = mk_const "earn" in
  let g = mk_const "gold" in
  let h = mk_const "hand" in
  let i = mk_const "idea" in
  let j = mk_const "jump" in
  let l1 = mk_term "list1" [a; b; c; d] in
  let l2 = mk_term "list2" [e; l1; g; h] in
  let l3 = mk_term "list3" [i; l2; j; l1] in
  let l4 = mk_term "list4" [l3; l2; l1] in
  let l5 = mk_term "list5" [l4; l3; l2; l1] in
  pp_nested_term l1 ;
  pp_nested_term l2 ;
  pp_nested_term l3 ;
  pp_nested_term l4 ;
  pp_nested_term l5 ;
  [%expect
    {|
    (list1 area band card door)
    (list2 earn (list1 area band card door) gold hand)
    (list3 idea (list2 earn (list1 area band card door) gold hand) jump (list1 area band card door))
    (list4
        (list3
            idea
            (list2 earn (list1 area band card door) gold hand)
            jump
            (list1 area band card door))
        (list2 earn (list1 area band card door) gold hand)
        (list1 area band card door))
    (list5
        (list4
            (list3
                idea
                (list2 earn (list1 area band card door) gold hand)
                jump
                (list1 area band card door))
            (list2 earn (list1 area band card door) gold hand)
            (list1 area band card door))
        (list3
            idea
            (list2 earn (list1 area band card door) gold hand)
            jump
            (list1 area band card door))
        (list2 earn (list1 area band card door) gold hand)
        (list1 area band card door))
    |}]


let%expect_test "show sharing" =
  restart () ;
  let x = mk_const "x" in
  let y = mk_const "y" in
  let z = mk_const "z" in
  let t1 = mk_term "mult" [x; mk_term "plus" [y; z]] in
  let t2 = mk_term "plus" [mk_term "mult" [x; y]; mk_term "mult" [x; z]] in
  F.printf "t1 := " ;
  pp_nested_term t1 ;
  F.printf "t2 := " ;
  pp_nested_term t2 ;
  F.printf "t1 == t2? %b@." (is_equiv !st t1 t2) ;
  let t3 = mk_term "plus" [mk_term "mult" [x; y]; mk_term "mult" [x; z]] in
  F.printf "t3 := " ;
  pp_nested_term t3 ;
  merge t1 (Atom t3) ;
  F.printf "t1 == t2? %b@." (is_equiv !st t1 t2) ;
  debug !st ;
  show_term_roots "plus" ;
  show_term_roots "mult" ;
  [%expect
    {|
    t1 := (mult x (plus y z))
    t2 := (plus (mult x y) (mult x z))
    t1 == t2? false
    t3 := (plus (mult x y) (mult x z))
    t1 == t2? true
    repr: x is x (repr=x)
          y is y (repr=y)
          z is z (repr=z)
          plus is plus (repr=plus)
          %4 is (plus y) (repr=%4)
          %5 is (plus y z) (repr=%5)
          mult is mult (repr=mult)
          %7 is (mult x) (repr=%7)
          %8 is (mult x (plus y z)) (repr=%12)
          %9 is (mult x z) (repr=%9)
          %10 is (mult x y) (repr=%10)
          %11 is (plus (mult x y)) (repr=%11)
          %12 is (plus (mult x y) (mult x z)) (repr=%12)
    roots[plus] = {(plus y z), (plus (mult x y) (mult x z))}
    roots[mult] = {(mult x z), (mult x y)}
    |}]


let gen_term_chain size =
  let mk_const1 i = mk_const (F.asprintf "a%d" i) in
  let mk_const2 i = mk_const (F.asprintf "b%d" i) in
  let rec aux i size eqs leafs =
    assert (0 < size) ;
    if size <= 1 then
      let c1 = mk_const1 i in
      let c2 = mk_const2 i in
      (c1, c2, eqs, (c1, c2) :: leafs)
    else
      let size_left = 1 + Random.int (size - 1) in
      let c1_left, c2_left, eqs, leafs = aux i size_left eqs leafs in
      let c1 = mk_const1 (i + size_left) in
      let c2 = mk_const2 (i + size_left) in
      let c1_right, c2_right, eqs, leafs = aux (i + 1 + size_left) (size - size_left) eqs leafs in
      let term1 = App (c1_left, c1_right) in
      let term2 = App (c2_left, c2_right) in
      (c1, c2, (c1, term1) :: (c2, term2) :: eqs, leafs)
  in
  aux 1 size [] []


let shuffle l =
  let a = Array.of_list l in
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i) ;
    a.(i) <- x
  done ;
  a


let gen_random_eqs l1 =
  let l2 = shuffle l1 |> Array.to_list in
  List.zip_exn l1 l2


let%expect_test "in order" =
  restart () ;
  let c1, c2, eqs, leafs = gen_term_chain 10 in
  List.iter eqs ~f:(fun (c, t) -> merge c t) ;
  let all = List.map eqs ~f:fst @ List.map ~f:fst leafs @ List.map ~f:snd leafs in
  List.take all (List.length all / 3)
  |> gen_random_eqs
  |> List.iter ~f:(fun (a, b) -> merge a (Atom b)) ;
  List.iter leafs ~f:(fun (a, b) -> merge a (Atom b)) ;
  F.printf "c1=c2? %b\n" (phys_equal (repr c1) (repr c2)) ;
  show_stats () ;
  [%expect {|
    c1=c2? true
    size=32
    max_depth=3
    |}]


let%expect_test "out of order" =
  restart () ;
  let c1, c2, eqs, leafs = gen_term_chain 10 in
  let all = List.map eqs ~f:fst @ List.map ~f:fst leafs @ List.map ~f:snd leafs in
  let random_eqs =
    List.take all (List.length all / 3) |> gen_random_eqs |> List.map ~f:(fun (a, b) -> (a, Atom b))
  in
  let leafs =
    List.map leafs ~f:(fun (a, b) -> if Random.bool () then (a, Atom b) else (b, Atom a))
  in
  let eqs = shuffle (eqs @ leafs @ random_eqs) in
  Array.iter eqs ~f:(fun (c, t) -> merge c t) ;
  F.printf "c1=c2? %b\n" (phys_equal (repr c1) (repr c2)) ;
  show_stats () ;
  [%expect {|
    c1=c2? true
    size=32
    max_depth=6
    |}]
