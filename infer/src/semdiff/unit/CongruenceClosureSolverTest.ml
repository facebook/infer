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

let mk_atom value = mk_atom !st value

let merge atom term = merge !st atom term

let repr atom = representative !st atom

let show_stats () = show_stats !st

let%expect_test "" =
  restart () ;
  let a = mk_atom "a" in
  let b = mk_atom "b" in
  let c = mk_atom "c" in
  let d = mk_atom "d" in
  let e = mk_atom "e" in
  let g = mk_atom "g" in
  let h = mk_atom "h" in
  merge d (App (g, h)) ;
  merge d (Atom c) ;
  merge a (App (g, d)) ;
  merge c (Atom e) ;
  merge b (Atom e) ;
  merge h (Atom b) ;
  show_stats () ;
  [%expect {|
    size=7
    max_depth=3
    |}]


let gen_term_chain size =
  let mk_atom1 i = mk_atom (F.asprintf "a%d" i) in
  let mk_atom2 i = mk_atom (F.asprintf "b%d" i) in
  let rec aux i size eqs leafs =
    assert (0 < size) ;
    if size <= 1 then
      let c1 = mk_atom1 i in
      let c2 = mk_atom2 i in
      (c1, c2, eqs, (c1, c2) :: leafs)
    else
      let size_left = 1 + Random.int (size - 1) in
      let c1_left, c2_left, eqs, leafs = aux i size_left eqs leafs in
      let c1 = mk_atom1 (i + size_left) in
      let c2 = mk_atom2 (i + size_left) in
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
