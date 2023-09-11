(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type record = {s: string; i: int} [@@deriving equal, hash, normalize]

module RecordHashNormalizer = HashNormalizer.Make (struct
  type t = record [@@deriving equal, hash, normalize]
end)

(* always return ["foo"] (when [suffix] is not provided), but hopefully we
   fool the compiler to return something that's not physically equal *)
let make_foo ?(suffix = "") () = "foo" ^ suffix

let%test "first_normalize_phys_equal" =
  HashNormalizer.reset_all_normalizers () ;
  let a = {s= make_foo (); i= 5} in
  let a' = RecordHashNormalizer.normalize a in
  phys_equal a a'


let%test "second_normalize_not_phys_equal" =
  HashNormalizer.reset_all_normalizers () ;
  let a = {s= make_foo (); i= 5} in
  let _ = RecordHashNormalizer.normalize a in
  let b = {s= make_foo (); i= 5} in
  let b' = RecordHashNormalizer.normalize b in
  equal_record b b' && not (phys_equal b b')


let%test "string_normalize" =
  HashNormalizer.reset_all_normalizers () ;
  let foo1 = make_foo () in
  let foo2 = make_foo () in
  (* assumptions *)
  String.equal foo1 foo2
  && (not (phys_equal foo1 foo2))
  &&
  let foo1' = HashNormalizer.StringNormalizer.normalize foo1 in
  let foo2' = HashNormalizer.StringNormalizer.normalize foo2 in
  String.equal foo1 foo1' && String.equal foo2 foo2' && phys_equal foo1' foo2'
  && phys_equal foo1 foo1'


type tuple = string * int * string [@@deriving equal, hash, normalize]

module TupleHashNormalizer = HashNormalizer.Make (struct
  type t = tuple [@@deriving equal, hash, normalize]
end)

let%test "tuple_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = (make_foo (), 5, make_foo ()) in
  let ((s1, _, s2) as a') = TupleHashNormalizer.normalize a in
  let b = (make_foo (), 5, make_foo ()) in
  let b' = TupleHashNormalizer.normalize b in
  equal_tuple a a' && phys_equal a' b' && phys_equal s1 s2
