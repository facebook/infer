(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type record = {s: string; i: int} [@@deriving equal, hash, normalize]

(* always return ["foo"] (when [suffix] is not provided), but hopefully we
   fool the compiler to return something that's not physically equal *)
let make_foo ?(suffix = "") () = "foo" ^ suffix

let%test "first_normalize_phys_equal" =
  HashNormalizer.reset_all_normalizers () ;
  let a = {s= make_foo (); i= 5} in
  let a' = hash_normalize_record a in
  phys_equal a a'


let%test "second_normalize_not_phys_equal" =
  HashNormalizer.reset_all_normalizers () ;
  let a = {s= make_foo (); i= 5} in
  let _ = hash_normalize_record a in
  let b = {s= make_foo (); i= 5} in
  let b' = hash_normalize_record b in
  equal_record b b' && not (phys_equal b b')


let%test "string_normalize" =
  HashNormalizer.reset_all_normalizers () ;
  let foo1 = make_foo () in
  let foo2 = make_foo () in
  (* assumptions *)
  String.equal foo1 foo2
  && (not (phys_equal foo1 foo2))
  &&
  let foo1' = HashNormalizer.String.hash_normalize foo1 in
  let foo2' = HashNormalizer.String.hash_normalize foo2 in
  String.equal foo1 foo1' && String.equal foo2 foo2' && phys_equal foo1' foo2'
  && phys_equal foo1 foo1'


type tuple = string * int * string [@@deriving equal, hash, normalize]

let%test "tuple_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = (make_foo (), 5, make_foo ()) in
  let ((s1, _, s2) as a') = hash_normalize_tuple a in
  let b = (make_foo (), 5, make_foo ()) in
  let b' = hash_normalize_tuple b in
  equal_tuple a a' && phys_equal a' b' && phys_equal s1 s2


type variant =
  | NoArgs
  | String of string
  | Int of int
  | Tuple of int * string
  | Record of {i: int; s: string}
  | NonInline of record
[@@deriving equal, hash, normalize]

let%test "variant_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = Record {i= 4; s= make_foo ()} in
  let a' = hash_normalize_variant a in
  let b = Record {i= 4; s= make_foo ()} in
  let b' = hash_normalize_variant b in
  equal_variant a a' && phys_equal a' b'


type record_with_ignore = {x: string; y: int [@ignore]} [@@deriving equal, hash, normalize]

let%test "record_with_ignore_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = {x= make_foo (); y= 0} in
  let a' = hash_normalize_record_with_ignore a in
  let b = {x= make_foo (); y= 1} in
  let b' = hash_normalize_record_with_ignore b in
  equal_record_with_ignore a a'
  && phys_equal a' b' (* normalization still happens *)
  && phys_equal a a' (* but ignores completely the [y] field *)


type ignored_string = (string[@ignore]) [@@deriving equal, hash, normalize]

let%test "ignored_string_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = "xxx" in
  let a' = hash_normalize_ignored_string a in
  let b = "yyy" in
  let b' = hash_normalize_ignored_string b in
  (* equality ignores value *)
  equal_ignored_string a b
  (* possible bug in ppx_hash: hashing does not ignores value *)
  (* [Int.equal (hash_ignored_string a) (hash_ignored_string b)] this is false *)
  && (* normalization does not happen *) not (phys_equal a' b')
