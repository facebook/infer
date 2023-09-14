(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type record = {s: string; i: int} [@@deriving equal, hash, normalize]

module RecordHashNormalizer : HashNormalizer.S with type t = record = struct
  type t = record

  let normalize = hash_normalize_record

  let normalize_opt = hash_normalize_record_opt

  let normalize_list = hash_normalize_record_list
end

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

module TupleHashNormalizer : HashNormalizer.S with type t = tuple = struct
  type nonrec t = tuple

  let normalize = hash_normalize_tuple

  let normalize_opt = hash_normalize_tuple_opt

  let normalize_list = hash_normalize_tuple_list
end

let%test "tuple_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = (make_foo (), 5, make_foo ()) in
  let ((s1, _, s2) as a') = TupleHashNormalizer.normalize a in
  let b = (make_foo (), 5, make_foo ()) in
  let b' = TupleHashNormalizer.normalize b in
  equal_tuple a a' && phys_equal a' b' && phys_equal s1 s2


type variant =
  | NoArgs
  | String of string
  | Int of int
  | Tuple of int * string
  | Record of {i: int; s: string}
  | NonInline of record
[@@deriving equal, hash, normalize]

module VariantHashNormalizer = struct
  type t = variant

  let normalize = hash_normalize_variant

  let normalize_opt = hash_normalize_variant_opt

  let normalize_list = hash_normalize_variant_list
end

let%test "variant_test" =
  HashNormalizer.reset_all_normalizers () ;
  let a = Record {i= 4; s= make_foo ()} in
  let a' = VariantHashNormalizer.normalize a in
  let b = Record {i= 4; s= make_foo ()} in
  let b' = VariantHashNormalizer.normalize b in
  equal_variant a a' && phys_equal a' b'
