(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

let make_base base_str =
  Pvar.mk (Mangled.from_string base_str) Procname.empty_block, Typ.Tvoid

let make_field_access access_str =
  AccessPath.FieldAccess (Ident.create_fieldname (Mangled.from_string access_str) 0, Typ.Tvoid)

let make_access_path base_str accesses =
  let rec make_accesses accesses_acc = function
    | [] -> accesses_acc
    | access_str :: l -> make_accesses ((make_field_access access_str) :: accesses_acc) l in
  let accesses = make_accesses [] accesses in
  make_base base_str, IList.rev accesses

let pp_diff fmt (actual, expected) =
  F.fprintf fmt "Expected output %a but got %a" AccessPath.pp expected AccessPath.pp actual

let assert_eq input expected =
  let open OUnit2 in
  assert_equal ~cmp:AccessPath.equal ~pp_diff input expected

let tests =
  let x = make_access_path "x" [] in
  let y = make_access_path "y" [] in
  let f_access = make_field_access "f" in
  let g_access = make_field_access "g" in
  let xF = make_access_path "x" ["f"] in
  let xFG = make_access_path "x" ["f"; "g";] in
  let yF = make_access_path "y" ["f"] in

  let open OUnit2 in
  let equal_test =
    let equal_test_ _ =
      assert_bool "equal works for bases" (AccessPath.equal x (make_access_path "x" []));
      assert_bool "equal works for paths" (AccessPath.equal xFG (make_access_path "x" ["f"; "g";]));
      assert_bool "disequality works for bases" (not (AccessPath.equal x y));
      assert_bool "disequality works for paths" (not (AccessPath.equal xF xFG)) in
    "equal">::equal_test_ in

  let append_test =
    let append_test_ _ =
      assert_eq xF (AccessPath.append x f_access);
      assert_eq xFG (AccessPath.append xF g_access) in
    "append">::append_test_ in

  let prefix_test =
    let prefix_test_ _ =
      assert_bool "x is prefix of self" (AccessPath.is_prefix x x);
      assert_bool "x.f is prefix of self" (AccessPath.is_prefix xF xF);
      assert_bool "x is not prefix of y" (not (AccessPath.is_prefix x y));
      assert_bool "x is prefix of x.f" (AccessPath.is_prefix x xF);
      assert_bool "x.f not prefix of x" (not (AccessPath.is_prefix xF x));
      assert_bool "x.f is prefix of x.f.g" (AccessPath.is_prefix xF xFG);
      assert_bool "x.f.g is not prefix of x.f" (not (AccessPath.is_prefix xFG xF));
      assert_bool "y.f is not prefix of x.f" (not (AccessPath.is_prefix yF xF));
      assert_bool "y.f is not prefix of x.f.g" (not (AccessPath.is_prefix yF xFG)) in
    "prefix">::prefix_test_ in

  "all_tests_suite">:::[equal_test; append_test; prefix_test;]
