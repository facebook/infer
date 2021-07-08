(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open OUnit2

let tests =
  let struct_typ = Typ.mk_struct (Typ.Name.C.from_string "struct") in
  let array_typ = Typ.mk_array StdTyp.int in
  let fieldname = AccessPathTestUtils.make_fieldname "field" in
  let var_id = Ident.create_normal (Ident.string_to_name "n") 0 in
  let f_resolve_id _ = None in
  let test_of_sil =
    let var_field _ =
      assert_equal ~printer:(Format.asprintf "%a" HilExp.pp)
        (HilExp.of_sil ~include_array_indexes:false ~f_resolve_id ~add_deref:true
           (Exp.Lfield (Exp.Var var_id, fieldname, struct_typ))
           StdTyp.void )
        (HilExp.AccessExpression
           (HilExp.AccessExpression.field_offset
              (HilExp.AccessExpression.base (Var.of_id var_id, struct_typ))
              fieldname ) )
    in
    let var_index _ =
      assert_equal ~printer:(Format.asprintf "%a" HilExp.pp)
        (HilExp.of_sil ~include_array_indexes:false ~f_resolve_id ~add_deref:true
           (Exp.Lindex (Exp.Var var_id, Exp.zero))
           StdTyp.int )
        (HilExp.AccessExpression
           (HilExp.AccessExpression.array_offset
              (HilExp.AccessExpression.base (Var.of_id var_id, array_typ))
              StdTyp.int None ) )
    in
    "of_sil" >::: ["var_field" >:: var_field; "var_index" >:: var_index]
  in
  "hil_exp_suite" >::: [test_of_sil]
