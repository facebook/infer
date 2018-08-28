(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let make_var var_str = Pvar.mk (Mangled.from_string var_str) Typ.Procname.empty_block

let make_base ?(typ = Typ.mk Tvoid) base_str = AccessPath.base_of_pvar (make_var base_str) typ

let make_fieldname = Typ.Fieldname.Java.from_string

let make_field_access access_str = AccessPath.FieldAccess (make_fieldname access_str)

let make_array_access typ = AccessPath.ArrayAccess (typ, [])

let make_access_path base_str access_strs =
  (make_base base_str, List.map ~f:make_field_access access_strs)
