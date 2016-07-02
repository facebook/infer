(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

type base = Pvar.t * Typ.t

type access =
  | FieldAccess of Ident.fieldname * Typ.t
  | ArrayAccess of Typ.t

type t = base * access list

let base_compare ((var1, typ1) as base1) ((var2, typ2) as base2) =
  if base1 == base2
  then 0
  else
    Pvar.compare var1 var2
    |> next Typ.compare typ1 typ2

let base_equal base1 base2 =
  base_compare base1 base2 = 0

let access_compare access1 access2 =
  if access1 == access2
  then 0
  else
    match access1, access2 with
    | FieldAccess (f1, typ1), FieldAccess (f2, typ2) ->
        Ident.fieldname_compare f1 f2
        |> next Typ.compare typ1 typ2
    | ArrayAccess typ1, ArrayAccess typ2 ->
        Typ.compare typ1 typ2
    | FieldAccess _, _ -> 1
    | _, FieldAccess _ -> -1

let access_equal access1 access2 =
  access_compare access1 access2 = 0

let compare ((base1, accesses1) as ap1) ((base2, accesses2) as ap2) =
  if ap1 == ap2
  then 0
  else
    base_compare base1 base2
    |> next (IList.compare access_compare) accesses1 accesses2

let equal ap1 ap2 =
  compare ap1 ap2 = 0

let of_pvar pvar typ =
  (pvar, typ), []

let append (base, accesses) access =
  base, accesses @ [access]

let rec is_prefix_path path1 path2 =
  if path1 == path2
  then true
  else
    match path1, path2 with
    | [], _ -> true
    | _, [] -> false
    | access1 :: p1, access2 :: p2 -> access_equal access1 access2 && is_prefix_path p1 p2

let is_prefix ((base1, path1) as ap1) ((base2, path2) as ap2) =
  if ap1 == ap2
  then true
  else
    base_equal base1 base2 && is_prefix_path path1 path2

let pp_base fmt (pvar, _) =
  (Pvar.pp pe_text) fmt pvar

let pp_access fmt = function
  | FieldAccess (field_name, _) -> F.fprintf fmt ".%a" Ident.pp_fieldname field_name
  | ArrayAccess _ -> F.fprintf fmt "[_]"

let pp fmt (base, accesses) =
  let pp_access_list fmt accesses =
    F.pp_print_list pp_access fmt accesses in
  F.fprintf fmt "%a%a" pp_base base pp_access_list accesses
