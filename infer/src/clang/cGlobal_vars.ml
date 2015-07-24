(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open CFrontend_utils
module L = Logging

type t = {
  _name : Sil.pvar;
  _type : Sil.typ
}

let var_get_name var =
  var._name

let var_get_typ var =
  var._type

module MangledMap = Map.Make (struct
    type t = Mangled.t
    let compare = Mangled.compare end)

type varMap = t MangledMap.t

let varMap = ref MangledMap.empty

let make_var name typ =
  { _name = name;
    _type = typ }

let add name typ =
  let name = (Mangled.from_string name) in
  let pvar = Sil.mk_pvar_global name in
  Printing.log_out "Adding global variable %s !!@." (Sil.pvar_to_string pvar);
  let var_el = make_var pvar typ in
  varMap := MangledMap.add name var_el !varMap

let find var =
  MangledMap.find var !varMap

let reset_map () =
  varMap := MangledMap.empty

let print_map () =
  let print_item key value =
    L.out "%a ->%a:%a@."
      Mangled.pp key
      (Sil.pp_pvar Utils.pe_text) value._name
      (Sil.pp_typ_full Utils.pe_text) value._type in
  if !CFrontend_config.debug_mode then
    (L.out "GLOBAL VARS:@.";
      MangledMap.iter print_item !varMap)
