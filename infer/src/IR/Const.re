/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language: Constants */
let module L = Logging;

let module F = Format;

type t =
  | Cint IntLit.t /** integer constants */
  | Cfun Typ.Procname.t /** function names */
  | Cstr string /** string constants */
  | Cfloat float /** float constants */
  | Cclass Ident.name /** class constant */
  | Cptr_to_fld Ident.fieldname Typ.t /** pointer to field constant, and type of the surrounding Csu.t type */
[@@deriving compare];

let equal = [%compare.equal : t];

let kind_equal c1 c2 => {
  let const_kind_number =
    fun
    | Cint _ => 1
    | Cfun _ => 2
    | Cstr _ => 3
    | Cfloat _ => 4
    | Cclass _ => 5
    | Cptr_to_fld _ => 6;
  Int.equal (const_kind_number c1) (const_kind_number c2)
};

let pp pe f =>
  fun
  | Cint i => IntLit.pp f i
  | Cfun fn =>
    switch pe.Pp.kind {
    | HTML => F.fprintf f "_fun_%s" (Escape.escape_xml (Typ.Procname.to_string fn))
    | _ => F.fprintf f "_fun_%s" (Typ.Procname.to_string fn)
    }
  | Cstr s => F.fprintf f "\"%s\"" (String.escaped s)
  | Cfloat v => F.fprintf f "%f" v
  | Cclass c => F.fprintf f "%a" Ident.pp_name c
  | Cptr_to_fld fn _ => F.fprintf f "__fld_%a" Ident.pp_fieldname fn;

let to_string c => F.asprintf "%a" (pp Pp.text) c;

let iszero_int_float =
  fun
  | Cint i => IntLit.iszero i
  | Cfloat 0.0 => true
  | _ => false;

let isone_int_float =
  fun
  | Cint i => IntLit.isone i
  | Cfloat 1.0 => true
  | _ => false;

let isminusone_int_float =
  fun
  | Cint i => IntLit.isminusone i
  | Cfloat (-1.0) => true
  | _ => false;
