/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** The Smallfoot Intermediate Language: Constants */
let module L = Logging;

let module F = Format;

type t =
  | Cint of IntLit.t /** integer constants */
  | Cfun of Procname.t /** function names */
  | Cstr of string /** string constants */
  | Cfloat of float /** float constants */
  | Cclass of Ident.name /** class constant */
  | Cptr_to_fld of Ident.fieldname Typ.t /** pointer to field constant,
                                             and type of the surrounding Csu.t type */;

let compare (c1: t) (c2: t) :int =>
  switch (c1, c2) {
  | (Cint i1, Cint i2) => IntLit.compare i1 i2
  | (Cint _, _) => (-1)
  | (_, Cint _) => 1
  | (Cfun fn1, Cfun fn2) => Procname.compare fn1 fn2
  | (Cfun _, _) => (-1)
  | (_, Cfun _) => 1
  | (Cstr s1, Cstr s2) => string_compare s1 s2
  | (Cstr _, _) => (-1)
  | (_, Cstr _) => 1
  | (Cfloat f1, Cfloat f2) => float_compare f1 f2
  | (Cfloat _, _) => (-1)
  | (_, Cfloat _) => 1
  | (Cclass c1, Cclass c2) => Ident.name_compare c1 c2
  | (Cclass _, _) => (-1)
  | (_, Cclass _) => 1
  | (Cptr_to_fld fn1 t1, Cptr_to_fld fn2 t2) =>
    let n = Ident.fieldname_compare fn1 fn2;
    if (n != 0) {
      n
    } else {
      Typ.compare t1 t2
    }
  };

let equal c1 c2 => compare c1 c2 == 0;

let kind_equal c1 c2 => {
  let const_kind_number =
    fun
    | Cint _ => 1
    | Cfun _ => 2
    | Cstr _ => 3
    | Cfloat _ => 4
    | Cclass _ => 5
    | Cptr_to_fld _ => 6;
  const_kind_number c1 == const_kind_number c2
};

let pp pe f =>
  fun
  | Cint i => IntLit.pp f i
  | Cfun fn =>
    switch pe.pe_kind {
    | PP_HTML => F.fprintf f "_fun_%s" (Escape.escape_xml (Procname.to_string fn))
    | _ => F.fprintf f "_fun_%s" (Procname.to_string fn)
    }
  | Cstr s => F.fprintf f "\"%s\"" (String.escaped s)
  | Cfloat v => F.fprintf f "%f" v
  | Cclass c => F.fprintf f "%a" Ident.pp_name c
  | Cptr_to_fld fn _ => F.fprintf f "__fld_%a" Ident.pp_fieldname fn;

let to_string c => pp_to_string (pp pe_text) c;

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
