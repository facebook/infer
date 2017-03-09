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


/** Constants */
type t =
  | Cint IntLit.t /** integer constants */
  | Cfun Typ.Procname.t /** function names */
  | Cstr string /** string constants */
  | Cfloat float /** float constants */
  | Cclass Ident.name /** class constant */
  | Cptr_to_fld Ident.fieldname Typ.t /** pointer to field constant, and type of the surrounding Csu.t type */
[@@deriving compare];

let equal: t => t => bool;


/** Return true if the constants have the same kind (both integers, ...) */
let kind_equal: t => t => bool;


/** Pretty print a const */
let pp: Pp.env => F.formatter => t => unit;

let to_string: t => string;

let iszero_int_float: t => bool;

let isone_int_float: t => bool;

let isminusone_int_float: t => bool;
