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


/** The Smallfoot Intermediate Language: Decompiled Expressions */
let module L = Logging;

let module F = Format;


/** expression representing the result of decompilation */
type t =
  | Darray t t
  | Dbinop Binop.t t t
  | Dconst Const.t
  | Dsizeof Typ.t (option t) Subtype.t
  | Dderef t
  | Dfcall t (list t) Location.t CallFlags.t
  | Darrow t Ident.fieldname
  | Ddot t Ident.fieldname
  | Dpvar Pvar.t
  | Dpvaraddr Pvar.t
  | Dunop Unop.t t
  | Dunknown
  | Dretcall t (list t) Location.t CallFlags.t;


/** Value paths: identify an occurrence of a value in a symbolic heap
    each expression represents a path, with Dpvar being the simplest one */
type vpath = option t;


/** convert to a string */
let to_string: t => string;


/** pretty print */
let pp: F.formatter => t => unit;


/** Pretty print a value path */
let pp_vpath: Pp.env => F.formatter => vpath => unit;


/** return true if [dexp] contains a temporary pvar */
let has_tmp_var: t => bool;
