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


/** The Smallfoot Intermediate Language: Expressions */
let module L = Logging;

let module F = Format;

type closure = {name: Procname.t, captured_vars: list (t, Pvar.t, Typ.t)}
/** dynamically determined length of an array value, if any */
and dynamic_length = option t
/** Program expressions. */
and t =
  /** Pure variable: it is not an lvalue */
  | Var of Ident.t
  /** Unary operator with type of the result if known */
  | UnOp of Unop.t t (option Typ.t)
  /** Binary operator */
  | BinOp of Binop.t t t
  /** Exception */
  | Exn of t
  /** Anonymous function */
  | Closure of closure
  /** Constants */
  | Const of Const.t
  /** Type cast */
  | Cast of Typ.t t
  /** The address of a program variable */
  | Lvar of Pvar.t
  /** A field offset, the type is the surrounding struct type */
  | Lfield of t Ident.fieldname Typ.t
  /** An array index offset: [exp1\[exp2\]] */
  | Lindex of t t
  /** A sizeof expression. [Sizeof (Tarray elt (Some static_length)) (Some dynamic_length)]
      represents the size of an array value consisting of [dynamic_length] elements of type [elt].
      The [dynamic_length], tracked by symbolic execution, may differ from the [static_length]
      obtained from the type definition, e.g. when an array is over-allocated.  For struct types,
      the [dynamic_length] is that of the final extensible array, if any. */
  | Sizeof of Typ.t dynamic_length Subtype.t;
