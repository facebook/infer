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


/** Comparison for expressions. */
let compare: t => t => int;


/** Equality for expressions. */
let equal: t => t => bool;


/** Hash function for expressions. */
let hash: t => int;


/** Set of expressions. */
let module Set: Set.S with type elt = t;


/** Map with expression keys. */
let module Map: Map.S with type key = t;


/** Hashtable with expression keys. */
let module Hash: Hashtbl.S with type key = t;


/** returns true is index is an array index of arr. */
let is_array_index_of: t => t => bool;

let is_null_literal: t => bool;


/** return true if [exp] is the special this/self expression */
let is_this: t => bool;

let is_zero: t => bool;


/** {2 Utility Functions for Expressions} */
/** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception */
let texp_to_typ: option Typ.t => t => Typ.t;


/** Return the root of [lexp]. */
let root_of_lexp: t => t;


/** Get an expression "undefined", the boolean indicates
    whether the undefined value goest into the footprint */
let get_undefined: bool => t;


/** Checks whether an expression denotes a location using pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. */
let pointer_arith: t => bool;


/** Integer constant 0 */
let zero: t;


/** Null constant */
let null: t;


/** Integer constant 1 */
let one: t;


/** Integer constant -1 */
let minus_one: t;


/** Create integer constant */
let int: IntLit.t => t;


/** Create float constant */
let float: float => t;


/** Create integer constant corresponding to the boolean value */
let bool: bool => t;


/** Create expresstion [e1 == e2] */
let eq: t => t => t;


/** Create expresstion [e1 != e2] */
let ne: t => t => t;


/** Create expresstion [e1 <= e2] */
let le: t => t => t;


/** Create expression [e1 < e2] */
let lt: t => t => t;


/** Extract the ids and pvars from an expression */
let get_vars: t => (list Ident.t, list Pvar.t);
