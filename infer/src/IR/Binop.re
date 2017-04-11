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


/** The Smallfoot Intermediate Language: Binary Operators */
module L = Logging;

module F = Format;


/** Binary operations */
type t =
  | PlusA /** arithmetic + */
  | PlusPI /** pointer + integer */
  | MinusA /** arithmetic - */
  | MinusPI /** pointer - integer */
  | MinusPP /** pointer - pointer */
  | Mult /** * */
  | Div /** / */
  | Mod /** % */
  | Shiftlt /** shift left */
  | Shiftrt /** shift right */
  | Lt /** <  (arithmetic comparison) */
  | Gt /** >  (arithmetic comparison) */
  | Le /** <= (arithmetic comparison) */
  | Ge /** >= (arithmetic comparison) */
  | Eq /** == (arithmetic comparison) */
  | Ne /** != (arithmetic comparison) */
  | BAnd /** bitwise and */
  | BXor /** exclusive-or */
  | BOr /** inclusive-or */
  | LAnd /** logical and. Does not always evaluate both operands. */
  | LOr /** logical or. Does not always evaluate both operands. */
  | PtrFld /** field offset via pointer to field: takes the address of a Csu.t and a Cptr_to_fld constant to form an Lfield expression (see prop.ml) */
[@@deriving compare];

let equal = [%compare.equal : t];


/** This function returns true if the operation is injective
    wrt. each argument: op(e,-) and op(-, e) is injective for all e.
    The return value false means "don't know". */
let injective =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI
  | MinusPP => true
  | _ => false;


/** This function returns true if the operation can be inverted. */
let invertible =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI => true
  | _ => false;


/** This function inverts an invertible injective binary operator.
    If the [binop] operation is not invertible, the function raises Assert_failure. */
let invert bop =>
  switch bop {
  | PlusA => MinusA
  | PlusPI => MinusPI
  | MinusA => PlusA
  | MinusPI => PlusPI
  | _ => assert false
  };


/** This function returns true if 0 is the right unit of [binop].
    The return value false means "don't know". */
let is_zero_runit =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI
  | MinusPP => true
  | _ => false;

let text =
  fun
  | PlusA => "+"
  | PlusPI => "+"
  | MinusA
  | MinusPP => "-"
  | MinusPI => "-"
  | Mult => "*"
  | Div => "/"
  | Mod => "%"
  | Shiftlt => "<<"
  | Shiftrt => ">>"
  | Lt => "<"
  | Gt => ">"
  | Le => "<="
  | Ge => ">="
  | Eq => "=="
  | Ne => "!="
  | BAnd => "&"
  | BXor => "^"
  | BOr => "|"
  | LAnd => "&&"
  | LOr => "||"
  | PtrFld => "_ptrfld_";


/** Pretty print a binary operator. */
let str pe binop =>
  switch pe.Pp.kind {
  | HTML =>
    switch binop {
    | Ge => " &gt;= "
    | Le => " &lt;= "
    | Gt => " &gt; "
    | Lt => " &lt; "
    | Shiftlt => " &lt;&lt; "
    | Shiftrt => " &gt;&gt; "
    | _ => text binop
    }
  | LATEX =>
    switch binop {
    | Ge => " \\geq "
    | Le => " \\leq "
    | _ => text binop
    }
  | _ => text binop
  };
