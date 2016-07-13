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


/** The Smallfoot Intermediate Language: Binary Operators */
let module L = Logging;

let module F = Format;


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
  | PtrFld /** field offset via pointer to field: takes the address of a
               Csu.t and a Cptr_to_fld constant to form an Lfield expression (see prop.ml) */;

let compare o1 o2 =>
  switch (o1, o2) {
  | (PlusA, PlusA) => 0
  | (PlusA, _) => (-1)
  | (_, PlusA) => 1
  | (PlusPI, PlusPI) => 0
  | (PlusPI, _) => (-1)
  | (_, PlusPI) => 1
  | (MinusA, MinusA) => 0
  | (MinusA, _) => (-1)
  | (_, MinusA) => 1
  | (MinusPI, MinusPI) => 0
  | (MinusPI, _) => (-1)
  | (_, MinusPI) => 1
  | (MinusPP, MinusPP) => 0
  | (MinusPP, _) => (-1)
  | (_, MinusPP) => 1
  | (Mult, Mult) => 0
  | (Mult, _) => (-1)
  | (_, Mult) => 1
  | (Div, Div) => 0
  | (Div, _) => (-1)
  | (_, Div) => 1
  | (Mod, Mod) => 0
  | (Mod, _) => (-1)
  | (_, Mod) => 1
  | (Shiftlt, Shiftlt) => 0
  | (Shiftlt, _) => (-1)
  | (_, Shiftlt) => 1
  | (Shiftrt, Shiftrt) => 0
  | (Shiftrt, _) => (-1)
  | (_, Shiftrt) => 1
  | (Lt, Lt) => 0
  | (Lt, _) => (-1)
  | (_, Lt) => 1
  | (Gt, Gt) => 0
  | (Gt, _) => (-1)
  | (_, Gt) => 1
  | (Le, Le) => 0
  | (Le, _) => (-1)
  | (_, Le) => 1
  | (Ge, Ge) => 0
  | (Ge, _) => (-1)
  | (_, Ge) => 1
  | (Eq, Eq) => 0
  | (Eq, _) => (-1)
  | (_, Eq) => 1
  | (Ne, Ne) => 0
  | (Ne, _) => (-1)
  | (_, Ne) => 1
  | (BAnd, BAnd) => 0
  | (BAnd, _) => (-1)
  | (_, BAnd) => 1
  | (BXor, BXor) => 0
  | (BXor, _) => (-1)
  | (_, BXor) => 1
  | (BOr, BOr) => 0
  | (BOr, _) => (-1)
  | (_, BOr) => 1
  | (LAnd, LAnd) => 0
  | (LAnd, _) => (-1)
  | (_, LAnd) => 1
  | (LOr, LOr) => 0
  | (LOr, _) => (-1)
  | (_, LOr) => 1
  | (PtrFld, PtrFld) => 0
  };

let equal o1 o2 => compare o1 o2 == 0;


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
  switch pe.pe_kind {
  | PP_HTML =>
    switch binop {
    | Ge => " &gt;= "
    | Le => " &lt;= "
    | Gt => " &gt; "
    | Lt => " &lt; "
    | Shiftlt => " &lt;&lt; "
    | Shiftrt => " &gt;&gt; "
    | _ => text binop
    }
  | PP_LATEX =>
    switch binop {
    | Ge => " \\geq "
    | Le => " \\leq "
    | _ => text binop
    }
  | _ => text binop
  };
