(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Binary Operators *)

open! IStd

type ikind_option_for_binop = Typ.ikind option

let compare_ikind_option_for_binop _ _ = 0

(** Binary operations *)
type t =
  | PlusA of ikind_option_for_binop  (** arithmetic + *)
  | PlusPI  (** pointer + integer *)
  | MinusA of ikind_option_for_binop  (** arithmetic - *)
  | MinusPI  (** pointer - integer *)
  | MinusPP  (** pointer - pointer *)
  | Mult of ikind_option_for_binop  (** * *)
  | Div  (** / *)
  | Mod  (** % *)
  | Shiftlt  (** shift left *)
  | Shiftrt  (** shift right *)
  | Lt  (** < (arithmetic comparison) *)
  | Gt  (** > (arithmetic comparison) *)
  | Le  (** <= (arithmetic comparison) *)
  | Ge  (** >= (arithmetic comparison) *)
  | Eq  (** == (arithmetic comparison) *)
  | Ne  (** != (arithmetic comparison) *)
  | BAnd  (** bitwise and *)
  | BXor  (** exclusive-or *)
  | BOr  (** inclusive-or *)
  | LAnd  (** logical and. Does not always evaluate both operands. *)
  | LOr  (** logical or. Does not always evaluate both operands. *)
[@@deriving compare]

let equal = [%compare.equal: t]

(** This function returns true if the operation is injective wrt. each argument: op(e,-) and op(-,
    e) is injective for all e. The return value false means "don't know". *)
let injective = function PlusA _ | PlusPI | MinusA _ | MinusPI | MinusPP -> true | _ -> false

(** This function returns true if 0 is the right unit of [binop]. The return value false means
    "don't know". *)
let is_zero_runit = function PlusA _ | PlusPI | MinusA _ | MinusPI | MinusPP -> true | _ -> false

let symmetric = function
  | (PlusA _ | PlusPI | Mult _ | Eq | Ne | BAnd | BXor | BOr | LAnd | LOr) as symmetric_op ->
      Some symmetric_op
  | MinusA _ | MinusPP | MinusPI | Div | Mod | Shiftlt | Shiftrt ->
      None
  | Lt ->
      Some Gt
  | Gt ->
      Some Lt
  | Le ->
      Some Ge
  | Ge ->
      Some Le


let negate = function
  | Eq ->
      Some Ne
  | Ne ->
      Some Eq
  | Lt ->
      Some Ge
  | Gt ->
      Some Le
  | Le ->
      Some Gt
  | Ge ->
      Some Lt
  | LAnd
  | LOr
  | PlusA _
  | PlusPI
  | Mult _
  | BAnd
  | BXor
  | BOr
  | MinusA _
  | MinusPP
  | MinusPI
  | Div
  | Mod
  | Shiftlt
  | Shiftrt ->
      None


let to_string = function
  | PlusA _ ->
      "+"
  | PlusPI ->
      "+"
  | MinusA _ | MinusPP ->
      "-"
  | MinusPI ->
      "-"
  | Mult _ ->
      "*"
  | Div ->
      "/"
  | Mod ->
      "%"
  | Shiftlt ->
      "<<"
  | Shiftrt ->
      ">>"
  | Lt ->
      "<"
  | Gt ->
      ">"
  | Le ->
      "<="
  | Ge ->
      ">="
  | Eq ->
      "=="
  | Ne ->
      "!="
  | BAnd ->
      "&"
  | BXor ->
      "^"
  | BOr ->
      "|"
  | LAnd ->
      "&&"
  | LOr ->
      "||"


let str pe binop =
  match pe.Pp.kind with
  | HTML -> (
    match binop with
    | Ge ->
        " &gt;= "
    | Le ->
        " &lt;= "
    | Gt ->
        " &gt; "
    | Lt ->
        " &lt; "
    | Shiftlt ->
        " &lt;&lt; "
    | Shiftrt ->
        " &gt;&gt; "
    | _ ->
        to_string binop )
  | _ ->
      to_string binop


let pp f binop = Format.fprintf f "%s" (to_string binop)
