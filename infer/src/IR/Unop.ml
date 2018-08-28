(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Unary Operators *)

open! IStd

(** Unary operations *)
type t =
  | Neg  (** Unary minus *)
  | BNot  (** Bitwise complement (~) *)
  | LNot  (** Logical Not (!) *)
[@@deriving compare]

let equal = [%compare.equal: t]

(** String representation of unary operator. *)
let to_string = function Neg -> "-" | BNot -> "~" | LNot -> "!"
