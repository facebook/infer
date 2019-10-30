(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = EqualTo of Const.t [@@deriving compare]

val pp : F.formatter -> t -> unit

val abduce_binop_is_true :
  negated:bool -> Binop.t -> t option -> t option -> bool * t option * t option
(** given [arith_lhs_opt bop arith_rhs_opt], return a triple
   [(satisfiable,abduced_lhs_opt,abduced_rhs_opt)] such that (taking lhs=true if lhs_opt is [None],
   same for rhs):

   - [satisfiable] iff lhs bop rhs ≠ ∅

   - [abduced_lhs_opt=Some alhs] if [satisfiable] and (lhs bop rhs ≠ ∅ => alhs⇔lhs) (and similarly for rhs)

   - likewise if [negated] with (lhs bop rhs = ∅) in the two points above
*)
