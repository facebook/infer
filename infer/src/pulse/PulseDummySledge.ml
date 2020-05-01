(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Var = struct
  type t = unit

  let of_absval _ = ()

  let to_absval () = assert false
end

module Term = struct
  type t = unit

  let zero = ()

  let le () () = ()

  let lt () () = ()

  let not_ () = ()

  let of_intlit _ = ()

  let of_absval _ = ()

  let of_unop _ () = ()

  let of_binop _ () () = ()
end

(* same type as {!PulsePathCondition.t} to be nice to summary serialization *)
type t = {eqs: Sledge.Equality.t lazy_t; non_eqs: Sledge.Term.t lazy_t}

(* still print to make sure the formula never changes in debug *)
let pp fmt {eqs= (lazy eqs); non_eqs= (lazy non_eqs)} =
  F.fprintf fmt "%a∧%a" Sledge.Equality.pp eqs Sledge.Term.pp non_eqs


let true_ = {eqs= Lazy.from_val Sledge.Equality.true_; non_eqs= Lazy.from_val Sledge.Term.true_}

let and_eq () () phi = phi

let and_term () phi = phi

let and_ phi1 _ = phi1

let is_known_zero () _ = false

let is_unsat _ = false

let fold_map_variables phi ~init ~f:_ = (init, phi)

let simplify ~keep:_ phi = phi
