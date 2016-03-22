(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

module type AbstractDomain = sig
  type astate

  val initial : astate
  val is_bottom : astate -> bool
  val (<=) : lhs:astate -> rhs:astate -> bool (* fst \sqsubseteq snd? *)
  val join : astate -> astate -> astate
  val widen : prev:astate -> next:astate -> num_iters:int -> astate
  val pp : F.formatter -> astate -> unit

end

module BottomLiftedAbstractDomain (A : AbstractDomain) : AbstractDomain = struct
  type astate =
    | Bottom
    | NonBottom of A.astate

  let is_bottom astate =
    astate = Bottom

  let initial = NonBottom A.initial

  let (<=) ~lhs ~rhs = match lhs, rhs with
    | Bottom, _ -> true
    | _ , Bottom -> false
    | NonBottom lhs, NonBottom rhs -> A.(<=) ~lhs ~rhs

  let join astate1 astate2 =
    match astate1, astate2 with
    | Bottom, _ -> astate2
    | _, Bottom -> astate1
    | NonBottom a1, NonBottom a2 -> NonBottom (A.join a1 a2)

  let widen ~prev ~next ~num_iters =
    match prev, next with
    | Bottom, _ -> next
    | _, Bottom -> prev
    | NonBottom prev, NonBottom next -> NonBottom (A.widen ~prev ~next ~num_iters)

  let pp fmt = function
    | Bottom -> F.fprintf fmt "_|_"
    | NonBottom astate -> A.pp fmt astate

end

(* lift a set to a domain. the elements of the set should be drawn from a *finite* collection of
   possible values, since the widening operator here is just union. *)
module FiniteSetDomain (S : PrettyPrintable.PPSet) = struct
  include S
  type astate = t

  let initial = empty
  let is_bottom _ = false
  let (<=) ~lhs ~rhs = subset lhs rhs
  let join = union
  let widen ~prev ~next ~num_iters:_ = union prev next

end
