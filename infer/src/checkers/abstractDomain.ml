(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

module type S = sig
  type astate

  val initial : astate
  val is_bottom : astate -> bool
  val (<=) : lhs:astate -> rhs:astate -> bool (* fst \sqsubseteq snd? *)
  val join : astate -> astate -> astate
  val widen : prev:astate -> next:astate -> num_iters:int -> astate
  val pp : F.formatter -> astate -> unit
end

module BottomLifted (Domain : S) = struct
  type astate =
    | Bottom
    | NonBottom of Domain.astate

  let is_bottom astate =
    astate = Bottom

  let initial = NonBottom Domain.initial

  let (<=) ~lhs ~rhs =
    if lhs == rhs
    then true
    else
      match lhs, rhs with
      | Bottom, _ -> true
      | _ , Bottom -> false
      | NonBottom lhs, NonBottom rhs -> Domain.(<=) ~lhs ~rhs

  let join astate1 astate2 =
    if astate1 == astate2
    then astate1
    else
      match astate1, astate2 with
      | Bottom, _ -> astate2
      | _, Bottom -> astate1
      | NonBottom a1, NonBottom a2 -> NonBottom (Domain.join a1 a2)

  let widen ~prev ~next ~num_iters =
    if prev == next
    then prev
    else
      match prev, next with
      | Bottom, _ -> next
      | _, Bottom -> prev
      | NonBottom prev, NonBottom next -> NonBottom (Domain.widen ~prev ~next ~num_iters)

  let pp fmt = function
    | Bottom -> F.fprintf fmt "_|_"
    | NonBottom astate -> Domain.pp fmt astate
end

(* cartestian product of two domains *)
module Pair (Domain1 : S) (Domain2 : S) = struct
  type astate = Domain1.astate * Domain2.astate

  let is_bottom (astate1, astate2) =
    Domain1.is_bottom astate1 || Domain2.is_bottom astate2

  let initial = Domain1.initial, Domain2.initial

  let (<=) ~lhs ~rhs =
    if lhs == rhs
    then true
    else
      match is_bottom lhs, is_bottom rhs with
      | true, _ -> true
      | _, true -> false
      | _ ->
          Domain1.(<=) ~lhs:(fst lhs) ~rhs:(fst rhs) && Domain2.(<=) ~lhs:(snd lhs) ~rhs:(snd rhs)

  let join astate1 astate2 =
    if astate1 == astate2
    then astate1
    else
      match is_bottom astate1, is_bottom astate2 with
      | true, _ -> astate2
      | _, true -> astate1
      | _ -> Domain1.join (fst astate1) (fst astate2), Domain2.join (snd astate1) (snd astate2)

  let widen ~prev ~next ~num_iters =
    if prev == next
    then prev
    else
      Domain1.widen ~prev:(fst prev) ~next:(fst next) ~num_iters,
      Domain2.widen ~prev:(snd prev) ~next:(snd next) ~num_iters

  let pp fmt (astate1, astate2) =
    F.fprintf fmt "(%a, %a)" Domain1.pp astate1 Domain2.pp astate2
end

(* lift a set to a domain. the elements of the set should be drawn from a *finite* collection of
   possible values, since the widening operator here is just union. *)
module FiniteSet (S : PrettyPrintable.PPSet) = struct
  include S
  type astate = t

  let initial = empty

  let is_bottom _ = false

  let (<=) ~lhs ~rhs =
    if lhs == rhs
    then true
    else subset lhs rhs

  let join astate1 astate2 =
    if astate1 == astate2
    then astate1
    else union astate1 astate2

  let widen ~prev ~next ~num_iters:_ =
    join prev next
end
