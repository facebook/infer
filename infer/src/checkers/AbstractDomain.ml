(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

module type S = sig
  type astate

  val (<=) : lhs:astate -> rhs:astate -> bool (* fst \sqsubseteq snd? *)
  val join : astate -> astate -> astate
  val widen : prev:astate -> next:astate -> num_iters:int -> astate
  val pp : F.formatter -> astate -> unit
end

module type WithBottom = sig
  include S

  val empty : astate
end

module BottomLifted (Domain : S) = struct
  type astate =
    | Bottom
    | NonBottom of Domain.astate

  let empty = Bottom

  let (<=) ~lhs ~rhs =
    if phys_equal lhs rhs
    then true
    else
      match lhs, rhs with
      | Bottom, _ -> true
      | _ , Bottom -> false
      | NonBottom lhs, NonBottom rhs -> Domain.(<=) ~lhs ~rhs

  let join astate1 astate2 =
    if phys_equal astate1 astate2
    then astate1
    else
      match astate1, astate2 with
      | Bottom, _ -> astate2
      | _, Bottom -> astate1
      | NonBottom a1, NonBottom a2 -> NonBottom (Domain.join a1 a2)

  let widen ~prev ~next ~num_iters =
    if phys_equal prev next
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

module Pair (Domain1 : S) (Domain2 : S) = struct
  type astate = Domain1.astate * Domain2.astate

  let (<=) ~lhs ~rhs =
    if phys_equal lhs rhs
    then true
    else
      Domain1.(<=) ~lhs:(fst lhs) ~rhs:(fst rhs) && Domain2.(<=) ~lhs:(snd lhs) ~rhs:(snd rhs)

  let join astate1 astate2 =
    if phys_equal astate1 astate2
    then astate1
    else Domain1.join (fst astate1) (fst astate2), Domain2.join (snd astate1) (snd astate2)

  let widen ~prev ~next ~num_iters =
    if phys_equal prev next
    then prev
    else
      Domain1.widen ~prev:(fst prev) ~next:(fst next) ~num_iters,
      Domain2.widen ~prev:(snd prev) ~next:(snd next) ~num_iters

  let pp fmt (astate1, astate2) =
    F.fprintf fmt "(%a, %a)" Domain1.pp astate1 Domain2.pp astate2
end

module FiniteSet (S : PrettyPrintable.PPSet) = struct
  include S
  type astate = t

  let (<=) ~lhs ~rhs =
    if phys_equal lhs rhs
    then true
    else subset lhs rhs

  let join astate1 astate2 =
    if phys_equal astate1 astate2
    then astate1
    else union astate1 astate2

  let widen ~prev ~next ~num_iters:_ =
    join prev next
end

module InvertedSet (S : PrettyPrintable.PPSet) = struct
  include S
  type astate = t

  let (<=) ~lhs ~rhs =
    if phys_equal lhs rhs
    then true
    else subset rhs lhs

  let join astate1 astate2 =
    if phys_equal astate1 astate2
    then astate1
    else inter astate1 astate2

  let widen ~prev ~next ~num_iters:_ =
    join prev next
end

module Map (M : PrettyPrintable.PPMap) (ValueDomain : S) = struct
  include M
  type astate = ValueDomain.astate M.t

  (** true if all keys in [lhs] are in [rhs], and each lhs value <= corresponding rhs value *)
  let (<=) ~lhs ~rhs =
    if phys_equal lhs rhs
    then true
    else
      M.for_all
        (fun k lhs_v ->
           try ValueDomain.(<=) ~lhs:lhs_v ~rhs:(M.find k rhs)
           with Not_found -> false)
        lhs

  let join astate1 astate2 =
    if phys_equal astate1 astate2
    then astate1
    else
      M.merge
        (fun _ v1_opt v2_opt -> match v1_opt, v2_opt with
           | Some v1, Some v2 -> Some (ValueDomain.join v1 v2)
           | Some v, _ | _, Some v -> Some v
           | None, None -> None)
        astate1
        astate2

  let widen ~prev ~next ~num_iters =
    if phys_equal prev next
    then prev
    else
      M.merge
        (fun _ v1_opt v2_opt -> match v1_opt, v2_opt with
           | Some v1, Some v2 -> Some (ValueDomain.widen ~prev:v1 ~next:v2 ~num_iters)
           | Some v, _ | _, Some v -> Some v
           | None, None -> None)
      prev
      next

  let pp fmt astate =
    M.pp ~pp_value:ValueDomain.pp fmt astate
end

module BooleanAnd = struct
  type astate = bool

  let (<=) ~lhs ~rhs = lhs || not rhs

  let join = (&&)

  let widen ~prev ~next ~num_iters:_ =
    join prev next

  let pp fmt astate =
    F.fprintf fmt "%b" astate
end
