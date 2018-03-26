(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging
open AbstractDomain.Types

module IntCost = struct
  type astate = int [@@deriving compare]

  let pp fmt i = F.fprintf fmt "%i" i

  let join = Int.max

  let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

  let ( <= ) ~lhs ~rhs = Int.( <= ) lhs rhs
end

module Cost = struct
  include AbstractDomain.TopLifted (IntCost)

  let widen ~prev ~next ~num_iters:_ =
    if phys_equal prev next then prev
    else
      match (prev, next) with
      | NonTop prev, NonTop next when IntCost.( <= ) ~lhs:next ~rhs:prev ->
          NonTop prev
      | _, _ ->
          Top


  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | Top, Top | NonTop _, Top ->
        true
    | Top, NonTop _ ->
        false
    | NonTop c1, NonTop c2 ->
        Int.( <= ) c1 c2


  let pp_l fmt c =
    let c'' = match c with Top -> Top | NonTop c' -> NonTop (-c') in
    pp fmt c''


  let pp_u = pp
end

module IntPair = struct
  (* Represents node id,instr index within node *)
  include AbstractDomain.Pair (IntCost) (IntCost)

  type t = IntCost.astate * IntCost.astate [@@deriving compare]
end

(* Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap = AbstractDomain.Map (IntPair) (Itv.Bound)

module ItvPureCost = struct
  (** (l, u) represents the closed interval [-l; u] (of course infinite bounds are open) *)
  type astate = Cost.astate * Cost.astate

  type t = astate

  let ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Cost.( <= ) ~lhs:l1 ~rhs:l2 && Cost.( <= ) ~lhs:u1 ~rhs:u2


  let join : t -> t -> t = fun (l1, u1) (l2, u2) -> (Cost.join l1 l2, Cost.join u1 u2)

  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters ->
    (Cost.widen ~prev:l1 ~next:l2 ~num_iters, Cost.widen ~prev:u1 ~next:u2 ~num_iters)


  let pp : F.formatter -> t -> unit =
   fun fmt (l, u) -> F.fprintf fmt "[%a, %a]" Cost.pp_l l Cost.pp_u u
end

module EnvDomain = AbstractDomain.Map (Exp) (ItvPureCost)
module EnvDomainBO = AbstractDomain.Map (Exp) (Itv)

type summary = {post: Itv.Bound.t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" Itv.Bound.pp post
