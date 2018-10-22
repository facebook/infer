(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Types = struct
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate
end

open! Types

exception Stop_analysis

module type S = sig
  type astate

  val ( <= ) : lhs:astate -> rhs:astate -> bool

  (* fst \sqsubseteq snd? *)

  val join : astate -> astate -> astate

  val widen : prev:astate -> next:astate -> num_iters:int -> astate

  val pp : F.formatter -> astate -> unit
end

module Empty : S with type astate = unit = struct
  type astate = unit

  let ( <= ) ~lhs:() ~rhs:() = true

  let join () () = ()

  let widen ~prev:() ~next:() ~num_iters:_ = ()

  let pp f () = F.pp_print_string f "()"
end

module type WithBottom = sig
  include S

  val empty : astate

  val is_empty : astate -> bool
end

module type WithTop = sig
  include S

  val top : astate
end

module BottomLifted (Domain : S) = struct
  type astate = Domain.astate bottom_lifted

  let empty = Bottom

  let is_empty = function Bottom -> true | NonBottom _ -> false

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | Bottom, _ ->
          true
      | _, Bottom ->
          false
      | NonBottom lhs, NonBottom rhs ->
          Domain.( <= ) ~lhs ~rhs


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | Bottom, _ ->
          astate2
      | _, Bottom ->
          astate1
      | NonBottom a1, NonBottom a2 ->
          NonBottom (Domain.join a1 a2)


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      match (prev, next) with
      | Bottom, _ ->
          next
      | _, Bottom ->
          prev
      | NonBottom prev, NonBottom next ->
          NonBottom (Domain.widen ~prev ~next ~num_iters)


  let pp fmt = function
    | Bottom ->
        F.pp_print_string fmt "_|_"
    | NonBottom astate ->
        Domain.pp fmt astate
end

module TopLifted (Domain : S) = struct
  type astate = Domain.astate top_lifted

  let top = Top

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | _, Top ->
          true
      | Top, _ ->
          false
      | NonTop lhs, NonTop rhs ->
          Domain.( <= ) ~lhs ~rhs


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | Top, _ | _, Top ->
          Top
      | NonTop a1, NonTop a2 ->
          NonTop (Domain.join a1 a2)


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      match (prev, next) with
      | Top, _ | _, Top ->
          Top
      | NonTop prev, NonTop next ->
          NonTop (Domain.widen ~prev ~next ~num_iters)


  let pp fmt = function Top -> F.pp_print_char fmt 'T' | NonTop astate -> Domain.pp fmt astate
end

module Pair (Domain1 : S) (Domain2 : S) = struct
  type astate = Domain1.astate * Domain2.astate

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      Domain1.( <= ) ~lhs:(fst lhs) ~rhs:(fst rhs) && Domain2.( <= ) ~lhs:(snd lhs) ~rhs:(snd rhs)


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else (Domain1.join (fst astate1) (fst astate2), Domain2.join (snd astate1) (snd astate2))


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      ( Domain1.widen ~prev:(fst prev) ~next:(fst next) ~num_iters
      , Domain2.widen ~prev:(snd prev) ~next:(snd next) ~num_iters )


  let pp fmt astate = Pp.pair ~fst:Domain1.pp ~snd:Domain2.pp fmt astate
end

module FiniteSetOfPPSet (S : PrettyPrintable.PPSet) = struct
  include S

  type astate = t

  let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else subset lhs rhs

  let join astate1 astate2 = if phys_equal astate1 astate2 then astate1 else union astate1 astate2

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module FiniteSet (Element : PrettyPrintable.PrintableOrderedType) =
  FiniteSetOfPPSet (PrettyPrintable.MakePPSet (Element))

module InvertedSet (Element : PrettyPrintable.PrintableOrderedType) = struct
  include PrettyPrintable.MakePPSet (Element)

  type astate = t

  let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else subset rhs lhs

  let join astate1 astate2 = if phys_equal astate1 astate2 then astate1 else inter astate1 astate2

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module MapOfPPMap (M : PrettyPrintable.PPMap) (ValueDomain : S) = struct
  include M

  type astate = ValueDomain.astate M.t

  (** true if all keys in [lhs] are in [rhs], and each lhs value <= corresponding rhs value *)
  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      M.for_all
        (fun k lhs_v ->
          try ValueDomain.( <= ) ~lhs:lhs_v ~rhs:(M.find k rhs) with Caml.Not_found -> false )
        lhs


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      M.merge
        (fun _ v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (ValueDomain.join v1 v2)
          | Some v, _ | _, Some v ->
              Some v
          | None, None ->
              None )
        astate1 astate2


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      M.merge
        (fun _ v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (ValueDomain.widen ~prev:v1 ~next:v2 ~num_iters)
          | Some v, _ | _, Some v ->
              Some v
          | None, None ->
              None )
        prev next


  let pp fmt astate = M.pp ~pp_value:ValueDomain.pp fmt astate
end

module Map (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) =
  MapOfPPMap (PrettyPrintable.MakePPMap (Key)) (ValueDomain)

module InvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) = struct
  module M = PrettyPrintable.MakePPMap (Key)
  include M

  type astate = ValueDomain.astate M.t

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      try M.for_all (fun k rhs_v -> ValueDomain.( <= ) ~lhs:(M.find k lhs) ~rhs:rhs_v) rhs
      with Caml.Not_found -> false


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      M.merge
        (fun _ v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (ValueDomain.join v1 v2)
          | _ ->
              None )
        astate1 astate2


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      M.merge
        (fun _ v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (ValueDomain.widen ~prev:v1 ~next:v2 ~num_iters)
          | _ ->
              None )
        prev next


  let pp fmt astate = M.pp ~pp_value:ValueDomain.pp fmt astate
end

module BooleanAnd = struct
  type astate = bool

  let ( <= ) ~lhs ~rhs = lhs || not rhs

  let join = ( && )

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.pp_print_bool fmt astate
end

module BooleanOr = struct
  type astate = bool

  let empty = false

  let is_empty astate = not astate

  let ( <= ) ~lhs ~rhs = (not lhs) || rhs

  let join = ( || )

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.pp_print_bool fmt astate
end

module type MaxCount = sig
  val max : int
end

module CountDomain (MaxCount : MaxCount) = struct
  type astate = int

  let top =
    assert (MaxCount.max > 0) ;
    MaxCount.max


  let empty = 0

  let is_top = Int.equal top

  let is_empty = Int.equal empty

  let ( <= ) ~lhs ~rhs = lhs <= rhs

  let join astate1 astate2 = Int.min top (Int.max astate1 astate2)

  let widen ~prev ~next ~num_iters:_ = join prev next

  let add astate1 astate2 = Int.min top (astate1 + astate2)

  let increment astate = if is_top astate then top else astate + 1

  let decrement astate = if is_empty astate then empty else astate - 1

  let pp = Int.pp
end

module StackDomain (Element : PrettyPrintable.PrintableOrderedType) = struct
  type astate = Element.t list

  let push = List.cons

  let pop = List.tl_exn

  let is_empty = List.is_empty

  let empty = []

  let pp fmt x = Pp.semicolon_seq Element.pp fmt x

  (* is (rev rhs) a prefix of (rev lhs)? *)
  let ( <= ) ~lhs ~rhs =
    let rec aux lhs rhs =
      match (lhs, rhs) with
      | _, [] ->
          true
      | [], _ ->
          false
      | x :: _, y :: _ when not (Int.equal 0 (Element.compare x y)) ->
          false
      | _ :: xs, _ :: ys ->
          aux xs ys
    in
    phys_equal lhs rhs || aux (List.rev lhs) (List.rev rhs)


  (* compute (rev (longest common prefix)) *)
  let join lhs rhs =
    let rec aux acc a b =
      match (a, b) with
      | x :: xs, y :: ys when Int.equal 0 (Element.compare x y) ->
          aux (x :: acc) xs ys
      | _, _ ->
          acc
    in
    if phys_equal lhs rhs then lhs else aux [] (List.rev lhs) (List.rev rhs)


  let widen ~prev ~next ~num_iters:_ = join prev next
end
