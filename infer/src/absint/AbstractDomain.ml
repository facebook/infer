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
  include PrettyPrintable.PrintableType

  val ( <= ) : lhs:t -> rhs:t -> bool

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t
end

module Empty : S with type t = unit = struct
  type t = unit

  let ( <= ) ~lhs:() ~rhs:() = true

  let join () () = ()

  let widen ~prev:() ~next:() ~num_iters:_ = ()

  let pp f () = F.pp_print_string f "()"
end

module type WithBottom = sig
  include S

  val empty : t

  val is_empty : t -> bool
end

module type WithTop = sig
  include S

  val top : t

  val is_top : t -> bool
end

module BottomLifted (Domain : S) = struct
  type t = Domain.t bottom_lifted

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
          PhysEqual.optim2 ~res:(NonBottom (Domain.join a1 a2)) astate1 astate2


  let widen ~prev:prev0 ~next:next0 ~num_iters =
    if phys_equal prev0 next0 then prev0
    else
      match (prev0, next0) with
      | Bottom, _ ->
          next0
      | _, Bottom ->
          prev0
      | NonBottom prev, NonBottom next ->
          PhysEqual.optim2 ~res:(NonBottom (Domain.widen ~prev ~next ~num_iters)) prev0 next0


  let map ~f astate =
    match astate with
    | Bottom ->
        astate
    | NonBottom a ->
        let a' = f a in
        if phys_equal a' a then astate else NonBottom a'


  let pp fmt = function
    | Bottom ->
        F.pp_print_string fmt SpecialChars.up_tack
    | NonBottom astate ->
        Domain.pp fmt astate
end

module TopLifted (Domain : S) = struct
  type t = Domain.t top_lifted

  let top = Top

  let is_top = function Top -> true | _ -> false

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
          PhysEqual.optim2 ~res:(NonTop (Domain.join a1 a2)) astate1 astate2


  let widen ~prev:prev0 ~next:next0 ~num_iters =
    if phys_equal prev0 next0 then prev0
    else
      match (prev0, next0) with
      | Top, _ | _, Top ->
          Top
      | NonTop prev, NonTop next ->
          PhysEqual.optim2 ~res:(NonTop (Domain.widen ~prev ~next ~num_iters)) prev0 next0


  let pp fmt = function
    | Top ->
        F.pp_print_string fmt SpecialChars.down_tack
    | NonTop astate ->
        Domain.pp fmt astate
end

module Pair (Domain1 : S) (Domain2 : S) = struct
  type t = Domain1.t * Domain2.t

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      Domain1.( <= ) ~lhs:(fst lhs) ~rhs:(fst rhs) && Domain2.( <= ) ~lhs:(snd lhs) ~rhs:(snd rhs)


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      PhysEqual.optim2
        ~res:(Domain1.join (fst astate1) (fst astate2), Domain2.join (snd astate1) (snd astate2))
        astate1 astate2


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      PhysEqual.optim2
        ~res:
          ( Domain1.widen ~prev:(fst prev) ~next:(fst next) ~num_iters
          , Domain2.widen ~prev:(snd prev) ~next:(snd next) ~num_iters )
        prev next


  let pp fmt astate = Pp.pair ~fst:Domain1.pp ~snd:Domain2.pp fmt astate
end

module Flat (V : PrettyPrintable.PrintableEquatableType) = struct
  type t = Bot | V of V.t | Top

  let empty = Bot

  let is_empty = function Bot -> true | _ -> false

  let top = Top

  let is_top = function Top -> true | _ -> false

  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    ||
    match (lhs, rhs) with
    | Bot, _ | _, Top ->
        true
    | Top, _ | _, Bot ->
        false
    | V v1, V v2 ->
        V.equal v1 v2


  let join a1 a2 =
    match (a1, a2) with
    | Top, _ | _, Top ->
        Top
    | Bot, a | a, Bot ->
        a
    | V v1, V v2 ->
        if V.equal v1 v2 then a1 else Top


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp f = function
    | Bot ->
        F.pp_print_string f SpecialChars.up_tack
    | V v ->
        V.pp f v
    | Top ->
        F.pp_print_string f SpecialChars.down_tack


  let v x = V x

  let get = function V v -> Some v | Bot | Top -> None
end

module type FiniteSetS = sig
  include PrettyPrintable.PPSet

  include WithBottom with type t := t
end

module FiniteSetOfPPSet (S : PrettyPrintable.PPSet) = struct
  include S

  let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else subset lhs rhs

  let join astate1 astate2 = if phys_equal astate1 astate2 then astate1 else union astate1 astate2

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module FiniteSet (Element : PrettyPrintable.PrintableOrderedType) =
  FiniteSetOfPPSet (PrettyPrintable.MakePPSet (Element))

module type InvertedSetS = sig
  include PrettyPrintable.PPSet

  include WithTop with type t := t
end

module InvertedSet (Element : PrettyPrintable.PrintableOrderedType) = struct
  include PrettyPrintable.MakePPSet (Element)

  let top = empty

  let is_top = is_empty

  let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else subset rhs lhs

  let join astate1 astate2 = if phys_equal astate1 astate2 then astate1 else inter astate1 astate2

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module type MapS = sig
  include PrettyPrintable.PPMonoMap

  include WithBottom with type t := t
end

module MapOfPPMap (M : PrettyPrintable.PPMap) (ValueDomain : S) = struct
  include (M : PrettyPrintable.PPMap with type 'a t := 'a M.t and type key = M.key)

  type t = ValueDomain.t M.t

  type value = ValueDomain.t

  (** true if all keys in [lhs] are in [rhs], and each lhs value <= corresponding rhs value *)
  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      M.for_all
        (fun k lhs_v ->
          try ValueDomain.( <= ) ~lhs:lhs_v ~rhs:(M.find k rhs) with Caml.Not_found -> false )
        lhs


  let increasing_union ~f astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      let equals1 = ref true in
      let equals2 = ref true in
      let res =
        M.merge
          (fun _ v1_opt v2_opt ->
            match (v1_opt, v2_opt) with
            | Some v1, Some v2 ->
                let v = f v1 v2 in
                if not (phys_equal v v1) then equals1 := false ;
                if not (phys_equal v v2) then equals2 := false ;
                Some v
            | Some _, None ->
                equals2 := false ;
                v1_opt
            | None, Some _ ->
                equals1 := false ;
                v2_opt
            | None, None ->
                None )
          astate1 astate2
      in
      if !equals1 then astate1 else if !equals2 then astate2 else res


  let join astate1 astate2 = increasing_union ~f:ValueDomain.join astate1 astate2

  let widen ~prev ~next ~num_iters =
    increasing_union prev next ~f:(fun prev next -> ValueDomain.widen ~prev ~next ~num_iters)


  let pp fmt astate = M.pp ~pp_value:ValueDomain.pp fmt astate
end

module Map (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) = struct
  module M = PrettyPrintable.MakePPMap (Key)
  include MapOfPPMap (M) (ValueDomain)
end

module type InvertedMapS = sig
  include PrettyPrintable.PPMonoMap

  include WithTop with type t := t
end

module InvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) = struct
  include PrettyPrintable.MakePPMonoMap (Key) (ValueDomain)

  let top = empty

  let is_top = is_empty

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      try for_all (fun k rhs_v -> ValueDomain.( <= ) ~lhs:(find k lhs) ~rhs:rhs_v) rhs
      with Caml.Not_found -> false


  let inter ~f astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      let equals1 = ref true in
      let equals2 = ref true in
      let res =
        merge
          (fun _ v1_opt v2_opt ->
            match (v1_opt, v2_opt) with
            | Some v1, Some v2 ->
                let v = f v1 v2 in
                if not (phys_equal v v1) then equals1 := false ;
                if not (phys_equal v v2) then equals2 := false ;
                Some v
            | Some _, None ->
                equals1 := false ;
                None
            | None, Some _ ->
                equals2 := false ;
                None
            | None, None ->
                None )
          astate1 astate2
      in
      if !equals1 then astate1 else if !equals2 then astate2 else res


  let join = inter ~f:ValueDomain.join

  let widen ~prev ~next ~num_iters =
    inter prev next ~f:(fun prev next -> ValueDomain.widen ~prev ~next ~num_iters)
end

module FiniteMultiMap
    (Key : PrettyPrintable.PrintableOrderedType)
    (Value : PrettyPrintable.PrintableOrderedType) =
struct
  module S = FiniteSet (Value)
  module M = Map (Key) (S)

  type t = M.t

  let empty = M.empty

  let is_empty = M.is_empty

  let ( <= ) = M.( <= )

  let join = M.join

  let widen = M.widen

  let pp = M.pp

  let add k v m =
    M.update k (function None -> Some (S.singleton v) | Some s -> Some (S.add v s)) m


  let mem k m = M.mem k m

  let remove k v m =
    M.update k
      (function
        | None ->
            None
        | Some s ->
            let s' = S.remove v s in
            if S.is_empty s' then None else Some s')
      m
end

module BooleanAnd = struct
  type t = bool

  let ( <= ) ~lhs ~rhs = lhs || not rhs

  let join = ( && )

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.pp_print_bool fmt astate
end

module BooleanOr = struct
  type t = bool

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
  type t = int

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
  type t = Element.t list

  let push = List.cons

  let pop = List.tl_exn

  let is_top = List.is_empty

  let top = []

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
