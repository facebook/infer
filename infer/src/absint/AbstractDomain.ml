(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Types = struct
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate

  type ('below, 'above) below_above = Below of 'below | Above of 'above
end

open! Types

exception Stop_analysis

module type NoJoin = sig
  include PrettyPrintable.PrintableType

  val leq : lhs:t -> rhs:t -> bool
end

module type S = sig
  include NoJoin

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t
end

module Empty : S with type t = unit = struct
  type t = unit

  let leq ~lhs:() ~rhs:() = true

  let join () () = ()

  let widen ~prev:() ~next:() ~num_iters:_ = ()

  let pp f () = F.pp_print_string f "()"
end

module type WithBottom = sig
  include S

  val bottom : t

  val is_bottom : t -> bool
end

module type WithTop = sig
  include S

  val top : t

  val is_top : t -> bool
end

module BottomLiftedUtils = struct
  let leq ~leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | Bottom, _ ->
          true
      | _, Bottom ->
          false
      | NonBottom lhs, NonBottom rhs ->
          leq ~lhs ~rhs


  let map ~f astate =
    match astate with
    | Bottom ->
        astate
    | NonBottom a ->
        let a' = f a in
        if phys_equal a' a then astate else NonBottom a'


  let pp_bottom f = F.pp_print_string f SpecialChars.up_tack

  let pp ~pp f = function Bottom -> pp_bottom f | NonBottom astate -> pp f astate
end

module BottomLifted (Domain : S) = struct
  type t = Domain.t bottom_lifted

  let bottom = Bottom

  let is_bottom = function Bottom -> true | NonBottom _ -> false

  let leq = BottomLiftedUtils.leq ~leq:Domain.leq

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


  let map = BottomLiftedUtils.map

  let pp = BottomLiftedUtils.pp ~pp:Domain.pp
end

module TopLiftedUtils = struct
  let leq ~leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | _, Top ->
          true
      | Top, _ ->
          false
      | NonTop lhs, NonTop rhs ->
          leq ~lhs ~rhs


  let pp_top f = F.pp_print_string f SpecialChars.down_tack

  let pp ~pp f = function Top -> pp_top f | NonTop astate -> pp f astate
end

module TopLifted (Domain : S) = struct
  type t = Domain.t top_lifted

  let top = Top

  let is_top = function Top -> true | _ -> false

  let leq = TopLiftedUtils.leq ~leq:Domain.leq

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


  let pp = TopLiftedUtils.pp ~pp:Domain.pp
end

module Pair (Domain1 : S) (Domain2 : S) = struct
  type t = Domain1.t * Domain2.t

  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else Domain1.leq ~lhs:(fst lhs) ~rhs:(fst rhs) && Domain2.leq ~lhs:(snd lhs) ~rhs:(snd rhs)


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

  let bottom = Bot

  let is_bottom = function Bot -> true | _ -> false

  let top = Top

  let is_top = function Top -> true | _ -> false

  let leq ~lhs ~rhs =
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
        BottomLiftedUtils.pp_bottom f
    | V v ->
        V.pp f v
    | Top ->
        TopLiftedUtils.pp_top f


  let v x = V x

  let get = function V v -> Some v | Bot | Top -> None
end

module StackedUtils = struct
  let compare x1 x2 ~cmp_below ~cmp_above =
    if phys_equal x1 x2 then 0
    else
      match (x1, x2) with
      | Below b1, Below b2 ->
          cmp_below b1 b2
      | Below _, Above _ ->
          -1
      | Above _, Below _ ->
          1
      | Above a1, Above a2 ->
          cmp_above a1 a2


  let leq ~leq_below ~leq_above ~lhs ~rhs =
    phys_equal lhs rhs
    ||
    match (lhs, rhs) with
    | Below lhs, Below rhs ->
        leq_below ~lhs ~rhs
    | Below _, Above _ ->
        true
    | Above _, Below _ ->
        false
    | Above lhs, Above rhs ->
        leq_above ~lhs ~rhs


  let combine ~dir x1 x2 ~f_below ~f_above =
    match (x1, x2) with
    | Below b1, Below b2 ->
        Below (f_below b1 b2)
    | (Below _ as below), (Above _ as above) | (Above _ as above), (Below _ as below) -> (
      match dir with `Increasing -> above | `Decreasing -> below )
    | Above a1, Above a2 ->
        Above (f_above a1 a2)


  let map x ~f_below ~f_above =
    match x with Below b -> Below (f_below b) | Above a -> Above (f_above a)


  let pp ~pp_below ~pp_above f = function Below b -> pp_below f b | Above a -> pp_above f a
end

module Stacked (Below : S) (Above : S) = struct
  type t = (Below.t, Above.t) below_above

  let leq = StackedUtils.leq ~leq_below:Below.leq ~leq_above:Above.leq

  let join = StackedUtils.combine ~dir:`Increasing ~f_below:Below.join ~f_above:Above.join

  let widen ~prev ~next ~num_iters =
    StackedUtils.combine ~dir:`Increasing prev next
      ~f_below:(fun prev next -> Below.widen ~prev ~next ~num_iters)
      ~f_above:(fun prev next -> Above.widen ~prev ~next ~num_iters)


  let pp = StackedUtils.pp ~pp_below:Below.pp ~pp_above:Above.pp
end

module MinReprSet (Element : PrettyPrintable.PrintableOrderedType) = struct
  type elt = Element.t [@@deriving compare]

  type t = elt option [@@deriving compare]

  let bottom = None

  let is_bottom = Option.is_none

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | None, _ ->
        true
    | Some _, None ->
        false
    | Some lhs, Some rhs ->
        Int.(Element.compare rhs lhs <= 0)


  let join x1 x2 =
    match (x1, x2) with
    | None, x | x, None ->
        x
    | Some e1, Some e2 ->
        if Int.(Element.compare e1 e2 <= 0) then x1 else x2


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp f = function None -> () | Some x -> Element.pp f x

  let singleton x = Some x

  let min_elt x = x

  let add e = function
    | None ->
        singleton e
    | Some e' when Int.(Element.compare e e' < 0) ->
        Some e
    | x ->
        x


  let map f x = Option.map x ~f

  let fold f x init = Option.fold x ~init ~f:(fun acc e -> f e acc)

  let exists f x = Option.exists x ~f
end

module type FiniteSetS = sig
  include PrettyPrintable.PPSet

  include WithBottom with type t := t
end

module FiniteSetOfPPSet (S : PrettyPrintable.PPSet) = struct
  include S

  let bottom = empty

  let is_bottom = is_empty

  let leq ~lhs ~rhs = if phys_equal lhs rhs then true else subset lhs rhs

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

  let leq ~lhs ~rhs = if phys_equal lhs rhs then true else subset rhs lhs

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

  let bottom = empty

  let is_bottom = is_empty

  (** true if all keys in [lhs] are in [rhs], and each lhs value <= corresponding rhs value *)
  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      M.for_all
        (fun k lhs_v ->
          try ValueDomain.leq ~lhs:lhs_v ~rhs:(M.find k rhs) with Caml.Not_found -> false )
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

  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      try for_all (fun k rhs_v -> ValueDomain.leq ~lhs:(find k lhs) ~rhs:rhs_v) rhs
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

module SafeInvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : WithTop) = struct
  module M = InvertedMap (Key) (ValueDomain)

  type key = M.key

  type value = M.value

  type t = M.t

  let empty = M.empty

  let is_empty = M.is_empty

  let mem = M.mem

  let add k v m = if ValueDomain.is_top v then M.remove k m else M.add k v m

  let none_if_top_opt = function Some v when ValueDomain.is_top v -> None | r -> r

  let update k f m =
    let f opt_v = f opt_v |> none_if_top_opt in
    M.update k f m


  let singleton k v = add k v empty

  let remove = M.remove

  let merge f x y =
    let f k opt_v1 opt_v2 = f k opt_v1 opt_v2 |> none_if_top_opt in
    M.merge f x y


  let union f x y =
    let f k v1 v2 = f k v1 v2 |> none_if_top_opt in
    M.union f x y


  let compare = M.compare

  let equal = M.equal

  let iter = M.iter

  let fold = M.fold

  let for_all = M.for_all

  let exists = M.exists

  let filter = M.filter

  let partition = M.partition

  let cardinal = M.cardinal

  let bindings = M.bindings

  let min_binding = M.min_binding

  let min_binding_opt = M.min_binding_opt

  let max_binding = M.max_binding

  let max_binding_opt = M.max_binding_opt

  let choose = M.choose

  let choose_opt = M.choose_opt

  let split = M.split

  let find = M.find

  let find_opt = M.find_opt

  let find_first = M.find_first

  let find_first_opt = M.find_first_opt

  let find_last = M.find_last

  let find_last_opt = M.find_last_opt

  let mapi f m =
    let tops = ref [] in
    let f k v =
      let v = f k v in
      if ValueDomain.is_top v then tops := k :: !tops ;
      v
    in
    let m = M.mapi f m in
    List.fold_left !tops ~init:m ~f:(fun m k -> remove k m)


  let map f m = mapi (fun _ v -> f v) m

  let is_singleton_or_more = M.is_singleton_or_more

  let pp_key = M.pp_key

  let pp = M.pp

  let leq = M.leq

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
                if ValueDomain.is_top v then (
                  equals1 := false ;
                  equals2 := false ;
                  None )
                else (
                  if not (phys_equal v v1) then equals1 := false ;
                  if not (phys_equal v v2) then equals2 := false ;
                  Some v )
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


  let top = M.top

  let is_top = M.is_top
end

module FiniteMultiMap
    (Key : PrettyPrintable.PrintableOrderedType)
    (Value : PrettyPrintable.PrintableOrderedType) =
struct
  module S = FiniteSet (Value)
  module M = Map (Key) (S)

  type t = M.t

  let bottom = M.empty

  let is_bottom = M.is_empty

  let leq = M.leq

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
            if S.is_empty s' then None else Some s' )
      m
end

module BooleanAnd = struct
  type t = bool

  let leq ~lhs ~rhs = lhs || not rhs

  let join = ( && )

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.pp_print_bool fmt astate
end

module BooleanOr = struct
  type t = bool

  let bottom = false

  let is_bottom astate = not astate

  let leq ~lhs ~rhs = (not lhs) || rhs

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


  let bottom = 0

  let is_top = Int.equal top

  let is_bottom = Int.equal bottom

  let leq ~lhs ~rhs = lhs <= rhs

  let join astate1 astate2 = Int.min top (Int.max astate1 astate2)

  let widen ~prev ~next ~num_iters:_ = join prev next

  let add astate1 astate2 = Int.min top (astate1 + astate2)

  let increment astate = if is_top astate then top else astate + 1

  let decrement astate = if is_bottom astate then bottom else astate - 1

  let pp = Int.pp
end

module DownwardIntDomain (MaxCount : MaxCount) = struct
  type t = int

  let bottom =
    assert (MaxCount.max > 0) ;
    MaxCount.max


  let top = 0

  let is_top = Int.equal top

  let is_bottom = Int.equal bottom

  let leq ~lhs ~rhs = lhs >= rhs

  let join astate1 astate2 = Int.min astate1 astate2

  let widen ~prev ~next ~num_iters:_ = join prev next

  let increment astate = if is_bottom astate then astate else astate + 1

  let decrement astate = if is_top astate then astate else astate - 1

  let pp = Int.pp
end
