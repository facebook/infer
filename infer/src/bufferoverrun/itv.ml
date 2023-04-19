(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module Bound = Bounds.Bound
open Ints
module SymbolPath = Symb.SymbolPath
module SymbolSet = Symb.SymbolSet

module ItvRange = struct
  type t = Bounds.NonNegativeBound.t

  let zero loop_head : t = Bounds.NonNegativeBound.zero loop_head

  let of_bounds : loop_head_loc:Location.t -> lb:Bound.t -> ub:Bound.t -> t =
   fun ~loop_head_loc ~lb ~ub ->
    let lb =
      (* Handle the case of[s, c] where s contains positive length path and c
         is constant. E.g [len, 2] would give 3 since len is always
         nonnegative *)
      if Bound.is_symbolic lb && not (Bound.is_symbolic ub) then
        Bound.remove_positive_length_symbol lb
      else lb
    in
    Bound.plus_u ~weak:true ub Bound.one
    |> Bound.plus_u ~weak:true (Bound.neg lb)
    |> Bound.simplify_min_one |> Bound.simplify_bound_ends_from_paths
    |> Bound.simplify_minimum_length
    |> Bounds.NonNegativeBound.of_loop_bound loop_head_loc


  let to_top_lifted_polynomial : t -> Polynomials.NonNegativePolynomial.t =
   fun r -> Polynomials.NonNegativePolynomial.of_non_negative_bound r
end

module ItvPure = struct
  (** (l, u) represents the closed interval [l; u] (of course infinite bounds are open) *)
  type t = Bound.t * Bound.t [@@deriving compare, equal]

  let lb : t -> Bound.t = fst

  let ub : t -> Bound.t = snd

  let get_bound (lb, ub) bound_end =
    match bound_end with Symb.BoundEnd.LowerBound -> lb | Symb.BoundEnd.UpperBound -> ub


  let is_lb_infty : t -> bool = fun (l, _) -> Bound.is_minf l

  let is_finite : t -> bool =
   fun (l, u) ->
    match (Bound.get_const l, Bound.get_const u) with Some _, Some _ -> true | _, _ -> false


  let have_similar_bounds (l1, u1) (l2, u2) = Bound.are_similar l1 l2 && Bound.are_similar u1 u2

  let has_infty (l, u) = Bound.is_minf l || Bound.is_pinf u

  let exists_str ~f (l, u) = Bound.exists_str ~f l || Bound.exists_str ~f u

  let leq : lhs:t -> rhs:t -> bool =
   fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Bound.le l2 l1 && Bound.le u1 u2


  let xcompare ~lhs:(l1, u1) ~rhs:(l2, u2) =
    let lcmp = Bound.xcompare ~lhs:l1 ~rhs:l2 in
    let ucmp = Bound.xcompare ~lhs:u1 ~rhs:u2 in
    match (lcmp, ucmp) with
    | `Equal, `Equal ->
        `Equal
    | `NotComparable, _ | _, `NotComparable -> (
      match (Bound.xcompare ~lhs:u1 ~rhs:l2, Bound.xcompare ~lhs:u2 ~rhs:l1) with
      | `Equal, `Equal ->
          `Equal (* weird, though *)
      | (`Equal | `LeftSmallerThanRight), _ ->
          `LeftSmallerThanRight
      | _, (`Equal | `LeftSmallerThanRight) ->
          `RightSmallerThanLeft
      | (`NotComparable | `RightSmallerThanLeft), (`NotComparable | `RightSmallerThanLeft) ->
          `NotComparable )
    | `Equal, `LeftSmallerThanRight
    | `RightSmallerThanLeft, `Equal
    | `RightSmallerThanLeft, `LeftSmallerThanRight ->
        `RightSubsumesLeft
    | `Equal, `RightSmallerThanLeft
    | `LeftSmallerThanRight, `Equal
    | `LeftSmallerThanRight, `RightSmallerThanLeft ->
        `LeftSubsumesRight
    | `LeftSmallerThanRight, `LeftSmallerThanRight ->
        `LeftSmallerThanRight
    | `RightSmallerThanLeft, `RightSmallerThanLeft ->
        `RightSmallerThanLeft


  let join : t -> t -> t =
   fun (l1, u1) (l2, u2) -> (Bound.underapprox_min l1 l2, Bound.overapprox_max u1 u2)


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters:_ -> (Bound.widen_l l1 l2, Bound.widen_u u1 u2)


  let widen_thresholds : thresholds:Z.t list -> prev:t -> next:t -> num_iters:int -> t =
   fun ~thresholds ~prev:(l1, u1) ~next:(l2, u2) ~num_iters:_ ->
    (Bound.widen_l_thresholds ~thresholds l1 l2, Bound.widen_u_thresholds ~thresholds u1 u2)


  let pp_mark : markup:bool -> F.formatter -> t -> unit =
   fun ~markup fmt (l, u) ->
    if Bound.equal l u then Bound.pp_mark ~markup fmt l
    else
      match Bound.get_same_one_symbol l u with
      | Some symbol when Config.bo_debug < 3 ->
          Symb.SymbolPath.pp_mark ~markup fmt symbol
      | _ ->
          F.fprintf fmt "[%a, %a]" (Bound.pp_mark ~markup) l (Bound.pp_mark ~markup) u


  let pp = pp_mark ~markup:false

  let of_bound bound = (bound, bound)

  let of_int n = of_bound (Bound.of_int n)

  let of_big_int n = of_bound (Bound.of_big_int n)

  let of_int_lit n = of_big_int (IntLit.to_big_int n)

  let mone = of_bound Bound.mone

  let zero_255 = (Bound.zero, Bound.z255)

  let m1_255 = (Bound.mone, Bound.z255)

  let nat = (Bound.zero, Bound.pinf)

  let one = of_bound Bound.one

  let pos = (Bound.one, Bound.pinf)

  let set_lb lb (_, ub) = (lb, ub)

  let set_lb_zero = set_lb Bound.zero

  let top = (Bound.minf, Bound.pinf)

  let zero = of_bound Bound.zero

  let zero_one = (Bound.zero, Bound.one)

  let get_range_of_iterator = set_lb_zero

  let true_sem = one

  let false_sem = zero

  let unknown_bool = join false_sem true_sem

  let is_top : t -> bool = fun (l, u) -> Bound.is_minf l && Bound.is_pinf u

  let is_nat : t -> bool = fun (l, u) -> Bound.is_zero l && Bound.is_pinf u

  let get_const : t -> Z.t option =
   fun (l, u) ->
    match (Bound.get_const l, Bound.get_const u) with
    | (Some n as z), Some m when Z.equal n m ->
        z
    | _, _ ->
        None


  let is_zero : t -> bool = fun (l, u) -> Bound.is_zero l && Bound.is_zero u

  let is_one : t -> bool = fun (l, u) -> Bound.eq l Bound.one && Bound.eq u Bound.one

  let is_mone : t -> bool = fun (l, u) -> Bound.eq l Bound.mone && Bound.eq u Bound.mone

  let is_true : t -> bool = fun (l, u) -> Bound.le Bound.one l || Bound.le u Bound.mone

  let is_symbolic : t -> bool = fun (lb, ub) -> Bound.is_symbolic lb || Bound.is_symbolic ub

  let is_ge_zero : t -> bool = fun (lb, _) -> Bound.le Bound.zero lb

  let is_le_zero : t -> bool = fun (_, ub) -> Bound.le ub Bound.zero

  let is_false : t -> bool = fun i -> is_le_zero i && is_ge_zero i

  let is_le_mone : t -> bool = fun (_, ub) -> Bound.le ub Bound.mone

  let is_same_one_symbol (l, u) = Bound.is_same_one_symbol l u

  let range : Location.t -> t -> ItvRange.t =
   fun loop_head_loc (lb, ub) -> ItvRange.of_bounds ~loop_head_loc ~lb ~ub


  let neg : t -> t =
   fun (l, u) ->
    let l' = Bound.neg u in
    let u' = Bound.neg l in
    (l', u')


  let to_boolean : t -> Boolean.t =
   fun x -> if is_false x then Boolean.False else if is_true x then Boolean.True else Boolean.Top


  let lnot : t -> Boolean.t = fun x -> to_boolean x |> Boolean.not_

  let plus : t -> t -> t =
   fun (l1, u1) (l2, u2) -> (Bound.plus_l ~weak:false l1 l2, Bound.plus_u ~weak:false u1 u2)


  let minus : t -> t -> t = fun i1 i2 -> plus i1 (neg i2)

  let succ : t -> t = fun x -> plus x one

  let mult_const : Z.t -> t -> t =
   fun n ((l, u) as itv) ->
    match NonZeroInt.of_big_int n with
    | None ->
        zero
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else if NonZeroInt.is_positive n then (Bound.mult_const_l n l, Bound.mult_const_u n u)
        else (Bound.mult_const_l n u, Bound.mult_const_u n l)


  (* Returns a precise value only when all coefficients are divided by
     n without remainder. *)
  let div_const : t -> Z.t -> t =
   fun ((l, u) as itv) n ->
    match NonZeroInt.of_big_int n with
    | None ->
        top
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else
          let l', u' =
            if NonZeroInt.is_positive n then (Bound.div_const_l l n, Bound.div_const_u u n)
            else (Bound.div_const_l u n, Bound.div_const_u l n)
          in
          let default_l, default_u =
            if Bound.(le zero l) then
              if NonZeroInt.is_positive n then
                (* if 0<=l<=u and 0<c, 0 <= [l/c, u/c] <= u *)
                (Bound.zero, u)
              else (* if 0<=l<=u and c<0, -u <= [u/c, l/c] <= 0 *)
                (Bound.neg u, Bound.zero)
            else if Bound.(le u zero) then
              if NonZeroInt.is_positive n then
                (* if l<=u<=0 and 0<c, l <= [l/c, u/c] <= 0 *)
                (l, Bound.zero)
              else (* if l<=u<=0 and c<0, 0 <= [u/c, l/c] <= -l *)
                (Bound.zero, Bound.neg l)
            else if Bound.(le l zero && le zero u) then
              if NonZeroInt.is_positive n then (* if l<=0<=u and 0<c, l <= [l/c, u/c] <= u *)
                (l, u)
              else (* if l<=0<=u and c<0, -u <= [u/c, l/c] <= -l *)
                (Bound.neg u, Bound.neg l)
            else (Bound.minf, Bound.pinf)
          in
          (Option.value ~default:default_l l', Option.value ~default:default_u u')


  let mult : t -> t -> t =
   fun x y ->
    match (get_const x, get_const y) with
    | _, Some n ->
        mult_const n x
    | Some n, _ ->
        mult_const n y
    | None, None ->
        if is_same_one_symbol x && is_same_one_symbol y then
          (Bound.mk_MultB (Z.zero, lb x, lb y), Bound.mk_MultB (Z.zero, ub x, ub y))
        else top


  let div : t -> t -> t = fun x y -> match get_const y with None -> top | Some n -> div_const x n

  let mod_sem : t -> t -> t =
   fun x y ->
    match get_const y with
    | None ->
        top
    | Some n when Z.(equal n zero) ->
        x (* x % [0,0] does nothing. *)
    | Some m -> (
      match get_const x with
      | Some n ->
          of_big_int Z.(n mod m)
      | None ->
          let abs_m = Z.abs m in
          if is_ge_zero x then
            (Bound.zero, Bound.overapprox_min (Bound.of_big_int Z.(abs_m - one)) (ub x))
          else if is_le_zero x then
            (Bound.overapprox_max (Bound.of_big_int Z.(one - abs_m)) (lb x), Bound.zero)
          else (Bound.of_big_int Z.(one - abs_m), Bound.of_big_int Z.(abs_m - one)) )


  (* x << [-1,-1] does nothing. *)
  let shiftlt : t -> t -> t =
   fun x y ->
    Option.value_map (get_const y) ~default:top ~f:(fun n ->
        match Z.to_int n with
        | n ->
            if n < 0 then x else mult_const Z.(one lsl n) x
        | exception Z.Overflow ->
            top )


  (* x >> [-1,-1] does nothing. *)
  let shiftrt : t -> t -> t =
   fun x y ->
    if is_zero x then x
    else
      match get_const y with
      | Some n when Z.(leq n zero) ->
          x
      | Some n when Z.(geq n (of_int 64)) ->
          zero
      | Some n -> (
        match Z.to_int n with n -> div_const x Z.(one lsl n) | exception Z.Overflow -> top )
      | None ->
          top


  let band_sem : t -> t -> t =
   fun x y ->
    match (get_const x, get_const y) with
    | Some x', Some y' ->
        if Z.(equal x' y') then x else of_big_int Z.(x' land y')
    | _, _ ->
        if is_ge_zero x && is_ge_zero y then (Bound.zero, Bound.overapprox_min (ub x) (ub y))
        else if is_le_zero x && is_le_zero y then (Bound.minf, Bound.overapprox_min (ub x) (ub y))
        else if is_ge_zero x then (Bound.zero, ub x)
        else if is_ge_zero y then (Bound.zero, ub y)
        else top


  let lt_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.lt u1 l2 then Boolean.True else if Bound.le u2 l1 then Boolean.False else Boolean.Top


  let gt_sem : t -> t -> Boolean.t = fun x y -> lt_sem y x

  let le_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.le u1 l2 then Boolean.True else if Bound.lt u2 l1 then Boolean.False else Boolean.Top


  let ge_sem : t -> t -> Boolean.t = fun x y -> le_sem y x

  let eq_sem : t -> t -> Boolean.t =
   fun ((l1, u1) as itv1) ((l2, u2) as itv2) ->
    if Bound.eq l1 u1 && Bound.eq u1 l2 && Bound.eq l2 u2 then Boolean.True
    else if Bound.lt u1 l2 || Bound.lt u2 l1 then Boolean.False
    else if
      Boolean.equal Boolean.True (ge_sem itv1 itv2) && Boolean.equal Boolean.True (le_sem itv1 itv2)
    then Boolean.True
    else Boolean.Top


  let ne_sem : t -> t -> Boolean.t = fun itv1 itv2 -> eq_sem itv1 itv2 |> Boolean.not_

  let land_sem : t -> t -> Boolean.t = fun x y -> Boolean.and_ (to_boolean x) (to_boolean y)

  let lor_sem : t -> t -> Boolean.t = fun x y -> Boolean.or_ (to_boolean x) (to_boolean y)

  let lift_minmax_bound ~use_minmax_bound ~mk ~f x y =
    let r = f x y in
    if use_minmax_bound && Bound.is_infty r && Bound.is_not_infty x && Bound.is_not_infty y then
      mk x y
    else r


  let min_sem : ?use_minmax_bound:bool -> t -> t -> t =
   fun ?(use_minmax_bound = false) (l1, u1) (l2, u2) ->
    let lift = lift_minmax_bound ~use_minmax_bound ~mk:Bound.of_minmax_bound_min in
    (lift ~f:Bound.underapprox_min l1 l2, lift ~f:Bound.overapprox_min u1 u2)


  let max_sem : ?use_minmax_bound:bool -> t -> t -> t =
   fun ?(use_minmax_bound = false) (l1, u1) (l2, u2) ->
    let lift = lift_minmax_bound ~use_minmax_bound ~mk:Bound.of_minmax_bound_max in
    (lift ~f:Bound.underapprox_max l1 l2, lift ~f:Bound.overapprox_max u1 u2)


  let is_invalid : t -> bool = fun (l, u) -> Bound.is_pinf l || Bound.is_minf u || Bound.lt u l

  let normalize : t -> t bottom_lifted = fun x -> if is_invalid x then Bottom else NonBottom x

  let subst : t -> Bound.eval_sym -> t bottom_lifted =
   fun (l, u) eval_sym ->
    match (Bound.subst_lb l eval_sym, Bound.subst_ub u eval_sym) with
    | NonBottom l, NonBottom u ->
        normalize (l, u)
    | _ ->
        Bottom


  let prune_le : t -> t -> t bottom_lifted =
   fun (l1, u1) (_, u2) -> normalize (l1, Bound.overapprox_min u1 u2)


  let prune_ge : t -> t -> t bottom_lifted =
   fun (l1, u1) (l2, _) -> normalize (Bound.underapprox_max l1 l2, u1)


  let prune_lt : t -> t -> t bottom_lifted = fun x y -> prune_le x (minus y one)

  let prune_gt : t -> t -> t bottom_lifted = fun x y -> prune_ge x (plus y one)

  let prune_diff : t -> Bound.t -> t bottom_lifted =
   fun ((l, u) as itv) b ->
    if Bound.le b l then prune_gt itv (of_bound b)
    else if Bound.le u b then prune_lt itv (of_bound b)
    else NonBottom itv


  let prune_ne_zero : t -> t bottom_lifted = fun x -> prune_diff x Bound.zero

  let prune_comp : Binop.t -> t -> t -> t bottom_lifted =
   fun c x y ->
    if is_invalid y then NonBottom x
    else
      match c with
      | Le ->
          prune_le x y
      | Ge ->
          prune_ge x y
      | Lt ->
          prune_lt x y
      | Gt ->
          prune_gt x y
      | _ ->
          assert false


  let prune_eq : t -> t -> t bottom_lifted =
   fun x y ->
    match prune_comp Binop.Le x y with Bottom -> Bottom | NonBottom x' -> prune_comp Binop.Ge x' y


  let prune_eq_zero : t -> t bottom_lifted =
   fun x -> match prune_le x zero with Bottom -> Bottom | NonBottom x' -> prune_ge x' zero


  let prune_ne : t -> t -> t bottom_lifted =
   fun x (l, u) ->
    if is_invalid (l, u) then NonBottom x else if Bound.eq l u then prune_diff x l else NonBottom x


  let prune_ge_one : t -> t bottom_lifted = fun x -> prune_comp Binop.Ge x one

  let prune_binop : Binop.t -> t -> t -> t bottom_lifted =
   fun bop x y ->
    match bop with
    | Lt | Gt | Le | Ge ->
        prune_comp bop x y
    | Eq ->
        prune_eq x y
    | Ne ->
        prune_ne x y
    | PlusA _
    | PlusPI
    | MinusA _
    | MinusPI
    | MinusPP
    | Mult _
    | DivI
    | DivF
    | Mod
    | Shiftlt
    | Shiftrt
    | BAnd
    | BXor
    | BOr
    | LAnd
    | LOr ->
        NonBottom x


  let get_symbols : t -> SymbolSet.t =
   fun (l, u) -> SymbolSet.union (Bound.get_symbols l) (Bound.get_symbols u)


  let has_only_non_int_symbols x =
    let symbols = get_symbols x in
    (not (SymbolSet.is_empty symbols)) && SymbolSet.for_all Symb.Symbol.is_non_int symbols


  let make_non_negative : t -> t =
    let max_zero x = if Bound.lt x Bound.zero then Bound.zero else x in
    fun (l, u) -> (max_zero l, max_zero u)


  let max_of_ikind integer_type_widths ikind =
    let _, max = IntegerWidths.range_of_ikind integer_type_widths ikind in
    of_big_int max


  let of_path bound_of_path path =
    if Symb.SymbolPath.represents_multiple_values_sound path then
      let lb = bound_of_path (Symb.Symbol.make_boundend Symb.BoundEnd.LowerBound) path in
      let ub = bound_of_path (Symb.Symbol.make_boundend Symb.BoundEnd.UpperBound) path in
      (lb, ub)
    else
      let b = bound_of_path Symb.Symbol.make_onevalue path in
      (b, b)


  let of_normal_path ~unsigned ?non_int = of_path (Bound.of_normal_path ~unsigned ?non_int)

  let of_offset_path ~is_void = of_path (Bound.of_offset_path ~is_void)

  let of_length_path ~is_void = of_path (Bound.of_length_path ~is_void)

  let of_modeled_path = of_path Bound.of_modeled_path

  let is_offset_path_of path x =
    Bound.is_offset_path_of path (lb x) && Bound.is_offset_path_of path (ub x)


  let is_length_path_of path x =
    Bound.is_length_path_of path (lb x) && Bound.is_length_path_of path (ub x)


  let has_void_ptr_symb x = Bound.has_void_ptr_symb (lb x) || Bound.has_void_ptr_symb (ub x)

  let is_incr_of path x = Bound.is_incr_of path (lb x) && Bound.is_incr_of path (ub x)
end

include AbstractDomain.BottomLifted (ItvPure)

let widen_thresholds ~thresholds ~prev:prev0 ~next:next0 ~num_iters =
  if phys_equal prev0 next0 then prev0
  else
    match (prev0, next0) with
    | Bottom, _ ->
        next0
    | _, Bottom ->
        prev0
    | NonBottom prev, NonBottom next ->
        PhysEqual.optim2
          ~res:(NonBottom (ItvPure.widen_thresholds ~thresholds ~prev ~next ~num_iters))
          prev0 next0


let compare : t -> t -> int =
 fun x y ->
  match (x, y) with
  | Bottom, Bottom ->
      0
  | Bottom, _ ->
      -1
  | _, Bottom ->
      1
  | NonBottom x, NonBottom y ->
      ItvPure.compare x y


let bot : t = Bottom

let top : t = NonBottom ItvPure.top

let get_bound itv bound_end =
  match itv with Bottom -> Bottom | NonBottom x -> NonBottom (ItvPure.get_bound x bound_end)


let false_sem = NonBottom ItvPure.false_sem

let zero_255 = NonBottom ItvPure.zero_255

let m1_255 = NonBottom ItvPure.m1_255

let nat = NonBottom ItvPure.nat

let one = NonBottom ItvPure.one

let pos = NonBottom ItvPure.pos

let true_sem = NonBottom ItvPure.true_sem

let unknown_bool = NonBottom ItvPure.unknown_bool

let zero = NonBottom ItvPure.zero

let zero_one = NonBottom ItvPure.zero_one

let of_bool = function
  | Boolean.Bottom ->
      bot
  | Boolean.False ->
      false_sem
  | Boolean.True ->
      true_sem
  | Boolean.Top ->
      unknown_bool


let of_int : int -> t = fun n -> NonBottom (ItvPure.of_int n)

let of_big_int : Z.t -> t = fun n -> NonBottom (ItvPure.of_big_int n)

let of_int_lit : IntLit.t -> t = fun n -> NonBottom (ItvPure.of_int_lit n)

let is_false : t -> bool = function NonBottom x -> ItvPure.is_false x | Bottom -> false

let le : lhs:t -> rhs:t -> bool = leq

let eq : t -> t -> bool = fun x y -> leq ~lhs:x ~rhs:y && leq ~lhs:y ~rhs:x

let range loop_head : t -> ItvRange.t = function
  | Bottom ->
      ItvRange.zero loop_head
  | NonBottom itv ->
      ItvPure.range loop_head itv


let lift1 : (ItvPure.t -> ItvPure.t) -> t -> t =
 fun f -> function Bottom -> Bottom | NonBottom x -> NonBottom (f x)


let bind1_gen : bot:'a -> (ItvPure.t -> 'a) -> t -> 'a =
 fun ~bot f x -> match x with Bottom -> bot | NonBottom x -> f x


let bind1 : (ItvPure.t -> t) -> t -> t = bind1_gen ~bot:Bottom

let bind1b : (ItvPure.t -> Boolean.t) -> t -> Boolean.t = bind1_gen ~bot:Boolean.Bottom

let bind1bool : (ItvPure.t -> bool) -> t -> bool = bind1_gen ~bot:false

let bind1zo : (ItvPure.t -> Z.t option) -> t -> Z.t option = bind1_gen ~bot:None

let lift2 : (ItvPure.t -> ItvPure.t -> ItvPure.t) -> t -> t -> t =
 fun f x y ->
  match (x, y) with
  | Bottom, _ | _, Bottom ->
      Bottom
  | NonBottom x, NonBottom y ->
      NonBottom (f x y)


let bind2_gen : bot:'a -> (ItvPure.t -> ItvPure.t -> 'a) -> t -> t -> 'a =
 fun ~bot f x y ->
  match (x, y) with Bottom, _ | _, Bottom -> bot | NonBottom x, NonBottom y -> f x y


let bind2 : (ItvPure.t -> ItvPure.t -> t) -> t -> t -> t = bind2_gen ~bot:Bottom

let bind2b : (ItvPure.t -> ItvPure.t -> Boolean.t) -> t -> t -> Boolean.t =
  bind2_gen ~bot:Boolean.Bottom


let plus : t -> t -> t = lift2 ItvPure.plus

let minus : t -> t -> t = lift2 ItvPure.minus

let incr = plus one

let decr x = minus x one

let decr_length x = map ~f:ItvPure.make_non_negative (decr x)

let set_lb lb = lift1 (ItvPure.set_lb lb)

let set_lb_zero = lift1 ItvPure.set_lb_zero

let get_range_of_iterator : t -> t = lift1 ItvPure.get_range_of_iterator

let get_const : t -> Z.t option = bind1zo ItvPure.get_const

let is_zero = bind1bool ItvPure.is_zero

let is_one = bind1bool ItvPure.is_one

let is_mone = bind1bool ItvPure.is_mone

let neg : t -> t = lift1 ItvPure.neg

let lnot : t -> Boolean.t = bind1b ItvPure.lnot

let mult : t -> t -> t = lift2 ItvPure.mult

let mult_const : t -> Z.t -> t = fun x z -> lift1 (fun x -> ItvPure.mult_const z x) x

let div : t -> t -> t = lift2 ItvPure.div

let div_const : t -> Z.t -> t = fun x z -> lift1 (fun x -> ItvPure.div_const x z) x

let mod_sem : t -> t -> t = lift2 ItvPure.mod_sem

let shiftlt : t -> t -> t = lift2 ItvPure.shiftlt

let shiftrt : t -> t -> t = lift2 ItvPure.shiftrt

let band_sem : t -> t -> t = lift2 ItvPure.band_sem

let lt_sem : t -> t -> Boolean.t = bind2b ItvPure.lt_sem

let gt_sem : t -> t -> Boolean.t = bind2b ItvPure.gt_sem

let le_sem : t -> t -> Boolean.t = bind2b ItvPure.le_sem

let ge_sem : t -> t -> Boolean.t = bind2b ItvPure.ge_sem

let eq_sem : t -> t -> Boolean.t = bind2b ItvPure.eq_sem

let ne_sem : t -> t -> Boolean.t = bind2b ItvPure.ne_sem

let land_sem : t -> t -> Boolean.t = bind2b ItvPure.land_sem

let lor_sem : t -> t -> Boolean.t = bind2b ItvPure.lor_sem

let min_sem : ?use_minmax_bound:bool -> t -> t -> t =
 fun ?use_minmax_bound -> lift2 (ItvPure.min_sem ?use_minmax_bound)


let max_sem : ?use_minmax_bound:bool -> t -> t -> t =
 fun ?use_minmax_bound -> lift2 (ItvPure.max_sem ?use_minmax_bound)


let prune_eq_zero : t -> t = bind1 ItvPure.prune_eq_zero

let prune_ne_zero : t -> t = bind1 ItvPure.prune_ne_zero

let prune_ge_one : t -> t = bind1 ItvPure.prune_ge_one

let prune_binop : Binop.t -> t -> t -> t = fun comp -> bind2 (ItvPure.prune_binop comp)

let prune_lt : t -> t -> t = bind2 ItvPure.prune_lt

let prune_le : t -> t -> t = bind2 ItvPure.prune_le

let prune_eq : t -> t -> t = bind2 ItvPure.prune_eq

let prune_ne : t -> t -> t = bind2 ItvPure.prune_ne

let subst : t -> Bound.eval_sym -> t =
 fun x eval_sym -> match x with NonBottom x' -> ItvPure.subst x' eval_sym | _ -> x


let is_symbolic = bind1bool ItvPure.is_symbolic

let get_symbols : t -> SymbolSet.t = function
  | Bottom ->
      SymbolSet.empty
  | NonBottom x ->
      ItvPure.get_symbols x


let normalize : t -> t = bind1 ItvPure.normalize

let max_of_ikind integer_type_widths ikind =
  NonBottom (ItvPure.max_of_ikind integer_type_widths ikind)


let of_normal_path ~unsigned ?non_int path =
  NonBottom (ItvPure.of_normal_path ~unsigned ?non_int path)


let of_offset_path ~is_void path = NonBottom (ItvPure.of_offset_path ~is_void path)

let of_length_path ~is_void path = NonBottom (ItvPure.of_length_path ~is_void path)

let of_modeled_path path = NonBottom (ItvPure.of_modeled_path path)

let is_offset_path_of path = bind1_gen ~bot:false (ItvPure.is_offset_path_of path)

let is_length_path_of path = bind1_gen ~bot:false (ItvPure.is_length_path_of path)

let has_only_non_int_symbols = bind1bool ItvPure.has_only_non_int_symbols

let is_incr_of path = bind1bool (ItvPure.is_incr_of path)

let is_top v = match v with Bottom -> false | NonBottom v_itv_pure -> ItvPure.is_top v_itv_pure
