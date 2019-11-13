(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module Bound = struct
  type t = Int of IntLit.t | MinusInfinity | PlusInfinity [@@deriving compare]

  let pp fmt = function
    | Int i ->
        IntLit.pp fmt i
    | MinusInfinity ->
        F.pp_print_string fmt "-∞"
    | PlusInfinity ->
        F.pp_print_string fmt "+∞"


  let equal b1 b2 =
    match (b1, b2) with
    | MinusInfinity, MinusInfinity | PlusInfinity, PlusInfinity ->
        true
    | Int i1, Int i2 ->
        IntLit.eq i1 i2
    | _ ->
        false


  let le b1 b2 =
    match (b1, b2) with
    | MinusInfinity, _ | _, PlusInfinity ->
        true
    | (Int _ | PlusInfinity), MinusInfinity | PlusInfinity, Int _ ->
        false
    | Int i1, Int i2 ->
        IntLit.leq i1 i2


  let lt b1 b2 =
    match (b1, b2) with
    | MinusInfinity, _ | _, PlusInfinity ->
        true
    | (Int _ | PlusInfinity), MinusInfinity | PlusInfinity, Int _ ->
        false
    | Int i1, Int i2 ->
        IntLit.lt i1 i2


  let ge b1 b2 =
    match (b1, b2) with
    | _, MinusInfinity | PlusInfinity, _ ->
        true
    | MinusInfinity, (Int _ | PlusInfinity) | Int _, PlusInfinity ->
        false
    | Int i1, Int i2 ->
        IntLit.geq i1 i2


  let gt b1 b2 =
    match (b1, b2) with
    | _, MinusInfinity | PlusInfinity, _ ->
        true
    | MinusInfinity, (Int _ | PlusInfinity) | Int _, PlusInfinity ->
        false
    | Int i1, Int i2 ->
        IntLit.gt i1 i2


  let min b1 b2 = if le b1 b2 then b1 else b2

  let max b1 b2 = if le b1 b2 then b2 else b1

  let add_int b i =
    match b with MinusInfinity | PlusInfinity -> b | Int i' -> Int (IntLit.add i' i)


  let is_interval b1 b2 =
    match (b1, b2) with
    | MinusInfinity, MinusInfinity | PlusInfinity, PlusInfinity ->
        false
    | _ ->
        le b1 b2
end

module Unsafe : sig
  type t = private
    | Between of Bound.t * Bound.t  (** we write [b1,b2] for these *)
    | Outside of IntLit.t * IntLit.t  (** we write i1][i2 for these *)
  [@@deriving compare]

  val between : Bound.t -> Bound.t -> t

  val outside : IntLit.t -> IntLit.t -> t

  val equal_to : IntLit.t -> t

  val not_equal_to : IntLit.t -> t
end = struct
  type t = Between of Bound.t * Bound.t | Outside of IntLit.t * IntLit.t [@@deriving compare]

  let between b1 b2 =
    assert (Bound.is_interval b1 b2) ;
    Between (b1, b2)


  let outside i1 i2 =
    assert (IntLit.leq i1 i2) ;
    Outside (i1, i2)


  let equal_to i =
    let b = Bound.Int i in
    Between (b, b)


  let not_equal_to i = Outside (i, i)
end

include Unsafe

let pp fmt = function
  | Between (MinusInfinity, PlusInfinity) ->
      F.fprintf fmt "∈ℕ"
  | Between (lower, PlusInfinity) ->
      F.fprintf fmt "≥%a" Bound.pp lower
  | Between (MinusInfinity, upper) ->
      F.fprintf fmt "≤%a" Bound.pp upper
  | Between (lower, upper) when Bound.equal lower upper ->
      F.fprintf fmt "=%a" Bound.pp lower
  | Between (lower, upper) ->
      F.fprintf fmt "∈[%a,%a]" Bound.pp lower Bound.pp upper
  | Outside (l, u) when IntLit.eq l u ->
      F.fprintf fmt "≠%a" IntLit.pp l
  | Outside (l, u) ->
      F.fprintf fmt "∉[%a,%a]" IntLit.pp l IntLit.pp u


let has_empty_intersection a1 a2 =
  match (a1, a2) with
  | Outside _, Outside _ ->
      false
  | Between (lower1, upper1), Between (lower2, upper2) ->
      Bound.lt upper1 lower2 || Bound.lt upper2 lower1
  | Between (lower1, upper1), Outside (l2, u2) | Outside (l2, u2), Between (lower1, upper1) ->
      (* is [l1, u1] inside [l2, u2]? *)
      Bound.le (Int l2) lower1 && Bound.ge (Int u2) upper1


let add_int a i =
  match a with
  | Between (lower, upper) ->
      between (Bound.add_int lower i) (Bound.add_int upper i)
  | Outside (l, u) ->
      outside (IntLit.add i l) (IntLit.add i u)


let to_singleton = function Between (Int l1, Int l2) when IntLit.eq l1 l2 -> Some l1 | _ -> None

(** [remove_element e a] compute [a - {e}] if representable and if it is not [a] *)
let remove_element e = function
  | Between (lower, upper) when Bound.equal lower upper ->
      (* empty sets are not allowed to be represented *) None
  | Between (Int l, upper) when IntLit.eq l e ->
      Some (between (Int (IntLit.(add one) l)) upper)
  | Between (lower, Int u) when IntLit.eq u e ->
      Some (between lower (Int (IntLit.(add minus_one) u)))
  | Between (MinusInfinity, PlusInfinity) ->
      Some (not_equal_to e)
  | Between _ ->
      None
  | Outside (l, u) ->
      let l_minus_one = IntLit.(add minus_one) l in
      if IntLit.eq e l_minus_one then Some (outside l_minus_one u)
      else
        (* can't have [l-1 = u+1] because [l≤u] *)
        let u_plus_one = IntLit.(add one) u in
        if IntLit.eq e u_plus_one then Some (outside l u_plus_one) else None


type abduction_result = Unsatisfiable | Satisfiable of t option * t option

let flip_abduced = function
  | Unsatisfiable ->
      Unsatisfiable
  | Satisfiable (lhs, rhs) ->
      Satisfiable (rhs, lhs)


let rec abduce_eq (a1 : t) (a2 : t) =
  match (a1, a2) with
  | Between (lower1, upper1), Between (lower2, upper2) ->
      (* ∃x. l1≤x≤u1 ∧ l2≤x≤u2 *)
      (* ⇔ ∃x. max(l1,l2)≤x≤min(u1,u2) *)
      let lower = Bound.max lower1 lower2 in
      let upper = Bound.min upper1 upper2 in
      if Bound.lt upper lower then Unsatisfiable
      else
        let tighter = Some (between lower upper) in
        Satisfiable (tighter, tighter)
  | Outside (l1, u1), Outside (l2, u2) ->
      (* ∃x. (x<l1 ∨ x>u1) ∧ (x<l2 ∨ x>u2) ∧ li<=ui*)
      (* all the possible cases:
         x: --------[   ]---------
         y: -----[       ]--------

         x: ---[           ]------
         y: -----[       ]--------

         x: ---[       ]----------
         y: -----[       ]--------

         x: ---------[       ]----
         y: -----[       ]--------

         -> SAT, can tighten both to min(l1,l2)][max(u1,u2)

         x: ---------------[   ]--
         y: -----[       ]--------
         or symmetrically x<->y => cannot express the 3 intervals that would be needed so return SAT
         (TODO: we might want to keep only one of these, which would be a kind of recency model of
         disequalities: remember the last known disequality)
      *)
      if IntLit.leq l1 u2 && IntLit.leq l2 u1 then
        let l = IntLit.min l1 l2 in
        let u = IntLit.max u1 u2 in
        let tighter = Some (outside l u) in
        Satisfiable (tighter, tighter)
      else Satisfiable (None, None)
  | Outside _, Between _ ->
      abduce_eq a2 a1 |> flip_abduced
  | Between (lower1, upper1), Outside (l2, u2) ->
      (* ∃x. l1≤x≤u1 ∧ (x<l2 ∨ x>u2) *)
      (* all the possible cases:

         x:  [-------]
         y: --[   ]---

         case 1 above: SAT, cannot say more unless a1 is [-∞,+∞] (then we can abduce that a1 is
         the same as a2)

         x:        [--]
         y: ------[   ]--

         case 2 above: UNSAT

         x:   [---]
         y: ------[   ]--

         case 3 above: SAT: x = x\cap y for both

         x:      [----]
         y: ------[   ]--

         case 4 above: SAT: x\cap y for both
      *)
      if Bound.lt lower1 (Int l2) && Bound.gt upper1 (Int u2) then
        (* case 1 *)
        match a1 with
        | Between (MinusInfinity, PlusInfinity) ->
            Satisfiable (Some a2, None)
        | _ ->
            Satisfiable (None, None)
      else if Bound.ge lower1 (Int l2) && Bound.le upper1 (Int u2) then (* case 2 *)
        Unsatisfiable
      else if
        (* l1≥l2 or u1≤u2 but not both, i.e. x is on only one side of y and their intersection is
           one interval *)
        Bound.lt lower1 (Int l2)
      then
        (* case 3 & 4: x left of y *)
        let lower = lower1 in
        let upper = Bound.min upper1 (Int (IntLit.(add minus_one) l2)) in
        let tighter = Some (between lower upper) in
        Satisfiable (tighter, tighter)
      else
        (* l1≥l2 ∧ u1>u2*)
        (* case 3 & 4: x right of y *)
        let lower = Bound.max lower1 (Int (IntLit.(add one) u2)) in
        let upper = upper1 in
        let tighter = Some (between lower upper) in
        Satisfiable (tighter, tighter)


let abduce_ne (a1 : t) (a2 : t) =
  if has_empty_intersection a1 a2 then Satisfiable (None, None)
  else
    match (to_singleton a1, to_singleton a2) with
    | Some _, Some _ ->
        (* non-empty intersection between 2 singletons => they are the same singleton and hence the
           same integer and have the same value, so cannot be disequal *)
        Unsatisfiable
    | None, None ->
        (* non-empty intersection and each predicate can be satisfied by ≥2 elements => we cannot
           know if they are disequal or not *)
        Satisfiable (None, None)
    | Some e1, None -> (
      match remove_element e1 a2 with
      | Some _ as abduced2 ->
          Satisfiable (None, abduced2)
      | None ->
          Satisfiable (None, None) )
    | None, Some e2 -> (
      match remove_element e2 a1 with
      | Some _ as abduced1 ->
          Satisfiable (abduced1, None)
      | None ->
          Satisfiable (None, None) )


let abduce_le (a1 : t) (a2 : t) =
  match (a1, a2) with
  | Between (lower1, upper1), Between (lower2, upper2) ->
      (* ∃x≤y. l1≤x≤u1 ∧ l2≤y≤u2 *)
      (* ⇔ ∃x,y. x≤y ∧ l1≤x≤min(u1,u2) ∧ max(l1,l2)≤y≤u2 *)
      let min_u1u2 = Bound.min upper1 upper2 in
      let max_l1l2 = Bound.max lower1 lower2 in
      if Bound.lt min_u1u2 lower1 || Bound.lt upper2 max_l1l2 then
        (* any of these contradict the formula above *)
        Unsatisfiable
      else Satisfiable (Some (between lower1 min_u1u2), Some (between max_l1l2 upper2))
  | Outside _, Outside _ ->
      Satisfiable (None, None)
  | Between (lower1, _upper1), Outside (l2, u2) ->
      (* ∃x≤y. l1≤x≤u1 ∧ y<l2 ∧ y>u2 *)
      (* two cases:
         1. l1<l2: we don't know if x≤y for sure and cannot express a good fact to abduce to make
         it true
         x:   [----doesn't matter where u1 is
         y: -----[       ]---------------------
         2. l1≥l2: we can abduce that y≥max(u2+1, l1) and that makes it SAT
         x:      [----doesn't matter either
         y: -----[       ]---------------------
      *)
      if Bound.lt lower1 (Int l2) then (* case 1: l1<l2 *) Satisfiable (None, None)
      else
        (* case 2: l1≥l2 *)
        let lower = Bound.max (Int (IntLit.(add one) u2)) lower1 in
        Satisfiable (None, Some (between lower PlusInfinity))
  | Outside (l1, u1), Between (_lower2, upper2) ->
      (* similarly, two cases:
         1. u1≥u2: can refine to x≤min(l1+1, u2)
         x: -----[       ]---------------------
         y: ..-]
         or y:      ...-----]
         2. u1<u2: cannot deduce anything
         x: -----[       ]---------------------
         y:          ...---]
      *)
      if Bound.ge (Int u1) upper2 then
        (* case 1: l1>l2 *)
        let upper = Bound.min (Int (IntLit.(add one) l1)) upper2 in
        Satisfiable (Some (between MinusInfinity upper), None)
      else (* case 2: l1≤l2 *)
        Satisfiable (None, None)


let abduce_lt (a1 : t) (a2 : t) =
  match abduce_le (add_int a1 IntLit.one) a2 with
  | Satisfiable (Some abduced1, abduced2_opt) ->
      Satisfiable (Some (add_int abduced1 IntLit.minus_one), abduced2_opt)
  | result ->
      result


let abduce_binop_constraints ~negated (bop : Binop.t) (a1 : t) (a2 : t) =
  let open Binop in
  match (bop, negated) with
  | Eq, false | Ne, true ->
      abduce_eq a1 a2
  | Eq, true | Ne, false ->
      abduce_ne a1 a2
  | Le, false | Gt, true ->
      abduce_le a1 a2
  | Ge, false | Lt, true ->
      abduce_le a2 a1 |> flip_abduced
  | Lt, false | Ge, true ->
      abduce_lt a1 a2
  | Gt, false | Le, true ->
      abduce_lt a2 a1 |> flip_abduced
  | _ ->
      Satisfiable (None, None)


let abduce_binop_is_true ~negated bop v1 v2 =
  Logging.d_printfln "abduce_binop_is_true ~negated:%b %s (%a) (%a)" negated (Binop.str Pp.text bop)
    (Pp.option pp) v1 (Pp.option pp) v2 ;
  match (v1, v2) with
  | None, None ->
      (* two existential variables: no way to express in the non-relational domain *)
      Satisfiable (None, None)
  | _ ->
      let unknown = between MinusInfinity PlusInfinity in
      let a1 = Option.value v1 ~default:unknown in
      let a2 = Option.value v2 ~default:unknown in
      abduce_binop_constraints ~negated bop a1 a2
