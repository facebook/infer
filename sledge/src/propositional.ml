(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Propositional formulas *)

include Propositional_intf
open Ses

module Make (Trm : TERM) = struct
  open Trm

  (** Sets of formulas *)
  module rec Fmls : (FORMULA_SET with type elt := Fml.fml) = struct
    module T = struct
      type t = Fml.fml [@@deriving compare, equal, sexp]
    end

    include Set.Make (T)
    include Provide_of_sexp (T)
  end

  (** Formulas, built from literals with predicate symbols from various
      theories, and propositional constants and connectives. Denote sets of
      structures. *)
  and Fml : (FORMULA with type trm := Trm.trm with type fmls := Fmls.t) =
  struct
    type fml =
      | Tt
      | Eq of trm * trm
      | Eq0 of trm
      | Pos of trm
      | Not of fml
      | And of {pos: Fmls.t; neg: Fmls.t}
      | Or of {pos: Fmls.t; neg: Fmls.t}
      | Iff of fml * fml
      | Cond of {cnd: fml; pos: fml; neg: fml}
      | Lit of Predsym.t * trm array
    [@@deriving compare, equal, sexp]

    let invariant f =
      let@ () = Invariant.invariant [%here] f [%sexp_of: fml] in
      match f with
      (* formulas are in negation-normal form *)
      | Not (Not _ | And _ | Or _ | Cond _) -> assert false
      (* conjunction and disjunction formulas are: *)
      | And {pos; neg} | Or {pos; neg} ->
          (* not "zero" (the negation of their unit) *)
          assert (Fmls.disjoint pos neg) ;
          (* not singleton *)
          assert (Fmls.cardinal pos + Fmls.cardinal neg > 1)
      (* conditional formulas are in "positive condition" form *)
      | Cond {cnd= Not _ | Or _} -> assert false
      | _ -> ()

    let sort_fml x y = if compare_fml x y <= 0 then (x, y) else (y, x)

    (** Some normalization is necessary for [embed_into_fml] (defined below)
        to be left inverse to [embed_into_cnd]. Essentially
        [0 ≠ (p ? 1 : 0)] needs to normalize to [p], by way of
        [0 ≠ (p ? 1 : 0)] ==> [(p ? 0 ≠ 1 : 0 ≠ 0)] ==>
        [(p ? tt : ff)] ==> [p]. *)

    let tt = Tt |> check invariant
    let ff = Not Tt |> check invariant
    let mk_Tt () = tt
    let bool b = if b then tt else ff

    let _Eq0 x =
      (match eval_eq0 x with Some b -> bool b | None -> Eq0 x)
      |> check invariant

    let _Eq x y =
      ( if x == zero then _Eq0 y
      else if y == zero then _Eq0 x
      else
        match eval_eq x y with
        | Some b -> bool b
        | None -> (
          match Sign.of_int (compare_trm x y) with
          | Neg -> Eq (x, y)
          | Zero -> tt
          | Pos -> Eq (y, x) ) )
      |> check invariant

    let _Pos x =
      (match eval_pos x with Some b -> bool b | None -> Pos x)
      |> check invariant

    let _Lit p xs = Lit (p, xs) |> check invariant

    let rec _Not p =
      ( match p with
      | Not x -> x
      | And {pos; neg} -> Or {pos= neg; neg= pos}
      | Or {pos; neg} -> And {pos= neg; neg= pos}
      | Cond {cnd; pos; neg} -> Cond {cnd; pos= _Not pos; neg= _Not neg}
      | Tt | Eq _ | Eq0 _ | Pos _ | Lit _ | Iff _ -> Not p )
      |> check invariant

    let _Join cons zero ~pos ~neg =
      if not (Fmls.disjoint pos neg) then zero
      else if Fmls.is_empty neg then
        match Fmls.only_elt pos with Some p -> p | _ -> cons ~pos ~neg
      else if Fmls.is_empty pos then
        match Fmls.only_elt neg with
        | Some n -> _Not n
        | _ -> cons ~pos ~neg
      else cons ~pos ~neg

    let _And ~pos ~neg =
      _Join (fun ~pos ~neg -> And {pos; neg}) ff ~pos ~neg

    let _Or ~pos ~neg = _Join (fun ~pos ~neg -> Or {pos; neg}) tt ~pos ~neg

    let join _Cons zero split_pos_neg p q =
      ( if equal_fml p zero || equal_fml q zero then zero
      else
        let pp, pn = split_pos_neg p in
        if Fmls.is_empty pp && Fmls.is_empty pn then q
        else
          let qp, qn = split_pos_neg q in
          if Fmls.is_empty qp && Fmls.is_empty qn then p
          else
            let pos = Fmls.union pp qp in
            let neg = Fmls.union pn qn in
            _Cons ~pos ~neg )
      |> check invariant

    let and_ p q =
      join _And ff
        (function
          | And {pos; neg} -> (pos, neg)
          | Not p -> (Fmls.empty, Fmls.of_ p)
          | p -> (Fmls.of_ p, Fmls.empty) )
        p q

    let or_ p q =
      join _Or tt
        (function
          | Or {pos; neg} -> (pos, neg)
          | Not p -> (Fmls.empty, Fmls.of_ p)
          | p -> (Fmls.of_ p, Fmls.empty) )
        p q

    type equal_or_opposite = Equal | Opposite | Unknown

    let rec equal_or_opposite p q =
      match (p, q) with
      | p, Not p' | Not p', p ->
          if equal_fml p p' then Opposite else Unknown
      | And {pos= ap; neg= an}, Or {pos= op; neg= on}
       |Or {pos= op; neg= on}, And {pos= ap; neg= an}
        when Fmls.equal ap on && Fmls.equal an op ->
          Opposite
      | Cond {cnd= c; pos= p; neg= n}, Cond {cnd= c'; pos= p'; neg= n'} ->
          if equal_fml c c' then
            match equal_or_opposite p p' with
            | Opposite -> (
              match equal_or_opposite n n' with
              | Opposite -> Opposite
              | _ -> Unknown )
            | Equal -> if equal_fml n n' then Equal else Unknown
            | Unknown -> Unknown
          else Unknown
      | _ -> if equal_fml p q then Equal else Unknown

    let is_negative = function Not _ | Or _ -> true | _ -> false

    let _Iff p q =
      ( match (p, q) with
      | Tt, p | p, Tt -> p
      | Not Tt, p | p, Not Tt -> _Not p
      | _ -> (
        match equal_or_opposite p q with
        | Equal -> tt
        | Opposite -> ff
        | Unknown ->
            let p, q = sort_fml p q in
            Iff (p, q) ) )
      |> check invariant

    let _Cond cnd pos neg =
      ( match (cnd, pos, neg) with
      (* (tt ? p : n) ==> p *)
      | Tt, _, _ -> pos
      (* (ff ? p : n) ==> n *)
      | Not Tt, _, _ -> neg
      (* (c ? tt : ff) ==> c *)
      | _, Tt, Not Tt -> cnd
      (* (c ? ff : tt) ==> ¬c *)
      | _, Not Tt, Tt -> _Not cnd
      (* (c ? p : ff) ==> c ∧ p *)
      | _, _, Not Tt -> and_ cnd pos
      (* (c ? ff : n) ==> ¬c ∧ n *)
      | _, Not Tt, _ -> and_ (_Not cnd) neg
      (* (c ? tt : n) ==> c ∨ n *)
      | _, Tt, _ -> or_ cnd neg
      (* (c ? p : tt) ==> ¬c ∨ p *)
      | _, _, Tt -> or_ (_Not cnd) pos
      | _ -> (
        match equal_or_opposite pos neg with
        (* (c ? p : p) ==> c *)
        | Equal -> cnd
        (* (c ? p : ¬p) ==> c <=> p *)
        | Opposite -> _Iff cnd pos
        (* (¬c ? n : p) ==> (c ? p : n) *)
        | Unknown when is_negative cnd ->
            Cond {cnd= _Not cnd; pos= neg; neg= pos}
        (* (c ? p : n) *)
        | _ -> Cond {cnd; pos; neg} ) )
      |> check invariant
  end
end
