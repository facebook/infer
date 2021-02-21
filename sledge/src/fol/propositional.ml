(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Propositional formulas *)

include Propositional_intf

module Make (Trm : sig
  type t [@@deriving compare, equal, sexp]
end) =
struct
  module Fml1 = struct
    type compare [@@deriving compare, equal, sexp]

    type t =
      | Tt
      | Eq of Trm.t * Trm.t
      | Eq0 of Trm.t
      | Pos of Trm.t
      | Not of t
      | And of {pos: set; neg: set}
      | Or of {pos: set; neg: set}
      | Iff of t * t
      | Cond of {cnd: t; pos: t; neg: t}
      | Lit of Predsym.t * Trm.t array

    and set = (t, compare) Set.t [@@deriving compare, equal, sexp]
  end

  module Fml2 = struct
    include Comparer.Counterfeit (Fml1)
    include Fml1
  end

  (** Sets of formulas *)
  module Fmls = struct
    include Set.Make_from_Comparer (Fml2)
    include Provide_of_sexp (Fml2)
  end

  (** Formulas, built from literals with predicate symbols from various
      theories, and propositional constants and connectives. Denote sets of
      structures. *)
  module Fml = struct
    include Fml2

    let invariant f =
      let@ () = Invariant.invariant [%here] f [%sexp_of: t] in
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

    let sort x y = if compare x y <= 0 then (x, y) else (y, x)

    (** Some normalization is necessary for [embed_into_fml] (defined below)
        to be left inverse to [embed_into_cnd]. Essentially
        [0 ≠ (p ? 1 : 0)] needs to normalize to [p], by way of
        [0 ≠ (p ? 1 : 0)] ==> [(p ? 0 ≠ 1 : 0 ≠ 0)] ==>
        [(p ? tt : ff)] ==> [p]. *)

    let tt = Tt |> check invariant
    let ff = Not Tt |> check invariant
    let mk_Tt () = tt
    let bool b = if b then tt else ff

    let rec _Not p =
      ( match p with
      | Not x -> x
      | And {pos; neg} -> Or {pos= neg; neg= pos}
      | Or {pos; neg} -> And {pos= neg; neg= pos}
      | Cond {cnd; pos; neg} -> Cond {cnd; pos= _Not pos; neg= _Not neg}
      | Tt | Eq _ | Eq0 _ | Pos _ | Lit _ | Iff _ -> Not p )
      |> check invariant

    let _Join cons unit zero ~pos ~neg =
      let pos = Fmls.remove unit pos in
      let neg = Fmls.remove zero neg in
      if
        Fmls.mem zero pos
        || Fmls.mem unit neg
        || not (Fmls.disjoint pos neg)
      then zero
      else
        match Fmls.classify neg with
        | Zero -> (
          match Fmls.classify pos with
          | Zero -> unit
          | One p -> p
          | Many -> cons ~pos ~neg )
        | One n when Fmls.is_empty pos -> _Not n
        | _ -> cons ~pos ~neg

    let _And ~pos ~neg =
      let pos, neg =
        Fmls.fold pos (pos, neg) ~f:(fun b (pos, neg) ->
            match b with
            | And {pos= p; neg= n} ->
                (Fmls.union p (Fmls.remove b pos), Fmls.union n neg)
            | _ -> (pos, neg) )
      in
      let pos, neg =
        Fmls.fold neg (pos, neg) ~f:(fun b (pos, neg) ->
            match b with
            | Or {pos= p; neg= n} ->
                (Fmls.union n pos, Fmls.union p (Fmls.remove b neg))
            | _ -> (pos, neg) )
      in
      _Join (fun ~pos ~neg -> And {pos; neg}) tt ff ~pos ~neg

    let _Or ~pos ~neg =
      let pos, neg =
        Fmls.fold pos (pos, neg) ~f:(fun b (pos, neg) ->
            match b with
            | Or {pos= p; neg= n} ->
                (Fmls.union p (Fmls.remove b pos), Fmls.union n neg)
            | _ -> (pos, neg) )
      in
      let pos, neg =
        Fmls.fold neg (pos, neg) ~f:(fun b (pos, neg) ->
            match b with
            | And {pos= p; neg= n} ->
                (Fmls.union n pos, Fmls.union p (Fmls.remove b neg))
            | _ -> (pos, neg) )
      in
      _Join (fun ~pos ~neg -> Or {pos; neg}) ff tt ~pos ~neg

    let join _Cons zero split_pos_neg p q =
      ( if equal p zero || equal q zero then zero
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
      join
        (_Join (fun ~pos ~neg -> And {pos; neg}) tt ff)
        ff
        (function
          | And {pos; neg} -> (pos, neg)
          | Tt -> (Fmls.empty, Fmls.empty)
          | Not p -> (Fmls.empty, Fmls.of_ p)
          | p -> (Fmls.of_ p, Fmls.empty) )
        p q

    let or_ p q =
      join
        (_Join (fun ~pos ~neg -> Or {pos; neg}) ff tt)
        tt
        (function
          | Or {pos; neg} -> (pos, neg)
          | Not Tt -> (Fmls.empty, Fmls.empty)
          | Not p -> (Fmls.empty, Fmls.of_ p)
          | p -> (Fmls.of_ p, Fmls.empty) )
        p q

    let rec eval_iff p q =
      match (p, q) with
      | p, Not p' | Not p', p -> if equal p p' then Some false else None
      | And {pos= ap; neg= an}, Or {pos= op; neg= on}
       |Or {pos= op; neg= on}, And {pos= ap; neg= an}
        when Fmls.equal ap on && Fmls.equal an op ->
          Some false
      | Cond {cnd= c; pos= p; neg= n}, Cond {cnd= c'; pos= p'; neg= n'} ->
          if equal c c' then
            match eval_iff p p' with
            | Some false -> (
              match eval_iff n n' with
              | Some false -> Some false
              | _ -> None )
            | Some true -> if equal n n' then Some true else None
            | None -> None
          else None
      | _ -> if equal p q then Some true else None

    let _Iff p q =
      ( match (p, q) with
      | Tt, p | p, Tt -> p
      | Not Tt, p | p, Not Tt -> _Not p
      | _ -> (
        match eval_iff p q with
        | Some b -> bool b
        | None ->
            let p, q = sort p q in
            Iff (p, q) ) )
      |> check invariant

    let is_negative = function Not _ | Or _ -> true | _ -> false

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
        match eval_iff pos neg with
        (* (c ? p : p) ==> c *)
        | Some true -> cnd
        (* (c ? p : ¬p) ==> c <=> p *)
        | Some false -> _Iff cnd pos
        (* (¬c ? n : p) ==> (c ? p : n) *)
        | None when is_negative cnd ->
            Cond {cnd= _Not cnd; pos= neg; neg= pos}
        (* (c ? p : n) *)
        | _ -> Cond {cnd; pos; neg} ) )
      |> check invariant

    let _Eq x y = Eq (x, y) |> check invariant
    let _Eq0 x = Eq0 x |> check invariant
    let _Pos x = Pos x |> check invariant
    let _Lit p xs = Lit (p, xs) |> check invariant

    let iter_pos_neg ~pos ~neg ~f =
      let f_not p = f (_Not p) in
      Fmls.iter ~f pos ;
      Fmls.iter ~f:f_not neg

    let rec iter_trms p ~f =
      match p with
      | Tt -> ()
      | Eq (x, y) ->
          f x ;
          f y
      | Eq0 x | Pos x -> f x
      | Not x -> iter_trms ~f x
      | And {pos; neg} | Or {pos; neg} ->
          iter_pos_neg ~f:(iter_trms ~f) ~pos ~neg
      | Iff (x, y) ->
          iter_trms ~f x ;
          iter_trms ~f y
      | Cond {cnd; pos; neg} ->
          iter_trms ~f cnd ;
          iter_trms ~f pos ;
          iter_trms ~f neg
      | Lit (_, xs) -> Array.iter ~f xs

    let trms p = Iter.from_labelled_iter (iter_trms p)

    type polarity = Con | Dis

    let map_join polarity b ~pos ~neg f =
      let pos_to_flatten = ref [] in
      let neg_to_flatten = ref [] in
      let pos0, neg0 = (pos, neg) in
      let pos =
        Fmls.map pos ~f:(fun p ->
            let p' = f p in
            if p' == p then p
            else
              match (polarity, p') with
              | Con, And {pos; neg} | Dis, Or {pos; neg} ->
                  pos_to_flatten := (p, pos, neg) :: !pos_to_flatten ;
                  p
              | _ -> p' )
      in
      let neg =
        Fmls.map neg ~f:(fun n ->
            let n' = f n in
            if n' == n then n
            else
              match (polarity, n') with
              | Con, Or {pos; neg} | Dis, And {pos; neg} ->
                  neg_to_flatten := (n, pos, neg) :: !neg_to_flatten ;
                  n
              | _ -> n' )
      in
      let pos, neg =
        if List.is_empty !pos_to_flatten then (pos, neg)
        else
          List.fold !pos_to_flatten (pos, neg)
            ~f:(fun (p, p', n') (pos, neg) ->
              (Fmls.union p' (Fmls.remove p pos), Fmls.union n' neg) )
      in
      let pos, neg =
        if List.is_empty !neg_to_flatten then (pos, neg)
        else
          List.fold !neg_to_flatten (pos, neg)
            ~f:(fun (n, p', n') (pos, neg) ->
              (Fmls.union n' pos, Fmls.union p' (Fmls.remove n neg)) )
      in
      if pos0 == pos && neg0 == neg then b
      else
        match polarity with
        | Con -> _Join (fun ~pos ~neg -> And {pos; neg}) tt ff ~pos ~neg
        | Dis -> _Join (fun ~pos ~neg -> Or {pos; neg}) ff tt ~pos ~neg

    let map_and = map_join Con
    let map_or = map_join Dis
  end
end
[@@inline]
