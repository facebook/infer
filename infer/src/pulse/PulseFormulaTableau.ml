(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CItv = PulseCItv
module SatUnsat = PulseSatUnsat
module Debug = PulseFormulaDebug
module Var = PulseFormulaVar
module Q = QSafeCapped

(** An implementation of \[2\] "Solving Linear Arithmetic Constraints" by Harald Rueß and Natarajan
    Shankar, SRI International, CSL Technical Report CSL-SRI-04-01, 15 January 2004. It uses a
    Simplex-like technique to reason about inequalities over linear arithmetic expressions.

    The main idea is to represent an inequality [x ≥ 0] as [x = u] with [u] belonging to a special
    class of "restricted" (or "slack") variables, which are always non-negative, and then deal with
    linear equalities on restricted variables (the tableau) instead of linear inequalities. Dark
    magic happens to massage the tableau so that contradictions are detected.

    Here restricted variables are distinguished by {!Var} directly using {!Var.is_restricted}. *)
module LinArith = PulseFormulaLinArit

let pp_var_map ?filter ~arrow pp_val pp_var fmt var_map =
  Pp.collection ~sep:"@;∧ "
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    ?filter
    (fun fmt (v, value) -> F.fprintf fmt "%a%s%a" pp_var v arrow pp_val value)
    fmt var_map


(** As for linear equalities [linear_eqs] below, the tableau is represented as a map of bindings
    [u -> l] meaning [u = l].

    Invariants:

    - all variables in the tableau are {e restricted}
    - the tableau is {e feasible}: each equality [u = c + q1·v1 + ... + qN·vN] is such that [c>0] *)
type t = LinArith.t Var.Map.t [@@deriving compare, equal]

let pp pp_var fmt tableau = pp_var_map ~arrow:" = " (LinArith.pp pp_var) pp_var fmt tableau

let yojson_of_t = Var.Map.yojson_of_t LinArith.yojson_of_t

let empty = Var.Map.empty

let do_pivot u l ((v, _) as v_coeff) t =
  let l_v = LinArith.pivot v_coeff (LinArith.subtract l (LinArith.of_var u)) in
  let t =
    Var.Map.filter_map
      (fun _ l ->
        let l' =
          LinArith.subst_variables l ~f:(fun v' ->
              if Var.equal v v' then LinSubst l_v else VarSubst v' )
        in
        Option.some_if (not (LinArith.is_minimized l')) l' )
      t
  in
  if LinArith.is_minimized l_v then t else Var.Map.add v l_v t


let pivot_unbounded_with_positive_coeff t u l =
  (* the set of variables in [t] that appear with a negative coefficient in at least one equality
       *)
  let bounded_vars_of_t =
    Var.Map.fold
      (fun _u l bounded ->
        LinArith.fold l ~init:bounded ~f:(fun bounded (v, coeff) ->
            if Q.(coeff < zero) then Var.Set.add v bounded else bounded ) )
      t Var.Set.empty
  in
  (* the smallest variable that is unbounded in [t] and appears with a positive coefficient in [l]
       *)
  LinArith.fold l ~init:None ~f:(fun candidate ((v, coeff) as v_coeff) ->
      if Option.is_none candidate && Q.(coeff > zero) && not (Var.Set.mem v bounded_vars_of_t) then
        Some v_coeff
      else candidate )
  |> Option.map ~f:(fun v_coeff -> do_pivot u l v_coeff t)


let find_pivotable_to v t =
  Debug.p "Looking for pivotable %a@\n" Var.pp v ;
  Var.Map.fold
    (fun u l candidate ->
      if Option.is_some candidate then candidate
      else
        LinArith.get_coefficient v l
        |> Option.bind ~f:(fun coeff ->
               if Q.(coeff < zero) then
                 let q_c = LinArith.get_constant_part l in
                 let gain = Q.(-(q_c / coeff)) in
                 if
                   Container.for_all ~iter:(Container.iter ~fold:LinArith.fold) l
                     ~f:(fun (_, coeff') -> Q.(coeff' >= zero || -(q_c / coeff') >= gain) )
                 then Some (u, (v, coeff))
                 else None
               else None ) )
    t None


let find_pivot l t =
  LinArith.fold l ~init:None ~f:(fun candidate (v, coeff) ->
      if Option.is_none candidate && Q.(coeff > zero) then find_pivotable_to v t else candidate )


let pivot l t =
  find_pivot l t
  |> Option.map ~f:(fun (u', v') ->
         Debug.p "found pivot: %a <- %a@\n" Var.pp u' Var.pp (fst v') ;
         let l_u' = Var.Map.find u' t in
         do_pivot u' l_u' v' (Var.Map.remove u' t) )
