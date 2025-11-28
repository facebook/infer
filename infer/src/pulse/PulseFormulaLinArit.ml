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
open SatUnsat.Import

(* define our own var map to get a custom order: we want to place unrestricted variables first in
     the map so that [solve_for_unrestricted] can be implemented in terms of [solve_eq] easily *)
module VarMap = struct
  include PrettyPrintable.MakePPMap (struct
    type t = Var.t

    let pp = Var.pp

    let compare v1 v2 = Var.compare_unrestricted_first v1 v2
  end)

  (* unpleasant that we have to duplicate this definition from [Var.Map] here *)
  let yojson_of_t yojson_of_val m =
    `List (List.map ~f:(fun (k, v) -> `List [Var.yojson_of_t k; yojson_of_val v]) (bindings m))
end

(** invariant: the representation is always "canonical": coefficients cannot be [Q.zero] *)
type t = Q.t * Q.t VarMap.t [@@deriving compare, equal]

let yojson_of_t (c, vs) = `List [VarMap.yojson_of_t Q.yojson_of_t vs; Q.yojson_of_t c]

let fold (_, vs) ~init ~f = IContainer.fold_of_pervasives_map_fold VarMap.fold vs ~init ~f

type 'term_t subst_target =
  | QSubst of Q.t
  | ConstantSubst of 'term_t * Var.t option
  | VarSubst of Var.t
  | LinSubst of t
  | NonLinearTermSubst of 'term_t

let pp pp_var fmt (c, vs) =
  if VarMap.is_empty vs then Q.pp_print fmt c
  else
    let pp_c fmt c =
      if not (Q.is_zero c) then
        let plusminus, c_pos = if Q.geq c Q.zero then ('+', c) else ('-', Q.neg c) in
        F.fprintf fmt " %c%a" plusminus Q.pp_print c_pos
    in
    let is_first = ref true in
    let pp_coeff fmt q =
      if Q.(q < zero) then F.pp_print_char fmt '-'
      else if Q.(q >= zero) && not !is_first then F.pp_print_char fmt '+' ;
      let abs_q = Q.abs q in
      if not (Q.is_one abs_q) then F.fprintf fmt "%a·" Q.pp_print abs_q
    in
    let pp_vs fmt vs =
      Pp.collection ~sep:"@;"
        ~fold:(IContainer.fold_of_pervasives_map_fold VarMap.fold)
        (fun fmt (v, q) ->
          F.fprintf fmt "%a%a" pp_coeff q pp_var v ;
          is_first := false )
        fmt vs
    in
    F.fprintf fmt "@[<h>%a%a@]" pp_vs vs pp_c c


let add (c1, vs1) (c2, vs2) =
  ( Q.add c1 c2
  , VarMap.union
      (fun _v c1 c2 ->
        let c = Q.add c1 c2 in
        if Q.is_zero c then None else Some c )
      vs1 vs2 )


let minus (c, vs) = (Q.neg c, VarMap.map (fun c -> Q.neg c) vs)

let subtract l1 l2 = add l1 (minus l2)

let zero = (Q.zero, VarMap.empty)

let is_zero (c, vs) = Q.is_zero c && VarMap.is_empty vs

let mult q ((c, vs) as l) =
  if Q.is_zero q then (* needed for correctness: coeffs cannot be zero *) zero
  else if Q.is_one q then (* purely an optimisation *) l
  else (Q.mul q c, VarMap.map (fun c -> Q.mul q c) vs)


let pivot (x, coeff) (c, vs) =
  let d = Q.neg coeff in
  let vs' =
    VarMap.fold
      (fun v' coeff' vs' -> if Var.equal v' x then vs' else VarMap.add v' (Q.div coeff' d) vs')
      vs VarMap.empty
  in
  (* note: [d≠0] by the invariant of the coefficient map [vs] *)
  let c' = Q.div c d in
  (c', vs')


let solve_eq_zero ((c, vs) as l) =
  match VarMap.min_binding_opt vs with
  | None ->
      if Q.is_zero c then Sat None
      else
        let reason () = F.asprintf "Unsat when solving %a = 0" (pp Var.pp) l in
        Unsat {reason; source= __POS__}
  | Some ((x, _) as x_coeff) ->
      Sat (Some (x, pivot x_coeff l))


let solve_eq l1 l2 = solve_eq_zero (subtract l1 l2)

let of_var v = (Q.zero, VarMap.singleton v Q.one)

let of_q q = (q, VarMap.empty)

let get_as_const (c, vs) = if VarMap.is_empty vs then Some c else None

let get_as_var (c, vs) =
  if Q.is_zero c then
    match VarMap.is_singleton_or_more vs with
    | Singleton (x, cx) when Q.is_one cx ->
        Some x
    | _ ->
        None
  else None


let get_as_variable_difference (c, vs) =
  if Q.is_zero c then
    (* the coefficient has to be 0 *)
    let vs_seq = VarMap.to_seq vs in
    let open IOption.Let_syntax in
    (* check that the expression consists of exactly two variables with coeffs 1 and -1 *)
    let* (x, cx), vs_seq = Seq.uncons vs_seq in
    let* (y, cy), vs_seq = Seq.uncons vs_seq in
    Option.some_if
      ( Seq.is_empty vs_seq
      && ((Q.is_one cx && Q.is_minus_one cy) || (Q.is_one cy && Q.is_minus_one cx)) )
      (x, y)
  else None


let get_constant_part (c, _) = c

let get_coefficient v (_, vs) = VarMap.find_opt v vs

let of_subst_target v0 = function
  | QSubst q ->
      of_q q
  | VarSubst v | ConstantSubst (_, Some v) ->
      of_var v
  | LinSubst l ->
      l
  | NonLinearTermSubst _ | ConstantSubst (_, None) ->
      of_var v0


let fold_subst_variables ((c, vs_foreign) as l0) ~init ~f =
  let changed = ref false in
  let acc_f, l' =
    VarMap.fold
      (fun v_foreign q0 (acc_f, l) ->
        let acc_f, op = f acc_f v_foreign in
        ( match op with
        | NonLinearTermSubst _ ->
            ()
        | VarSubst v when Var.equal v v_foreign ->
            ()
        | _ ->
            changed := true ) ;
        (acc_f, add (mult q0 (of_subst_target v_foreign op)) l) )
      vs_foreign
      (init, (c, VarMap.empty))
  in
  let l' = if !changed then l' else l0 in
  (acc_f, l')


let subst_variables l ~f = fold_subst_variables l ~init:() ~f:(fun () v -> ((), f v)) |> snd

(* OPTIM: for a single variable we can avoid iterating over the coefficient map *)
let subst_variable x subst_target ((c, vs) as l0) =
  match VarMap.find_opt x vs with
  | None ->
      l0
  | Some q ->
      let vs' = VarMap.remove x vs in
      add (mult q (of_subst_target x subst_target)) (c, vs')


let get_variables (_, vs) = VarMap.to_seq vs |> Seq.map fst

let get_simplest l = VarMap.min_binding_opt (snd l) |> Option.map ~f:fst

(** {2 Tableau-Specific Operations} *)

let is_restricted l =
  (* HACK: unrestricted variables come first so we first test if there exists any unrestricted
       variable in the map by checking its min element *)
  not (get_simplest l |> Option.exists ~f:Var.is_unrestricted)


let solve_for_unrestricted w l =
  if not (is_restricted l) then (
    match solve_eq l (of_var w) with
    | Unsat unsat_info ->
        SatUnsat.log_unsat unsat_info ;
        None
    | Sat None ->
        None
    | Sat (Some (x, _) as r) ->
        assert (Var.is_unrestricted x) ;
        r )
  else None


let classify_minimized_maximized (_, vs) =
  let all_pos, all_neg =
    VarMap.fold
      (fun _ coeff (all_pos, all_neg) ->
        (Q.(coeff >= zero) && all_pos, Q.(coeff <= zero) && all_neg) )
      vs (true, true)
  in
  match (all_pos, all_neg) with
  | true, true ->
      `Constant
  | true, false ->
      `Minimized
  | false, true ->
      `Maximized
  | false, false ->
      `Neither


let is_minimized l =
  match classify_minimized_maximized l with
  | `Minimized | `Constant ->
      true
  | `Maximized | `Neither ->
      false
