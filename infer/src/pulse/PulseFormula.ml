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
module ValueHistory = PulseValueHistory

module Var = struct
  include PulseAbstractValue

  let is_simpler_than v1 v2 = PulseAbstractValue.compare v1 v2 < 0

  let is_simpler_or_equal v1 v2 = PulseAbstractValue.compare v1 v2 <= 0
end

module Q = QSafeCapped
module Z = ZSafe
open SatUnsat.Import

(** a humble debug mechanism: set [debug] to [true] and run [make -C infer/src runtest] to see the
    arithmetic engine at work in more details *)
module Debug = struct
  (** change this to [true] for more debug information *)
  let debug = false

  let dummy_formatter = F.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let p fmt =
    if debug then if Config.is_running_unit_test then F.printf fmt else L.d_printf fmt
    else F.ifprintf dummy_formatter fmt
end

type function_symbol = Unknown of Var.t | Procname of Procname.t
[@@deriving compare, equal, yojson_of]

let pp_function_symbol fmt = function
  | Unknown v ->
      Var.pp fmt v
  | Procname proc_name ->
      (* templates mess up HTML debug output and are too much info most of the time *)
      Procname.pp_without_templates fmt proc_name


type operand =
  | AbstractValueOperand of Var.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: function_symbol; actuals: Var.t list}
[@@deriving compare, equal]

let pp_operand fmt = function
  | AbstractValueOperand v ->
      Var.pp fmt v
  | ConstOperand i ->
      Const.pp Pp.text fmt i
  | FunctionApplicationOperand {f; actuals} ->
      F.fprintf fmt "%a(%a)" pp_function_symbol f (Pp.seq ~sep:"," Var.pp) actuals


(** Linear Arithmetic *)
module LinArith : sig
  (** linear combination of variables, eg [2·x + 3/4·y + 12] *)
  type t [@@deriving compare, yojson_of, equal]

  (* ['term_t] is meant to be [Term.t] but we cannot mention [Term] yet as it depends on [LinArith];
     we resolve the circular dependency further down this file *)
  type 'term_t subst_target =
    | QSubst of Q.t
    | ConstantSubst of 'term_t * Var.t option
        (** the constant term to substitute and, optionally, a fallback variable to substitute with
            (eg if a variable is equal to a constant string but also to a new canonical
            representative, the linear arithmetic domain needs to use the latter *)
    | VarSubst of Var.t
    | LinSubst of t
    | NonLinearTermSubst of 'term_t

  val pp : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

  val is_zero : t -> bool

  val add : t -> t -> t

  val minus : t -> t

  val subtract : t -> t -> t

  val mult : Q.t -> t -> t

  val solve_eq : t -> t -> (Var.t * t) option SatUnsat.t
  (** [solve_eq l1 l2] is [Sat (Some (x, l))] if [l1=l2 <=> x=l], [Sat None] if [l1 = l2] is always
      true, and [Unsat] if it is always false *)

  val of_q : Q.t -> t

  val of_var : Var.t -> t

  val get_as_const : t -> Q.t option
  (** [get_as_const l] is [Some c] if [l=c], else [None] *)

  val get_as_var : t -> Var.t option
  (** [get_as_var l] is [Some x] if [l=x], else [None] *)

  val get_as_variable_difference : t -> (Var.t * Var.t) option
  (** [get_as_variable_difference l] is [Some (x,y)] if [l=(x-y)] or [l=(y-x)], else [None] *)

  val get_constant_part : t -> Q.t
  (** [get_as_const (c + l)] is [c] *)

  val get_coefficient : Var.t -> t -> Q.t option

  val get_variables : t -> Var.t Seq.t

  val fold_subst_variables : t -> init:'a -> f:('a -> Var.t -> 'a * _ subst_target) -> 'a * t

  val fold : t -> init:'a -> f:('a -> Var.t * Q.t -> 'a) -> 'a

  val subst_variables : t -> f:(Var.t -> _ subst_target) -> t

  val subst_variable : Var.t -> _ subst_target -> t -> t
  (** same as above for a single variable to substitute (more optimized) *)

  val get_simplest : t -> Var.t option
  (** the smallest [v∊l] according to [Var.is_simpler_than] *)

  (** {2 Tableau-Specific Operations} *)

  val is_restricted : t -> bool
  (** [true] iff all the variables involved in the expression satisfy {!Var.is_restricted} *)

  val solve_for_unrestricted : Var.t -> t -> (Var.t * t) option
  (** if [l] contains at least one unrestricted variable then [solve_for_unrestricted u l] is
      [Some (x, l')] where [x] is the smallest unrestricted variable in [l] and [u=l <=> x=l']. If
      there are no unrestricted variables in [l] then [solve_for_unrestricted u l] is [None].
      Assumes [u∉l]. *)

  val pivot : Var.t * Q.t -> t -> t
  (** [pivot (v, q) l] assumes [v] appears in [l] with coefficient [q] and returns [l'] such that
      [l' = -(1/q)·(l - q·v)]*)

  val classify_minimized_maximized :
       t
    -> [ `Minimized
         (** all the coefficients are positive, hence the constant part is a lower bound of the
             value of the linear expression (assuming all variables are restricted) *)
       | `Maximized
         (** all the coefficients are negative, hence the constant part is an upper bound of the
             value of the linear expression (assuming all variables are restricted) *)
       | `Neither
       | `Constant  (** no variables in this linear expression *) ]

  val is_minimized : t -> bool
  (** [is_minimized l] iff [classify_minimized_maximized l] is either [`Minimized] or [`Constant] *)
end = struct
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
        else (
          L.d_printfln "Unsat when solving %a = 0" (pp Var.pp) l ;
          Unsat )
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
      | Unsat | Sat None ->
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
end

let pp_var_set pp_var fmt var_set =
  Pp.collection ~sep:","
    ~fold:(IContainer.fold_of_pervasives_set_fold Var.Set.fold)
    pp_var fmt var_set


let pp_var_map ?filter ~arrow pp_val pp_var fmt var_map =
  Pp.collection ~sep:"@;∧ "
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    ?filter
    (fun fmt (v, value) -> F.fprintf fmt "%a%s%a" pp_var v arrow pp_val value)
    fmt var_map


(** An implementation of \[2\] "Solving Linear Arithmetic Constraints" by Harald Rueß and Natarajan
    Shankar, SRI International, CSL Technical Report CSL-SRI-04-01, 15 January 2004. It uses a
    Simplex-like technique to reason about inequalities over linear arithmetic expressions.

    The main idea is to represent an inequality [x ≥ 0] as [x = u] with [u] belonging to a special
    class of "restricted" (or "slack") variables, which are always non-negative, and then deal with
    linear equalities on restricted variables (the tableau) instead of linear inequalities. Dark
    magic happens to massage the tableau so that contradictions are detected.

    Here restricted variables are distinguished by {!Var} directly using {!Var.is_restricted}. *)
module Tableau = struct
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
        if Option.is_none candidate && Q.(coeff > zero) && not (Var.Set.mem v bounded_vars_of_t)
        then Some v_coeff
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
end

type 'term_t subst_target = 'term_t LinArith.subst_target =
  | QSubst of Q.t
  | ConstantSubst of 'term_t * Var.t option
  | VarSubst of Var.t
  | LinSubst of LinArith.t
  | NonLinearTermSubst of 'term_t

let subst_f subst x = match Var.Map.find_opt x subst with Some y -> y | None -> x

let targetted_subst_var subst_var x = VarSubst (subst_f subst_var x)

(** Expressive term structure to be able to express all of SIL, but the main smarts of the formulas
    are for the equality between variables and linear arithmetic subsets. Terms (and atoms, below)
    are kept as a last-resort for when outside that fragment. *)
module Term = struct
  type t =
    | Const of Q.t
    | String of string
    | Var of Var.t
    | Procname of Procname.t
    | FunctionApplication of {f: t; actuals: t list}
    | Linear of LinArith.t
    | Add of t * t
    | Minus of t
    | LessThan of t * t
    | LessEqual of t * t
    | Equal of t * t
    | NotEqual of t * t
    | Mult of t * t
    | DivI of t * t
    | DivF of t * t
    | And of t * t
    | Or of t * t
    | Not of t
    | Mod of t * t
    | BitAnd of t * t
    | BitOr of t * t
    | BitNot of t
    | BitShiftLeft of t * t
    | BitShiftRight of t * t
    | BitXor of t * t
    | StringConcat of t * t
    | IsInstanceOf of {var: Var.t; typ: Typ.t; nullable: bool}
    | IsInt of t
  [@@deriving compare, equal, yojson_of]

  let equal_syntax = equal

  let needs_paren = function
    | Const c when Q.geq c Q.zero && Z.equal (Q.den c) Z.one ->
        (* nonnegative integer *)
        false
    | Const _ ->
        (* negative and/or a fraction *) true
    | String _ ->
        false
    | Var _ ->
        false
    | Linear _ ->
        false
    | Procname _ ->
        false
    | FunctionApplication _ ->
        false
    | Minus _
    | BitNot _
    | Not _
    | Add _
    | Mult _
    | DivI _
    | DivF _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | And _
    | Or _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | IsInstanceOf _
    | IsInt _ ->
        true


  let rec pp_paren pp_var ~needs_paren fmt t =
    if needs_paren t then F.fprintf fmt "(%a)" (pp_no_paren pp_var) t else pp_no_paren pp_var fmt t


  and pp_no_paren pp_var fmt = function
    | Var v ->
        pp_var fmt v
    | Const c ->
        Q.pp_print fmt c
    | String s ->
        F.fprintf fmt "\"%s\"" s
    | Procname proc_name ->
        (* templates mess up HTML debug output and are too much info most of the time *)
        Procname.pp_without_templates fmt proc_name
    | FunctionApplication {f; actuals} ->
        F.fprintf fmt "%a(%a)" (pp_paren pp_var ~needs_paren) f
          (Pp.seq ~sep:"," (pp_paren pp_var ~needs_paren))
          actuals
    | Linear l ->
        F.fprintf fmt "[%a]" (LinArith.pp pp_var) l
    | Minus t ->
        F.fprintf fmt "-%a" (pp_paren pp_var ~needs_paren) t
    | BitNot t ->
        F.fprintf fmt "BitNot%a" (pp_paren pp_var ~needs_paren) t
    | Not t ->
        F.fprintf fmt "¬%a" (pp_paren pp_var ~needs_paren) t
    | Add (t1, Minus t2) ->
        F.fprintf fmt "%a-%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | Add (t1, t2) ->
        F.fprintf fmt "%a+%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | Mult (t1, t2) ->
        F.fprintf fmt "%a×%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | DivI (t1, t2) ->
        F.fprintf fmt "%a÷%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | DivF (t1, t2) ->
        F.fprintf fmt "%a÷.%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | Mod (t1, t2) ->
        F.fprintf fmt "%a mod %a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren)
          t2
    | BitAnd (t1, t2) ->
        F.fprintf fmt "%a&%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | BitOr (t1, t2) ->
        F.fprintf fmt "%a|%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | BitShiftLeft (t1, t2) ->
        F.fprintf fmt "%a<<%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | BitShiftRight (t1, t2) ->
        F.fprintf fmt "%a>>%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | BitXor (t1, t2) ->
        F.fprintf fmt "%a xor %a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren)
          t2
    | StringConcat (t1, t2) ->
        F.fprintf fmt "%a^%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | And (t1, t2) ->
        F.fprintf fmt "%a∧%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | Or (t1, t2) ->
        F.fprintf fmt "%a∨%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | LessThan (t1, t2) ->
        F.fprintf fmt "%a<%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | LessEqual (t1, t2) ->
        F.fprintf fmt "%a≤%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | Equal (t1, t2) ->
        F.fprintf fmt "%a=%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | NotEqual (t1, t2) ->
        F.fprintf fmt "%a≠%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
    | IsInstanceOf {var; typ; nullable} ->
        F.fprintf fmt "%a instanceof %a nullable=%a" pp_var var (Typ.pp_full Pp.text) typ
          Format.pp_print_bool nullable
    | IsInt t ->
        F.fprintf fmt "is_int(%a)" (pp_no_paren pp_var) t


  let pp = pp_paren ~needs_paren

  let of_q q = Const q

  let of_intlit i = IntLit.to_big_int i |> Q.of_bigint |> of_q

  let of_const (c : Const.t) =
    match c with
    | Cint i ->
        of_intlit i
    | Cfloat f ->
        Q.of_float f |> of_q
    | Cfun proc_name ->
        Procname proc_name
    | Cstr s ->
        String s
    | Cclass ident_name ->
        String (Ident.name_to_string ident_name)


  let of_operand = function
    | AbstractValueOperand v ->
        Var v
    | ConstOperand c ->
        of_const c
    | FunctionApplicationOperand {f; actuals} ->
        let f = match f with Unknown v -> Var v | Procname proc_name -> Procname proc_name in
        FunctionApplication {f; actuals= List.map actuals ~f:(fun v -> Var v)}


  let of_subst_target = function
    | QSubst q ->
        of_q q
    | VarSubst v ->
        Var v
    | LinSubst l ->
        Linear l
    | ConstantSubst (t, _) | NonLinearTermSubst t ->
        t


  let one = of_q Q.one

  let zero = of_q Q.zero

  let of_bool b = if b then one else zero

  let of_unop (unop : Unop.t) t =
    match unop with Neg -> Minus t | BNot -> BitNot t | LNot -> Not t


  let of_binop (bop : Binop.t) t1 t2 =
    match bop with
    | PlusA _ | PlusPI ->
        Add (t1, t2)
    | MinusA _ | MinusPI | MinusPP ->
        Add (t1, Minus t2)
    | Mult _ ->
        Mult (t1, t2)
    | DivI ->
        DivI (t1, t2)
    | DivF ->
        DivF (t1, t2)
    | Mod ->
        Mod (t1, t2)
    | Shiftlt ->
        BitShiftLeft (t1, t2)
    | Shiftrt ->
        BitShiftRight (t1, t2)
    | Lt ->
        LessThan (t1, t2)
    | Gt ->
        LessThan (t2, t1)
    | Le ->
        LessEqual (t1, t2)
    | Ge ->
        LessEqual (t2, t1)
    | Eq ->
        Equal (t1, t2)
    | Ne ->
        NotEqual (t1, t2)
    | BAnd ->
        BitAnd (t1, t2)
    | BXor ->
        BitXor (t1, t2)
    | BOr ->
        BitOr (t1, t2)
    | LAnd ->
        And (t1, t2)
    | LOr ->
        Or (t1, t2)


  let is_zero = function Const c -> Q.is_zero c | _ -> false

  let is_non_zero_const = function Const c -> Q.is_not_zero c | _ -> false

  let has_known_non_boolean_type = function
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | And _
    | Or _
    | Not _
    | IsInstanceOf _
    | IsInt _
    | Var _ ->
        false
    | Linear l ->
        Option.is_none (LinArith.get_as_var l)
    | Const c ->
        not Q.(c = zero || c = one)
    | String _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | Mult _
    | DivI _
    | DivF _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _ ->
        true


  (** Fold [f] on the strict sub-terms of [t], if any. Preserve physical equality if [f] does. *)
  let fold_map_direct_subterms t ~init ~f =
    match t with
    (* no sub-terms *)
    | Var _ | Const _ | String _ | Procname _ | Linear _ | IsInstanceOf _ ->
        (init, t)
    (* list of sub-terms *)
    | FunctionApplication {f= t_f; actuals} ->
        let acc, t_f' = f init t_f in
        let changed = ref (not (phys_equal t_f t_f')) in
        let acc, actuals' =
          List.fold_map actuals ~init:acc ~f:(fun acc actual ->
              let acc, actual' = f acc actual in
              changed := !changed || not (phys_equal actual actual') ;
              (acc, actual') )
        in
        let t' = if !changed then FunctionApplication {f= t_f'; actuals= actuals'} else t in
        (acc, t')
    (* one sub-term *)
    | Minus sub_t | BitNot sub_t | Not sub_t | IsInt sub_t ->
        let acc, sub_t' = f init sub_t in
        let t' =
          if phys_equal sub_t sub_t' then t
          else
            match[@warning "-partial-match"] t with
            | Minus _ ->
                Minus sub_t'
            | BitNot _ ->
                BitNot sub_t'
            | Not _ ->
                Not sub_t'
            | IsInt _ ->
                IsInt sub_t'
        in
        (acc, t')
    (* two sub-terms *)
    | Add (t1, t2)
    | Mult (t1, t2)
    | DivI (t1, t2)
    | DivF (t1, t2)
    | Mod (t1, t2)
    | BitAnd (t1, t2)
    | BitOr (t1, t2)
    | BitShiftLeft (t1, t2)
    | BitShiftRight (t1, t2)
    | BitXor (t1, t2)
    | StringConcat (t1, t2)
    | And (t1, t2)
    | Or (t1, t2)
    | LessThan (t1, t2)
    | LessEqual (t1, t2)
    | Equal (t1, t2)
    | NotEqual (t1, t2) ->
        let acc, t1' = f init t1 in
        let acc, t2' = f acc t2 in
        let t' =
          if phys_equal t1 t1' && phys_equal t2 t2' then t
          else
            match[@warning "-partial-match"] t with
            | Add _ ->
                Add (t1', t2')
            | Mult _ ->
                Mult (t1', t2')
            | DivI _ ->
                DivI (t1', t2')
            | DivF _ ->
                DivF (t1', t2')
            | Mod _ ->
                Mod (t1', t2')
            | BitAnd _ ->
                BitAnd (t1', t2')
            | BitOr _ ->
                BitOr (t1', t2')
            | BitShiftLeft _ ->
                BitShiftLeft (t1', t2')
            | BitShiftRight _ ->
                BitShiftRight (t1', t2')
            | BitXor _ ->
                BitXor (t1', t2')
            | StringConcat _ ->
                StringConcat (t1', t2')
            | And _ ->
                And (t1', t2')
            | Or _ ->
                Or (t1', t2')
            | LessThan _ ->
                LessThan (t1', t2')
            | LessEqual _ ->
                LessEqual (t1', t2')
            | Equal _ ->
                Equal (t1', t2')
            | NotEqual _ ->
                NotEqual (t1', t2')
        in
        (acc, t')


  let map_direct_subterms t ~f =
    fold_map_direct_subterms t ~init:() ~f:(fun () t' -> ((), f t')) |> snd


  let satunsat_map_direct_subterms t ~f =
    let exception FoundUnsat in
    try
      Sat
        (map_direct_subterms t ~f:(fun t' ->
             match f t' with Unsat -> raise_notrace FoundUnsat | Sat t'' -> t'' ) )
    with FoundUnsat -> Unsat


  let rec fold_subst_variables ~init ~f_subst ?(f_post = fun ~prev:_ acc t -> (acc, t)) t =
    match t with
    | Var v ->
        let acc, op = f_subst init v in
        let t' = match op with VarSubst v' when Var.equal v v' -> t | _ -> of_subst_target op in
        f_post ~prev:t acc t'
    | IsInstanceOf {var= v; typ; nullable} ->
        let acc, op = f_subst init v in
        let t' =
          match op with
          | (VarSubst v' | ConstantSubst (_, Some v')) when not (Var.equal v v') ->
              IsInstanceOf {var= v'; typ; nullable}
          | QSubst q when Q.is_zero q ->
              (* TODO: I still think this maybe isn't quite right, since we lose the instanceof fact which we previously
                 tried to keep in order to get the right latency info, but worry about that later *)
              if nullable then one else zero
          | QSubst _ | ConstantSubst _ | VarSubst _ | LinSubst _ | NonLinearTermSubst _ ->
              t
        in
        f_post ~prev:t acc t'
    | Linear l -> (
      match LinArith.get_as_var l with
      | Some v ->
          (* avoid code duplication for the [Var v] case *)
          fold_subst_variables ~init ~f_subst ~f_post (Var v)
      | None ->
          let acc, l' = LinArith.fold_subst_variables l ~init ~f:f_subst in
          let t' = if phys_equal l l' then t else Linear l' in
          f_post ~prev:t acc t' )
    | Const _
    | String _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | Mult _
    | DivI _
    | DivF _
    | And _
    | Or _
    | Not _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | IsInt _ ->
        let acc, t' =
          fold_map_direct_subterms t ~init ~f:(fun acc t' ->
              fold_subst_variables t' ~init:acc ~f_subst ~f_post )
        in
        f_post ~prev:t acc t'


  let fold_variables ~init ~f ?f_post t =
    fold_subst_variables t ~init ~f_subst:(fun acc v -> (f acc v, VarSubst v)) ?f_post |> fst


  let iter_variables t ~f = fold_variables t ~init:() ~f:(fun () v -> f v)

  let subst_variables ~f ?f_post t =
    fold_subst_variables t ~init:() ~f_subst:(fun () v -> ((), f v)) ?f_post |> snd


  let has_var_notin vars t =
    Container.exists t ~iter:iter_variables ~f:(fun v -> not (Var.Set.mem v vars))


  (** reduce to a constant when the direct sub-terms are constants *)
  let eval_const_shallow_ t0 =
    let map_const t f = match t with Const c -> f c | _ -> t0 in
    let map_const2 t1 t2 f = match (t1, t2) with Const c1, Const c2 -> f c1 c2 | _ -> t0 in
    let q_map t q_f = map_const t (fun c -> Const (q_f c)) in
    let q_map2 t1 t2 q_f = map_const2 t1 t2 (fun c1 c2 -> Const (q_f c1 c2)) in
    let q_predicate_map t q_f = map_const t (fun c -> q_f c |> of_bool) in
    let q_predicate_map2 t1 t2 q_f = map_const2 t1 t2 (fun c1 c2 -> q_f c1 c2 |> of_bool) in
    let conv2 conv1 conv2 conv_back c1 c2 f =
      let open IOption.Let_syntax in
      let* i1 = conv1 c1 in
      let+ i2 = conv2 c2 in
      f i1 i2 |> conv_back
    in
    let map_i64_i64 = conv2 Q.to_int64 Q.to_int64 Q.of_int64 in
    let map_i64_i = conv2 Q.to_int64 Q.to_int Q.of_int64 in
    let map_z_z_opt q1 q2 f =
      conv2 Q.to_bigint Q.to_bigint (Option.map ~f:Q.of_bigint) q1 q2 f |> Option.join
    in
    let exception Undefined in
    let or_raise = function Some q -> q | None -> raise_notrace Undefined in
    let eval_const_shallow_or_raise t0 =
      match t0 with
      | Const _ | Var _ | IsInstanceOf _ | String _ | Procname _ | FunctionApplication _ ->
          t0
      | Linear l ->
          LinArith.get_as_const l |> Option.value_map ~default:t0 ~f:(fun c -> Const c)
      | IsInt t' ->
          q_map t' (fun q ->
              if Z.(equal one) (Q.den q) then (* an integer *) Q.one
              else (
                (* a non-integer rational *)
                L.d_printfln ~color:Orange "CONTRADICTION: is_int(%a)" Q.pp_print q ;
                Q.zero ) )
      | Minus t' ->
          q_map t' Q.(mul minus_one)
      | Add (t1, t2) ->
          q_map2 t1 t2 Q.add
      | BitNot t' ->
          q_map t' (fun c ->
              let open Option.Monad_infix in
              Q.to_int64 c >>| Int64.bit_not >>| Q.of_int64 |> or_raise )
      | Mult (t1, t2) ->
          q_map2 t1 t2 Q.mul
      | DivI (t1, t2) | DivF (t1, t2) ->
          q_map2 t1 t2 (fun c1 c2 ->
              let open Option.Monad_infix in
              if Q.is_zero c2 then raise_notrace Undefined
              else
                match t0 with
                | DivI _ ->
                    (* OPTIM: do the division in [Z] and not [Q] to avoid [Q] normalizing the
                       intermediate result for nothing *)
                    Z.(Q.num c1 * Q.den c2 / (Q.den c1 * Q.num c2)) >>| Q.of_bigint |> or_raise
                | _ ->
                    (* DivF *) Q.(c1 / c2) )
      | Mod (t1, t2) ->
          q_map2 t1 t2 (fun c1 c2 ->
              if Q.is_zero c2 then raise_notrace Undefined
              else map_z_z_opt c1 c2 Z.( mod ) |> or_raise )
      | Not t' ->
          q_predicate_map t' Q.is_zero
      | And (t1, t2) ->
          map_const2 t1 t2 (fun c1 c2 -> of_bool (Q.is_not_zero c1 && Q.is_not_zero c2))
      | Or (t1, t2) ->
          map_const2 t1 t2 (fun c1 c2 -> of_bool (Q.is_not_zero c1 || Q.is_not_zero c2))
      | LessThan (t1, t2) ->
          q_predicate_map2 t1 t2 Q.lt
      | LessEqual (t1, t2) ->
          q_predicate_map2 t1 t2 Q.leq
      | Equal (t1, t2) ->
          q_predicate_map2 t1 t2 Q.equal
      | NotEqual (t1, t2) ->
          q_predicate_map2 t1 t2 Q.not_equal
      | BitAnd (t1, t2)
      | BitOr (t1, t2)
      | BitShiftLeft (t1, t2)
      | BitShiftRight (t1, t2)
      | BitXor (t1, t2) ->
          q_map2 t1 t2 (fun c1 c2 ->
              match[@warning "-partial-match"] t0 with
              | BitAnd _ ->
                  map_i64_i64 c1 c2 Int64.bit_and |> or_raise
              | BitOr _ ->
                  map_i64_i64 c1 c2 Int64.bit_or |> or_raise
              | BitShiftLeft _ ->
                  map_i64_i c1 c2 Int64.shift_left |> or_raise
              | BitShiftRight _ ->
                  map_i64_i c1 c2 Int64.shift_right |> or_raise
              | BitXor _ ->
                  map_i64_i64 c1 c2 Int64.bit_xor |> or_raise )
      | StringConcat (String s1, String s2) ->
          String (s1 ^ s2)
      | StringConcat _ ->
          t0
    in
    match eval_const_shallow_or_raise t0 with
    | Const q as t ->
        if Q.is_rational q then Some t else None
    | t ->
        Some t
    | exception Undefined ->
        None


  (* defend in depth against exceptions and debug *)
  let eval_const_shallow t =
    let t' = Z.protect eval_const_shallow_ t |> Option.join in
    match t' with
    | None ->
        L.d_printfln ~color:Orange "Undefined result when evaluating %a" (pp_no_paren Var.pp) t ;
        Unsat
    | Some t' ->
        if not (phys_equal t t') then
          Debug.p "eval_const_shallow: %a -> %a@\n" (pp_no_paren Var.pp) t (pp_no_paren Var.pp) t' ;
        Sat t'


  (* defend in depth against exceptions and debug *)
  let simplify_shallow t =
    let exception Undefined in
    let rec simplify_shallow_or_raise t =
      match t with
      | Var _ | Const _ ->
          t
      | Minus (Minus t) ->
          (* [--t = t] *)
          t
      | BitNot (BitNot t) ->
          (* [~~t = t] *)
          t
      | Add (Const c, t) when Q.is_zero c ->
          (* [0 + t = t] *)
          t
      | Add (t, Const c) when Q.is_zero c ->
          (* [t + 0 = t] *)
          t
      | Mult (Const c, t) when Q.is_one c ->
          (* [1 × t = t] *)
          t
      | Mult (t, Const c) when Q.is_one c ->
          (* [t × 1 = t] *)
          t
      | Mult (Const c, _) when Q.is_zero c ->
          (* [0 × t = 0] *)
          zero
      | Mult (_, Const c) when Q.is_zero c ->
          (* [t × 0 = 0] *)
          zero
      | (DivI (Const c, _) | DivF (Const c, _)) when Q.is_zero c ->
          (* [0 / t = 0] *)
          zero
      | (DivI (t, Const c) | DivF (t, Const c)) when Q.is_one c ->
          (* [t / 1 = t] *)
          t
      | (DivI (t, Const c) | DivF (t, Const c)) when Q.is_minus_one c ->
          (* [t / (-1) = -t] *)
          simplify_shallow_or_raise (Minus t)
      | (DivI (_, Const c) | DivF (_, Const c)) when Q.is_zero c ->
          (* [t / 0 = undefined] *)
          raise_notrace Undefined
      | DivI (Minus t1, Minus t2) ->
          (* [(-t1) / (-t2) = t1 / t2] *)
          simplify_shallow_or_raise (DivI (t1, t2))
      | DivF (Minus t1, Minus t2) ->
          (* [(-t1) /. (-t2) = t1 /. t2] *)
          simplify_shallow_or_raise (DivF (t1, t2))
      | (DivI (t1, t2) | DivF (t1, t2)) when equal_syntax t1 t2 ->
          (* [t / t = 1] *)
          one
      | Mod (Const c, _) when Q.is_zero c ->
          (* [0 % t = 0] *)
          zero
      | Mod (_, Const q) when Q.is_one q ->
          (* [t % 1 = 0] *)
          zero
      | Mod (t1, t2) when equal_syntax t1 t2 ->
          (* [t % t = 0] *)
          zero
      | BitAnd (t1, t2) when is_zero t1 || is_zero t2 ->
          zero
      | BitXor (t1, t2) when equal_syntax t1 t2 ->
          zero
      | (BitShiftLeft (t1, _) | BitShiftRight (t1, _)) when is_zero t1 ->
          zero
      | (BitShiftLeft (t1, t2) | BitShiftRight (t1, t2)) when is_zero t2 ->
          t1
      | BitShiftLeft (t', Const q) | BitShiftRight (t', Const q) -> (
        match Q.to_int q with
        | None ->
            (* overflows or otherwise undefined, propagate puzzlement *)
            raise_notrace Undefined
        | Some i -> (
            if i >= 64 then (* assume 64-bit or fewer architecture *) zero
            else if i < 0 then
              (* this is undefined, maybe we should report a bug here *)
              raise_notrace Undefined
            else
              let factor = Const Q.(of_int 1 lsl i) in
              match[@warning "-partial-match"] t with
              | BitShiftLeft _ ->
                  simplify_shallow_or_raise (Mult (t', factor))
              | BitShiftRight _ ->
                  simplify_shallow_or_raise (DivI (t', factor)) ) )
      | (BitShiftLeft (t1, t2) | BitShiftRight (t1, t2)) when is_zero t2 ->
          t1
      | And (t1, t2) when is_zero t1 || is_zero t2 ->
          (* [false ∧ t = t ∧ false = false] *) zero
      | And (t1, t2) when is_non_zero_const t1 ->
          (* [true ∧ t = t] *) t2
      | And (t1, t2) when is_non_zero_const t2 ->
          (* [t ∧ true = t] *) t1
      | Or (t1, t2) when is_non_zero_const t1 || is_non_zero_const t2 ->
          (* [true ∨ t = t ∨ true = true] *) one
      | Or (t1, t2) when is_zero t1 ->
          (* [false ∨ t = t] *) t2
      | Or (t1, t2) when is_zero t2 ->
          (* [t ∨ false = t] *) t1
      | Not (Or (t1, t2)) ->
          (* prefer conjunctive normal form *)
          simplify_shallow_or_raise (And (Not t1, Not t2))
      | _ ->
          t
    in
    let t' = try Z.protect simplify_shallow_or_raise t with Undefined -> None in
    match t' with
    | None ->
        L.d_printfln ~color:Orange "Undefined result when simplifying %a" (pp_no_paren Var.pp) t ;
        Unsat
    | Some (Const q) when not (Q.is_rational q) ->
        L.d_printfln ~color:Orange "Non-rational result %a when simplifying %a" Q.pp_print q
          (pp_no_paren Var.pp) t ;
        Unsat
    | Some t' ->
        if not (phys_equal t t') then
          Debug.p "simplify_shallow: %a -> %a@\n" (pp_no_paren Var.pp) t (pp_no_paren Var.pp) t' ;
        Sat t'


  (** more or less syntactic attempt at detecting when an arbitrary term is a linear formula; call
      {!Atom.eval_term} first for best results *)
  let linearize t =
    let rec aux_linearize t =
      let open IOption.Let_syntax in
      match t with
      | Var v ->
          Some (LinArith.of_var v)
      | Const c ->
          if Q.is_rational c then Some (LinArith.of_q c) else None
      | Linear l ->
          Some l
      | Minus t ->
          let+ l = aux_linearize t in
          LinArith.minus l
      | Add (t1, t2) ->
          let* l1 = aux_linearize t1 in
          let+ l2 = aux_linearize t2 in
          LinArith.add l1 l2
      | Mult (Const c, t) | Mult (t, Const c) ->
          let+ l = aux_linearize t in
          LinArith.mult c l
      | Procname _
      | String _
      | FunctionApplication _
      | Mult _
      | DivI _
      | DivF _
      | Mod _
      | BitNot _
      | BitAnd _
      | BitOr _
      | BitShiftLeft _
      | BitShiftRight _
      | BitXor _
      | StringConcat _
      | Not _
      | And _
      | Or _
      | LessThan _
      | LessEqual _
      | Equal _
      | NotEqual _
      | IsInstanceOf _
      | IsInt _ ->
          None
    in
    match aux_linearize t with
    | None ->
        t
    | Some l ->
        Debug.p "linearized: %a -> %a@\n" (pp_no_paren Var.pp) t (LinArith.pp Var.pp) l ;
        Linear l


  let simplify_linear = function
    | Linear l -> (
      match LinArith.get_as_const l with
      | Some c ->
          Const c
      | None -> (
        match LinArith.get_as_var l with Some v -> Var v | None -> Linear l ) )
    | t ->
        t


  let get_as_isinstanceof t =
    match t with IsInstanceOf {var; typ; nullable} -> Some (var, typ, nullable) | _ -> None


  let get_as_var = function
    | Var x ->
        Some x
    | Linear l ->
        LinArith.get_as_var l
    | Const _
    | String _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | Mult _
    | DivI _
    | DivF _
    | And _
    | Or _
    | Not _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | IsInstanceOf _
    | IsInt _ ->
        None


  let get_as_const = function
    | Const q ->
        Some q
    | Linear l ->
        LinArith.get_as_const l
    | Var _
    | String _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | Mult _
    | DivI _
    | DivF _
    | And _
    | Or _
    | Not _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | IsInstanceOf _
    | IsInt _ ->
        None


  let is_non_numeric_constant = function
    | String _ ->
        true
    | Const _
    | Linear _
    | Var _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | Mult _
    | DivI _
    | DivF _
    | And _
    | Or _
    | Not _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | IsInstanceOf _
    | IsInt _ ->
        false


  let get_as_linear = function
    | Linear l ->
        Some l
    | Const q ->
        Some (LinArith.of_q q)
    | Var x ->
        Some (LinArith.of_var x)
    | String _
    | Procname _
    | FunctionApplication _
    | Add _
    | Minus _
    | LessThan _
    | LessEqual _
    | Equal _
    | NotEqual _
    | Mult _
    | DivI _
    | DivF _
    | And _
    | Or _
    | Not _
    | Mod _
    | BitAnd _
    | BitOr _
    | BitNot _
    | BitShiftLeft _
    | BitShiftRight _
    | BitXor _
    | StringConcat _
    | IsInstanceOf _
    | IsInt _ ->
        None


  let to_subst_target t =
    match get_as_const t with
    | Some q ->
        QSubst q
    | None -> (
        if is_non_numeric_constant t then ConstantSubst (t, None)
        else
          match get_as_var t with
          | Some v ->
              VarSubst v
          | None -> (
            match t with Linear l -> LinSubst l | _ -> NonLinearTermSubst t ) )


  module VarMap = struct
    include Caml.Map.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    type t_ = Var.t t [@@deriving compare, equal]

    let pp_with_pp_var pp_var fmt m =
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_map_fold fold)
        (fun fmt (term, var) -> F.fprintf fmt "%a=%a" (pp pp_var) term pp_var var)
        fmt m


    let yojson_of_t_ m = `List (List.map (bindings m) ~f:[%yojson_of: t * Var.t])
  end
end

(** Basically boolean terms, used to build the part of a formula that is not equalities between
    linear arithmetic. *)
module Atom = struct
  type t =
    | LessEqual of Term.t * Term.t
    | LessThan of Term.t * Term.t
    | Equal of Term.t * Term.t
    | NotEqual of Term.t * Term.t
  [@@deriving compare, equal, yojson_of]

  let pp_with_pp_var pp_var fmt atom =
    (* add parens around terms that look like atoms to disambiguate *)
    let needs_paren (t : Term.t) =
      match t with LessThan _ | LessEqual _ | Equal _ | NotEqual _ -> true | _ -> false
    in
    let pp_term = Term.pp_paren pp_var ~needs_paren in
    match atom with
    | LessEqual (t1, t2) ->
        F.fprintf fmt "%a ≤ %a" pp_term t1 pp_term t2
    | LessThan (t1, t2) ->
        F.fprintf fmt "%a < %a" pp_term t1 pp_term t2
    | Equal (t1, t2) ->
        F.fprintf fmt "%a = %a" pp_term t1 pp_term t2
    | NotEqual (t1, t2) ->
        F.fprintf fmt "%a ≠ %a" pp_term t1 pp_term t2


  let get_terms atom =
    let (LessEqual (t1, t2) | LessThan (t1, t2) | Equal (t1, t2) | NotEqual (t1, t2)) = atom in
    (t1, t2)


  (** preserve physical equality if [f] does *)
  let fold_map_terms atom ~init ~f =
    let t1, t2 = get_terms atom in
    let acc, t1' = f init t1 in
    let acc, t2' = f acc t2 in
    let t' =
      if phys_equal t1' t1 && phys_equal t2' t2 then atom
      else
        match atom with
        | LessEqual _ ->
            LessEqual (t1', t2')
        | LessThan _ ->
            LessThan (t1', t2')
        | Equal _ ->
            Equal (t1', t2')
        | NotEqual _ ->
            NotEqual (t1', t2')
    in
    (acc, t')


  let fold_terms atom ~init ~f = fold_map_terms atom ~init ~f:(fun acc t -> (f acc t, t)) |> fst

  let fold_variables atom ~init ~f =
    fold_terms atom ~init ~f:(fun acc t -> Term.fold_variables t ~init:acc ~f)


  let equal t1 t2 = Equal (t1, t2)

  let not_equal t1 t2 = NotEqual (t1, t2)

  let less_equal t1 t2 = LessEqual (t1, t2)

  let less_than t1 t2 = LessThan (t1, t2)

  let nnot = function
    | Equal (t1, t2) ->
        NotEqual (t1, t2)
    | NotEqual (t1, t2) ->
        Equal (t1, t2)
    | LessEqual (t1, t2) ->
        LessThan (t2, t1)
    | LessThan (t1, t2) ->
        LessEqual (t2, t1)


  let map_terms atom ~f = fold_map_terms atom ~init:() ~f:(fun () t -> ((), f t)) |> snd

  let to_term : t -> Term.t = function
    | LessEqual (t1, t2) ->
        LessEqual (t1, t2)
    | LessThan (t1, t2) ->
        LessThan (t1, t2)
    | Equal (t1, t2) ->
        Equal (t1, t2)
    | NotEqual (t1, t2) ->
        NotEqual (t1, t2)


  type eval_result = True | False | Atom of t

  let eval_result_of_bool b = if b then True else False

  let nnot_if b atom = if b then nnot atom else atom

  (** [atoms_of_term ~negated t] is [Some [atom1; ..; atomN]] if [t] (or [¬t] if [negated]) is
      (mostly syntactically) equivalent to [atom1 ∧ .. ∧ atomN]. For example
      [atoms_of_term ~negated:false (Equal (Or (x, Not y), 0))] should be
      [[Equal (x, 0); NotEqual (y, 0)]]. When the term [y] is known as a boolean, it generates a
      preciser atom [Equal (y, 1)].

      [is_neq_zero] is a function that can tell if a term is known to be [≠0], and [force_to_atom]
      can be used to force converting the term into an atom even if it does not make it simpler or
      stronger. *)
  let rec atoms_of_term ~is_neq_zero ~negated ?(force_to_atom = false) t =
    let rec aux ~negated ~force_to_atom (t : Term.t) : t list option =
      match t with
      | LessEqual (t1, t2) ->
          Some [LessEqual (t1, t2) |> nnot_if negated]
      | LessThan (t1, t2) ->
          Some [LessThan (t1, t2) |> nnot_if negated]
      | Equal (t1, t2) ->
          Some [Equal (t1, t2) |> nnot_if negated]
      | NotEqual (t1, t2) ->
          Some [NotEqual (t1, t2) |> nnot_if negated]
      | And (t1, t2) when not negated -> (
        match
          (aux ~negated:false ~force_to_atom:true t1, aux ~negated:false ~force_to_atom:true t2)
        with
        | Some atoms1, Some atoms2 ->
            Some (List.rev_append atoms1 atoms2)
        | _ ->
            assert false )
      | Or (t1, t2) when negated -> (
        match
          (aux ~negated:true ~force_to_atom:true t1, aux ~negated:true ~force_to_atom:true t2)
        with
        | Some atoms1, Some atoms2 ->
            Some (List.rev_append atoms1 atoms2)
        | _ ->
            assert false )
      | Not t ->
          (* NOTE: [Not (Or _)] is taken care of by term normalization so no need to handle it here *)
          aux ~negated:(not negated) ~force_to_atom t
      | t ->
          if force_to_atom then
            Some
              [ ( if negated then Equal (t, Term.zero)
                  else if Term.has_known_non_boolean_type t then NotEqual (t, Term.zero)
                  else Equal (t, Term.one) ) ]
          else None
    in
    aux ~negated ~force_to_atom t
    |> Option.map ~f:(fun atoms ->
           List.concat_map atoms ~f:(fun atom ->
               get_as_embedded_atoms ~is_neq_zero atom |> Option.value ~default:[atom] ) )


  (** similar to [atoms_of_term] but takes an atom and "flattens" it to possibly several atoms whose
      conjunction is equivalent to the original atom *)
  and get_as_embedded_atoms ~is_neq_zero atom =
    let of_terms is_equal t c =
      let negated =
        (* [atom = 0] or [atom ≠ 1] when [atom] is boolean means [atom] is false, [atom ≠ 0] or
           [atom = 1] means [atom] is true *)
        (is_equal && Q.is_zero c)
        || ((not is_equal) && (not (Term.has_known_non_boolean_type t)) && Q.is_one c)
      in
      atoms_of_term ~is_neq_zero ~negated t
    in
    (* [of_terms] is written for only one side, the one where [t1] is the potential atom *)
    let of_terms_symmetry is_equal atom =
      let t1, t2 = get_terms atom in
      match (t1, t2) with
      | Term.Const _, Term.Const _ ->
          (* to make the pattern match below unambiguous; this case should be handled by [eval_const_shallow] *)
          None
      | Term.Const c, t | t, Term.Const c ->
          of_terms is_equal t c
      | _ ->
          None
    in
    match atom with
    | Equal (Const _, _) | Equal (_, Const _) ->
        of_terms_symmetry true atom
    | NotEqual (Const _, _) | NotEqual (_, Const _) ->
        of_terms_symmetry false atom
    | Equal (t1, t2) ->
        if is_neq_zero t2 then
          (* [t1 = t2] and [t2 ≠ 0] so [t1 ≠ 0] *)
          get_as_embedded_atoms ~is_neq_zero (NotEqual (t1, Term.zero))
        else if is_neq_zero t1 then
          (* symmetric of the previous case *)
          get_as_embedded_atoms ~is_neq_zero (NotEqual (t2, Term.zero))
        else None
    | _ ->
        None


  let eval_const_shallow atom =
    match atom with
    | LessEqual (Const c1, Const c2) ->
        Q.leq c1 c2 |> eval_result_of_bool
    (* [A+c' ≥ c] with [A ≥ 0] (as happens if [l] is restricted and minimized) is always true if [c'
       ≥ c] *)
    | LessEqual (Const c, Linear l)
      when LinArith.is_restricted l && LinArith.is_minimized l
           && Q.geq (LinArith.get_constant_part l) c ->
        True
    (* [A+c' > c] with [A ≥ 0] is always true if [c' > c] *)
    | LessThan (Const c, Linear l)
      when LinArith.is_restricted l && LinArith.is_minimized l
           && Q.gt (LinArith.get_constant_part l) c ->
        True
    (* NOTE: the corresponding contradiction cases, eg [A+c' ≤ c], [A ≥ 0], and [c' > c], as well as
       other more substle reasoning, are handled by the tableau and need not be duplicated here. The
       [True] cases above are included to avoid cluttering the formula with tautologies as these
       (in)equalities are added in other parts of the formula than the tableau too otherwise (in
       particular [linear_eqs] and [term_eqs]), which are not equipped to discover they are
       trivial. *)
    | LessThan (Const c1, Const c2) ->
        Q.lt c1 c2 |> eval_result_of_bool
    | Equal (Const c1, Const c2) ->
        Q.equal c1 c2 |> eval_result_of_bool
    | NotEqual (Const c1, Const c2) ->
        Q.not_equal c1 c2 |> eval_result_of_bool
    | LessEqual (String s1, String s2) ->
        String.compare s1 s2 <= 0 |> eval_result_of_bool
    | LessThan (String s1, String s2) ->
        String.compare s1 s2 < 0 |> eval_result_of_bool
    | Equal (String s1, String s2) ->
        String.equal s1 s2 |> eval_result_of_bool
    | NotEqual (String s1, String s2) ->
        (not (String.equal s1 s2)) |> eval_result_of_bool
    | _ ->
        Atom atom


  let var_terms_to_linear atom =
    map_terms atom ~f:(function Var v -> Linear (LinArith.of_var v) | t -> t)


  let get_as_linear atom =
    match get_terms @@ var_terms_to_linear atom with
    | Linear l1, Linear l2 ->
        let l = LinArith.subtract l1 l2 in
        let t = Term.simplify_linear (Linear l) in
        Some
          ( match atom with
          | Equal _ ->
              Equal (t, Term.zero)
          | NotEqual _ ->
              NotEqual (t, Term.zero)
          | LessEqual _ ->
              LessEqual (t, Term.zero)
          | LessThan _ ->
              LessThan (t, Term.zero) )
    | _ ->
        None


  let eval_syntactically_equal_terms atom =
    let t1, t2 = get_terms atom in
    if Term.equal_syntax t1 t2 then
      match atom with
      | Equal _ ->
          True
      | NotEqual _ ->
          False
      | LessEqual _ ->
          True
      | LessThan _ ->
          False
    else Atom atom


  let rec eval_with_normalized_terms ~is_neq_zero (atom : t) =
    match eval_const_shallow atom with
    | True ->
        Sat (Some [])
    | False ->
        L.d_printfln "UNSAT atom according to eval_const_shallow: %a" (pp_with_pp_var Var.pp) atom ;
        Unsat
    | Atom atom -> (
      match get_as_linear atom with
      | Some atom' ->
          let+ atoms' = eval_with_normalized_terms ~is_neq_zero atom' in
          Some (Option.value atoms' ~default:[atom'])
      | None -> (
        match get_as_embedded_atoms ~is_neq_zero atom with
        | None -> (
          match eval_syntactically_equal_terms atom with
          | True ->
              Sat (Some [])
          | False ->
              L.d_printfln "UNSAT atom according to eval_syntactically_equal_terms: %a"
                (pp_with_pp_var Var.pp) atom ;
              Unsat
          | Atom _atom ->
              (* HACK: [eval_syntactically_equal_terms] return [Atom _] only when it didn't manage
                 to normalize anything *)
              Sat None )
        | Some atoms ->
            Debug.p "Found that %a is equivalent to embedded atoms %a@\n" (pp_with_pp_var Var.pp)
              atom
              (Pp.seq ~sep:"," (pp_with_pp_var Var.pp))
              atoms ;
            let+ atoms' = eval_atoms_with_normalized_terms ~is_neq_zero atoms in
            Some atoms' ) )


  and eval_atoms_with_normalized_terms ~is_neq_zero atoms =
    SatUnsat.list_fold ~init:[] atoms ~f:(fun atoms atom' ->
        let+ normalized_atoms_opt = eval_with_normalized_terms ~is_neq_zero atom' in
        match normalized_atoms_opt with
        | None ->
            (* did not normalize further *) atom' :: atoms
        | Some atoms' ->
            List.rev_append atoms' atoms )


  let rec eval_term ~is_neq_zero t =
    Debug.p "Atom.eval_term %a@\n" (Term.pp_no_paren Var.pp) t ;
    let+ t =
      Term.satunsat_map_direct_subterms ~f:(eval_term ~is_neq_zero) t
      >>= Term.eval_const_shallow >>= Term.simplify_shallow >>| Term.linearize
      >>| Term.simplify_linear
    in
    match atoms_of_term ~is_neq_zero ~negated:false t with
    | None ->
        t
    | Some atoms -> (
      (* terms that are atoms can be simplified in [eval_atom] *)
      match eval_atoms_with_normalized_terms ~is_neq_zero atoms with
      | Unsat ->
          Term.zero
      | Sat [] ->
          Term.one
      | Sat (atom0 :: atoms') ->
          (* TODO: juggling between atom and term representations is cumbersome. Either move the
             atom eval logic into [Term] to do everything there or have [And] be n-ary to cheapen
             the back and forth *)
          List.fold atoms' ~init:(to_term atom0) ~f:(fun term atom' ->
              Term.And (to_term atom', term) ) )


  let eval ~is_neq_zero atom =
    Debug.p "Atom.val %a@\n" (pp_with_pp_var Var.pp) atom ;
    let* atom =
      let exception FoundUnsat in
      try
        Sat
          (map_terms atom ~f:(fun t ->
               match eval_term ~is_neq_zero t with
               | Sat t' ->
                   t'
               | Unsat ->
                   raise_notrace FoundUnsat ) )
      with FoundUnsat -> Unsat
    in
    let+ atoms_opt = eval_with_normalized_terms ~is_neq_zero atom in
    Option.value atoms_opt ~default:[atom]


  let fold_subst_variables ~init ~f_subst ?f_post a =
    fold_map_terms a ~init ~f:(fun acc t -> Term.fold_subst_variables t ~init:acc ~f_subst ?f_post)


  let subst_variables l ~f = fold_subst_variables l ~init:() ~f_subst:(fun () v -> ((), f v)) |> snd

  let has_var_notin vars atom =
    let t1, t2 = get_terms atom in
    Term.has_var_notin vars t1 || Term.has_var_notin vars t2


  (* assumes the atom is normalized *)
  let get_as_var_neq_zero = function
    | NotEqual (Const _, Const _) | LessThan (Const _, Const _) ->
        (* to make sure the side condition below is unambiguous *)
        None
    | (NotEqual (t, Const q) | NotEqual (Const q, t) | LessThan (Const q, t) | LessThan (t, Const q))
      when Q.(equal q zero) ->
        (* match [x≠0] or [x>0]. Note that [0] is represented as a [Const _] when normalized but
           variables will usually (always?) be represented by [LinArith _] in normalized formulas *)
        Term.get_as_var t
    | _ ->
        None


  (* assumes the atom is normalized *)
  let get_as_disequal_vars = function
    | NotEqual (t1, t2) -> (
      match Option.both (Term.get_as_var t1) (Term.get_as_var t2) with
      | Some _ as vars ->
          (* detects [x≠y] *)
          vars
      | None -> (
        match (t1, Term.get_as_const t2) with
        | Linear l, Some q when Q.(q = zero) ->
            (* detects [x-y≠0] *)
            LinArith.get_as_variable_difference l
        | _ ->
            None ) )
    | _ ->
        None


  let simplify_linear atom = map_terms atom ~f:Term.simplify_linear

  module Set = struct
    include Caml.Set.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    let pp_with_pp_var ?filter pp_var fmt atoms =
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_set_fold fold)
        ?filter
        (fun fmt atom -> F.fprintf fmt "{%a}" (pp_with_pp_var pp_var) atom)
        fmt atoms


    let yojson_of_t atoms = `List (List.map (elements atoms) ~f:yojson_of_t)
  end
end

module VarUF = UnionFind.Make (Var) (Var.Set) (Var.Map)

type new_eq = EqZero of Var.t | Equal of Var.t * Var.t

let pp_new_eq fmt = function
  | EqZero v ->
      F.fprintf fmt "%a=0" Var.pp v
  | Equal (v1, v2) ->
      F.fprintf fmt "%a=%a" Var.pp v1 Var.pp v2


type new_eqs = new_eq RevList.t

let pp_new_eqs fmt new_eqs =
  F.fprintf fmt "[@[%a@]]" (Pp.seq ~sep:"," pp_new_eq) (RevList.to_list new_eqs)


module MakeOccurrences (In : sig
  type t

  module Set : Caml.Set.S with type elt = t

  val pp_set : (F.formatter -> Var.t -> unit) -> F.formatter -> Set.t -> unit
end) =
struct
  type t = In.Set.t Var.Map.t [@@deriving compare, equal]

  let pp pp_var fmt occurrences = (pp_var_map ~arrow:"->" (In.pp_set pp_var) pp_var) fmt occurrences

  let yojson_of_t = [%yojson_of: _]

  (** add [in_] to [occurrences(v)] *)
  let add v ~occurs_in:in_ occurrences =
    Var.Map.update v
      (fun in_vs_opt ->
        let in_vs = Option.value ~default:In.Set.empty in_vs_opt in
        Some (In.Set.add in_ in_vs) )
      occurrences


  (** remove [in_] from [occurrences(v)] *)
  let remove v ~occurred_in:in_ occurrences =
    Var.Map.update v
      (function
        | None ->
            None
        | Some in_vs ->
            let in_vs' = In.Set.remove in_ in_vs in
            if In.Set.is_empty in_vs' then None else Some in_vs' )
      occurrences
end

module VarMapOccurrences = MakeOccurrences (struct
  include Var

  let pp_set = pp_var_set
end)

module TermDomainOrRange = struct
  (** for a binding [t=v], describes whether a variable [v'] appears in [t] (domain), is [v]
      (range), or both; we could have a different representation of term occurrences using a map
      [t -> Domain | Range | DomainAndRange] to match what we are trying to do closer but we use a
      set like other occurrence maps, oh well. *)
  type domain_or_range = Domain | Range | DomainAndRange [@@deriving compare]

  type t = Term.t * domain_or_range [@@deriving compare]

  module Set = Caml.Set.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let pp_set pp_var fmt set =
    if Set.is_empty set then F.pp_print_string fmt "(empty)"
    else
      Pp.collection ~sep:","
        ~fold:(IContainer.fold_of_pervasives_set_fold Set.fold)
        (fun fmt (term, domain_or_range) ->
          ( match domain_or_range with
          | Domain ->
              F.pp_print_char fmt 'd'
          | Range ->
              F.pp_print_char fmt 'r'
          | DomainAndRange ->
              F.pp_print_string fmt "dr" ) ;
          F.fprintf fmt "(%a)" (Term.pp_no_paren pp_var) term )
        fmt set
end

module TermMapOccurrences = MakeOccurrences (TermDomainOrRange)

module AtomMapOccurrences = MakeOccurrences (struct
  include Atom

  let pp_set fmt atoms = Atom.Set.pp_with_pp_var fmt atoms
end)

module InstanceOf = struct
  (** Domain for tracking dynamic type of variables via positive and negative instanceof constraints *)

  (* *Intended* invariant is that these all be normalised wrt alias expansion *)
  type dynamic_type_data = {typ: Typ.t; source_file: (SourceFile.t[@yojson.opaque]) option}
  [@@deriving compare, equal, yojson_of]

  type instance_fact =
    | Known of dynamic_type_data
    | Unknown of {below: Typ.t list; notbelow: Typ.t list}
  [@@deriving compare, equal, yojson_of]

  let pp_instance_fact fmt inf =
    match inf with
    | Known {typ; source_file} ->
        F.fprintf fmt "%a, SourceFile %a" (Typ.pp_full Pp.text) typ (Pp.option SourceFile.pp)
          source_file
    | Unknown {below; notbelow} ->
        let tlist_pp = Pp.seq ~sep:"," (Typ.pp_full Pp.text) in
        F.fprintf fmt "{%a \\ %a}" tlist_pp below tlist_pp notbelow


  type t = instance_fact Var.Map.t [@@deriving compare, equal, yojson_of]

  let pp_with_pp_var pp_var fmt m =
    Pp.collection ~sep:"∧"
      ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
      (fun fmt (var, inf) -> F.fprintf fmt "%a:%a" pp_var var pp_instance_fact inf)
      fmt m


  let is_subtype t1 t2 =
    (* Don't really like doing dereference on every call *)
    let tenv = PulseContext.tenv_exn () in
    match (Typ.name t1, Typ.name t2) with
    | Some n1, Some n2 ->
        PatternMatch.is_subtype tenv n1 n2
    | _, _ ->
        Typ.equal t1 t2


  let is_final t =
    let tenv = PulseContext.tenv_exn () in
    match Typ.name t with
    | None ->
        false
    | Some (ErlangType _) ->
        true
        (* TODO: Actually wrong for Any, if we ever get that, and maybe there are subtypes of bool?? *)
    | Some tn -> (
      match Tenv.lookup tenv tn with None -> false | Some {annots} -> Annot.Item.is_final annots )


  (* The following are a bit conservative, as they currently only return true for Hack types
     TODO: check soundness of reasoning for other languages and extend the logic
  *)
  let is_concrete_or_abstract t =
    let tenv = PulseContext.tenv_exn () in
    match Typ.name t with
    | None ->
        false
    | Some (ErlangType _) ->
        true
    | Some (JavaClass _ as tn) -> (
      match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_not_java_interface str )
    | Some (HackClass _ as tn) -> (
      match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_hack_class str )
    | _ ->
        false


  let is_abstract t =
    let tenv = PulseContext.tenv_exn () in
    match Typ.name t with
    | None ->
        false
    | Some tn -> (
      match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_hack_abstract_class str )
end

module Formula = struct
  (* redefined for yojson output *)
  type var_eqs = VarUF.t [@@deriving compare, equal]

  let yojson_of_var_eqs var_eqs =
    `List
      (VarUF.fold_congruences var_eqs ~init:[] ~f:(fun jsons (repr, eqs) ->
           `List
             (Var.yojson_of_t (repr :> Var.t) :: List.map ~f:Var.yojson_of_t (Var.Set.elements eqs))
           :: jsons ) )


  type linear_eqs = LinArith.t Var.Map.t [@@deriving compare, equal]

  let yojson_of_linear_eqs linear_eqs = Var.Map.yojson_of_t LinArith.yojson_of_t linear_eqs

  type intervals = CItv.t Var.Map.t [@@deriving compare, equal]

  module Unsafe : sig
    (** opaque because we need to normalize variables in the co-domain of term equalities on the fly *)
    type term_eqs

    type t = private
      { var_eqs: var_eqs
            (** Equality relation between variables. We want to only use canonical representatives
                from this equality relation in the rest of the formula and more generally in all of
                the abstract state. See also {!AbductiveDomain}. *)
      ; const_eqs: Term.t Var.Map.t
      ; type_constraints: InstanceOf.t
      ; linear_eqs: linear_eqs
            (** Equalities of the form [x = l] where [l] is from linear arithmetic. These are
                interpreted over the *rationals*, not integers (or floats), so this will be
                incomplete. There are mitigations to recover a little of that lost completeness:

                - [atoms] have [is_int()] terms that will detect when we get (constant) rationals
                  where we expected integers eg [is_int(1.5)] becomes [false].

                - [intervals] are over integers

                INVARIANT:

                1. the domain and the range of [phi.linear_eqs] mention distinct variables:
                [domain(linear_eqs) ∩ range(linear_eqs) = ∅], when seeing [linear_eqs] as a map
                [x->l]

                2. for all [x=l ∊ linear_eqs], [x < min({x'|x'∊l})] according to [is_simpler_than]
                (in other words: [x] is the simplest variable in [x=l]). *)
      ; term_eqs: term_eqs
            (** Equalities of the form [t = x], used to detect when two abstract values are equal to
                the same term (hence equal). Together with [var_eqs] and [linear_eqs] this gives a
                congruence closure capability to the domain over uninterpreted parts of the domain
                (meaning the atoms and term equalities that are not just linear arithmetic and
                handled in a complete-ish fashion by [linear_eqs], [var_eqs], [tableau]). Even on
                interpreted domains like linear arithmetic [term_eqs] is needed to provide
                on-the-fly normalisation by detecting when two variables are equal to the same term,
                which [linear_eqs] will do nothing about (eg [x=0∧y=0] doesn't trigger [x=y] in
                [linear_eqs] but [term_eqs] is in charge of detecting it).

                Under the hood this is a map [t -> x] that may contain un-normalized [x]s in its
                co-domain; these are normalized on the fly when reading from the map

                INVARIANT: each term in [term_eqs] is *shallow*, meaning it is either a constant or
                a term of the form [f(x1, ..., xN)] with [x1], ..., [xN] either variables or
                constants themselves. *)
      ; tableau: Tableau.t
            (** linear equalities similar to [linear_eqs] but involving only "restricted" (aka
                "slack") variables; this is used for reasoning about inequalities, see \[2\]

                INVARIANT: see {!Tableau} *)
      ; intervals: (intervals[@yojson.opaque])
            (** A simple, non-relational domain of concrete integer intervals of the form [x∈[i,j]]
                or [x∉[i,j]].

                This is used to recover a little bit of completeness on integer reasoning at no
                great cost. *)
      ; atoms: Atom.Set.t
            (** "everything else": atoms that cannot be expressed in a form suitable for one of the
                other domains, in particular disequalities.

                INVARIANT: Contrarily to [term_eqs] atoms are *maximally expanded* meaning if a
                variable [x] is equal to a term [t] then [x] shouldn't appear in [atoms] and should
                be substituted by [t] instead. This is looser than other invariants since a given
                variable can be equal to several syntactically-distinct terms so "maximally
                expanded" doesn't really make sense in general and there is incompleteness there. *)
      ; linear_eqs_occurrences: VarMapOccurrences.t
            (** occurrences of variables in [linear_eqs]: a binding [x -> y] means that [x] appears
                in [linear_eqs(y)] and will be used to propagate new (linear) equalities about [x]
                to maintain the [linear_eqs] invariant without having to do a linear scan of
                [linear_eqs] *)
      ; tableau_occurrences: VarMapOccurrences.t  (** likewise for [tableau] *)
      ; term_eqs_occurrences: TermMapOccurrences.t
            (** like [linear_eqs_occurrences] but for [term_eqs] so bindings are from variables to
                sets of terms *)
      ; atoms_occurrences: AtomMapOccurrences.t  (** likewise for [atoms] *) }
    [@@deriving compare, equal, yojson_of]

    val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

    val get_repr : t -> Var.t -> VarUF.repr
    (** the canonical representative of a given variable *)

    val ttrue : t

    (* {2 [term_eqs] interface due to the totally opaque type} *)

    val get_term_eq : t -> Term.t -> Var.t option

    val term_eqs_fold : (Term.t -> Var.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

    val term_eqs_iter : (Term.t -> Var.t -> unit) -> t -> unit

    val term_eqs_exists : (Term.t -> Var.t -> bool) -> t -> bool

    val term_eqs_filter : (Term.t -> Var.t -> bool) -> t -> Term.VarMap.t_

    val fold_term_eqs_vars : t -> init:'acc -> f:('acc -> Var.t -> 'acc) -> 'acc

    val subst_term_eqs : Var.t Var.Map.t -> t -> Term.VarMap.t_

    val pp_term_eqs_with_pp_var :
         ?filter:(Term.t -> Var.t -> bool)
      -> (F.formatter -> Var.t -> unit)
      -> F.formatter
      -> t
      -> unit

    (* {2 mutations} *)

    val add_const_eq : Var.t -> Term.t -> t -> t SatUnsat.t
    (** [add_const_eq v t phi] adds [v=t] to [const_eqs]; [Unsat] if [v] was already bound to a
        different constant *)

    val remove_const_eq : Var.t -> t -> t

    val add_dynamic_type : Var.t -> Typ.t -> ?source_file:SourceFile.t -> t -> t * bool
    (** We're now tracking nullability more precisely. If the language includes nulls and we have
        apparently contradictory type information then, instead of immediately returning [Unsat], we
        want to add an assertion that the value must be null (which inhabits all (nullable) types).
        But at this low level in the solver stack (the [Unsafe] module), we can't yet decide if the
        resulting state is [Unsat]. So we return an extra boolean that indicates whether or not a
        higher-level wrapper should add, and compute the consequences of, that assertion. See
        [and_dynamic_type] further down (in the [Normalizer] module) for such a wrapper around
        [add_dynamic_type] *)

    val add_below : Var.t -> Typ.t -> t -> t * bool

    val add_notbelow : Var.t -> Typ.t -> t -> t * bool

    val copy_type_constraints : Var.t -> Var.t -> t -> t

    val add_linear_eq : Var.t -> LinArith.t -> t -> t * Var.t option
    (** [add_linear_eq v l phi] adds [v=l] to [linear_eqs] and updates the occurrences maps and
        [term_eqs] appropriately; don't forget to call [propagate_linear_eq] after this *)

    val remove_linear_eq : Var.t -> LinArith.t -> t -> t
    (** [remove_linear_eq v l phi] removes [v] from [linear_eqs] and updates the occurrences maps
        and [term_eqs] appropriately; [l] is the existing binding for [v] *)

    val remove_tableau_eq : Var.t -> LinArith.t -> t -> t

    val add_term_eq : Term.t -> Var.t -> t -> t * Var.t option
    (** [add_term_eq t v phi] adds [t=v] to [term_eqs] and updates the occurrences maps
        appropriately; don't forget to call [propagate_term_eq] after this *)

    val remove_term_eq : Term.t -> Var.t -> t -> t
    (** [remove_term_eq t v phi] removes [t] from the [term_eqs] map and updates the occurrences
        maps appropriately; [v] is the existing (representative for the) binding for [t] *)

    val add_atom : Atom.t -> t -> t

    val remove_atom : Atom.t -> t -> t

    val add_tableau_eq : Var.t -> LinArith.t -> t -> t

    val add_interval : Var.t -> CItv.t -> t -> t SatUnsat.t

    val add_interval_ : Var.t -> CItv.t -> intervals -> intervals SatUnsat.t
    (** same as [add_interval] but operates on the inner [intervals] datatype *)

    val add_occurrence_to_range_of_term_eq : Term.t -> VarUF.repr -> t -> t
    (** assumes that the variable doesn't appear in the term itself *)

    val remove_from_linear_eqs_occurrences : Var.t -> t -> t

    val remove_from_tableau_occurrences : Var.t -> t -> t

    val remove_from_term_eqs_occurrences : Var.t -> t -> t

    val remove_from_atoms_occurrences : Var.t -> t -> t

    val set_var_eqs : var_eqs -> t -> t

    val set_tableau : Tableau.t -> t -> t

    val set_intervals : intervals -> t -> t

    val unsafe_mk :
         var_eqs:var_eqs
      -> const_eqs:Term.t Var.Map.t
      -> type_constraints:InstanceOf.t
      -> linear_eqs:linear_eqs
      -> term_eqs:Term.VarMap.t_
      -> tableau:Tableau.t
      -> intervals:intervals
      -> atoms:Atom.Set.t
      -> linear_eqs_occurrences:VarMapOccurrences.t
      -> tableau_occurrences:VarMapOccurrences.t
      -> term_eqs_occurrences:TermMapOccurrences.t
      -> atoms_occurrences:AtomMapOccurrences.t
      -> t
    (** escape hatch *)
  end = struct
    type term_eqs = Term.VarMap.t_ [@@deriving compare, equal, yojson_of]

    let term_eqs_is_empty = Term.VarMap.is_empty

    type t =
      { var_eqs: var_eqs
      ; const_eqs: Term.t Var.Map.t
      ; type_constraints: InstanceOf.t
      ; linear_eqs: linear_eqs
      ; term_eqs: term_eqs
      ; tableau: Tableau.t
      ; intervals: (intervals[@yojson.opaque])
      ; atoms: Atom.Set.t
      ; linear_eqs_occurrences: VarMapOccurrences.t
      ; tableau_occurrences: VarMapOccurrences.t
      ; term_eqs_occurrences: TermMapOccurrences.t
      ; atoms_occurrences: AtomMapOccurrences.t }
    [@@deriving compare, equal, yojson_of]

    let ttrue =
      { var_eqs= VarUF.empty
      ; const_eqs= Var.Map.empty
      ; type_constraints= Var.Map.empty
      ; linear_eqs= Var.Map.empty
      ; term_eqs= Term.VarMap.empty
      ; tableau= Tableau.empty
      ; intervals= Var.Map.empty
      ; atoms= Atom.Set.empty
      ; linear_eqs_occurrences= Var.Map.empty
      ; tableau_occurrences= Var.Map.empty
      ; term_eqs_occurrences= Var.Map.empty
      ; atoms_occurrences= Var.Map.empty }


    let get_repr phi x = VarUF.find phi.var_eqs x

    let get_repr_as_var phi x = (get_repr phi x :> Var.t)

    let is_empty
        ({ var_eqs
         ; const_eqs
         ; type_constraints
         ; linear_eqs
         ; term_eqs
         ; tableau
         ; intervals
         ; atoms
         ; linear_eqs_occurrences= _
         ; tableau_occurrences= _
         ; term_eqs_occurrences= _
         ; atoms_occurrences= _ } [@warning "+missing-record-field-pattern"] ) =
      VarUF.is_empty var_eqs && Var.Map.is_empty const_eqs && Var.Map.is_empty type_constraints
      && Var.Map.is_empty linear_eqs && term_eqs_is_empty term_eqs && Var.Map.is_empty tableau
      && Var.Map.is_empty intervals && Atom.Set.is_empty atoms


    (* {2 [term_eqs] interface due to the totally opaque type} *)

    let get_term_eq phi t =
      Term.VarMap.find_opt t phi.term_eqs |> Option.map ~f:(get_repr_as_var phi)


    let term_eqs_fold f phi init =
      Term.VarMap.fold (fun t x acc -> f t (get_repr_as_var phi x) acc) phi.term_eqs init


    let term_eqs_iter f phi = Term.VarMap.iter (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs

    let term_eqs_exists f phi =
      Term.VarMap.exists (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs


    let term_eqs_filter f phi =
      Term.VarMap.filter (fun t x -> f t (get_repr_as_var phi x)) phi.term_eqs


    let fold_term_eqs_vars phi ~init ~f =
      let f_eq term var acc =
        Term.fold_variables term ~f ~init:(f acc (get_repr phi var :> Var.t))
      in
      Term.VarMap.fold f_eq phi.term_eqs init


    let subst_term_eqs subst phi =
      term_eqs_fold
        (fun t v acc ->
          let t' = Term.subst_variables t ~f:(targetted_subst_var subst) in
          let v' = subst_f subst v in
          Term.VarMap.add t' v' acc )
        phi Term.VarMap.empty


    let pp_term_eqs_with_pp_var ?(filter = fun _ _ -> true) pp_var fmt phi =
      (* Change this to get the raw data in the map, otherwise by default print what clients would
         get out of the map. The only difference is in the range of the map: whether variables get
         through [get_repr] or not before being printed. *)
      let print_raw_map = false in
      if print_raw_map then Term.VarMap.pp_with_pp_var pp_var fmt phi.term_eqs
      else
        (* inspired by {!Pp.collection} *)
        let is_first = ref true in
        F.pp_open_hvbox fmt 0 ;
        term_eqs_iter
          (fun term var ->
            if filter term var then (
              F.pp_open_hbox fmt () ;
              if not !is_first then F.pp_print_string fmt "∧" ;
              is_first := false ;
              F.fprintf fmt "%a=%a@]" (Term.pp pp_var) term pp_var var ) )
          phi ;
        F.pp_close_box fmt ()


    let pp_with_pp_var pp_var fmt
        ( ({ var_eqs
           ; const_eqs
           ; type_constraints
           ; linear_eqs
           ; term_eqs
           ; tableau
           ; intervals
           ; atoms
           ; linear_eqs_occurrences
           ; tableau_occurrences
           ; term_eqs_occurrences
           ; atoms_occurrences } [@warning "+missing-record-field-pattern"] ) as phi ) =
      let is_first = ref true in
      let pp_if condition header pp fmt x =
        let pp_and fmt = if not !is_first then F.fprintf fmt "@;&& " else is_first := false in
        if condition then F.fprintf fmt "%t%s: %a" pp_and header pp x
      in
      F.pp_open_hvbox fmt 0 ;
      if is_empty phi then F.pp_print_string fmt "(empty)" ;
      (pp_if (not (VarUF.is_empty var_eqs)) "var_eqs" (VarUF.pp pp_var)) fmt var_eqs ;
      (pp_if
         (not (Var.Map.is_empty const_eqs))
         "const_eqs"
         (pp_var_map ~arrow:"=" (Term.pp pp_var) pp_var) )
        fmt const_eqs ;
      (pp_if
         (not (Var.Map.is_empty type_constraints))
         "type_constraints"
         (InstanceOf.pp_with_pp_var pp_var) )
        fmt type_constraints ;
      (pp_if
         (not (Var.Map.is_empty linear_eqs))
         "linear_eqs"
         (pp_var_map ~arrow:" = " (LinArith.pp pp_var) pp_var) )
        fmt linear_eqs ;
      (pp_if (not (term_eqs_is_empty term_eqs)) "term_eqs" (pp_term_eqs_with_pp_var pp_var)) fmt phi ;
      (pp_if (not (Var.Map.is_empty tableau)) "tableau" (Tableau.pp pp_var)) fmt tableau ;
      (pp_if (not (Var.Map.is_empty intervals)) "intervals" (pp_var_map ~arrow:"" CItv.pp pp_var))
        fmt intervals ;
      (pp_if (not (Atom.Set.is_empty atoms)) "atoms" (Atom.Set.pp_with_pp_var pp_var)) fmt atoms ;
      if Config.debug_level_analysis >= 3 then (
        (pp_if
           (not (Var.Map.is_empty linear_eqs_occurrences))
           "linear_eqs_occurrences" (VarMapOccurrences.pp pp_var) )
          fmt linear_eqs_occurrences ;
        (pp_if
           (not (Var.Map.is_empty tableau_occurrences))
           "tableau_occurrences" (VarMapOccurrences.pp pp_var) )
          fmt tableau_occurrences ;
        (pp_if
           (not (Var.Map.is_empty term_eqs_occurrences))
           "term_eqs_occurrences" (TermMapOccurrences.pp pp_var) )
          fmt term_eqs_occurrences ;
        (pp_if
           (not (Var.Map.is_empty atoms_occurrences))
           "atoms_occurrences" (AtomMapOccurrences.pp pp_var) )
          fmt atoms_occurrences ) ;
      F.pp_close_box fmt ()


    (* {2 mutations} *)

    let add_const_eq v t phi =
      Debug.p "add_const_eq %a->%a@\n" Var.pp v (Term.pp Var.pp) t ;
      match Var.Map.find_opt v phi.const_eqs with
      | None ->
          Sat {phi with const_eqs= Var.Map.add v t phi.const_eqs}
      | Some t' ->
          if Term.equal_syntax t t' then Sat phi else Unsat


    let remove_const_eq v phi =
      Debug.p "remove_const_eq for %a@\n" Var.pp v ;
      {phi with const_eqs= Var.Map.remove v phi.const_eqs}


    let add_dynamic_type v t ?source_file phi =
      match Var.Map.find_opt v phi.type_constraints with
      | None ->
          ( { phi with
              type_constraints=
                Var.Map.add v (InstanceOf.Known {typ= t; source_file}) phi.type_constraints }
          , false )
      | Some (InstanceOf.Known {typ= t'}) ->
          if Typ.equal t t' then (phi, false) else (phi, true)
      | Some (InstanceOf.Unknown {below; notbelow}) ->
          if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t t' |> not) then (phi, true)
          else if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then (phi, true)
          else
            ( { phi with
                type_constraints=
                  Var.Map.add v (InstanceOf.Known {typ= t; source_file= None}) phi.type_constraints
              }
            , false )


    let add_below v t phi =
      Debug.p "add_below %a %a@\n" Var.pp v (Typ.pp_full Pp.text) t ;
      if InstanceOf.is_final t then
        if InstanceOf.is_abstract t then (
          (* Not sure this ever actually happens *)
          Debug.p "type is abstract and final so v=0" ;
          (phi, true) )
        else (
          Debug.p "type is final so adding as known dynamic type" ;
          add_dynamic_type v t phi )
      else
        match Var.Map.find_opt v phi.type_constraints with
        | None ->
            Debug.p "not found so adding below constraint@\n" ;
            let phi =
              { phi with
                type_constraints=
                  Var.Map.add v (InstanceOf.Unknown {below= [t]; notbelow= []}) phi.type_constraints
              }
            in
            Debug.p "new phi is %a@\n" (pp_with_pp_var Var.pp) phi ;
            (phi, false)
        | Some (InstanceOf.Known {typ= t'}) ->
            (phi, not (InstanceOf.is_subtype t' t))
        | Some (InstanceOf.Unknown {below; notbelow}) ->
            if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t' t) then
              (* new below constraint is redundant with an existing one *) (phi, false)
            else if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then
              (* New below constraint is incompatible with a notbelow one.
                 Note that once we know the value is zero, we shouldn't care
                 any more what type_constraints says about it, so we don't
                 bother to update, or do any further checks
              *) (phi, true)
            else if
              InstanceOf.is_concrete_or_abstract t
              && List.exists below ~f:(fun t' ->
                     InstanceOf.is_concrete_or_abstract t'
                     && (not (InstanceOf.is_subtype t t'))
                     && not (InstanceOf.is_subtype t' t) )
            then (
              Debug.p "inconsistent concrete upper bounds %a and <something>, zeroing"
                (Typ.pp Pp.text) t ;
              (phi, true) )
            else
              ( { phi with
                  type_constraints=
                    Var.Map.add v
                      (InstanceOf.Unknown {below= t :: below; notbelow})
                      phi.type_constraints }
              , false )


    let add_notbelow v t phi =
      match Var.Map.find_opt v phi.type_constraints with
      | None ->
          Debug.p "couldn't find %a in type constraints, writing singleton notbelow" Var.pp v ;
          ( { phi with
              type_constraints=
                Var.Map.add v (InstanceOf.Unknown {below= []; notbelow= [t]}) phi.type_constraints
            }
          , false )
      | Some (InstanceOf.Known {typ= t'}) ->
          if InstanceOf.is_subtype t' t then (phi, true) else (phi, false)
      | Some (InstanceOf.Unknown {below; notbelow}) ->
          (* new notbelow constraint is redundant with an existing one *)
          if List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype t t') then (
            Debug.p "adding %a to notbelow is redundant" (Typ.pp_full Pp.text) t ;
            (phi, false) )
          else if List.exists below ~f:(fun t' -> InstanceOf.is_subtype t' t) then (
            Debug.p "adding %a to notbelow is inconsistent" (Typ.pp_full Pp.text) t ;
            (phi, true) )
          else (
            Debug.p "actually going ahead and adding %a to notbelow" (Typ.pp_full Pp.text) t ;
            ( { phi with
                type_constraints=
                  Var.Map.add v
                    (InstanceOf.Unknown {below; notbelow= t :: notbelow})
                    phi.type_constraints }
            , false ) )


    let copy_type_constraints v_src v_target phi =
      match Var.Map.find_opt v_src phi.type_constraints with
      | None ->
          phi
      | Some src_constraints -> (
        match Var.Map.find_opt v_target phi.type_constraints with
        | None ->
            {phi with type_constraints= Var.Map.add v_target src_constraints phi.type_constraints}
        | Some _ ->
            L.die InternalError "Failed attempt to copy type constraints" )


    let remove_term_eq t v phi =
      Debug.p "remove_term_eq %a->%a in %a@\n" (Term.pp Var.pp) t Var.pp v (pp_with_pp_var Var.pp)
        phi ;
      let term_eqs_occurrences =
        match Term.get_as_linear t with
        | Some _ ->
            phi.term_eqs_occurrences
        | None ->
            Term.fold_variables t ~init:phi.term_eqs_occurrences ~f:(fun occurrences v' ->
                TermMapOccurrences.remove v' ~occurred_in:(t, Domain) occurrences )
            |> TermMapOccurrences.remove v ~occurred_in:(t, Range)
            |> TermMapOccurrences.remove v ~occurred_in:(t, DomainAndRange)
      in
      {phi with term_eqs= Term.VarMap.remove t phi.term_eqs; term_eqs_occurrences}


    let add_term_eq t v phi =
      Debug.p "add_term_eq %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
      match get_term_eq phi t with
      | Some v' when Var.equal v v' ->
          (phi, None)
      | (Some _ | None) as new_eq ->
          let term_eqs_occurrences =
            match Term.get_as_linear t with
            | Some _ ->
                phi.term_eqs_occurrences
            | None ->
                let term_eqs_occurrences, added_to_range =
                  Term.fold_variables t ~init:(phi.term_eqs_occurrences, false)
                    ~f:(fun (occurrences, added_to_range) v' ->
                      let target, added_to_range =
                        if Var.equal v v' then (TermDomainOrRange.DomainAndRange, true)
                        else (TermDomainOrRange.Domain, added_to_range)
                      in
                      (TermMapOccurrences.add v' ~occurs_in:(t, target) occurrences, added_to_range) )
                in
                if added_to_range then term_eqs_occurrences
                else TermMapOccurrences.add v ~occurs_in:(t, Range) term_eqs_occurrences
          in
          ({phi with term_eqs= Term.VarMap.add t v phi.term_eqs; term_eqs_occurrences}, new_eq)


    let remove_linear_eq v l phi =
      Debug.p "remove_linear_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let linear_eqs_occurrences =
        LinArith.get_variables l
        |> Seq.fold_left
             (fun occurrences v' -> VarMapOccurrences.remove v' ~occurred_in:v occurrences)
             phi.linear_eqs_occurrences
      in
      let phi = {phi with linear_eqs= Var.Map.remove v phi.linear_eqs; linear_eqs_occurrences} in
      let t = Linear l |> Term.simplify_linear in
      match get_term_eq phi t with
      | Some v' when Var.equal v v' ->
          remove_term_eq t v phi
      | _ ->
          phi


    let add_linear_eq v l phi =
      Debug.p "add_linear_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let phi =
        match Var.Map.find_opt v phi.linear_eqs with
        | Some l_old ->
            (* get rid of soon-to-be-junk occurrences and [term_eqs] associated with the previous
               binding *)
            remove_linear_eq v l_old phi
        | None ->
            phi
      in
      let linear_eqs_occurrences =
        LinArith.get_variables l
        |> Seq.fold_left
             (fun occurrences v' -> VarMapOccurrences.add v' ~occurs_in:v occurrences)
             phi.linear_eqs_occurrences
      in
      let phi = {phi with linear_eqs= Var.Map.add v l phi.linear_eqs; linear_eqs_occurrences} in
      (* update [term_eqs] too with the reverse equality *)
      add_term_eq (Linear l |> Term.simplify_linear) v phi


    let remove_tableau_eq v l phi =
      Debug.p "remove_tableau_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let tableau_occurrences =
        LinArith.get_variables l
        |> Seq.fold_left
             (fun occurrences v' -> VarMapOccurrences.remove v' ~occurred_in:v occurrences)
             phi.tableau_occurrences
      in
      {phi with tableau= Var.Map.remove v phi.tableau; tableau_occurrences}


    let add_tableau_eq v l phi =
      Debug.p "add_tableau_eq %a=%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let phi =
        match Var.Map.find_opt v phi.tableau with
        | Some l_old ->
            (* get rid of soon-to-be-junk occurrences associated with the previous
               binding *)
            remove_tableau_eq v l_old phi
        | None ->
            phi
      in
      let tableau_occurrences =
        LinArith.get_variables l
        |> Seq.fold_left
             (fun occurrences v' -> VarMapOccurrences.add v' ~occurs_in:v occurrences)
             phi.tableau_occurrences
      in
      {phi with tableau= Var.Map.add v l phi.tableau; tableau_occurrences}


    let add_interval_ v intv intervals =
      let+ possibly_better_intv =
        match Var.Map.find_opt v intervals with
        | None ->
            Sat intv
        | Some intv' ->
            Debug.p "intersection %a*%a@\n" CItv.pp intv CItv.pp intv' ;
            CItv.intersection intv intv' |> SatUnsat.of_option
      in
      Debug.p "adding %a%a@\n" Var.pp v CItv.pp possibly_better_intv ;
      Var.Map.add v possibly_better_intv intervals


    let add_interval v intv phi =
      let+ intervals = add_interval_ v intv phi.intervals in
      {phi with intervals}


    let add_atom atom phi =
      let atoms_occurrences =
        Atom.fold_variables atom ~init:phi.atoms_occurrences ~f:(fun occurrences v' ->
            AtomMapOccurrences.add v' ~occurs_in:atom occurrences )
      in
      {phi with atoms= Atom.Set.add atom phi.atoms; atoms_occurrences}


    let remove_atom atom phi =
      let atoms_occurrences =
        Atom.fold_variables atom ~init:phi.atoms_occurrences ~f:(fun occurrences v' ->
            AtomMapOccurrences.remove v' ~occurred_in:atom occurrences )
      in
      {phi with atoms= Atom.Set.remove atom phi.atoms; atoms_occurrences}


    let remove_from_linear_eqs_occurrences v phi =
      {phi with linear_eqs_occurrences= Var.Map.remove v phi.linear_eqs_occurrences}


    let remove_from_tableau_occurrences v phi =
      {phi with tableau_occurrences= Var.Map.remove v phi.tableau_occurrences}


    let add_occurrence_to_range_of_term_eq t (v : VarUF.repr) phi =
      { phi with
        term_eqs_occurrences=
          TermMapOccurrences.add (v :> Var.t) ~occurs_in:(t, Range) phi.term_eqs_occurrences }


    let remove_from_term_eqs_occurrences v phi =
      {phi with term_eqs_occurrences= Var.Map.remove v phi.term_eqs_occurrences}


    let remove_from_atoms_occurrences v phi =
      {phi with atoms_occurrences= Var.Map.remove v phi.atoms_occurrences}


    let set_var_eqs var_eqs phi = if phys_equal phi.var_eqs var_eqs then phi else {phi with var_eqs}

    let set_tableau tableau phi = {phi with tableau}

    let set_intervals intervals phi = {phi with intervals}

    let unsafe_mk ~var_eqs ~const_eqs ~type_constraints ~linear_eqs ~term_eqs ~tableau ~intervals
        ~atoms ~linear_eqs_occurrences ~tableau_occurrences ~term_eqs_occurrences ~atoms_occurrences
        =
      { var_eqs
      ; const_eqs
      ; type_constraints
      ; linear_eqs
      ; term_eqs
      ; tableau
      ; intervals
      ; atoms
      ; linear_eqs_occurrences
      ; tableau_occurrences
      ; term_eqs_occurrences
      ; atoms_occurrences }
  end

  include Unsafe

  let fold_constant_var_map map ~init ~f =
    let f var _constant acc = f acc var in
    Var.Map.fold f map init


  let fold_type_constraints_map map ~init ~f =
    let f var _constraint acc = f acc var in
    Var.Map.fold f map init


  let fold_linear_eqs_vars linear_eqs ~init ~f =
    let f_eq var linarith acc = Seq.fold_left f (f acc var) (LinArith.get_variables linarith) in
    Var.Map.fold f_eq linear_eqs init


  let fold_variables
      ( ({ var_eqs
         ; const_eqs
         ; type_constraints
         ; linear_eqs
         ; term_eqs= _
         ; tableau
         ; intervals
         ; atoms
         ; linear_eqs_occurrences= _
         ; tableau_occurrences= _
         ; term_eqs_occurrences= _
         ; atoms_occurrences= _ } [@warning "+missing-record-field-pattern"] ) as phi ) ~init ~f =
    let init = VarUF.fold_elements var_eqs ~init ~f in
    let init = fold_constant_var_map const_eqs ~init ~f in
    let init = fold_type_constraints_map type_constraints ~init ~f in
    let init = fold_linear_eqs_vars linear_eqs ~init ~f in
    let init = fold_term_eqs_vars phi ~init ~f in
    let init = fold_linear_eqs_vars tableau ~init ~f in
    let init = fold_constant_var_map intervals ~init ~f in
    Atom.Set.fold (fun atom acc -> Atom.fold_variables atom ~init:acc ~f) atoms init


  let is_neq_zero phi t =
    Term.get_as_var t
    |> Option.exists ~f:(fun v ->
           Var.Map.find_opt v phi.intervals |> Option.exists ~f:CItv.is_not_equal_to_zero )
    || Atom.Set.mem (NotEqual (t, Term.zero)) phi.atoms
    || Atom.Set.mem (LessThan (Term.zero, t)) phi.atoms


  let is_non_pointer {var_eqs; linear_eqs; intervals; atoms} var =
    let repr = (VarUF.find var_eqs var :> Var.t) in
    Option.exists (Var.Map.find_opt repr linear_eqs) ~f:(fun v ->
        Option.is_some (LinArith.get_as_const v) )
    || Option.exists (Var.Map.find_opt repr intervals) ~f:CItv.is_non_pointer
    || Atom.Set.mem (Equal (IsInt (Var repr), Term.one)) atoms


  (** module that breaks invariants more often that the rest, with an interface that is safer to use *)
  module Normalizer : sig
    val and_var_linarith : Var.t -> LinArith.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_var_term : Var.t -> Term.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_var_var : Var.t -> Var.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val normalize_atom : t -> Atom.t -> Atom.t list SatUnsat.t

    val and_normalized_atoms : t * new_eqs -> Atom.t list -> (t * new_eqs) SatUnsat.t
    (** use with the result of {!normalize_atom} in place of {!and_atom} *)

    val and_atom : Atom.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_dynamic_type :
      Var.t -> Typ.t -> ?source_file:SourceFile.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_below : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_notbelow : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val propagate_atom : Atom.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t
    (** [and_atom atom (phi, new_eqs)] is
        [SatUnsat.(normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs))] *)
  end = struct
    (* Use the monadic notations when normalizing formulas. *)
    open SatUnsat.Import

    (** OVERVIEW: the best way to think about this is as a (somewhat half-assed) Shostak technique.

        The [var_eqs] and [linear_eqs] parts of a formula are kept in a normal form of sorts. We
        apply some deduction every time a new equality is discovered. Where this is incomplete is
        that we stop discovering new consequences of facts after some fixed number of steps (the
        [fuel] argument of some of the functions of this module). saturating the consequences of
        what we know implies keeping track of *all* the consequences (to avoid diverging by
        re-discovering the same facts over and over), which would be expensive. *)

    exception OutOfFuel of (t * new_eqs * (F.formatter -> unit))

    (* the only way to initialize fuel: no functions in the interface of this module
       take fuel as argument, and no functions in this module pass concrete fuel values,
       so this will always catch [OutOfFuel] exceptions and these exceptions will not
       escape this module *)
    let with_base_fuel f =
      (* an arbitrary value *)
      let base_fuel = 10 in
      try f ~fuel:base_fuel
      with OutOfFuel (phi, new_eqs, why) ->
        L.d_printfln "%t" why ;
        Sat (phi, new_eqs)


    let normalize_linear_ phi linear_eqs l =
      LinArith.subst_variables l ~f:(fun v ->
          let repr = (get_repr phi v :> Var.t) in
          match Var.Map.find_opt repr linear_eqs with
          | None ->
              VarSubst repr
          | Some l' ->
              LinSubst l' )


    (** substitute vars in [l] *once* with their linear form to discover more simplification
        opportunities *)
    let normalize_linear phi l = normalize_linear_ phi phi.linear_eqs l

    (** same as [normalize_linear] but for slack variables, used for inequalities *)
    let normalize_restricted phi l = normalize_linear_ phi phi.tableau l

    let normalize_var_const phi t =
      Debug.p "normalize_var_const initial term is %a@\n" (Term.pp Var.pp) t ;
      let t' =
        Term.subst_variables t ~f:(fun v ->
            let v_canon = (get_repr phi v :> Var.t) in
            match Var.Map.find_opt v_canon phi.linear_eqs with
            | None -> (
              match Var.Map.find_opt v_canon phi.const_eqs with
              | None ->
                  VarSubst v_canon
              | Some c ->
                  ConstantSubst (c, Some v_canon) )
            | Some l -> (
              match LinArith.get_as_const l with
              | None ->
                  (* OPTIM: don't make the term bigger *) VarSubst v_canon
              | Some q ->
                  (* replace vars by constants when available to possibly trigger further
                     simplifications in atoms. This is not actually needed for [term_eqs]. *)
                  QSubst q ) )
      in
      Debug.p "normalized term is %a@\n" (Term.pp Var.pp) t' ;
      t'


    let add_lin_eq_to_new_eqs v l new_eqs =
      match LinArith.get_as_const l with
      | Some q when Q.is_zero q ->
          RevList.cons (EqZero v) new_eqs
      | _ ->
          new_eqs


    (** add [l1 = l2] to [phi.linear_eqs] and resolves consequences of that new fact

        [l1] and [l2] should have already been through {!normalize_linear} (w.r.t. [phi]) *)
    let rec solve_normalized_lin_eq ~fuel ?(force_no_tableau = false) new_eqs l1 l2 phi =
      Debug.p "solve_normalized_lin_eq: %a=%a@\n" (LinArith.pp Var.pp) l1 (LinArith.pp Var.pp) l2 ;
      LinArith.solve_eq l1 l2
      >>= function
      | None ->
          Sat (phi, new_eqs)
      | Some (v, l) -> (
        match LinArith.get_as_var l with
        | Some v' ->
            merge_vars ~fuel new_eqs v v' phi
        | None -> (
            let* phi, new_eqs =
              if (not force_no_tableau) && Var.is_restricted v && LinArith.is_restricted l then
                (* linear equalities between restricted variables can be forwarded to [tableau] *)
                solve_tableau_restricted_eq ~fuel new_eqs v l phi
              else Sat (phi, new_eqs)
            in
            match Var.Map.find_opt v phi.linear_eqs with
            | None ->
                (* add to the [term_eqs] relation only when we also add to [linear_eqs] *)
                let* phi, new_eqs =
                  solve_normalized_term_eq_no_lin ~fuel new_eqs (Term.Linear l) v phi
                in
                (* the rep might be changed by [solve_normalized_term_eq_no_lin] *)
                let v = (get_repr phi v :> Var.t) in
                let new_eqs = add_lin_eq_to_new_eqs v l new_eqs in
                (* this can break the invariant that variables in the domain of [linear_eqs] do not
                   appear in the range of [linear_eqs], restore it *)
                add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v l phi
                >>= propagate_linear_eq ~fuel v l
            | Some l' ->
                (* This is the only step that consumes fuel: discovering an equality [l = l']: because we
                   do not record these anywhere (except when their consequence can be recorded as [y =
                   l''] or [y = y']), we could potentially discover the same equality over and over and
                   diverge otherwise. Or could we?) *)
                let* phi, new_eqs = propagate_linear_eq ~fuel v l (phi, new_eqs) in
                if fuel > 0 then (
                  L.d_printfln "Consuming fuel solving linear equality (from %d)" fuel ;
                  solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs l l' phi )
                else
                  (* [fuel = 0]: give up simplifying further for fear of diverging *)
                  raise
                    (OutOfFuel
                       ( phi
                       , new_eqs
                       , fun fmt -> F.fprintf fmt "Ran out of fuel solving linear equality" ) ) ) )


    and discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi =
      match new_eq_opt with
      | None ->
          Sat (phi, new_eqs)
      | Some v' ->
          merge_vars ~fuel new_eqs v v' phi


    (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; don't use directly
        as it doesn't do any checks on what else should be done about [t = v] *)
    and add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi =
      Debug.p "add_term_eq_and_solve_new_eq_opt %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
      let phi, new_eq_opt = add_term_eq t v phi in
      discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


    and add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v l phi =
      Debug.p "add_linear_eq_and_merge_new_eq_opt %a->%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let phi, new_eq_opt = add_linear_eq v l phi in
      discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


    (** TODO: at the moment this doesn't try to discover and return new equalities implied by the
        tableau (see Chapter 5 in \[2\]) *)
    and solve_tableau_restricted_eq ?(force_no_lin_arith = false) ~fuel new_eqs w l phi =
      Debug.p "tableau %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
      let l_c = LinArith.get_constant_part l in
      let l_c_sign =
        if Q.(l_c > zero) then `Positive else if Q.(l_c = zero) then `Zero else `Negative
      in
      match (l_c_sign, LinArith.classify_minimized_maximized l) with
      | `Zero, (`Maximized | `Constant) ->
          (* [w = k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0, so [w] = [v1] = ... = [vn] = 0. *)
          let* phi, new_eqs =
            if force_no_lin_arith then Sat (phi, new_eqs)
            else
              solve_normalized_lin_eq ~force_no_tableau:true ~fuel new_eqs (LinArith.of_var w)
                (LinArith.of_q Q.zero) phi
          in
          LinArith.get_variables l
          |> SatUnsat.seq_fold ~init:(phi, new_eqs) ~f:(fun (phi, new_eqs) v ->
                 merge_vars ~fuel new_eqs w v phi )
      | (`Positive | `Zero), (`Minimized | `Constant) ->
          (* [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≥0 (and so are all possible
             values of all [vi]s since they are restricted variables), hence any possible value
             of [w] is ≥0 so [w ≥ 0] is a tautologie and we can just discard the atom *)
          Debug.p "Tautology@\n" ;
          Sat (phi, new_eqs)
      | (`Positive | `Zero), (`Maximized | `Neither) ->
          (* [w = l] is feasible and not a tautologie (and [l] is restricted), add to the
             tableau *)
          let phi = add_tableau_eq w l phi in
          Debug.p "Add to tableau@\n" ;
          Sat (phi, new_eqs)
      | `Negative, (`Maximized | `Constant) ->
          (* [l_c < 0], [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0 so [l] denotes
             only negative values, hence cannot be ≥0: contradiction *)
          Debug.p "Contradiction!@\n" ;
          Unsat
      | `Negative, (`Minimized | `Neither) -> (
        (* stuck, let's pivot to try to add a feasible equality to the tableau; there are two ways
           to pivot in \[2\] *)
        match Tableau.pivot_unbounded_with_positive_coeff phi.tableau w l with
        | Some tableau ->
            Sat (set_tableau tableau phi, new_eqs)
        | None ->
            if fuel > 0 then (
              Debug.p "PIVOT %d in %a@\n" fuel (Tableau.pp Var.pp) phi.tableau ;
              match Tableau.pivot l phi.tableau with
              | None ->
                  (* Huho, we cannot put the equality in a feasible form (i.e. [u = c + k1·v1 +
                     ... + kn·vn] with [c≥0]). This isn't supposed to happen in theory but because
                     we're a bit sloppy with normalization we can exceptionally get there in
                     practice. Store it as an unrestricted linear equality to avoid losing
                     completeness and hope a later re-normalization will take care of it better. *)
                  L.debug Analysis Verbose "No pivot found for %a in %a@\n" (LinArith.pp Var.pp) l
                    (Tableau.pp Var.pp) phi.tableau ;
                  (* set [force_no_tableau] so that the equality won't just ping-pong back to here *)
                  solve_normalized_lin_eq ~fuel ~force_no_tableau:true new_eqs (LinArith.of_var w)
                    (normalize_restricted phi l) phi
              | Some tableau ->
                  let phi = set_tableau tableau phi in
                  Debug.p "pivoted tableau: %a@\n" (Tableau.pp Var.pp) phi.tableau ;
                  solve_tableau_restricted_eq ~fuel:(fuel - 1) new_eqs w
                    (normalize_restricted phi l) phi )
            else
              raise
                (OutOfFuel
                   ( phi
                   , new_eqs
                   , fun fmt ->
                       F.fprintf fmt "Ran out of fuel pivoting the tableau %a@\n"
                         (Tableau.pp Var.pp) phi.tableau ) ) )


    (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; assumes that
        linear facts have been or will be added to [phi.linear_eqs] separately

        [t] should have already been through {!normalize_var_const} and [v] should be a
        representative from {!get_repr} (w.r.t. [phi]) *)
    and solve_normalized_term_eq_no_lin ~fuel new_eqs (t : Term.t) v phi =
      Debug.p "solve_normalized_term_eq_no_lin %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
      match t with
      | Linear l when LinArith.get_as_var l |> Option.is_some ->
          (* [v1=v2] is already taken care of by [var_eqs] *)
          Sat (phi, new_eqs)
      | _ ->
          let t =
            match t with
            | Linear l -> (
              match LinArith.get_as_const l with Some c -> Term.Const c | None -> t )
            | _ ->
                t
          in
          add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi


    (** same as {!solve_normalized_eq_no_lin} but also adds linear to [phi.linear_eqs] *)
    and solve_normalized_term_eq ~fuel new_eqs (t : Term.t) v phi =
      Debug.p "solve_normalized_term_eq: %a=%a in %a, new_eqs=%a@\n" (Term.pp Var.pp) t Var.pp v
        (pp_with_pp_var Var.pp) phi pp_new_eqs new_eqs ;
      match t with
      | Var v' ->
          merge_vars ~fuel new_eqs v v' phi
      | Linear l when LinArith.get_as_var l |> Option.is_some ->
          let v' = Option.value_exn (LinArith.get_as_var l) in
          merge_vars ~fuel new_eqs v v' phi
      | Linear l -> (
          (* [l = v]: need to first solve it to get [l' = v'] such that [v' < vars(l')], and [l'] is
             normalized wrt [phi.linear_eqs] (to get a canonical form), add this to [term_eqs], then
             in order to know which equality should be added to [linear_eqs] we still need to
             substitute [v'] with [linear_eqs] and solve again *)
          LinArith.solve_eq (normalize_linear phi l) (LinArith.of_var v)
          >>= function
          | None ->
              (* [v = v], we can drop this tautology *)
              Sat (phi, new_eqs)
          | Some (v', l') ->
              (* [l'] could contain [v], which hasn't been through [linear_eqs], so normalize again *)
              let l' = normalize_linear phi l' in
              let* phi, new_eqs =
                add_term_eq_and_solve_new_eq_opt ~fuel new_eqs (Term.Linear l') v' phi
              in
              solve_normalized_lin_eq ~fuel new_eqs l'
                (normalize_linear phi (LinArith.of_var v'))
                phi )
      | Const c ->
          (* same as above but constants ([c]) are always normalized so it's simpler *)
          let* phi, new_eqs = add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi in
          solve_normalized_lin_eq ~fuel new_eqs (LinArith.of_q c) (LinArith.of_var v) phi
      | String _ ->
          (* same as above for non-numeric constants *)
          let* phi = add_const_eq v t phi in
          add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi >>= propagate_term_eq ~fuel t v
      | _ ->
          solve_normalized_term_eq_no_lin ~fuel new_eqs t v phi >>= propagate_term_eq ~fuel t v


    and merge_vars ~fuel new_eqs v1 v2 phi =
      Debug.p "merge_vars: %a=%a@\n" Var.pp v1 Var.pp v2 ;
      let var_eqs, subst_opt = VarUF.union phi.var_eqs v1 v2 in
      let phi = set_var_eqs var_eqs phi in
      match subst_opt with
      | None ->
          (* we already knew the equality *)
          Debug.p "we already knew %a=%a@\n" Var.pp v1 Var.pp v2 ;
          Sat (phi, new_eqs)
      | Some (v_old, v_new) ->
          (* new equality [v_old = v_new]: we need to propagate this fact to the various domains,
             especially [linear_eqs]: we update a potential [v_old = l_old] to be [v_new = l_old],
             and if [v_new = l_new] was known we add [l_old = l_new] *)
          let v_new = (v_new :> Var.t) in
          Debug.p "new eq: %a->%a@\n" Var.pp v_old Var.pp v_new ;
          L.d_printfln "new eq: %a = %a" Var.pp v_old Var.pp v_new ;
          let new_eqs = RevList.cons (Equal (v_old, v_new)) new_eqs in
          (* substitute [v_old -> v_new] in [phi.linear_eqs] while maintaining the [linear_eqs]
             invariant *)
          propagate_var_eq ~fuel v_old v_new (phi, new_eqs)


    and propagate_in_const_eqs x y (phi, new_eqs) =
      Debug.p "[propagate_in_const_eqs] %a=%a@\n  @[" Var.pp x Var.pp y ;
      let r =
        match Var.Map.find_opt x phi.const_eqs with
        | None ->
            Sat (phi, new_eqs)
        | Some c -> (
            let phi = remove_const_eq x phi in
            match Var.Map.find_opt y phi.const_eqs with
            | None ->
                let+ phi = add_const_eq y c phi in
                (phi, new_eqs)
            | Some c' ->
                if Term.equal_syntax c c' then Sat (phi, new_eqs) else Unsat )
      in
      Debug.p "@]end [propagate_in_const_eqs] %a=%a@\n" Var.pp x Var.pp y ;
      r


    and propagate_in_linear_eqs_domain ~fuel v_old l (phi, new_eqs) =
      match LinArith.get_as_var l with
      | None ->
          Sat (phi, new_eqs)
      | Some v_new -> (
          Debug.p "[propagate_in_linear_eqs_domain] %a->%a@\n" Var.pp v_old Var.pp v_new ;
          let l_new = Var.Map.find_opt v_new phi.linear_eqs in
          let phi, l_old =
            match Var.Map.find_opt v_old phi.linear_eqs with
            | None ->
                (phi, None)
            | Some l_old ->
                (remove_linear_eq v_old l_old phi, Some l_old)
          in
          match (l_old, l_new) with
          | None, None ->
              Sat (phi, new_eqs)
          | None, Some l ->
              let new_eqs = add_lin_eq_to_new_eqs v_new l new_eqs in
              Sat (phi, new_eqs)
          | Some l, None ->
              let new_eqs = add_lin_eq_to_new_eqs v_new l new_eqs in
              add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v_new l phi
              >>= propagate_linear_eq ~fuel v_new l
          | Some l1, Some l2 ->
              let new_eqs = add_lin_eq_to_new_eqs v_new l1 new_eqs in
              let new_eqs = add_lin_eq_to_new_eqs v_new l2 new_eqs in
              (* no need to consume fuel here as we can only go through this branch finitely many
                 times because there are finitely many variables in a given formula *)
              (* TODO: we may want to keep the "simpler" representative for [v_new] between [l1] and [l2] *)
              solve_normalized_lin_eq ~fuel new_eqs l1 l2 phi )


    and propagate_in_linear_eqs_range ~fuel x lx (phi, new_eqs) =
      Debug.p "[propagate_in_linear_eqs_range] %a=%a@\n  @[" Var.pp x (LinArith.pp Var.pp) lx ;
      let r =
        match Var.Map.find_opt x phi.linear_eqs_occurrences with
        | None ->
            Sat (phi, new_eqs)
        | Some in_linear_eqs ->
            (* [x=l] has been added to the linear equalities so by the invariant (that we are about to
               restore) there are no further occurrences of [x] in [phi.linear_eqs] *)
            let phi = remove_from_linear_eqs_occurrences x phi in
            Var.Set.fold
              (fun v phi_new_eqs_sat ->
                let* phi, new_eqs = phi_new_eqs_sat in
                match Var.Map.find_opt v phi.linear_eqs with
                | None ->
                    Debug.p "huh? no %a in linear eqs but %a claimed it was in a linear eq for %a"
                      Var.pp v Var.pp x Var.pp v ;
                    phi_new_eqs_sat
                | Some lv ->
                    (* renormalize [lx] in case we have discovered new equalities since the beginning
                       of the fold(!!) *)
                    let lx = normalize_linear phi lx in
                    Debug.p "substituting %a->%a in %a->%a@\n" Var.pp x (LinArith.pp Var.pp) lx
                      Var.pp v (LinArith.pp Var.pp) lv ;
                    let r =
                      let lv' = LinArith.subst_variable x (LinSubst lx) lv in
                      (* check the invariant that [v] is (strictly) simpler than any variable in
                         [lv']; because the invariant was true before it's enough to check that it's
                         simpler than any variable in [lx] *)
                      let needs_pivot =
                        match LinArith.get_simplest lx with
                        | None ->
                            false
                        | Some min ->
                            Var.is_simpler_or_equal min v
                      in
                      Debug.p "needs_pivot= %b@\n" needs_pivot ;
                      if needs_pivot then
                        (* need to pivot the equality [v = lv'] to restore the above invariant since
                           it's not in the right form *)
                        let phi = remove_linear_eq v lv phi in
                        solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs
                          (LinArith.of_var v |> normalize_linear phi)
                          lv' phi
                      else
                        (* To restore the linarith invariants just subst [x->lx] in [lv],
                           i.e. [lv']. Detect if this is a variable equality and propagate it as such
                           if so instead of adding it to [linear_eqs] (which aren't supposed to
                           contain plain variable equalities). *)
                        match LinArith.get_as_var lv' with
                        | Some v' ->
                            let phi = remove_linear_eq v lv phi in
                            merge_vars ~fuel new_eqs v v' phi
                        | _ ->
                            add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v lv' phi
                    in
                    Debug.p "@\nResult of %a->%a in %a->%a=@\n  @[%a@]@\n" Var.pp x
                      (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv
                      (SatUnsat.pp (fun fmt (phi, _) -> pp_with_pp_var Var.pp fmt phi))
                      r ;
                    r )
              in_linear_eqs
              (Sat (phi, new_eqs))
      in
      Debug.p "@]end [propagate_in_linear_eqs_range] %a=%a@\n" Var.pp x (LinArith.pp Var.pp) lx ;
      r


    and propagate_in_tableau ~fuel x lx (phi, new_eqs) =
      Debug.p "[propagate_in_tableau] %a=%a@\n  @[" Var.pp x (LinArith.pp Var.pp) lx ;
      let r =
        if not (Var.is_restricted x && LinArith.is_restricted lx) then Sat (phi, new_eqs)
        else
          let* phi, new_eqs =
            match Var.Map.find_opt x phi.tableau_occurrences with
            | None ->
                Sat (phi, new_eqs)
            | Some in_tableau ->
                (* [x=l] has been added to the tableau so by the invariant (that we are about to
                   restore) there are no further occurrences of [x] in [phi.tableau] *)
                let phi = remove_from_tableau_occurrences x phi in
                Var.Set.fold
                  (fun v phi_new_eqs_sat ->
                    let* phi, new_eqs = phi_new_eqs_sat in
                    match Var.Map.find_opt v phi.tableau with
                    | None ->
                        Debug.p "huh? no %a in tableau but %a claimed it was" Var.pp v Var.pp x ;
                        phi_new_eqs_sat
                    | Some lv ->
                        (* renormalize [lx] in case we have discovered new equalities since the beginning
                           of the fold(!!) *)
                        let lx = normalize_restricted phi lx in
                        Debug.p "tableau substituting %a->%a in %a->%a@\n" Var.pp x
                          (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv ;
                        let r =
                          let lv' = LinArith.subst_variable x (LinSubst lx) lv in
                          remove_tableau_eq v lv phi
                          |> solve_tableau_restricted_eq ~force_no_lin_arith:true ~fuel new_eqs v
                               lv'
                        in
                        Debug.p "@\nResult of %a->%a in %a->%a=@\n  @[%a@]@\n" Var.pp x
                          (LinArith.pp Var.pp) lx Var.pp v (LinArith.pp Var.pp) lv
                          (SatUnsat.pp (fun fmt (phi, _) -> pp_with_pp_var Var.pp fmt phi))
                          r ;
                        r )
                  in_tableau
                  (Sat (phi, new_eqs))
          in
          match Var.Map.find_opt x phi.tableau with
          | None ->
              Sat (phi, new_eqs)
          | Some l -> (
              let phi = remove_tableau_eq x l phi in
              let* tableau_eq_opt = LinArith.solve_eq l lx in
              match tableau_eq_opt with
              | None ->
                  Sat (phi, new_eqs)
              | Some (w, lw) ->
                  solve_tableau_restricted_eq ~force_no_lin_arith:true ~fuel new_eqs w lw phi )
      in
      Debug.p "@]end [propagate_in_tableau] %a=%a@\n" Var.pp x (LinArith.pp Var.pp) lx ;
      r


    and propagate_in_term_eqs ~fuel (tx : Term.t) x ((phi, new_eqs) as phi_new_eqs) =
      match Term.to_subst_target tx with
      | LinSubst _ | NonLinearTermSubst _ ->
          Debug.p "prop in term eqs tx=%a, x=%a being ignored" (Term.pp Var.pp) tx Var.pp x ;
          Sat phi_new_eqs
      | (VarSubst _ | QSubst _ | ConstantSubst _) as subst_target_x -> (
        match Var.Map.find_opt x phi.term_eqs_occurrences with
        | None ->
            Sat phi_new_eqs
        | Some in_term_eqs ->
            ( (* [tx=x] with [tx] a constant or a variable has been added to the term equalities so by
                 the invariant (that we are about to restore) there are no further occurrences of [x]
                 on the LHS in [phi.term_eqs] (and occurrences on the RHS are dealt on the fly by
                 [Formula.Unsafe]) *)
              Debug.p "term_eq propagating %a = %a in %a@\n" (Term.pp Var.pp) tx Var.pp x
                (pp_with_pp_var Var.pp) phi ;
              let phi = remove_from_term_eqs_occurrences x phi in
              TermDomainOrRange.Set.fold (fun (t, domain_or_range) phi_new_eqs_sat ->
                  if Term.equal t tx then phi_new_eqs_sat
                  else
                    match get_term_eq phi t with
                    | None ->
                        Debug.p "huh? %a was supposed to appear in %a@\n" Var.pp x (Term.pp Var.pp)
                          t ;
                        phi_new_eqs_sat
                    | Some y -> (
                      match domain_or_range with
                      | Range -> (
                          let* phi, new_eqs = phi_new_eqs_sat in
                          (* If t is an IsInstanceOf formula, and we've just found its truth value, propagate
                             the information into the below/notbelow type constraints on the relevant
                             variable, and also add >0 facts where appropriate *)
                          let* phi, new_eqs =
                            match Term.get_as_isinstanceof t with
                            | Some (var, typ, nullable) ->
                                if is_neq_zero phi tx then (
                                  Debug.p "prop in term_eq adding below with nullable=%b\n" nullable ;
                                  let* phi, new_eqs = and_below var typ (phi, new_eqs) in
                                  if not nullable then (
                                    Debug.p "adding %a not equal to zero" Var.pp var ;
                                    let* atoms =
                                      Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                        (NotEqual (Var var, Term.zero))
                                    in
                                    and_normalized_atoms (phi, new_eqs) atoms >>| snd )
                                  else Sat (phi, new_eqs) )
                                else if
                                  match tx with
                                  | Linear l ->
                                      LinArith.is_zero l
                                  | Const c ->
                                      Q.is_zero c
                                  | _ ->
                                      false
                                then (
                                  Debug.p "prop in term_eq adding notbelow@\n" ;
                                  let* phi, new_eqs = and_notbelow var typ (phi, new_eqs) in
                                  if nullable then
                                    let* atoms =
                                      Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                        (NotEqual (Var var, Term.zero))
                                    in
                                    and_normalized_atoms (phi, new_eqs) atoms >>| snd
                                  else Sat (phi, new_eqs) )
                                else (
                                  Debug.p "%a is neither zero nor non-zero, leaving phi alone@\n"
                                    (Term.pp Var.pp) tx ;
                                  Sat (phi, new_eqs) )
                            | None ->
                                Sat (phi, new_eqs)
                          in
                          (* Now check if the new equality on [x] introduced contradictions in [t=x] or new atoms *)
                          let* atoms_opt =
                            Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                              (Equal (t, Term.simplify_linear tx))
                          in
                          match atoms_opt with
                          | None ->
                              (* need to add back that [x] occurs in [term_eqs(t)] since we removed [x] from
                                 [term_eqs_occurrences] altogether before the fold; the repr of [x] might have
                                 changed from [subst_target_x] *)
                              let x' = get_repr phi x in
                              Debug.p "no relevant atoms, adding dependency %a->%a back@\n" Var.pp
                                (x' :> Var.t)
                                (Term.pp Var.pp) t ;
                              Sat (add_occurrence_to_range_of_term_eq t x' phi, new_eqs)
                          | Some atoms ->
                              Debug.p "Found new atoms %a@\n"
                                (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                                atoms ;
                              and_normalized_atoms (phi, new_eqs) atoms >>| snd )
                      | Domain | DomainAndRange -> (
                          let* phi, new_eqs = phi_new_eqs_sat in
                          let subst_target_x =
                            match subst_target_x with
                            | VarSubst v ->
                                VarSubst (get_repr phi v :> Var.t)
                            | _ ->
                                subst_target_x
                          in
                          let* t' =
                            let exception Unsat in
                            try
                              Sat
                                (Term.subst_variables t
                                   ~f:(fun v -> if Var.equal v x then subst_target_x else VarSubst v)
                                   ~f_post:(fun ~prev () sub_t ->
                                     let sub_t' =
                                       if phys_equal prev sub_t then sub_t
                                       else
                                         match
                                           sub_t |> Term.eval_const_shallow
                                           >>= Term.simplify_shallow >>| Term.linearize
                                           >>| Term.simplify_linear
                                         with
                                         | Sat sub_t' ->
                                             sub_t'
                                         | Unsat ->
                                             raise Unsat
                                     in
                                     ((), sub_t') ) )
                            with Unsat -> Unsat
                          in
                          let phi = remove_term_eq t y phi in
                          Debug.p "phi=%a@\n" (pp_with_pp_var Var.pp) phi ;
                          match Term.get_as_var t' with
                          | Some y' when Var.equal y y' ->
                              Debug.p "Discarding tautology %a -> %a@\n" (Term.pp Var.pp) t' Var.pp
                                y ;
                              Sat (phi, new_eqs)
                          | Some y' ->
                              merge_vars ~fuel new_eqs y y' phi
                          | None -> (
                              (* resolve whether the term is some atoms in disguise *)
                              let ty = normalize_var_const phi (Var y) in
                              let* atoms_opt =
                                Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                                  (Equal (t', ty))
                              in
                              match atoms_opt with
                              | Some atoms ->
                                  Debug.p "adding atoms %a instead of term_eq@\n"
                                    (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                                    atoms ;
                                  and_normalized_atoms (phi, new_eqs) atoms >>| snd
                              | None -> (
                                match get_term_eq phi t' with
                                | None -> (
                                    Debug.p "New term_eq %a -> %a@\n" (Term.pp Var.pp) t' Var.pp y ;
                                    match Term.get_as_linear t' with
                                    | Some l' ->
                                        Debug.p "delegating to [solve_normalized_lin_eq]@\n" ;
                                        solve_normalized_lin_eq ~fuel new_eqs
                                          (LinArith.of_var y |> normalize_linear phi)
                                          l' phi
                                    | None ->
                                        if Term.is_non_numeric_constant t' then
                                          let+ phi = add_const_eq y t' phi in
                                          (phi, new_eqs)
                                        else add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t' y phi
                                    )
                                | Some y' ->
                                    Debug.p "Existing term_eq %a -> %a, merging %a=%a@\n"
                                      (Term.pp Var.pp) t' Var.pp y' Var.pp y Var.pp y' ;
                                    merge_vars ~fuel new_eqs y y' phi ) ) ) ) ) )
              in_term_eqs
              (Sat (phi, new_eqs)) )


    and propagate_in_atoms ~fuel:_ tx x ((phi, new_eqs) as phi_new_eqs) =
      match Var.Map.find_opt x phi.atoms_occurrences with
      | None ->
          Sat phi_new_eqs
      | Some in_atoms ->
          (* [tx=x] has been added to the term equalities so by the invariant (that we are about
             to restore) there are no further occurrences of [x] in [phi.atoms] as we are going to
             substitute them with [tx] to get maximally-expanded atoms *)
          Debug.p "propagating %a = %a in atoms@\n" (Term.pp Var.pp) tx Var.pp x ;
          let phi = remove_from_atoms_occurrences x phi in
          let subst_target_x = Term.to_subst_target tx in
          (* TODO: could be more efficient to Atom.Set.map + linearly follow along in in_atoms,
             raising Unsat as needed and accumulating in a ref (no fold_map...) *)
          Atom.Set.fold
            (fun atom phi_new_eqs_sat ->
              let* phi, new_eqs = phi_new_eqs_sat in
              if Atom.Set.mem atom phi.atoms then
                let phi = remove_atom atom phi in
                and_atom
                  (Atom.subst_variables
                     ~f:(fun x' -> if Var.equal x' x then subst_target_x else VarSubst x')
                     atom )
                  (phi, new_eqs)
                >>| snd
              else phi_new_eqs_sat )
            in_atoms
            (Sat (phi, new_eqs))


    and propagate_term_eq ~fuel tx x phi_new_eqs =
      propagate_in_term_eqs ~fuel tx x phi_new_eqs >>= propagate_in_atoms ~fuel tx x


    and propagate_linear_eq ~fuel x lx phi_new_eqs =
      propagate_in_linear_eqs_range ~fuel x lx phi_new_eqs
      >>= propagate_in_linear_eqs_domain ~fuel x lx
      >>= propagate_in_tableau ~fuel x lx
      >>= propagate_term_eq ~fuel (Term.Linear lx) x


    and propagate_var_eq ~fuel x y phi_new_eqs =
      propagate_in_const_eqs x y phi_new_eqs >>= propagate_linear_eq ~fuel x (LinArith.of_var y)


    and propagate_atom atom phi_new_eqs =
      Debug.p "propagate atom called on %a@\n" (Atom.pp_with_pp_var Var.pp) atom ;
      match Atom.get_as_var_neq_zero atom with
      | None ->
          Sat phi_new_eqs
      | Some v -> (
          Debug.p "got as var neq zero with v=%a@\n" Var.pp v ;
          match Var.Map.find_opt v (fst phi_new_eqs).term_eqs_occurrences with
          | None ->
              Debug.p "failed to find in term_eqs@\n" ;
              Sat phi_new_eqs
          | Some in_term_eqs ->
              Debug.p "found in term_eqs@\n" ;
              TermDomainOrRange.Set.fold
                (fun (t, domain_or_range) phi_new_eqs_sat ->
                  Debug.p "found var maps to %a@\n" (Term.pp Var.pp) t ;
                  let* phi, new_eqs = phi_new_eqs_sat in
                  match domain_or_range with
                  | Domain ->
                      Debug.p "domain@\n" ;
                      phi_new_eqs_sat
                  | Range | DomainAndRange -> (
                      Debug.p "range or both\n" ;
                      let* phi, new_eqs =
                        match Term.get_as_isinstanceof t with
                        | Some (var, typ, nullable) ->
                            Debug.p "prop atom in term_eq adding below, nullable=%b\n" nullable ;
                            let* phi, new_eqs =
                              if not nullable then (
                                Debug.p "also adding greater than zero" ;
                                let* atoms =
                                  Atom.eval ~is_neq_zero:(is_neq_zero phi)
                                    (LessThan (Term.zero, Var var))
                                in
                                and_normalized_atoms (phi, new_eqs) atoms >>| snd )
                              else Sat (phi, new_eqs)
                            in
                            and_below var typ (phi, new_eqs)
                        | None ->
                            Sat (phi, new_eqs)
                      in
                      let* atoms_opt =
                        (* TODO: double-check eval_with_normalized_terms does the intended thing here *)
                        Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                          (Equal (t, Var v))
                      in
                      match atoms_opt with
                      | None ->
                          Sat (phi, new_eqs)
                      | Some atoms ->
                          Debug.p "Found new atoms thanks to %a≠0: [%a]@\n" Var.pp v
                            (Pp.seq ~sep:"," (Atom.pp_with_pp_var Var.pp))
                            atoms ;
                          and_normalized_atoms (phi, new_eqs) atoms >>| snd ) )
                in_term_eqs (Sat phi_new_eqs) )


    (* Assumes [w] is restricted, [l] is normalized. This is called [addlineq] in \[2\]. *)
    and solve_tableau_eq ~fuel new_eqs w l phi =
      Debug.p "Solving %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
      match LinArith.solve_for_unrestricted w l with
      | Some (v, l_v) ->
          (* there is at least one variable [v] whose value is not known to always be non-negative;
             we cannot add it to the tableau, let's add the equality [v=l_v <=> w=l] to the "normal"
             linear equalities for now *)
          Debug.p "Unrestricted %a = %a@\n" Var.pp v (LinArith.pp Var.pp) l_v ;
          solve_normalized_lin_eq ~fuel new_eqs l_v (LinArith.of_var v) phi
      | None ->
          (* [l] can go into the tableau as it contains only restricted (non-negative) variables *)
          let* phi, new_eqs =
            solve_normalized_lin_eq ~force_no_tableau:true ~fuel new_eqs l (LinArith.of_var w) phi
          in
          solve_tableau_restricted_eq ~fuel new_eqs w l phi


    and solve_lin_ineq new_eqs l1 l2 phi =
      (* [l1 ≤ l2] becomes [(l2-l1) ≥ 0], encoded as [l = w] with [w] a fresh restricted variable *)
      let l = LinArith.subtract l2 l1 |> normalize_linear phi |> normalize_restricted phi in
      let l_c_sign = Q.sign (LinArith.get_constant_part l) in
      let l_restricted = LinArith.is_restricted l in
      match LinArith.classify_minimized_maximized l with
      | (`Minimized | `Constant) when l_restricted && l_c_sign >= 0 ->
          Debug.p "Skip adding %a≥0: known@\n" (LinArith.pp Var.pp) l ;
          Sat (phi, new_eqs) (* already trivially known: don't add to formula to avoid divergence *)
      | (`Maximized | `Constant) when l_restricted && l_c_sign < 0 ->
          Debug.p "Skip adding %a≥0: unsat@\n" (LinArith.pp Var.pp) l ;
          Unsat
      | _ ->
          let w = Var.mk_fresh_restricted () in
          with_base_fuel (solve_tableau_eq new_eqs w l phi)


    and solve_lin_eq new_eqs t1 t2 phi =
      with_base_fuel
        (solve_normalized_lin_eq new_eqs (normalize_linear phi t1) (normalize_linear phi t2) phi)


    and and_var_linarith v l (phi, new_eqs) = solve_lin_eq new_eqs l (LinArith.of_var v) phi

    (* TODO: should we check if [φ ⊢ atom] (i.e. whether [φ ∧ ¬atom] is unsat) in [normalize_atom],
       or is [normalize_atom] already just as strong? *)
    and normalize_atom phi (atom : Atom.t) =
      let atom' = Atom.map_terms atom ~f:(fun t -> normalize_var_const phi t) in
      Debug.p "Normalizer.mormalize_atom atom'=%a" (Atom.pp_with_pp_var Var.pp) atom' ;
      Atom.eval ~is_neq_zero:(is_neq_zero phi) atom'


    (** return [(new_linear_equalities, phi ∧ atom)], where [new_linear_equalities] is [true] if
        [phi.linear_eqs] was changed as a result *)
    and and_normalized_atom (phi, new_eqs) atom =
      match Atom.var_terms_to_linear atom with
      | Atom.Equal (Linear _, Linear _) ->
          assert false
      | Atom.Equal (Linear l, Const c) | Atom.Equal (Const c, Linear l) ->
          (* NOTE: {!normalize_atom} calls {!Atom.eval}, which normalizes linear equalities so
             they end up only on one side, hence only this match case is needed to detect linear
             equalities *)
          let+ phi', new_eqs = solve_lin_eq new_eqs l (LinArith.of_q c) phi in
          (true, (phi', new_eqs))
      | (Atom.Equal (Linear l, t) | Atom.Equal (t, Linear l))
        when Option.is_some (LinArith.get_as_var l) ->
          let v = Option.value_exn (LinArith.get_as_var l) in
          let+ phi_new_eqs' = with_base_fuel (solve_normalized_term_eq new_eqs t v phi) in
          (false, phi_new_eqs')
      | Atom.LessEqual (Linear l, Const c) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q c) phi in
          (true, (phi', new_eqs))
      | Atom.LessEqual (Const c, Linear l) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q c) l phi in
          (true, (phi', new_eqs))
      | Atom.LessThan (Linear l, Const c) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q (Q.sub c Q.one)) phi in
          (true, (phi', new_eqs))
      | Atom.LessThan (Const c, Linear l) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q (Q.add c Q.one)) l phi in
          (true, (phi', new_eqs))
      | atom' ->
          (* the previous normalization has "simplified" [Var] terms into [Linear] ones, revert
             this *)
          let atom = Atom.simplify_linear atom' in
          let+ phi_new_eqs = (add_atom atom phi, new_eqs) |> propagate_atom atom in
          (false, phi_new_eqs)


    and and_normalized_atoms phi_new_eqs atoms =
      SatUnsat.list_fold atoms ~init:(false, phi_new_eqs)
        ~f:(fun (linear_changed, phi_new_eqs) atom ->
          let+ changed', phi_new_eqs = and_normalized_atom phi_new_eqs atom in
          (linear_changed || changed', phi_new_eqs) )


    and and_atom atom (phi, new_eqs) =
      normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs)


    and and_var_is_zero v (phi, neweqs) =
      if Language.curr_language_is Erlang then (* No null pointers in Erlang *) Unsat
      else solve_lin_eq neweqs (LinArith.of_var v) (LinArith.of_q Q.zero) phi


    and and_below v t (phi, new_eqs) =
      let phi, should_zero = add_below v t phi in
      if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


    and and_notbelow v t (phi, new_eqs) =
      let phi, should_zero = add_notbelow v t phi in
      if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


    (* [and_dynamic_type] wraps [add_dynamic_type]. In particular, if the call to the former
        returns [(phi, true)], then [and_dynamic_type] also adds and propagates the assertion that
        the value to which we added the type must actually be null.
    *)
    let and_dynamic_type v t ?source_file (phi, new_eqs) =
      let phi, should_zero = add_dynamic_type v t ?source_file phi in
      if should_zero then and_var_is_zero v (phi, new_eqs) else Sat (phi, new_eqs)


    let and_var_term ~fuel v t (phi, new_eqs) =
      Debug.p "and_var_term: %a=%a in %a,@;new_eqs=%a@\n" Var.pp v (Term.pp Var.pp) t
        (pp_with_pp_var Var.pp) phi pp_new_eqs new_eqs ;
      let* (t' : Term.t) =
        normalize_var_const phi t |> Atom.eval_term ~is_neq_zero:(is_neq_zero phi)
      in
      let v' = (get_repr phi v :> Var.t) in
      let* t_v =
        normalize_var_const phi (Term.Var v') |> Atom.eval_term ~is_neq_zero:(is_neq_zero phi)
      in
      let* phi, new_eqs =
        let* atoms_opt =
          Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi) (Equal (t', t_v))
        in
        match atoms_opt with
        | None ->
            Sat (phi, new_eqs)
        | Some atoms ->
            and_normalized_atoms (phi, new_eqs) atoms >>| snd
      in
      solve_normalized_term_eq ~fuel new_eqs t' v' phi


    (* interface *)

    let and_atom atom phi_new_eqs =
      Debug.p "BEGIN and_atom %a@\n" (Atom.pp_with_pp_var Var.pp) atom ;
      let phi_new_eqs' = and_atom atom phi_new_eqs >>| snd in
      Debug.p "END and_atom %a -> %a@\n" (Atom.pp_with_pp_var Var.pp) atom
        (SatUnsat.pp (Pp.pair ~fst:(pp_with_pp_var Var.pp) ~snd:pp_new_eqs))
        phi_new_eqs' ;
      phi_new_eqs'


    let and_normalized_atoms phi_new_eqs atoms =
      let phi_new_eqs' = and_normalized_atoms phi_new_eqs atoms >>| snd in
      Debug.p "and_normalized_atoms [@[<v>%a@]] -> %a@\n"
        (Pp.seq ~sep:";" (Atom.pp_with_pp_var Var.pp))
        atoms
        (SatUnsat.pp (pp_with_pp_var Var.pp))
        (SatUnsat.map fst phi_new_eqs') ;
      phi_new_eqs'


    let and_var_term v t phi_new_eqs = with_base_fuel (and_var_term v t phi_new_eqs)

    let and_var_var v1 v2 (phi, new_eqs) = with_base_fuel (merge_vars new_eqs v1 v2 phi)
  end
end

type t =
  { conditions: Atom.Set.t
        (** collection of conditions that have been assumed (via [PRUNE] CFG nodes) along the path.
            Note that these conditions are *not* normalized w.r.t. [phi]: [phi] already contains
            them so normalization w.r.t. [phi] would make them trivially true most of the time. *)
  ; phi: Formula.t
        (** the arithmetic constraints of the current symbolic state; true in both the pre and post
            since abstract values [Var.t] have immutable semantics *) }
[@@deriving compare, equal, yojson_of]

let ttrue = {conditions= Atom.Set.empty; phi= Formula.ttrue}

let pp_with_pp_var pp_var fmt {conditions; phi} =
  let pp_conditions fmt conditions =
    if Atom.Set.is_empty conditions then F.pp_print_string fmt "(empty)"
    else Atom.Set.pp_with_pp_var pp_var fmt conditions
  in
  F.fprintf fmt "@[<hv>conditions: %a@;phi: %a@]" pp_conditions conditions
    (Formula.pp_with_pp_var pp_var) phi


let pp = pp_with_pp_var Var.pp

module Intervals = struct
  let interval_and_var_of_operand phi = function
    | AbstractValueOperand v ->
        (Some (Formula.get_repr phi v :> Var.t), Var.Map.find_opt v phi.Formula.intervals)
    | ConstOperand (Cint i) ->
        (None, Some (CItv.equal_to i))
    | ConstOperand (Cfun _ | Cstr _ | Cfloat _ | Cclass _) ->
        (None, None)
    | FunctionApplicationOperand _ ->
        (None, None)


  let interval_of_operand phi operand = interval_and_var_of_operand phi operand |> snd

  let update_formula formula intervals =
    {formula with phi= Formula.set_intervals intervals formula.phi}


  let incorporate_new_eqs new_eqs formula =
    Debug.p "Intervals.incorporate_new_eqs %a@\n" pp_new_eqs new_eqs ;
    RevList.to_list new_eqs
    |> SatUnsat.list_fold ~init:formula.phi.intervals ~f:(fun intervals new_eq ->
           Debug.p "intervals incorporating %a@\n" pp_new_eq new_eq ;
           match new_eq with
           | EqZero v ->
               Formula.add_interval_ v (CItv.equal_to IntLit.zero) intervals
           | Equal (v_old, v_new) -> (
             match Var.Map.find_opt v_old intervals with
             | None ->
                 Sat intervals
             | Some intv_old ->
                 Debug.p "found old interval for %a: %a@\n" Var.pp v_old CItv.pp intv_old ;
                 Var.Map.remove v_old intervals
                 (* this will take care of taking the intersection of the intervals if [v_new]
                    already had an interval associated to it too *)
                 |> Formula.add_interval_ v_new intv_old ) )
    >>| update_formula formula


  let and_binop ~negated binop op1 op2 (formula, new_eqs) =
    Debug.p "Intervals.and_binop ~negated:%b %a %a %a@\n" negated Binop.pp binop pp_operand op1
      pp_operand op2 ;
    let v1_opt, i1_opt = interval_and_var_of_operand formula.phi op1 in
    let v2_opt, i2_opt = interval_and_var_of_operand formula.phi op2 in
    match CItv.abduce_binop_is_true ~negated binop i1_opt i2_opt with
    | Unsatisfiable ->
        Unsat
    | Satisfiable (i1_better_opt, i2_better_opt) ->
        let refine v_opt i_better_opt formula_new_eqs =
          Option.both v_opt i_better_opt
          |> Option.fold ~init:(Sat formula_new_eqs) ~f:(fun formula_new_eqs (v, i_better) ->
                 Debug.p "Refining interval for %a to %a@\n" Var.pp v CItv.pp i_better ;
                 let* formula, new_eqs = formula_new_eqs in
                 let* phi = Formula.add_interval v i_better formula.phi in
                 let* phi, new_eqs =
                   match Var.Map.find v phi.Formula.intervals |> CItv.to_singleton with
                   | None ->
                       Sat (phi, new_eqs)
                   | Some i ->
                       Formula.Normalizer.and_var_term v (Term.of_intlit i) (phi, new_eqs)
                 in
                 let+ formula = incorporate_new_eqs new_eqs {formula with phi} in
                 (formula, new_eqs) )
        in
        refine v1_opt i1_better_opt (formula, new_eqs) >>= refine v2_opt i2_better_opt


  let binop v bop op_lhs op_rhs formula =
    match
      Option.both (interval_of_operand formula.phi op_lhs) (interval_of_operand formula.phi op_rhs)
      |> Option.bind ~f:(fun (lhs, rhs) -> CItv.binop bop lhs rhs)
    with
    | None ->
        Sat formula
    | Some binop_itv ->
        Formula.add_interval_ v binop_itv formula.phi.intervals >>| update_formula formula


  let unop v op x formula =
    match
      let open Option.Monad_infix in
      interval_of_operand formula.phi x >>= CItv.unop op
    with
    | None ->
        Sat formula
    | Some unop_itv ->
        Formula.add_interval_ v unop_itv formula.phi.intervals >>| update_formula formula


  let and_callee_interval v citv_callee (phi, new_eqs) =
    let citv_caller_opt = Var.Map.find_opt v phi.Formula.intervals in
    match CItv.abduce_binop_is_true ~negated:false Eq citv_caller_opt (Some citv_callee) with
    | Unsatisfiable ->
        Unsat
    | Satisfiable (Some abduce_caller, _abduce_callee) ->
        let+ phi = Formula.add_interval v abduce_caller phi in
        (phi, new_eqs)
    | Satisfiable (None, _) ->
        Sat (phi, new_eqs)
end

let and_atom atom formula =
  let open SatUnsat.Import in
  let* phi, new_eqs = Formula.Normalizer.and_atom atom (formula.phi, RevList.empty) in
  let+ formula = Intervals.incorporate_new_eqs new_eqs {formula with phi} in
  (formula, new_eqs)


let mk_atom_of_binop (binop : Binop.t) =
  match binop with
  | Eq ->
      Atom.equal
  | Ne ->
      Atom.not_equal
  | Le ->
      Atom.less_equal
  | Lt ->
      Atom.less_than
  | _ ->
      L.die InternalError "wrong argument to [mk_atom_of_binop]: %a" Binop.pp binop


let and_mk_atom binop op1 op2 formula =
  let* formula, new_eqs =
    Intervals.and_binop ~negated:false binop op1 op2 (formula, RevList.empty)
  in
  let atom = (mk_atom_of_binop binop) (Term.of_operand op1) (Term.of_operand op2) in
  let+ formula, new_eqs' = and_atom atom formula in
  (formula, RevList.append new_eqs new_eqs')


let and_equal op1 op2 formula = and_mk_atom Eq op1 op2 formula

let and_equal_vars v1 v2 formula =
  and_equal (AbstractValueOperand v1) (AbstractValueOperand v2) formula


let and_not_equal = and_mk_atom Ne

let and_is_int v formula =
  let atom = Atom.equal (IsInt (Var v)) Term.one in
  and_atom atom formula


let and_less_equal = and_mk_atom Le

let and_less_than = and_mk_atom Lt

let and_equal_unop v (op : Unop.t) x formula =
  let* formula = Intervals.unop v op x formula in
  and_atom (Equal (Var v, Term.of_unop op (Term.of_operand x))) formula


let and_equal_binop v (bop : Binop.t) x y formula =
  let* formula = Intervals.binop v bop x y formula in
  and_atom (Equal (Var v, Term.of_binop bop (Term.of_operand x) (Term.of_operand y))) formula


let and_equal_string_concat v x y formula =
  and_atom (Equal (Var v, StringConcat (Term.of_operand x, Term.of_operand y))) formula


let prune_atom atom (formula, new_eqs) =
  (* Use [phi] to normalize [atom] here to take previous [prune]s into account. *)
  Debug.p "prune atom %a" (Atom.pp_with_pp_var Var.pp) atom ;
  let* normalized_atoms = Formula.Normalizer.normalize_atom formula.phi atom in
  let* phi, new_eqs =
    Formula.Normalizer.and_normalized_atoms (formula.phi, new_eqs) normalized_atoms
  in
  (* Sticking this call in slightly hopefully *)
  let* phi, new_eqs = Formula.Normalizer.propagate_atom atom (phi, new_eqs) in
  let conditions =
    List.fold normalized_atoms ~init:formula.conditions ~f:(fun conditions atom ->
        Atom.Set.add atom conditions )
  in
  let+ formula = Intervals.incorporate_new_eqs new_eqs {phi; conditions} in
  (formula, new_eqs)


let prune_atoms atoms formula_new_eqs =
  SatUnsat.list_fold atoms ~init:formula_new_eqs ~f:(fun formula_new_eqs atom ->
      prune_atom atom formula_new_eqs )


let prune_binop ~negated (bop : Binop.t) x y formula =
  let tx = Term.of_operand x in
  let ty = Term.of_operand y in
  let t = Term.of_binop bop tx ty in
  (* [Option.value_exn] is justified by [force_to_atom:true] *)
  let atoms =
    Option.value_exn
      (Atom.atoms_of_term ~is_neq_zero:(Formula.is_neq_zero formula.phi) ~force_to_atom:true
         ~negated t )
  in
  (* NOTE: [Intervals.and_binop] may tip off the rest of the formula about the new equality so it's
     important to do [prune_atoms] *first* otherwise it might become trivial. For instance adding [x
     = 4] would prune [4 = 4] and so not add anything to [formula.conditions] instead of adding [x =
     4]. *)
  prune_atoms atoms (formula, RevList.empty) >>= Intervals.and_binop ~negated bop x y


let is_known_zero formula v =
  Var.Map.find_opt v formula.phi.intervals |> Option.exists ~f:CItv.is_equal_to_zero
  || Var.Map.find_opt (VarUF.find formula.phi.var_eqs v :> Var.t) formula.phi.linear_eqs
     |> Option.exists ~f:LinArith.is_zero


module DynamicTypes = struct
  let get_dynamic_type v formula =
    (* TODO: canonicalize more uniformly - sticking this here is almost certainly not enough since we look things up in lots of places *)
    let v_canon = (Formula.get_repr formula.phi v :> Var.t) in
    match Var.Map.find_opt v_canon formula.phi.type_constraints with
    | Some (InstanceOf.Known dtd) ->
        Some dtd
    | _ ->
        None


  let evaluate_instanceof formula v typ nullable =
    if is_known_zero formula v then Some (Term.of_bool nullable)
    else
      let known_non_zero = Formula.is_neq_zero formula.phi (Var v) in
      Debug.p "known non zero of %a is %b\n" Var.pp v known_non_zero ;
      match Var.Map.find_opt v formula.phi.type_constraints with
      | None ->
          None
      | Some (InstanceOf.Known {typ= t}) ->
          if InstanceOf.is_subtype t typ then Some Term.one
          else if (not nullable) || known_non_zero then Some Term.zero
          else None
      | Some (InstanceOf.Unknown {below; notbelow}) ->
          if
            List.exists below ~f:(fun t' -> InstanceOf.is_subtype t' typ)
            && (nullable || known_non_zero)
          then Some Term.one
          else if
            List.exists notbelow ~f:(fun t' -> InstanceOf.is_subtype typ t')
            && ((not nullable) || known_non_zero)
          then Some Term.zero
          else if
            InstanceOf.is_concrete_or_abstract typ
            && List.exists below ~f:(fun t' ->
                   InstanceOf.is_concrete_or_abstract t'
                   && (not (InstanceOf.is_subtype typ t'))
                   && not (InstanceOf.is_subtype t' typ) )
          then (* inconsistent *) if (not nullable) || known_non_zero then Some Term.zero else None
          else None


  (* TODO: fix messy separation between (new) InstanceOf and (old) DynamicTypes - not sure where to put the next definition *)
  let and_callee_type_constraints v type_constraints_foreign (phi, new_eqs) =
    match type_constraints_foreign with
    | InstanceOf.Known {typ= t; source_file} ->
        Formula.Normalizer.and_dynamic_type v t ?source_file (phi, new_eqs)
    | InstanceOf.Unknown {below; notbelow} ->
        let* phi, new_eqs =
          PulseSatUnsat.list_fold below ~init:(phi, new_eqs) ~f:(fun (phi, new_eqs) upper_bound ->
              Formula.Normalizer.and_below v upper_bound (phi, new_eqs) )
        in
        PulseSatUnsat.list_fold notbelow ~init:(phi, new_eqs) ~f:(fun (phi, new_eqs) not_upper ->
            Formula.Normalizer.and_notbelow v not_upper (phi, new_eqs) )
end

type dynamic_type_data = InstanceOf.dynamic_type_data =
  {typ: Typ.t; source_file: SourceFile.t option}

let get_dynamic_type = DynamicTypes.get_dynamic_type

let add_dynamic_type_unsafe v t ?source_file _location {conditions; phi} =
  let v = (Formula.get_repr phi v :> Var.t) in
  let tenv = PulseContext.tenv_exn () in
  let t = Tenv.expand_hack_alias_in_typ tenv t in
  let phi, should_zero = Formula.add_dynamic_type v t ?source_file phi in
  ( if should_zero then
      (* This situation corresponds (roughly) to the ones in which we'd previously have
         returned Unsat (which was not supposed to happen in calls to the unsafe version)
         For now we keep the logging and default behaviour
         TODO: revisit this *)
      let prev_fact = Var.Map.find_opt v phi.type_constraints in
      L.d_printfln "failed to add dynamic type %a to value %a. Previous constraints were %a@\n"
        (Typ.pp_full Pp.text) t PulseAbstractValue.pp v
        (Pp.option InstanceOf.pp_instance_fact)
        prev_fact ) ;
  {conditions; phi}


let copy_type_constraints v_src v_target {conditions; phi} =
  {conditions; phi= Formula.copy_type_constraints v_src v_target phi}


let and_equal_instanceof v1 v2 t ~nullable formula =
  let v2 = (Formula.get_repr formula.phi v2 :> Var.t) in
  let tenv = PulseContext.tenv_exn () in
  let t = Tenv.expand_hack_alias_in_typ tenv t in
  let* formula, new_eqs' =
    and_atom (Atom.equal (Var v1) (IsInstanceOf {var= v2; typ= t; nullable})) formula
  in
  let* formula, new_eqs' =
    match DynamicTypes.evaluate_instanceof formula v2 t nullable with
    | None ->
        Sat (formula, new_eqs')
    | Some value_term ->
        let* phi, neweqs' =
          (* It might look odd to keep the instanceof around, but removing it messes up the latency calculations
             because there's then no dependency on v2 *)
          Formula.Normalizer.and_atom (Atom.equal (Var v1) value_term) (formula.phi, new_eqs')
        in
        Sat ({formula with phi}, neweqs')
  in
  Debug.p "formula is %a" pp formula ;
  Sat (formula, new_eqs')


let and_dynamic_type v t ?source_file formula =
  let v = (Formula.get_repr formula.phi v :> Var.t) in
  let tenv = PulseContext.tenv_exn () in
  let t = Tenv.expand_hack_alias_in_typ tenv t in
  let+ phi, new_eqns =
    Formula.Normalizer.and_dynamic_type v t ?source_file (formula.phi, RevList.empty)
  in
  ({formula with phi}, new_eqns)


(** translate each variable in [formula_foreign] according to [f] then incorporate each fact into
    [formula0] *)
let and_fold_subst_variables formula0 ~up_to_f:formula_foreign ~init ~f:f_var =
  let f_subst acc v =
    let acc', v' = f_var acc v in
    (acc', VarSubst v')
  in
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat -> raise_notrace Contradiction | Sat x -> x
  in
  let and_var_eqs var_eqs_foreign acc_phi_new_eqs =
    VarUF.fold_congruences var_eqs_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) (repr_foreign, vs_foreign) ->
        let acc_f, repr = f_var acc_f (repr_foreign :> Var.t) in
        IContainer.fold_of_pervasives_set_fold Var.Set.fold vs_foreign ~init:(acc_f, phi_new_eqs)
          ~f:(fun (acc_f, phi_new_eqs) v_foreign ->
            let acc_f, v = f_var acc_f v_foreign in
            let phi_new_eqs = Formula.Normalizer.and_var_var repr v phi_new_eqs |> sat_value_exn in
            (acc_f, phi_new_eqs) ) )
  in
  let and_linear_eqs linear_eqs_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_map_fold Var.Map.fold linear_eqs_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) (v_foreign, l_foreign) ->
        let acc_f, v = f_var acc_f v_foreign in
        let acc_f, l = LinArith.fold_subst_variables l_foreign ~init:acc_f ~f:f_subst in
        let phi_new_eqs = Formula.Normalizer.and_var_linarith v l phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  let and_term_eqs phi_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_map_fold Formula.term_eqs_fold phi_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) (t_foreign, v_foreign) ->
        let acc_f, t = Term.fold_subst_variables t_foreign ~init:acc_f ~f_subst in
        let acc_f, v = f_var acc_f v_foreign in
        let phi_new_eqs = Formula.Normalizer.and_var_term v t phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  let and_intervals intervals_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_map_fold Var.Map.fold intervals_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) (v_foreign, interval_foreign) ->
        let acc_f, v = f_var acc_f v_foreign in
        let phi_new_eqs =
          Intervals.and_callee_interval v interval_foreign phi_new_eqs |> sat_value_exn
        in
        (acc_f, phi_new_eqs) )
  in
  let and_type_constraints type_constraints_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_map_fold Var.Map.fold type_constraints_foreign
      ~init:acc_phi_new_eqs ~f:(fun (acc_f, phi_new_eqs) (v_foreign, type_constraints_foreign) ->
        let acc_f, v = f_var acc_f v_foreign in
        let phi_new_eqs =
          DynamicTypes.and_callee_type_constraints v type_constraints_foreign phi_new_eqs
          |> sat_value_exn
        in
        (acc_f, phi_new_eqs) )
  in
  let and_atoms atoms_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold atoms_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) atom_foreign ->
        let acc_f, atom = Atom.fold_subst_variables atom_foreign ~init:acc_f ~f_subst in
        let phi_new_eqs = Formula.Normalizer.and_atom atom phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  let and_ phi_foreign acc phi =
    try
      Sat
        ( and_var_eqs phi_foreign.Formula.var_eqs (acc, (phi, RevList.empty))
        |> and_type_constraints phi_foreign.Formula.type_constraints
        |> and_linear_eqs phi_foreign.Formula.linear_eqs
        |> and_term_eqs phi_foreign
        |> and_intervals phi_foreign.Formula.intervals
        |> and_atoms phi_foreign.Formula.atoms )
    with Contradiction -> Unsat
  in
  let open SatUnsat.Import in
  let+ acc, (phi, new_eqs) = and_ formula_foreign.phi init formula0.phi in
  (acc, {formula0 with phi}, new_eqs)


let and_conditions_fold_subst_variables phi0 ~up_to_f:phi_foreign ~init ~f:f_var =
  Debug.p "START and_conditions_fold_subst_variables" ;
  let f_subst acc v =
    let acc', v' = f_var acc v in
    (acc', VarSubst v')
  in
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat -> raise_notrace Contradiction | Sat x -> x
  in
  let add_conditions conditions_foreign init =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold conditions_foreign ~init
      ~f:(fun (acc_f, phi_new_eqs) atom_foreign ->
        let acc_f, atom = Atom.fold_subst_variables atom_foreign ~init:acc_f ~f_subst in
        let phi_new_eqs = prune_atom atom phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  try
    let acc, (phi, new_eqs) = add_conditions phi_foreign.conditions (init, (phi0, RevList.empty)) in
    Debug.p "END and_conditions_fold_subst_variables" ;
    Sat (acc, phi, new_eqs)
  with Contradiction -> Unsat


module QuantifierElimination : sig
  val eliminate_vars : precondition_vocabulary:Var.Set.t -> keep:Var.Set.t -> t -> t SatUnsat.t
  (** [eliminate_vars ~precondition_vocabulary ~keep formula] substitutes every variable [x] in
      [formula.phi] with [x'] whenever [x'] is a distinguished representative of the equivalence
      class of [x] in [formula.phi] such that [x' ∈ keep_pre ∪ keep_post]. It also similarly
      substitutes variables in [formula.conditions] that are not in [precondition_vocabulary]. *)
end = struct
  exception Contradiction

  let subst_var_linear_eqs subst linear_eqs =
    Var.Map.fold
      (fun x l new_map ->
        let x' = subst_f subst x in
        let l' = LinArith.subst_variables ~f:(targetted_subst_var subst) l in
        match LinArith.solve_eq (LinArith.of_var x') l' with
        | Unsat ->
            L.d_printfln "Contradiction found: %a=%a became %a=%a with is Unsat" Var.pp x
              (LinArith.pp Var.pp) l Var.pp x' (LinArith.pp Var.pp) l' ;
            raise_notrace Contradiction
        | Sat None ->
            new_map
        | Sat (Some (x'', l'')) ->
            Var.Map.add x'' l'' new_map )
      linear_eqs Var.Map.empty


  (* used for both intervals and type constraints *)
  let subst_key_var_only subst intervals =
    Var.Map.fold
      (fun x citv new_map ->
        let x' = subst_f subst x in
        (* concrete intervals/types have no variables inside them *)
        Var.Map.add x' citv new_map )
      intervals Var.Map.empty


  let subst_var_atoms subst atoms =
    Atom.Set.map (fun atom -> Atom.subst_variables ~f:(targetted_subst_var subst) atom) atoms


  let subst_var_atoms_for_conditions ~precondition_vocabulary subst atoms =
    Atom.Set.fold
      (fun atom atoms' ->
        let changed, atom' =
          Atom.fold_subst_variables atom ~init:false ~f_subst:(fun changed v ->
              if Var.Set.mem v precondition_vocabulary then (changed, VarSubst v)
              else
                (* [v] is not mentioned in the precondition but maybe call sites can still affect its
                   truth value if it is related to other values from the precondition via other
                   atoms. Substitute [v] by its canonical representative otherwise the atom will
                   become truly dead as it will mention a variable that is not to be kept. *)
                let v' = subst_f subst v in
                ((not (Var.equal v v')) || changed, VarSubst v') )
        in
        if changed then
          match Atom.eval ~is_neq_zero:(fun _ -> false) atom' with
          | Unsat ->
              raise_notrace Contradiction
          | Sat atoms'' ->
              Atom.Set.add_seq (Caml.List.to_seq atoms'') atoms'
        else Atom.Set.add atom' atoms' )
      atoms Atom.Set.empty


  let subst_var_phi subst
      ( {Formula.var_eqs; type_constraints; linear_eqs; tableau; term_eqs= _; intervals; atoms} as
        phi ) =
    Formula.unsafe_mk ~var_eqs:(VarUF.apply_subst subst var_eqs)
      ~const_eqs:(* trust that rebuilding [term_eqs] will re-generate this map *) Var.Map.empty
      ~type_constraints:(subst_key_var_only subst type_constraints)
      ~linear_eqs:(subst_var_linear_eqs subst linear_eqs)
      ~term_eqs:(Formula.subst_term_eqs subst phi)
      ~tableau:(subst_var_linear_eqs subst tableau)
      ~intervals:(subst_key_var_only subst intervals)
      ~atoms:(subst_var_atoms subst atoms)
        (* this is only ever called during summary creation, it's safe to ditch the occurrence maps
           at this point since they will be reconstructed by callers *)
      ~linear_eqs_occurrences:Var.Map.empty ~tableau_occurrences:Var.Map.empty
      ~term_eqs_occurrences:Var.Map.empty ~atoms_occurrences:Var.Map.empty


  let extend_with_restricted_reps_of keep formula =
    (* extending [keep] with a restricted variable [a] when there is [x∈keep] such that [x=a] so
       that we remember that [x≥0] is not interesting if we know the more precise fact that [x=c]
       for some constant [c≥0]. Furthermore, we don't need to check that [c≥0] when we find that
       [x=a=c] because [c<0] would be a contradiction already detected by the tableau at an earlier
       step. *)
    let is_constant repr =
      Var.Map.find_opt repr formula.phi.linear_eqs
      |> Option.exists ~f:(fun linear -> LinArith.get_as_const linear |> Option.is_some)
    in
    VarUF.fold_congruences formula.phi.var_eqs ~init:keep ~f:(fun acc (repr, vs) ->
        let repr = (repr :> Var.t) in
        if
          Var.is_restricted repr
          && (not (is_constant repr))
          && Var.Set.exists (fun v -> Var.is_unrestricted v && Var.Set.mem v keep) vs
        then Var.Set.add repr acc
        else acc )


  let eliminate_vars ~precondition_vocabulary ~keep formula =
    (* Beware of not losing information: if [x=u] with [u] is restricted then [x≥0], so extend
       [keep] accordingly. *)
    let keep = extend_with_restricted_reps_of keep formula in
    let subst = VarUF.reorient formula.phi.var_eqs ~should_keep:(fun x -> Var.Set.mem x keep) in
    try
      Sat
        { conditions=
            subst_var_atoms_for_conditions ~precondition_vocabulary subst formula.conditions
        ; phi= subst_var_phi subst formula.phi }
    with Contradiction -> Unsat
end

module DeadVariables = struct
  (** Intermediate step of [simplify]: build a directed graph between variables:

      - when two variables are in an atom or a linear equation, there are bi-directional edges
        between them,
      - when a restricted variable is a representative of a unrestricted variable, there is an edge
        from the variable to representative. *)
  let build_var_graph phi =
    (* a map where a vertex maps to the set of destination vertices *)
    (* unused but can be useful for debugging *)
    let _pp_graph fmt graph =
      Caml.Hashtbl.iter (fun v vs -> F.fprintf fmt "%a->{%a}" Var.pp v Var.Set.pp vs) graph
    in
    (* 16 because why not *)
    let graph = Caml.Hashtbl.create 16 in
    (* add [src->vs] to [graph] (but not the symmetric edges) *)
    let add_set src vs =
      let dest =
        match Caml.Hashtbl.find_opt graph src with
        | None ->
            vs
        | Some dest0 ->
            Var.Set.union vs dest0
      in
      Caml.Hashtbl.replace graph src dest
    in
    (* add edges between all pairs of [vs] *)
    let add_all vs = Var.Set.iter (fun v -> add_set v vs) vs in
    (* add an edge *)
    let add src v = add_set src (Var.Set.singleton v) in
    Container.iter ~fold:VarUF.fold_congruences phi.Formula.var_eqs
      ~f:(fun ((repr : VarUF.repr), vs) ->
        let repr = (repr :> Var.t) in
        Var.Set.iter
          (fun v ->
            (* HACK: no need to check the converse ([v] restricted and [repr] unrestricted) since
               restricted variables always come before unrestricted ones in the [Var] order and
               [repr] comes before [v] by construction *)
            if Var.is_unrestricted v && Var.is_restricted repr then add v repr )
          vs ) ;
    Var.Map.iter
      (fun v l ->
        LinArith.get_variables l
        |> Seq.fold_left (fun vs v -> Var.Set.add v vs) (Var.Set.singleton v)
        |> add_all )
      phi.Formula.linear_eqs ;
    (* helper function: compute [vs U vars(t)] *)
    let union_vars_of_term t vs =
      Term.fold_variables t ~init:vs ~f:(fun vs v -> Var.Set.add v vs)
    in
    (* add edges between all pairs of variables appearing in [t1] or [t2] (yes this is quadratic in
       the number of variables of these terms) *)
    let add_from_terms t1 t2 =
      union_vars_of_term t1 Var.Set.empty |> union_vars_of_term t2 |> add_all
    in
    Formula.term_eqs_iter (fun t v -> union_vars_of_term t (Var.Set.singleton v) |> add_all) phi ;
    Atom.Set.iter
      (fun atom ->
        let t1, t2 = Atom.get_terms atom in
        add_from_terms t1 t2 )
      phi.Formula.atoms ;
    graph


  (** Intermediate step of [simplify]: construct transitive closure of variables reachable from [vs]
      in [graph]. *)
  let get_reachable_from graph vs =
    (* HashSet represented as a [Hashtbl.t] mapping items to [()], start with the variables in [vs] *)
    let reachable = Caml.Hashtbl.create (Var.Set.cardinal vs) in
    Var.Set.iter (fun v -> Caml.Hashtbl.add reachable v ()) vs ;
    (* Do a Dijkstra-style graph transitive closure in [graph] starting from [vs]. At each step,
       [new_vs] contains the variables to explore next. Iterative to avoid blowing the stack. *)
    let new_vs = ref (Var.Set.elements vs) in
    while not (List.is_empty !new_vs) do
      (* pop [new_vs] *)
      let[@warning "-partial-match"] (v :: rest) = !new_vs in
      new_vs := rest ;
      Caml.Hashtbl.find_opt graph v
      |> Option.iter ~f:(fun vs' ->
             Var.Set.iter
               (fun v' ->
                 if not (Caml.Hashtbl.mem reachable v') then (
                   (* [v'] seen for the first time: we need to explore it *)
                   Caml.Hashtbl.replace reachable v' () ;
                   new_vs := v' :: !new_vs ) )
               vs' )
    done ;
    Caml.Hashtbl.to_seq_keys reachable |> Var.Set.of_seq


  (** Get rid of atoms when they contain only variables that do not appear in atoms mentioning
      variables in [keep], or variables appearing in atoms together with variables in these sets,
      and so on. In other words, the variables to keep are all the ones transitively reachable from
      variables in [keep] in the graph connecting two variables whenever they appear together in a
      same atom of the formula.

      HACK: since this only used for summary creation we take the opportunity to also get rid of
      redundant facts in the formula. *)
  let eliminate ~precondition_vocabulary ~keep formula =
    let var_graph = build_var_graph formula.phi in
    (* INVARIANT: [vars_to_keep] contains a var in an atom of the formula (a linear eq or term eq
       or actual atom) iff it contains all vars in that atom *)
    let vars_to_keep = get_reachable_from var_graph keep in
    L.d_printfln "Reachable vars: %a" Var.Set.pp_hov vars_to_keep ;
    let simplify_phi phi =
      let var_eqs = VarUF.filter ~f:(fun x -> Var.Set.mem x vars_to_keep) phi.Formula.var_eqs in
      let type_constraints =
        Var.Map.filter (fun x _ -> Var.Set.mem x vars_to_keep) phi.Formula.type_constraints
      in
      (* all linear equalities have a counterpart in [term_eqs] so it's safe to drop them here to
         save space in summaries *)
      let linear_eqs = Var.Map.empty in
      let tableau =
        Var.Map.filter
          (fun v _ ->
            (* by INVARIANT it's enough to check membership of only one variable, eg [v] which is
               readily available *)
            Var.Set.mem v vars_to_keep )
          phi.Formula.tableau
      in
      let term_eqs = Formula.term_eqs_filter (fun _ v -> Var.Set.mem v vars_to_keep) phi in
      (* discard atoms which have variables *not* in [vars_to_keep], which in particular is enough
         to guarantee that *none* of their variables are in [vars_to_keep] thanks to transitive
         closure on the graph above *)
      let atoms =
        Atom.Set.filter
          (fun atom ->
            (* by INVARIANT it's enough to check membership of only one variable, pick whatever
               first one we come across in [Atom.fold_variables] *)
            let exception FirstVar of Var.t in
            match Atom.fold_variables atom ~init:() ~f:(fun () v -> raise (FirstVar v)) with
            | () ->
                (* only constants, should never get there but let's not crash in case something
                   else went wrong *)
                false
            | exception FirstVar v ->
                Var.Set.mem v vars_to_keep )
          phi.Formula.atoms
      in
      let intervals =
        Var.Map.filter
          (fun v interval ->
            (* some intervals will have been propagated to the rest of the formula and we don't
               need to keep them around *)
            CItv.requires_integer_reasoning interval && Var.Set.mem v vars_to_keep )
          phi.Formula.intervals
      in
      Formula.unsafe_mk ~var_eqs
        ~const_eqs:
          (* we simplify for summaries creation, this information is already present in
             [term_eqs] *)
          Var.Map.empty ~linear_eqs ~term_eqs ~tableau ~intervals ~type_constraints
        ~atoms
          (* we simplify for summaries creation, it's safe to ditch the occurrence maps at this
             point since they will be reconstructed by callers *)
        ~linear_eqs_occurrences:Var.Map.empty ~tableau_occurrences:Var.Map.empty
        ~term_eqs_occurrences:Var.Map.empty ~atoms_occurrences:Var.Map.empty
    in
    let phi = simplify_phi formula.phi in
    let conditions =
      (* discard atoms that callers have no way of influencing, i.e. more or less those that do not
         contain variables related to variables in the pre *)
      let closed_prunable_vars = get_reachable_from var_graph precondition_vocabulary in
      Atom.Set.filter
        (fun atom -> not (Atom.has_var_notin closed_prunable_vars atom))
        formula.conditions
    in
    Sat ({conditions; phi}, vars_to_keep)
end

let simplify ~precondition_vocabulary ~keep formula =
  let open SatUnsat.Import in
  L.d_printfln_escaped "@[Simplifying %a@ wrt %a (keep),@ with prunables=%a@]" pp formula
    Var.Set.pp_hov keep Var.Set.pp_hov precondition_vocabulary ;
  (* get rid of as many variables as possible *)
  let* formula = QuantifierElimination.eliminate_vars ~precondition_vocabulary ~keep formula in
  (* TODO: doing [QuantifierElimination.eliminate_vars; DeadVariables.eliminate] a few times may
     eliminate even more variables *)
  let+ formula, live_vars = DeadVariables.eliminate ~precondition_vocabulary ~keep formula in
  (formula, live_vars, RevList.empty)


let is_known_non_pointer formula v = Formula.is_non_pointer formula.phi v

let is_manifest ~is_allocated formula =
  Atom.Set.for_all
    (fun atom ->
      let is_ground = not @@ Term.has_var_notin Var.Set.empty @@ Atom.to_term atom in
      is_ground
      ||
      match Atom.get_as_var_neq_zero atom with
      | Some x ->
          (* ignore [x≠0] when [x] is known to be allocated: pointers being allocated doesn't make
             an issue latent and we still need to remember that [x≠0] was tested by the program
             explicitly *)
          is_allocated x
      | None -> (
        match Atom.get_as_disequal_vars atom with
        | Some (x, y) ->
            (* ignore [x≠y] when [x] and [y] are both known to be allocated since it already
               implies they are different (the heap uses separation logic implicitly) *)
            is_allocated x && is_allocated y
        | None ->
            false ) )
    formula.conditions


let get_var_repr formula v = (Formula.get_repr formula.phi v :> Var.t)

let as_constant_q formula v =
  Var.Map.find_opt (get_var_repr formula v) formula.phi.linear_eqs
  |> Option.bind ~f:LinArith.get_as_const


let as_constant_string formula v =
  match Var.Map.find_opt (get_var_repr formula v) formula.phi.const_eqs with
  | Some (String s) ->
      Some s
  | _ ->
      None


(** for use in applying callee path conditions: we need to translate callee variables to make sense
    for the caller, thereby possibly extending the current substitution *)
let subst_find_or_new subst addr_callee =
  match Var.Map.find_opt addr_callee subst with
  | None ->
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_caller = Var.mk_fresh_same_kind addr_callee in
      L.d_printfln "new subst %a <-> %a (fresh)" Var.pp addr_callee Var.pp addr_caller ;
      let addr_hist_fresh = (addr_caller, ValueHistory.epoch) in
      (Var.Map.add addr_callee addr_hist_fresh subst, fst addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, fst addr_hist_caller)


let and_callee_formula subst formula ~callee:formula_callee =
  let* subst, formula, new_eqs =
    and_conditions_fold_subst_variables formula ~up_to_f:formula_callee ~f:subst_find_or_new
      ~init:subst
  in
  let+ subst, formula, new_eqs' =
    and_fold_subst_variables ~up_to_f:formula_callee ~f:subst_find_or_new ~init:subst formula
  in
  (subst, formula, RevList.append new_eqs' new_eqs)


let fold_variables {conditions; phi} ~init ~f =
  let init =
    let f atom acc = Atom.fold_variables atom ~init:acc ~f in
    Atom.Set.fold f conditions init
  in
  Formula.fold_variables phi ~init ~f


let absval_of_int formula i =
  match Formula.get_term_eq formula.phi (Term.of_intlit i) with
  | Some v ->
      (formula, v)
  | None ->
      let assert_sat = function Sat x -> x | Unsat -> assert false in
      let v = Var.mk_fresh () in
      let formula =
        and_equal (AbstractValueOperand v) (ConstOperand (Cint i)) formula |> assert_sat |> fst
      in
      (formula, v)


let absval_of_string formula s =
  match Formula.get_term_eq formula.phi (String s) with
  | Some v ->
      (formula, v)
  | None ->
      let assert_sat = function Sat x -> x | Unsat -> assert false in
      let v = Var.mk_fresh () in
      let formula =
        and_equal (AbstractValueOperand v) (ConstOperand (Cstr s)) formula |> assert_sat |> fst
      in
      (formula, v)


type term = Term.t

let explain_as_term formula x =
  Option.first_some
    (as_constant_q formula x |> Option.map ~f:(fun q -> Term.Const q))
    (as_constant_string formula x |> Option.map ~f:(fun s -> Term.String s))


let pp_term = Term.pp

let pp_formula_explained pp_var fmt {phi} =
  let ({ Formula.var_eqs= _
       ; const_eqs= _
       ; type_constraints
       ; linear_eqs
       ; term_eqs= _ (* still get printed but the API doesn't use [term_eqs] directly *)
       ; tableau
       ; intervals= _
       ; atoms
       ; linear_eqs_occurrences= _
       ; tableau_occurrences= _
       ; term_eqs_occurrences= _
       ; atoms_occurrences= _ } [@warning "+missing-record-field-pattern"] ) =
    phi
  in
  let is_map_non_empty m = not (Var.Map.is_empty m) in
  let should_print_linear linear = Option.is_none @@ LinArith.get_as_const linear in
  let should_print_term_eq term _ =
    match Term.linearize term with Term.Linear _ -> false | _ -> true
  in
  let should_print_atom atom =
    match atom with Atom.Equal (IsInt _, Const _) -> false | _ -> true
  in
  let pp_if should_print pp fmt x = if should_print x then F.fprintf fmt "@;∧ %a" pp x in
  pp_if is_map_non_empty (InstanceOf.pp_with_pp_var pp_var) fmt type_constraints ;
  pp_if
    (Var.Map.exists (fun _ linear -> should_print_linear linear))
    (pp_var_map
       ~filter:(fun (_, linear) -> should_print_linear linear)
       ~arrow:" = " (LinArith.pp pp_var) pp_var )
    fmt linear_eqs ;
  pp_if is_map_non_empty (Tableau.pp pp_var) fmt tableau ;
  pp_if
    (Formula.term_eqs_exists should_print_term_eq)
    (Formula.pp_term_eqs_with_pp_var ~filter:should_print_term_eq pp_var)
    fmt phi ;
  pp_if
    (fun atoms -> Atom.Set.exists should_print_atom atoms)
    (Atom.Set.pp_with_pp_var ~filter:should_print_atom pp_var)
    fmt atoms


let pp_conditions_explained pp_var fmt {conditions} =
  if not (Atom.Set.is_empty conditions) then
    F.fprintf fmt "@;∧ %a" (Atom.Set.pp_with_pp_var pp_var) conditions
