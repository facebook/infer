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
      Procname.pp fmt proc_name


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
        Pp.collection ~sep:" "
          ~fold:(IContainer.fold_of_pervasives_map_fold VarMap.fold)
          ~pp_item:(fun fmt (v, q) ->
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


  let get_constant_part (c, _) = c

  let get_coefficient v (_, vs) = VarMap.find_opt v vs

  let of_subst_target v0 = function
    | QSubst q ->
        of_q q
    | VarSubst v ->
        of_var v
    | LinSubst l ->
        l
    | NonLinearTermSubst _ ->
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
    ~pp_item:pp_var fmt var_set


let pp_var_map ~arrow pp_val pp_var fmt var_map =
  Pp.collection ~sep:" ∧ "
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    ~pp_item:(fun fmt (v, value) -> F.fprintf fmt "%a%s%a" pp_var v arrow pp_val value)
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
                       ~f:(fun (_, coeff') -> Q.(coeff' >= zero || -(q_c / coeff') >= gain))
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
    | IsInstanceOf of Var.t * Typ.t
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
        Procname.pp fmt proc_name
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
    | IsInstanceOf (v, t) ->
        F.fprintf fmt "%a instanceof %a" pp_var v (Typ.pp_full Pp.text) t
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
    | NonLinearTermSubst t ->
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
    | BitXor _ ->
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


  let rec fold_map_subterms t ~init ~f =
    let acc, t' =
      fold_map_direct_subterms t ~init ~f:(fun acc t' -> fold_map_subterms t' ~init:acc ~f)
    in
    f acc t'


  let satunsat_map_direct_subterms t ~f =
    let exception FoundUnsat in
    try
      Sat
        (map_direct_subterms t ~f:(fun t' ->
             match f t' with Unsat -> raise_notrace FoundUnsat | Sat t'' -> t'' ) )
    with FoundUnsat -> Unsat


  let fold_subterms t ~init ~f = fold_map_subterms t ~init ~f:(fun acc t' -> (f acc t', t')) |> fst

  let map_subterms t ~f = fold_map_subterms t ~init:() ~f:(fun () t' -> ((), f t')) |> snd

  let rec fold_subst_variables ~init ~f_subst ?(f_post = fun ~prev:_ acc t -> (acc, t)) t =
    match t with
    | Var v ->
        let acc, op = f_subst init v in
        let t' = match op with VarSubst v' when Var.equal v v' -> t | _ -> of_subst_target op in
        f_post ~prev:t acc t'
    | IsInstanceOf (v, typ) ->
        let acc, op = f_subst init v in
        let t' =
          match op with
          | VarSubst v' when not (Var.equal v v') ->
              IsInstanceOf (v', typ)
          | QSubst q when Q.is_zero q ->
              zero
          | QSubst _ | VarSubst _ | LinSubst _ | NonLinearTermSubst _ ->
              t
        in
        f_post ~prev:t acc t'
    | Linear l ->
        let acc, l' = LinArith.fold_subst_variables l ~init ~f:f_subst in
        let t' = if phys_equal l l' then t else Linear l' in
        f_post ~prev:t acc t'
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
      match LinArith.get_as_const l with Some c -> Const c | None -> Linear l )
    | t ->
        t


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
    | IsInstanceOf _
    | IsInt _ ->
        None


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
    | IsInstanceOf _
    | IsInt _ ->
        None


  let to_subst_target t =
    match get_as_const t with
    | Some q ->
        QSubst q
    | None -> (
      match get_as_var t with
      | Some v ->
          VarSubst v
      | None -> (
        match t with Linear l -> LinSubst l | _ -> NonLinearTermSubst t ) )


  module Set = struct
    include Caml.Set.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    let pp_with_pp_var pp_var fmt terms =
      if is_empty terms then F.pp_print_string fmt "(empty)"
      else
        Pp.collection ~sep:","
          ~fold:(IContainer.fold_of_pervasives_set_fold fold)
          ~pp_item:(fun fmt term -> F.fprintf fmt "%a" (pp pp_var) term)
          fmt terms
  end

  module VarMap = struct
    include Caml.Map.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    type t_ = Var.t t [@@deriving compare, equal]

    let pp_with_pp_var pp_var fmt m =
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_map_fold fold)
        ~pp_item:(fun fmt (term, var) -> F.fprintf fmt "%a=%a" (pp pp_var) term pp_var var)
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

  let fold_subterms atom ~init ~f =
    fold_terms atom ~init ~f:(fun acc t -> Term.fold_subterms t ~init:acc ~f)


  let iter_subterms atom ~f = Container.iter ~fold:fold_subterms atom ~f

  let exists_subterm atom ~f = Container.exists ~iter:iter_subterms atom ~f

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

  (* Preseves physical equality if [f] does. *)
  let map_subterms atom ~f = map_terms atom ~f:(fun t -> Term.map_subterms t ~f)

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

  (** [atoms_of_term ~negated t] is [Some \[atom1; ..; atomN\]] if [t] (or [¬t] if [negated]) is
      (mostly syntactically) equivalent to [atom1 ∧ .. ∧ atomN]. For example
      [atoms_of_term ~negated:false (Equal (Or (x, Not y), 0))] should be
      [\[Equal (x, 0); NotEqual (y, 0)\]]. When the term [y] is known as a boolean, it generates a
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


  let get_as_linear atom =
    match get_terms atom with
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
        Sat []
    | False ->
        L.d_printfln "UNSAT atom according to eval_const_shallow: %a" (pp_with_pp_var Var.pp) atom ;
        Unsat
    | Atom atom -> (
      match get_as_linear atom with
      | Some atom' ->
          eval_with_normalized_terms ~is_neq_zero atom'
      | None -> (
        match get_as_embedded_atoms ~is_neq_zero atom with
        | None -> (
          match eval_syntactically_equal_terms atom with
          | True ->
              Sat []
          | False ->
              L.d_printfln "UNSAT atom according to eval_syntactically_equal_terms: %a"
                (pp_with_pp_var Var.pp) atom ;
              Unsat
          | Atom atom ->
              Sat [atom] )
        | Some atoms ->
            Debug.p "Found that %a is equivalent to embedded atoms %a@\n" (pp_with_pp_var Var.pp)
              atom
              (Pp.seq ~sep:"," (pp_with_pp_var Var.pp))
              atoms ;
            eval_atoms_with_normalized_terms ~is_neq_zero atoms ) )


  and eval_atoms_with_normalized_terms ~is_neq_zero atoms =
    SatUnsat.list_fold ~init:[] atoms ~f:(fun atoms atom' ->
        let+ atoms' = eval_with_normalized_terms ~is_neq_zero atom' in
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
    Debug.p "Atom.eval %a@\n" (pp_with_pp_var Var.pp) atom ;
    let exception FoundUnsat in
    try
      map_terms atom ~f:(fun t ->
          match eval_term ~is_neq_zero t with Sat t' -> t' | Unsat -> raise_notrace FoundUnsat )
      |> eval_with_normalized_terms ~is_neq_zero
    with FoundUnsat -> Unsat


  let fold_subst_variables ~init ~f_subst ?f_post a =
    fold_map_terms a ~init ~f:(fun acc t -> Term.fold_subst_variables t ~init:acc ~f_subst ?f_post)


  let subst_variables l ~f = fold_subst_variables l ~init:() ~f_subst:(fun () v -> ((), f v)) |> snd

  let has_var_notin vars atom =
    let t1, t2 = get_terms atom in
    Term.has_var_notin vars t1 || Term.has_var_notin vars t2


  (* assumes the atom is normalized *)
  let get_as_var_ne_or_gt_zero = function
    | NotEqual (Const _, Const _) | LessThan (Const _, Const _) ->
        (* to make sure the side condition below is unambiguous *)
        None
    | (NotEqual (t, Const q) | NotEqual (Const q, t) | LessThan (Const q, t)) when Q.(equal q zero)
      ->
        (* match [x≠0] or [x>0]. Note that [0] is represented as a [Const _] when normalized but
           variables will usually (always?) be represented by [LinArith _] in normalized formulas *)
        Term.get_as_var t
    | _ ->
        None


  module Set = struct
    include Caml.Set.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    let pp_with_pp_var pp_var fmt atoms =
      if is_empty atoms then F.pp_print_string fmt "(empty)"
      else
        Pp.collection ~sep:"∧"
          ~fold:(IContainer.fold_of_pervasives_set_fold fold)
          ~pp_item:(fun fmt atom -> F.fprintf fmt "{%a}" (pp_with_pp_var pp_var) atom)
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

module TermMapOccurrences = MakeOccurrences (struct
  include Term

  let pp_set = Term.Set.pp_with_pp_var
end)

module AtomMapOccurrences = MakeOccurrences (struct
  include Atom

  let pp_set = Atom.Set.pp_with_pp_var
end)

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

    val term_eqs_is_empty : term_eqs -> bool

    type t = private
      { var_eqs: var_eqs
            (** Equality relation between variables. We want to only use canonical representatives
                from this equality relation in the rest of the formula and more generally in all of
                the abstract state. See also {!AbductiveDomain}. *)
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
            (** A simple, non-relational domain of concrete integer intervals of the form
                [x∈\[i,j\]] or [x∉\[i,j\]].

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
      ; term_eqs_occurrences: TermMapOccurrences.t
            (** like [linear_eqs_occurrences] but for [term_eqs] so bindings are from variables to
                sets of terms *)
      ; atoms_occurrences: AtomMapOccurrences.t  (** likewise for [atoms] *) }
    [@@deriving compare, equal, yojson_of]

    val get_repr : t -> Var.t -> VarUF.repr
    (** the canonical representative of a given variable *)

    val ttrue : t

    (* {2 [term_eqs] interface due to the totally opaque type} *)

    val pp_term_eqs_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

    val get_term_eq : t -> Term.t -> Var.t option

    val term_eqs_fold : (Term.t -> Var.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

    val term_eqs_iter : (Term.t -> Var.t -> unit) -> t -> unit

    val term_eqs_exists : (Term.t -> Var.t -> bool) -> t -> bool

    val term_eqs_filter : (Term.t -> Var.t -> bool) -> t -> Term.VarMap.t_

    val fold_term_eqs_vars : t -> init:'acc -> f:('acc -> Var.t -> 'acc) -> 'acc

    val subst_term_eqs : Var.t Var.Map.t -> t -> Term.VarMap.t_

    (* {2 mutations} *)

    val add_linear_eq : Var.t -> LinArith.t -> t -> t * Var.t option
    (** don't forget to call [propagate_linear_eq] after this *)

    val remove_linear_eq : Var.t -> LinArith.t -> t -> t

    val add_term_eq : Term.t -> Var.t -> t -> t * Var.t option
    (** don't forget to call [propagate_term_eq] after this *)

    val remove_term_eq : Term.t -> t -> t

    val remove_atom : Atom.t -> t -> t

    val add_tableau_eq : Var.t -> LinArith.t -> t -> t

    val add_interval : Var.t -> CItv.t -> t -> t SatUnsat.t

    val add_interval_ : Var.t -> CItv.t -> intervals -> intervals SatUnsat.t
    (** same as [add_interval] but operates on the inner [intervals] datatype *)

    val add_atom : Atom.t -> t -> t

    val remove_from_linear_eqs_occurrences : Var.t -> t -> t

    val remove_from_term_eqs_occurrences : Var.t -> t -> t

    val remove_from_atoms_occurrences : Var.t -> t -> t

    val set_var_eqs : var_eqs -> t -> t

    val set_tableau : Tableau.t -> t -> t

    val set_intervals : intervals -> t -> t

    val reset_linear : t -> t

    val reset_atoms : t -> t

    val reset_term_eqs : t -> t

    val unsafe_mk :
         var_eqs:var_eqs
      -> linear_eqs:linear_eqs
      -> term_eqs:Term.VarMap.t_
      -> tableau:Tableau.t
      -> intervals:intervals
      -> atoms:Atom.Set.t
      -> linear_eqs_occurrences:VarMapOccurrences.t
      -> term_eqs_occurrences:TermMapOccurrences.t
      -> atoms_occurrences:AtomMapOccurrences.t
      -> t
    (** escape hatch *)
  end = struct
    type term_eqs = Term.VarMap.t_ [@@deriving compare, equal, yojson_of]

    let term_eqs_is_empty = Term.VarMap.is_empty

    type t =
      { var_eqs: var_eqs
      ; linear_eqs: linear_eqs
      ; term_eqs: term_eqs
      ; tableau: Tableau.t
      ; intervals: (intervals[@yojson.opaque])
      ; atoms: Atom.Set.t
      ; linear_eqs_occurrences: VarMapOccurrences.t
      ; term_eqs_occurrences: TermMapOccurrences.t
      ; atoms_occurrences: AtomMapOccurrences.t }
    [@@deriving compare, equal, yojson_of]

    let ttrue =
      { var_eqs= VarUF.empty
      ; linear_eqs= Var.Map.empty
      ; term_eqs= Term.VarMap.empty
      ; tableau= Tableau.empty
      ; intervals= Var.Map.empty
      ; atoms= Atom.Set.empty
      ; linear_eqs_occurrences= Var.Map.empty
      ; term_eqs_occurrences= Var.Map.empty
      ; atoms_occurrences= Var.Map.empty }


    let get_repr phi x = VarUF.find phi.var_eqs x

    let get_repr_as_var phi x = (get_repr phi x :> Var.t)

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


    let pp_term_eqs_with_pp_var pp_var fmt phi =
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
            F.pp_open_hbox fmt () ;
            if not !is_first then F.pp_print_string fmt "∧" ;
            is_first := false ;
            F.fprintf fmt "%a=%a@]" (Term.pp pp_var) term pp_var var )
          phi ;
        F.pp_close_box fmt ()


    (* {2 mutations} *)

    let remove_term_eq t phi =
      Debug.p "remove_term_eq %a@\n" (Term.pp Var.pp) t ;
      let term_eqs_occurrences =
        match Term.get_as_linear t with
        | Some _ ->
            phi.term_eqs_occurrences
        | None ->
            Term.fold_variables t ~init:phi.term_eqs_occurrences ~f:(fun occurrences v' ->
                TermMapOccurrences.remove v' ~occurred_in:t occurrences )
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
                Term.fold_variables t ~init:phi.term_eqs_occurrences ~f:(fun occurrences v' ->
                    TermMapOccurrences.add v' ~occurs_in:t occurrences )
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
      match get_term_eq phi t with Some v' when Var.equal v v' -> remove_term_eq t phi | _ -> phi


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


    let add_tableau_eq v l phi = {phi with tableau= Var.Map.add v l phi.tableau}

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


    let remove_from_term_eqs_occurrences v phi =
      {phi with term_eqs_occurrences= Var.Map.remove v phi.term_eqs_occurrences}


    let remove_from_atoms_occurrences v phi =
      {phi with atoms_occurrences= Var.Map.remove v phi.atoms_occurrences}


    let set_var_eqs var_eqs phi = if phys_equal phi.var_eqs var_eqs then phi else {phi with var_eqs}

    let set_tableau tableau phi = {phi with tableau}

    let set_intervals intervals phi = {phi with intervals}

    let reset_linear phi =
      { phi with
        linear_eqs= Var.Map.empty
      ; tableau= Var.Map.empty
      ; linear_eqs_occurrences= Var.Map.empty }


    let reset_atoms phi = {phi with atoms= Atom.Set.empty; atoms_occurrences= Var.Map.empty}

    let reset_term_eqs phi =
      {phi with term_eqs= Term.VarMap.empty; term_eqs_occurrences= Var.Map.empty}


    let unsafe_mk ~var_eqs ~linear_eqs ~term_eqs ~tableau ~intervals ~atoms ~linear_eqs_occurrences
        ~term_eqs_occurrences ~atoms_occurrences =
      { var_eqs
      ; linear_eqs
      ; term_eqs
      ; tableau
      ; intervals
      ; atoms
      ; linear_eqs_occurrences
      ; term_eqs_occurrences
      ; atoms_occurrences }
  end

  include Unsafe

  let is_empty
      ({ var_eqs
       ; linear_eqs
       ; term_eqs
       ; tableau
       ; intervals
       ; atoms
       ; linear_eqs_occurrences= _
       ; term_eqs_occurrences= _
       ; atoms_occurrences= _ } [@warning "+missing-record-field-pattern"] ) =
    VarUF.is_empty var_eqs && Var.Map.is_empty linear_eqs && term_eqs_is_empty term_eqs
    && Var.Map.is_empty tableau && Var.Map.is_empty intervals && Atom.Set.is_empty atoms


  let fold_linear_eqs_vars linear_eqs ~init ~f =
    let f_eq var linarith acc = Seq.fold_left f (f acc var) (LinArith.get_variables linarith) in
    Var.Map.fold f_eq linear_eqs init


  let fold_intervals_vars intervals ~init ~f =
    let f_interval var _interval acc = f acc var in
    Var.Map.fold f_interval intervals init


  let fold_variables
      ( ({ var_eqs
         ; linear_eqs
         ; term_eqs= _
         ; tableau
         ; intervals
         ; atoms
         ; linear_eqs_occurrences= _
         ; term_eqs_occurrences= _
         ; atoms_occurrences= _ } [@warning "+missing-record-field-pattern"] ) as phi ) ~init ~f =
    let init = VarUF.fold_elements var_eqs ~init ~f in
    let init = fold_linear_eqs_vars linear_eqs ~init ~f in
    let init = fold_term_eqs_vars phi ~init ~f in
    let init = fold_linear_eqs_vars tableau ~init ~f in
    let init = fold_intervals_vars intervals ~init ~f in
    Atom.Set.fold (fun atom acc -> Atom.fold_variables atom ~init:acc ~f) atoms init


  let pp_with_pp_var pp_var fmt
      ( ({ var_eqs
         ; linear_eqs
         ; term_eqs
         ; tableau
         ; intervals
         ; atoms
         ; linear_eqs_occurrences
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
       (not (Var.Map.is_empty linear_eqs))
       "linear_eqs"
       (pp_var_map ~arrow:" = " (LinArith.pp pp_var) pp_var) )
      fmt linear_eqs ;
    (pp_if (not (term_eqs_is_empty term_eqs)) "term_eqs" (pp_term_eqs_with_pp_var pp_var)) fmt phi ;
    (pp_if (not (Var.Map.is_empty tableau)) "tableau" (Tableau.pp pp_var)) fmt tableau ;
    (pp_if (not (Var.Map.is_empty intervals)) "intervals" (pp_var_map ~arrow:"" CItv.pp pp_var))
      fmt intervals ;
    (pp_if (not (Atom.Set.is_empty atoms)) "atoms" (Atom.Set.pp_with_pp_var pp_var)) fmt atoms ;
    (pp_if
       (not (Var.Map.is_empty linear_eqs_occurrences))
       "linear_eqs_occurrences" (VarMapOccurrences.pp pp_var) )
      fmt linear_eqs_occurrences ;
    (pp_if
       (not (Var.Map.is_empty term_eqs_occurrences))
       "term_eqs_occurrences" (TermMapOccurrences.pp pp_var) )
      fmt term_eqs_occurrences ;
    (pp_if
       (not (Var.Map.is_empty atoms_occurrences))
       "atoms_occurrences" (AtomMapOccurrences.pp pp_var) )
      fmt atoms_occurrences ;
    F.pp_close_box fmt ()


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
    (** [and_atom atom (phi, new_eqs)] is
        [SatUnsat.(normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs))] *)

    val normalize : t * new_eqs -> (t * new_eqs) SatUnsat.t
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

    (** an arbitrary value *)
    let base_fuel = 10

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
      Term.subst_variables t ~f:(fun v ->
          let v_canon = (get_repr phi v :> Var.t) in
          match Var.Map.find_opt v_canon phi.linear_eqs with
          | None ->
              VarSubst v_canon
          | Some l -> (
            match LinArith.get_as_const l with
            | None ->
                (* OPTIM: don't make the term bigger *) VarSubst v_canon
            | Some q ->
                (* replace vars by constants when available to possibly trigger further
                   simplifications in atoms. This is not actually needed for [term_eqs]. *)
                QSubst q ) )


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
                let* phi, new_eqs = propagate_linear_eq ~fuel v l' (phi, new_eqs) in
                if fuel > 0 then (
                  L.d_printfln "Consuming fuel solving linear equality (from %d)" fuel ;
                  solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs l l' phi )
                else (
                  (* [fuel = 0]: give up simplifying further for fear of diverging *)
                  L.d_printfln "Ran out of fuel solving linear equality" ;
                  Sat (phi, new_eqs) ) ) )


    and discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi =
      match new_eq_opt with
      | None ->
          Sat (phi, new_eqs)
      | Some v' ->
          merge_vars ~fuel new_eqs v v' phi


    (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; don't use directly
        as it doesn't do any checks on what else should be done about [t = v] *)
    and add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t v phi =
      Debug.p "add_term_eq_and_merge_new_eq_opt %a->%a@\n" (Term.pp Var.pp) t Var.pp v ;
      let phi, new_eq_opt = add_term_eq t v phi in
      discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


    and add_linear_eq_and_solve_new_eq_opt ~fuel new_eqs v l phi =
      Debug.p "add_linear_eq_and_merge_new_eq_opt %a->%a@\n" Var.pp v (LinArith.pp Var.pp) l ;
      let phi, new_eq_opt = add_linear_eq v l phi in
      discharge_new_eq_opt ~fuel new_eqs v new_eq_opt phi


    (** TODO: at the moment this doesn't try to discover and return new equalities implied by the
        tableau (see Chapter 5 in \[2\]) *)
    and solve_tableau_restricted_eq ~fuel new_eqs w l phi =
      Debug.p "tableau %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
      let l_c = LinArith.get_constant_part l in
      let l_c_sign =
        if Q.(l_c > zero) then `Positive else if Q.(l_c = zero) then `Zero else `Negative
      in
      match (l_c_sign, LinArith.classify_minimized_maximized l) with
      | `Zero, (`Maximized | `Constant) ->
          (* [w = k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0, so [w] = [v1] = ... = [vn] = 0. *)
          let* phi, new_eqs =
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
            else (
              L.debug Analysis Verbose "Ran out of fuel pivoting the tableau %a@\n"
                (Tableau.pp Var.pp) phi.tableau ;
              Sat (phi, new_eqs) ) )


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
      | Some (v_old, v_new) -> (
          (* new equality [v_old = v_new]: we need to propagate this fact to the various domains,
             especially [linear_eqs]: we update a potential [v_old = l_old] to be [v_new = l_old],
             and if [v_new = l_new] was known we add [l_old = l_new] *)
          let v_new = (v_new :> Var.t) in
          Debug.p "new eq: %a->%a@\n" Var.pp v_old Var.pp v_new ;
          L.d_printfln "new eq: %a = %a" Var.pp v_old Var.pp v_new ;
          let new_eqs = RevList.cons (Equal (v_old, v_new)) new_eqs in
          (* substitute [v_old -> v_new] in [phi.linear_eqs] while maintaining the [linear_eqs]
             invariant *)
          let* phi, new_eqs =
            propagate_linear_eq ~fuel v_old (LinArith.of_var v_new) (phi, new_eqs)
          in
          let l_new = Var.Map.find_opt v_new phi.linear_eqs in
          let phi, l_old =
            match Var.Map.find_opt v_old phi.linear_eqs with
            | None ->
                (phi, None)
            | Some l_old ->
                (remove_linear_eq v_old l_old phi, Some l_old)
          in
          (* NOTE: we don't propagate new variable equalities to the tableau eagerly at the moment *)
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


    and propagate_in_linear_eqs ~fuel x lx ((phi, new_eqs) as phi_new_eqs) =
      Debug.p "[propagate_in_linear_eqs] %a=%a@\n  @[" Var.pp x (LinArith.pp Var.pp) lx ;
      let r =
        match Var.Map.find_opt x phi.linear_eqs_occurrences with
        | None ->
            Sat phi_new_eqs
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
      Debug.p "@]@\n" ;
      r


    and propagate_in_term_eqs ~fuel (tx : Term.t) x ((phi, new_eqs) as phi_new_eqs) =
      match Term.to_subst_target tx with
      | LinSubst _ | NonLinearTermSubst _ ->
          Sat phi_new_eqs
      | (VarSubst _ | QSubst _) as subst_target_x -> (
        match Var.Map.find_opt x phi.term_eqs_occurrences with
        | None ->
            Sat phi_new_eqs
        | Some in_term_eqs ->
            (* [tx=x] with [tx] a constant or a variable has been added to the term equalities so by
               the invariant (that we are about to restore) there are no further occurrences of [x]
               on the LHS in [phi.term_eqs] (and occurrences on the RHS are dealt on the fly by
               [Formula.Unsafe]) *)
            Debug.p "term_eq propagating %a = %a@\n" (Term.pp Var.pp) tx Var.pp x ;
            let phi = remove_from_term_eqs_occurrences x phi in
            Term.Set.fold
              (fun t phi_new_eqs_sat ->
                if Term.equal t tx then phi_new_eqs_sat
                else
                  match get_term_eq phi t with
                  | None ->
                      Debug.p "huh? %a was supposed to appear in %a@\n" Var.pp x (Term.pp Var.pp) t ;
                      phi_new_eqs_sat
                  | Some y -> (
                      let* phi, new_eqs = phi_new_eqs_sat in
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
                                       sub_t |> Term.eval_const_shallow >>= Term.simplify_shallow
                                       >>| Term.linearize >>| Term.simplify_linear
                                     with
                                     | Sat sub_t' ->
                                         sub_t'
                                     | Unsat ->
                                         raise Unsat
                                 in
                                 ((), sub_t') ) )
                        with Unsat -> Unsat
                      in
                      (* TODO: this could surface new atoms if the term contains logical operations
                         (=, ∧, ...) *)
                      let* _atoms =
                        let ty = normalize_var_const phi (Var y) in
                        Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi)
                          (Equal (t', ty))
                      in
                      let phi = remove_term_eq t phi in
                      match Term.get_as_var t' with
                      | Some y' when Var.equal y y' ->
                          Debug.p "Discarding tautology %a -> %a@\n" (Term.pp Var.pp) t' Var.pp y ;
                          Sat (phi, new_eqs)
                      | Some y' ->
                          merge_vars ~fuel new_eqs y y' phi
                      | None -> (
                        match get_term_eq phi t' with
                        | None -> (
                            Debug.p "New term_eq %a -> %a@\n" (Term.pp Var.pp) t' Var.pp y ;
                            match t' with
                            | Term.Linear l' ->
                                Debug.p "delegating to [solve_normalized_lin_eq]@\n" ;
                                solve_normalized_lin_eq ~fuel new_eqs
                                  (LinArith.of_var y |> normalize_linear phi)
                                  l' phi
                            | _ ->
                                add_term_eq_and_solve_new_eq_opt ~fuel new_eqs t' y phi )
                        | Some y' ->
                            Debug.p "Existing term_eq %a -> %a, merging %a=%a@\n" (Term.pp Var.pp)
                              t' Var.pp y' Var.pp y Var.pp y' ;
                            merge_vars ~fuel new_eqs y y' phi ) ) )
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
      propagate_in_linear_eqs ~fuel x lx phi_new_eqs >>= propagate_term_eq ~fuel (Term.Linear lx) x


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
      let w = Var.mk_fresh_restricted () in
      solve_tableau_eq ~fuel:base_fuel new_eqs w l phi


    and solve_lin_eq new_eqs t1 t2 phi =
      solve_normalized_lin_eq ~fuel:base_fuel new_eqs (normalize_linear phi t1)
        (normalize_linear phi t2) phi


    and and_var_linarith v l (phi, new_eqs) = solve_lin_eq new_eqs l (LinArith.of_var v) phi

    and normalize_linear_eqs (phi0, new_eqs) =
      let one_linear_relation ~normalize linear_eqs changed_phi_new_eqs =
        Var.Map.fold
          (fun v l acc ->
            let* changed, ((_, new_eqs) as phi_new_eqs) = acc in
            let l' = normalize phi0 l in
            let+ phi', new_eqs' = and_var_linarith v l' phi_new_eqs in
            let changed', new_eqs' =
              if phys_equal l l' then (changed, new_eqs) else (true, new_eqs')
            in
            (changed', (phi', new_eqs')) )
          linear_eqs changed_phi_new_eqs
      in
      one_linear_relation ~normalize:normalize_linear phi0.linear_eqs
        (Sat (false, (reset_linear phi0, new_eqs)))
      |> one_linear_relation
           ~normalize:(fun phi l -> normalize_linear phi l |> normalize_restricted phi)
           phi0.tableau


    (* TODO: should we check if [φ ⊢ atom] (i.e. whether [φ ∧ ¬atom] is unsat) in [normalize_atom],
       or is [normalize_atom] already just as strong? *)
    and normalize_atom phi (atom : Atom.t) =
      let atom' = Atom.map_terms atom ~f:(fun t -> normalize_var_const phi t) in
      Atom.eval ~is_neq_zero:(is_neq_zero phi) atom'


    (** return [(new_linear_equalities, phi ∧ atom)], where [new_linear_equalities] is [true] if
        [phi.linear_eqs] was changed as a result *)
    and and_normalized_atom (phi, new_eqs) = function
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
          let+ phi_new_eqs' = solve_normalized_term_eq ~fuel:base_fuel new_eqs t v phi in
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
          Sat (false, (add_atom atom' phi, new_eqs))


    and and_normalized_atoms phi_new_eqs atoms =
      SatUnsat.list_fold atoms ~init:(false, phi_new_eqs)
        ~f:(fun (linear_changed, phi_new_eqs) atom ->
          let+ changed', phi_new_eqs = and_normalized_atom phi_new_eqs atom in
          (linear_changed || changed', phi_new_eqs) )


    and and_atom atom (phi, new_eqs) =
      normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs)


    let normalize_atoms (phi, new_eqs) =
      let atoms0 = phi.atoms in
      let init = Sat (false, (reset_atoms phi, new_eqs)) in
      IContainer.fold_of_pervasives_set_fold Atom.Set.fold atoms0 ~init ~f:(fun acc atom ->
          let* changed, phi_new_eqs = acc in
          let+ changed', phi_new_eqs = and_atom atom phi_new_eqs in
          (changed || changed', phi_new_eqs) )


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
        Atom.eval_with_normalized_terms ~is_neq_zero:(is_neq_zero phi) (Equal (t', t_v))
        >>= and_normalized_atoms (phi, new_eqs)
        >>| snd
      in
      solve_normalized_term_eq ~fuel new_eqs t' v' phi


    let normalize_term_eqs ~fuel (phi0, new_eqs0) =
      term_eqs_fold
        (fun t v acc_sat_unsat ->
          let* new_lin, ((_phi, new_eqs) as phi_new_eqs) = acc_sat_unsat in
          let+ phi', new_eqs' = and_var_term ~fuel v t phi_new_eqs in
          let new_lin, new_eqs' =
            if phys_equal phi'.linear_eqs phi0.linear_eqs then (new_lin, new_eqs)
            else (true, new_eqs')
          in
          (new_lin, (phi', new_eqs')) )
        phi0
        (Sat (false, (reset_term_eqs phi0, new_eqs0)))


    let normalize_intervals phi =
      let exception FoundUnsat in
      try
        let intervals =
          Var.Map.fold
            (fun v interval intervals' ->
              let v' = (get_repr phi v :> Var.t) in
              let interval' =
                match Var.Map.find_opt v' intervals' with
                | None ->
                    interval
                | Some interval' -> (
                  (* already had another interval from another representative of the same
                     equivalence class of [v], the value is in the intersection *)
                  match CItv.intersection interval interval' with
                  | None ->
                      (* empty intersection *)
                      raise_notrace FoundUnsat
                  | Some interval'' ->
                      interval'' )
              in
              Var.Map.add v' interval' intervals' )
            phi.intervals Var.Map.empty
        in
        Sat (set_intervals intervals phi)
      with FoundUnsat -> Unsat


    let rec normalize_with_fuel ~fuel phi_new_eqs0 =
      if fuel < 0 then (
        L.d_printfln "ran out of fuel when normalizing" ;
        Sat phi_new_eqs0 )
      else
        let* new_linear_eqs, phi_new_eqs' =
          let* new_linear_eqs_from_linear, phi_new_eqs = normalize_linear_eqs phi_new_eqs0 in
          if new_linear_eqs_from_linear && fuel > 0 then
            (* do another round of linear normalization early if we will renormalize again anyway;
               no need to first normalize term_eqs and atoms w.r.t. a linear relation that's going
               to change and trigger a recompute of these anyway *)
            Sat (true, phi_new_eqs)
          else
            let* new_linear_eqs_from_terms, phi_new_eqs = normalize_term_eqs ~fuel phi_new_eqs in
            let* new_linear_eqs_from_atoms, (phi, new_eqs) = normalize_atoms phi_new_eqs in
            let+ phi = normalize_intervals phi in
            ( new_linear_eqs_from_linear || new_linear_eqs_from_terms || new_linear_eqs_from_atoms
            , (phi, new_eqs) )
        in
        if new_linear_eqs then (
          L.d_printfln "new linear equalities, consuming fuel (from %d)" fuel ;
          normalize_with_fuel ~fuel:(fuel - 1) phi_new_eqs' )
        else Sat phi_new_eqs'


    (* interface *)

    let normalize phi_new_eqs = normalize_with_fuel ~fuel:base_fuel phi_new_eqs

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


    let and_var_term v t phi_new_eqs = and_var_term ~fuel:base_fuel v t phi_new_eqs

    let and_var_var v1 v2 (phi, new_eqs) = merge_vars ~fuel:base_fuel new_eqs v1 v2 phi
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
  F.fprintf fmt "@[<hv>conditions: %a@;phi: %a@]" (Atom.Set.pp_with_pp_var pp_var) conditions
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

let and_equal_instanceof v1 v2 t formula =
  let atom = Atom.equal (Var v1) (IsInstanceOf (v2, t)) in
  and_atom atom formula


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


let prune_atom atom (formula, new_eqs) =
  (* Use [phi] to normalize [atom] here to take previous [prune]s into account. *)
  let* normalized_atoms = Formula.Normalizer.normalize_atom formula.phi atom in
  let* phi, new_eqs =
    Formula.Normalizer.and_normalized_atoms (formula.phi, new_eqs) normalized_atoms
  in
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


module DynamicTypes = struct
  let evaluate_instanceof tenv ~get_dynamic_type v typ =
    get_dynamic_type v
    |> Option.map ~f:(fun dynamic_type ->
           let is_instanceof =
             match (Typ.name dynamic_type, Typ.name typ) with
             | Some name1, Some name2 ->
                 PatternMatch.is_subtype tenv name1 name2
             | _, _ ->
                 Typ.equal dynamic_type typ
           in
           Term.of_bool is_instanceof )


  let really_simplify tenv ~get_dynamic_type formula =
    let simplify_term (t : Term.t) =
      match t with
      | IsInstanceOf (v, typ) -> (
        match evaluate_instanceof tenv ~get_dynamic_type v typ with None -> t | Some t' -> t' )
      | t ->
          t
    in
    let simplify_atom atom = Atom.map_subterms ~f:simplify_term atom in
    let open SatUnsat.Import in
    let old_atoms = formula.phi.atoms in
    let* phi, new_eqs =
      let f t v acc_phi =
        let* acc_phi in
        let t = simplify_term t in
        Formula.Normalizer.and_var_term v t acc_phi
      in
      Formula.term_eqs_fold f formula.phi
        (Sat (formula.phi |> Formula.reset_term_eqs |> Formula.reset_atoms, RevList.empty))
    in
    let+ phi, new_eqs =
      let f atom acc_phi =
        let* acc_phi in
        let atom = simplify_atom atom in
        Formula.Normalizer.and_atom atom acc_phi
      in
      Atom.Set.fold f old_atoms (Sat (phi, new_eqs))
    in
    ({formula with phi}, new_eqs)


  let has_instanceof formula =
    let in_term (t : Term.t) = match t with IsInstanceOf _ -> true | _ -> false in
    let in_atom atom = Atom.exists_subterm atom ~f:in_term in
    Formula.term_eqs_exists (fun t _v -> in_term t) formula.phi
    || Atom.Set.exists in_atom formula.phi.atoms


  let simplify tenv ~get_dynamic_type formula =
    if has_instanceof formula then really_simplify tenv ~get_dynamic_type formula
    else Sat (formula, RevList.empty)
end

let normalize tenv ~get_dynamic_type formula =
  Debug.p "normalizing now@\n" ;
  let open SatUnsat.Import in
  let* formula, new_eqs = DynamicTypes.simplify tenv ~get_dynamic_type formula in
  let+ phi, new_eqs = Formula.Normalizer.normalize (formula.phi, new_eqs) in
  ({formula with phi}, new_eqs)


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


  let subst_var_intervals subst intervals =
    Var.Map.fold
      (fun x citv new_map ->
        let x' = subst_f subst x in
        (* concrete intervals have no variables inside them *)
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
      ({Formula.var_eqs; linear_eqs; tableau; term_eqs= _; intervals; atoms} as phi) =
    Formula.unsafe_mk ~var_eqs:(VarUF.apply_subst subst var_eqs)
      ~linear_eqs:(subst_var_linear_eqs subst linear_eqs)
      ~term_eqs:(Formula.subst_term_eqs subst phi)
      ~tableau:(subst_var_linear_eqs subst tableau)
      ~intervals:(subst_var_intervals subst intervals)
      ~atoms:(subst_var_atoms subst atoms)
        (* this is only ever called during summary creation, it's safe to ditch the occurrence maps
           at this point since they will be reconstructed by callers *)
      ~linear_eqs_occurrences:Var.Map.empty ~term_eqs_occurrences:Var.Map.empty
      ~atoms_occurrences:Var.Map.empty


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
      same atom of the phi. *)
  let eliminate ~precondition_vocabulary ~keep formula =
    let var_graph = build_var_graph formula.phi in
    let vars_to_keep = get_reachable_from var_graph keep in
    L.d_printfln "Reachable vars: %a" Var.Set.pp vars_to_keep ;
    let simplify_phi phi =
      let var_eqs = VarUF.filter ~f:(fun x -> Var.Set.mem x vars_to_keep) phi.Formula.var_eqs in
      let linear_eqs =
        Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.Formula.linear_eqs
      in
      let tableau = Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.Formula.tableau in
      let term_eqs =
        Formula.term_eqs_filter
          (fun t v -> Var.Set.mem v vars_to_keep && not (Term.has_var_notin vars_to_keep t))
          phi
      in
      (* discard atoms which have variables *not* in [vars_to_keep], which in particular is enough
         to guarantee that *none* of their variables are in [vars_to_keep] thanks to transitive
         closure on the graph above *)
      let atoms =
        Atom.Set.filter (fun atom -> not (Atom.has_var_notin vars_to_keep atom)) phi.Formula.atoms
      in
      let intervals =
        Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.Formula.intervals
      in
      Formula.unsafe_mk ~var_eqs ~linear_eqs ~term_eqs ~tableau ~intervals
        ~atoms
          (* we simplify for summaries creation, it's safe to ditch the occurrence maps at this
             point since they will be reconstructed by callers *)
        ~linear_eqs_occurrences:Var.Map.empty ~term_eqs_occurrences:Var.Map.empty
        ~atoms_occurrences:Var.Map.empty
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

let simplify tenv ~get_dynamic_type ~precondition_vocabulary ~keep formula =
  let open SatUnsat.Import in
  let* formula, new_eqs = normalize tenv ~get_dynamic_type formula in
  L.d_printfln_escaped "@[Simplifying %a@ wrt %a (keep),@ with prunables=%a@]" pp formula Var.Set.pp
    keep Var.Set.pp precondition_vocabulary ;
  (* get rid of as many variables as possible *)
  let* formula = QuantifierElimination.eliminate_vars ~precondition_vocabulary ~keep formula in
  (* TODO: doing [QuantifierElimination.eliminate_vars; DeadVariables.eliminate] a few times may
     eliminate even more variables *)
  let+ formula, live_vars = DeadVariables.eliminate ~precondition_vocabulary ~keep formula in
  (formula, live_vars, new_eqs)


let is_known_zero formula v =
  Var.Map.find_opt v formula.phi.intervals |> Option.exists ~f:CItv.is_equal_to_zero
  || Var.Map.find_opt (VarUF.find formula.phi.var_eqs v :> Var.t) formula.phi.linear_eqs
     |> Option.exists ~f:LinArith.is_zero


let is_known_non_pointer formula v = Formula.is_non_pointer formula.phi v

let is_manifest ~is_allocated formula =
  Atom.Set.for_all
    (fun atom ->
      (* ignore [x≠0] when [x] is known to be allocated: pointers being allocated doesn't make an
         issue latent and we still need to remember that [x≠0] was tested by the program explicitly
      *)
      match Atom.get_as_var_ne_or_gt_zero atom with None -> false | Some x -> is_allocated x )
    formula.conditions


let get_var_repr formula v = (Formula.get_repr formula.phi v :> Var.t)

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


let and_callee_pre subst formula ~callee:formula_callee =
  and_conditions_fold_subst_variables formula ~up_to_f:formula_callee ~f:subst_find_or_new
    ~init:subst


let and_callee_post subst formula_caller ~callee:formula_callee =
  and_fold_subst_variables formula_caller ~up_to_f:formula_callee ~f:subst_find_or_new ~init:subst


let fold_variables {conditions; phi} ~init ~f =
  let init =
    let f atom acc = Atom.fold_variables atom ~init:acc ~f in
    Atom.Set.fold f conditions init
  in
  Formula.fold_variables phi ~init ~f


module Constants = struct
  module M = Caml.Map.Make (IntLit)

  let initial_cache = M.empty

  let cache = ref initial_cache

  let get_int formula i =
    match M.find_opt i !cache with
    | Some v ->
        get_var_repr formula v
    | None ->
        let v = Var.mk_fresh () in
        cache := M.add i v !cache ;
        v
end

let () = AnalysisGlobalState.register_ref Constants.cache ~init:(fun () -> Constants.initial_cache)

let absval_of_int formula i = Constants.get_int formula i
