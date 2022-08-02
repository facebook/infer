(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module SatUnsat = PulseSatUnsat
module Var = PulseAbstractValue
module Q = QSafeCapped
module Z = ZSafe
open SatUnsat.Import

(** a humble debug mechanism: set [debug] to [true] and run [make -C infer/src test] to see the
    arithmetic engine at work in more details *)
module Debug = struct
  (** change this to [true] for more debug information *)
  let debug = false

  let dummy_formatter = F.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let p fmt = if debug then F.printf fmt else F.ifprintf dummy_formatter fmt
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

  type subst_target = QSubst of Q.t | VarSubst of Var.t | LinSubst of t

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

  val has_var : Var.t -> t -> bool

  val get_constant_part : t -> Q.t
  (** [get_as_const (c + l)] is [c] *)

  val get_coefficient : Var.t -> t -> Q.t option

  val subst : Var.t -> Var.t -> t -> t

  val get_variables : t -> Var.t Seq.t

  val fold_subst_variables : t -> init:'a -> f:('a -> Var.t -> 'a * subst_target) -> 'a * t

  val fold : t -> init:'a -> f:('a -> Var.t * Q.t -> 'a) -> 'a

  val subst_variables : t -> f:(Var.t -> subst_target) -> t

  (** {2 Tableau-Specific Operations} *)

  val is_restricted : t -> bool
  (** [true] iff all the variables involved in the expression satisfy {!Var.is_restricted} *)

  val solve_for_unrestricted : Var.t -> t -> (Var.t * t) option
  (** if [l] contains an unrestricted variable then [solve_for_unrestricted u l] is [Some (x, l')]
      where [x] is the smallest unrestricted variable in [l] and [u=l <=> x=l']. If there are no
      unrestricted variables in [l] then [solve_for_unrestricted u l] is [None]. Assumes [u∉l]. *)

  val pivot : Var.t * Q.t -> t -> t
  (** [pivot (v, q) l] assumes [v] appears in [l] with coefficient [q] and returns [l'] such that
      [l' = -(1/q)·(l - q·v)]*)

  val is_minimized : t -> bool
  (** [true] iff all the coefficients are positive, hence the constant part is a lower bound of the
      value of the linear expression (assuming all variables are restricted) *)

  val is_maximized : t -> bool
  (** [true] iff all the coefficients are negative, hence the constant part is an upper bound of the
      value of the linear expression (assuming all variables are restricted) *)
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

  type subst_target = QSubst of Q.t | VarSubst of Var.t | LinSubst of t

  let pp pp_var fmt (c, vs) =
    if VarMap.is_empty vs then Q.pp_print fmt c
    else
      let pp_c fmt c =
        if not (Q.is_zero c) then
          let plusminus, c_pos = if Q.geq c Q.zero then ('+', c) else ('-', Q.neg c) in
          F.fprintf fmt " %c%a" plusminus Q.pp_print c_pos
      in
      let pp_coeff fmt q =
        if not (Q.is_one q) then
          if Q.is_minus_one q then F.pp_print_string fmt "-" else F.fprintf fmt "%a·" Q.pp_print q
      in
      let pp_vs fmt vs =
        Pp.collection ~sep:" + "
          ~fold:(IContainer.fold_of_pervasives_map_fold VarMap.fold)
          ~pp_item:(fun fmt (v, q) -> F.fprintf fmt "%a%a" pp_coeff q pp_var v)
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


  let has_var x (_, vs) = VarMap.mem x vs

  let get_constant_part (c, _) = c

  let get_coefficient v (_, vs) = VarMap.find_opt v vs

  let subst x y ((c, vs) as l) =
    match VarMap.find_opt x vs with
    | None ->
        l
    | Some cx ->
        let vs' = VarMap.remove x vs |> VarMap.add y cx in
        (c, vs')


  let of_subst_target = function QSubst q -> of_q q | VarSubst v -> of_var v | LinSubst l -> l

  let fold_subst_variables ((c, vs_foreign) as l0) ~init ~f =
    let changed = ref false in
    let acc_f, l' =
      VarMap.fold
        (fun v_foreign q0 (acc_f, l) ->
          let acc_f, op = f acc_f v_foreign in
          (match op with VarSubst v when Var.equal v v_foreign -> () | _ -> changed := true) ;
          (acc_f, add (mult q0 (of_subst_target op)) l) )
        vs_foreign
        (init, (c, VarMap.empty))
    in
    let l' = if !changed then l' else l0 in
    (acc_f, l')


  let subst_variables l ~f = fold_subst_variables l ~init:() ~f:(fun () v -> ((), f v)) |> snd

  let get_variables (_, vs) = VarMap.to_seq vs |> Seq.map fst

  (** {2 Tableau-Specific Operations} *)

  let is_restricted (_, vs) = VarMap.for_all (fun v _ -> Var.is_restricted v) vs

  let solve_for_unrestricted w l =
    (* HACK: unrestricted variables come first so we first test if there exists any unrestricted
       variable in the map by checking its min element *)
    if VarMap.min_binding_opt (snd l) |> Option.exists ~f:(fun (v, _) -> Var.is_unrestricted v) then (
      match solve_eq l (of_var w) with
      | Unsat | Sat None ->
          None
      | Sat (Some (x, _) as r) ->
          assert (Var.is_unrestricted x) ;
          r )
    else None


  let is_minimized (_, vs) = VarMap.for_all (fun _ c -> Q.(c >= zero)) vs

  let is_maximized (_, vs) = VarMap.for_all (fun _ c -> Q.(c <= zero)) vs
end

let pp_linear_eqs what pp_var fmt m =
  if Var.Map.is_empty m then F.fprintf fmt "true (no %s)" what
  else
    Pp.collection ~sep:" ∧ "
      ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
      ~pp_item:(fun fmt (v, l) -> F.fprintf fmt "%a = %a" pp_var v (LinArith.pp pp_var) l)
      fmt m


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

  let pp = pp_linear_eqs "tableau"

  let yojson_of_t = Var.Map.yojson_of_t LinArith.yojson_of_t

  let empty = Var.Map.empty

  let do_pivot u l ((v, _) as v_coeff) t =
    let l_v = LinArith.pivot v_coeff (LinArith.subtract l (LinArith.of_var u)) in
    Var.Map.map
      (fun l ->
        LinArith.subst_variables l ~f:(fun v' ->
            if Var.equal v v' then LinSubst l_v else VarSubst v' ) )
      t
    |> Var.Map.add v l_v


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

type subst_target = LinArith.subst_target =
  | QSubst of Q.t
  | VarSubst of Var.t
  | LinSubst of LinArith.t

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
        F.fprintf fmt "(%a instanceof %a)" pp_var v (Typ.pp Pp.text) t
    | IsInt t ->
        F.fprintf fmt "is_int(%a)" (pp_no_paren pp_var) t


  let of_q q = Const q

  let of_const (c : Const.t) =
    match c with
    | Cint i ->
        IntLit.to_big_int i |> Q.of_bigint |> of_q
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


  let of_subst_target = function QSubst q -> of_q q | VarSubst v -> Var v | LinSubst l -> Linear l

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
            match[@warning "-8"] t with
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
            match[@warning "-8"] t with
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
             match f t' with Unsat -> raise FoundUnsat | Sat t'' -> t'' ) )
    with FoundUnsat -> Unsat


  let fold_subterms t ~init ~f = fold_map_subterms t ~init ~f:(fun acc t' -> (f acc t', t')) |> fst

  let map_subterms t ~f = fold_map_subterms t ~init:() ~f:(fun () t' -> ((), f t')) |> snd

  let rec fold_subst_variables t ~init ~f =
    match t with
    | Var v ->
        let acc, op = f init v in
        let t' = match op with VarSubst v' when Var.equal v v' -> t | _ -> of_subst_target op in
        (acc, t')
    | IsInstanceOf (v, typ) ->
        let acc, op = f init v in
        let t' =
          match op with
          | VarSubst v' when not (Var.equal v v') ->
              IsInstanceOf (v', typ)
          | QSubst q when Q.is_zero q ->
              zero
          | QSubst _ | VarSubst _ | LinSubst _ ->
              t
        in
        (acc, t')
    | Linear l ->
        let acc, l' = LinArith.fold_subst_variables l ~init ~f in
        let t' = if phys_equal l l' then t else Linear l' in
        (acc, t')
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
        fold_map_direct_subterms t ~init ~f:(fun acc t' -> fold_subst_variables t' ~init:acc ~f)


  let fold_variables t ~init ~f =
    fold_subst_variables t ~init ~f:(fun acc v -> (f acc v, VarSubst v)) |> fst


  let iter_variables t ~f = fold_variables t ~init:() ~f:(fun () v -> f v)

  let subst_variables t ~f = fold_subst_variables t ~init:() ~f:(fun () v -> ((), f v)) |> snd

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
    let or_raise = function Some q -> q | None -> raise Undefined in
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
              if Q.is_zero c2 then raise Undefined
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
              if Q.is_zero c2 then raise Undefined else map_z_z_opt c1 c2 Z.( mod ) |> or_raise )
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
              match[@warning "-8"] t0 with
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
          raise Undefined
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
            raise Undefined
        | Some i -> (
            if i >= 64 then (* assume 64-bit or fewer architecture *) zero
            else if i < 0 then
              (* this is undefined, maybe we should report a bug here *)
              raise Undefined
            else
              let factor = Const Q.(of_int 1 lsl i) in
              match[@warning "-8"] t with
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


  module VarMap = struct
    include Caml.Map.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    type t_ = Var.t t [@@deriving compare, equal]

    let pp_with_pp_var pp_var fmt m =
      if is_empty m then F.pp_print_string fmt "true (no term_eqs)"
      else
        Pp.collection ~sep:"∧"
          ~fold:(IContainer.fold_of_pervasives_map_fold fold)
          ~pp_item:(fun fmt (term, var) ->
            F.fprintf fmt "%a=%a" (pp_no_paren pp_var) term pp_var var )
          fmt m


    let yojson_of_t_ m = `List (List.map (bindings m) ~f:[%yojson_of: t * Var.t])

    let apply_var_subst subst m =
      fold
        (fun t v acc ->
          let t' = subst_variables t ~f:(targetted_subst_var subst) in
          let v' = subst_f subst v in
          add t' v' acc )
        m empty
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

  let equal t1 t2 = Equal (t1, t2)

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

  module EvalResultMonad = struct
    let bind_eval_result eval_result f =
      match eval_result with True | False -> eval_result | Atom atom -> f atom


    let ( let* ) x f = bind_eval_result x f
  end

  let eval_result_of_bool b = if b then True else False

  let term_of_eval_result = function
    | True ->
        Term.one
    | False ->
        Term.zero
    | Atom atom ->
        to_term atom


  let atom_of_term : Term.t -> t option = function
    (* terms that are atoms can be simplified in [eval_atom] *)
    | LessEqual (t1, t2) ->
        Some (LessEqual (t1, t2))
    | LessThan (t1, t2) ->
        Some (LessThan (t1, t2))
    | Equal (t1, t2) ->
        Some (Equal (t1, t2))
    | NotEqual (t1, t2) ->
        Some (NotEqual (t1, t2))
    | _ ->
        None


  let term_is_atom t = atom_of_term t |> Option.is_some

  let eval_const_shallow atom =
    let on_const f =
      match get_terms atom with
      | Const c1, Const c2 ->
          f c1 c2 |> eval_result_of_bool
      | _ ->
          Atom atom
    in
    match atom with
    | Equal _ ->
        on_const Q.equal
    | NotEqual _ ->
        on_const Q.not_equal
    | LessEqual _ ->
        on_const Q.leq
    | LessThan _ ->
        on_const Q.lt


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


  let get_as_embedded_atom atom =
    let of_terms is_equal t1 t2 =
      match (atom_of_term t1, t2) with
      | Some atom, Term.Const c ->
          (* [atom = 0] or [atom ≠ 1] means [atom] is false, [atom ≠ 0] or [atom = 1] means [atom]
             is true *)
          let positive = (is_equal && Q.is_one c) || ((not is_equal) && Q.is_zero c) in
          if positive then Some atom else Some (nnot atom)
      | _ ->
          None
    in
    (* [of_terms] is written for only one side, the one where [t1] is the potential atom *)
    let of_terms_symmetry is_equal atom =
      let t1, t2 = get_terms atom in
      let t1, t2 = if term_is_atom t1 then (t1, t2) else (t2, t1) in
      of_terms is_equal t1 t2
    in
    match atom with
    | Equal (Const _, _) | Equal (_, Const _) ->
        of_terms_symmetry true atom
    | NotEqual (Const _, _) | NotEqual (_, Const _) ->
        of_terms_symmetry false atom
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


  (** This assumes that the terms in the atom have been normalized/evaluated already. *)
  let rec eval_atom (atom : t) =
    let open EvalResultMonad in
    let* atom = eval_const_shallow atom in
    match get_as_linear atom with
    | Some atom' ->
        eval_atom atom'
    | None -> (
      match get_as_embedded_atom atom with
      | Some atom' ->
          eval_atom atom'
      | None ->
          eval_syntactically_equal_terms atom )


  let rec eval_term t =
    let+ t =
      Term.satunsat_map_direct_subterms ~f:eval_term t
      >>= Term.eval_const_shallow >>= Term.simplify_shallow >>| Term.linearize
      >>| Term.simplify_linear
    in
    match atom_of_term t with
    | Some atom ->
        (* terms that are atoms can be simplified in [eval_atom] *)
        eval_atom atom |> term_of_eval_result
    | None ->
        t


  let eval atom =
    let exception FoundUnsat in
    try
      map_terms atom ~f:(fun t -> match eval_term t with Sat t' -> t' | Unsat -> raise FoundUnsat)
      |> eval_atom
    with FoundUnsat -> False


  let fold_subst_variables a ~init ~f =
    fold_map_terms a ~init ~f:(fun acc t -> Term.fold_subst_variables t ~init:acc ~f)


  let subst_variables l ~f = fold_subst_variables l ~init:() ~f:(fun () v -> ((), f v)) |> snd

  let has_var_notin vars atom =
    let t1, t2 = get_terms atom in
    Term.has_var_notin vars t1 || Term.has_var_notin vars t2


  module Set = struct
    include Caml.Set.Make (struct
      type nonrec t = t [@@deriving compare]
    end)

    let pp_with_pp_var pp_var fmt atoms =
      if is_empty atoms then F.pp_print_string fmt "true (no atoms)"
      else
        Pp.collection ~sep:"∧"
          ~fold:(IContainer.fold_of_pervasives_set_fold fold)
          ~pp_item:(fun fmt atom -> F.fprintf fmt "{%a}" (pp_with_pp_var pp_var) atom)
          fmt atoms


    let yojson_of_t atoms = `List (List.map (elements atoms) ~f:yojson_of_t)
  end
end

let sat_of_eval_result (eval_result : Atom.eval_result) ~on_unsat =
  match eval_result with
  | True ->
      Sat None
  | False ->
      on_unsat () ;
      Unsat
  | Atom atom ->
      Sat (Some atom)


module VarUF =
  UnionFind.Make
    (struct
      type t = Var.t [@@deriving compare, equal]

      let is_simpler_than (v1 : Var.t) (v2 : Var.t) = (v1 :> int) < (v2 :> int)
    end)
    (Var.Set)
    (Var.Map)

type new_eq = EqZero of Var.t | Equal of Var.t * Var.t

let pp_new_eq fmt = function
  | EqZero v ->
      F.fprintf fmt "%a=0" Var.pp v
  | Equal (v1, v2) ->
      F.fprintf fmt "%a=%a" Var.pp v1 Var.pp v2


type new_eqs = new_eq list

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

  type t =
    { var_eqs: var_eqs  (** equality relation between variables *)
    ; linear_eqs: linear_eqs
          (** equalities of the form [x = l] where [l] is from linear arithmetic *)
    ; term_eqs: Term.VarMap.t_
          (** equalities of the form [t = x], used to detect when two abstract values are equal to
              the same term (hence equal) *)
    ; tableau: Tableau.t
          (** linear equalities similar to [linear_eqs] but involving only "restricted" (aka
              "slack") variables; this is used for reasoning about inequalities, see \[2\] *)
    ; atoms: Atom.Set.t  (** "everything else"; not always normalized w.r.t. the components above *)
    }
  [@@deriving compare, equal, yojson_of]

  let ttrue =
    { var_eqs= VarUF.empty
    ; linear_eqs= Var.Map.empty
    ; term_eqs= Term.VarMap.empty
    ; tableau= Tableau.empty
    ; atoms= Atom.Set.empty }


  let pp_with_pp_var pp_var fmt phi =
    F.fprintf fmt "@[<hv>%a@ &&@ %a@ &&@ %a@ &&@ %a@ &&@ %a@]"
      (VarUF.pp ~pp_empty:(fun fmt -> F.pp_print_string fmt "true (no var=var)") pp_var)
      phi.var_eqs (pp_linear_eqs "linear" pp_var) phi.linear_eqs
      (Term.VarMap.pp_with_pp_var pp_var)
      phi.term_eqs (Tableau.pp pp_var) phi.tableau (Atom.Set.pp_with_pp_var pp_var) phi.atoms


  (** module that breaks invariants more often that the rest, with an interface that is safer to use *)
  module Normalizer : sig
    val and_var_linarith : Var.t -> LinArith.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_var_term : Var.t -> Term.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_var_var : Var.t -> Var.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val and_atom : Atom.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

    val normalize_atom : t -> Atom.t -> Atom.t option SatUnsat.t

    val normalize : t * new_eqs -> (t * new_eqs) SatUnsat.t

    val implies_atom : t -> Atom.t -> bool

    val get_repr : t -> Var.t -> VarUF.repr
  end = struct
    (* Use the monadic notations when normalizing formulas. *)
    open SatUnsat.Import

    (** OVERVIEW: the best way to think about this is as a half-assed Shostak technique.

        The [var_eqs] and [linear_eqs] parts of a formula are kept in a normal form of sorts. We
        apply some deduction every time a new equality is discovered. Where this is incomplete is
        that 1) we don't insist on normalizing the linear part of the relation always, and 2) we
        stop discovering new consequences of facts after some fixed number of steps (the [fuel]
        argument of some of the functions of this module).

        Normalizing more than 1) happens on [normalize], where we rebuild the linear equality
        relation (the equalities between variables are always "normalized" thanks to the union-find
        data structure).

        For 2), there is no mitigation as saturating the consequences of what we know implies
        keeping track of *all* the consequences (to avoid diverging by re-discovering the same facts
        over and over), which would be expensive.

        There is also an interaction between equality classes and linear equalities [x = l], as each
        such key [x] in the [linear_eqs] map is (or was at some point) the representative of its
        class. Unlike (non-diverging...) Shostak techniques, we do not try very hard to normalize
        the [l] in the linear equalities.

        Disclaimer: It could be that this half-assedness is premature optimisation and that we could
        afford much more completeness. *)

    (** the canonical representative of a given variable *)
    let get_repr phi x = VarUF.find phi.var_eqs x

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
          EqZero v :: new_eqs
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
            merge_vars ~fuel new_eqs (v :> Var.t) v' phi
        | None -> (
            if (not force_no_tableau) && Var.is_restricted v && LinArith.is_restricted l then
              (* linear equalities between restricted variables belong in the [tableau] instead of
                 [linear_eqs], except if [force_no_tableau] is set *)
              solve_tableau_restricted_eq ~fuel new_eqs v l phi
            else
              match Var.Map.find_opt (v :> Var.t) phi.linear_eqs with
              | None ->
                  (* add to the [term_eqs] relation only when we also add to [linear_eqs] *)
                  let+ phi, new_eqs =
                    solve_normalized_term_eq_no_lin ~fuel new_eqs (Term.Linear l) (v :> Var.t) phi
                  in
                  let new_eqs = add_lin_eq_to_new_eqs v l new_eqs in
                  (* this can break the (as a result non-)invariant that variables in the domain of
                     [linear_eqs] do not appear in the range of [linear_eqs] *)
                  ({phi with linear_eqs= Var.Map.add (v :> Var.t) l phi.linear_eqs}, new_eqs)
              | Some l' ->
                  (* This is the only step that consumes fuel: discovering an equality [l = l']: because we
                     do not record these anywhere (except when their consequence can be recorded as [y =
                     l''] or [y = y'], we could potentially discover the same equality over and over and
                     diverge otherwise. Or could we?) *)
                  (* [l'] is possibly not normalized w.r.t. the current [phi] so take this opportunity to
                     normalize it, and replace [v]'s current binding *)
                  let l'' = normalize_linear phi l' in
                  let phi =
                    if phys_equal l' l'' then phi
                    else {phi with linear_eqs= Var.Map.add (v :> Var.t) l'' phi.linear_eqs}
                  in
                  if fuel > 0 then (
                    L.d_printfln "Consuming fuel solving linear equality (from %d)" fuel ;
                    solve_normalized_lin_eq ~fuel:(fuel - 1) new_eqs l l'' phi )
                  else (
                    (* [fuel = 0]: give up simplifying further for fear of diverging *)
                    L.d_printfln "Ran out of fuel solving linear equality" ;
                    Sat (phi, new_eqs) ) ) )


    (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; don't use directly
        as it doesn't do any checks on what else should be done about [t = v] *)
    and solve_normalized_term_eq_no_lin_ ~fuel new_eqs t v phi =
      match Term.VarMap.find_opt t phi.term_eqs with
      | None ->
          (* [t] isn't known already: add it *)
          Sat ({phi with term_eqs= Term.VarMap.add t v phi.term_eqs}, new_eqs)
      | Some v' ->
          (* [t = v'] already in the map, need to explore consequences of [v = v']*)
          merge_vars ~fuel new_eqs v v' phi


    (** TODO: at the moment this doesn't try to discover and return new equalities implied by the
        tableau (see Chapter 5 in \[2\]) *)
    and solve_tableau_restricted_eq ~fuel new_eqs w l phi =
      Debug.p "tableau %a = %a@\n" Var.pp w (LinArith.pp Var.pp) l ;
      let l_c = LinArith.get_constant_part l in
      if Q.(l_c >= zero) then (
        if LinArith.is_minimized l then (
          (* [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≥0 (and so are all possible
             values of all [vi]s since they are restricted variables), hence any possible value
             of [w] is ≥0 so [w ≥ 0] is a tautologie and we can just discard the atom *)
          Debug.p "Tautologie@\n" ;
          Sat (phi, new_eqs) )
        else
          (* [w = l] is feasible and not a tautologie (and [l] is restricted), add to the
             tableau *)
          let tableau = Var.Map.add w l phi.tableau in
          Debug.p "Add to tableau@\n" ;
          Sat ({phi with tableau}, new_eqs) )
      else if LinArith.is_maximized l then (
        (* [l_c < 0], [w = l_c + k1·v1 + ... + kn·vn], all coeffs [ki] are ≤0 so [l] denotes
           only negative values, hence cannot be ≥0: contradiction *)
        Debug.p "Contradiction!@\n" ;
        Unsat )
      else
        (* stuck, let's pivot to try to add a feasible equality to the tableau; there are two ways
           to pivot in \[2\] *)
        match Tableau.pivot_unbounded_with_positive_coeff phi.tableau w l with
        | Some tableau ->
            Sat ({phi with tableau}, new_eqs)
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
                  let phi = {phi with tableau} in
                  Debug.p "pivoted tableau: %a@\n" (Tableau.pp Var.pp) phi.tableau ;
                  solve_tableau_restricted_eq ~fuel:(fuel - 1) new_eqs w
                    (normalize_restricted phi l) phi )
            else (
              L.debug Analysis Verbose "Ran out of fuel pivoting the tableau %a@\n"
                (Tableau.pp Var.pp) phi.tableau ;
              Sat (phi, new_eqs) )


    (** add [t = v] to [phi.term_eqs] and resolves consequences of that new fact; assumes that
        linear facts have been or will be added to [phi.linear_eqs] separately

        [t] should have already been through {!normalize_var_const} and [v] should be a
        representative from {!get_repr} (w.r.t. [phi]) *)
    and solve_normalized_term_eq_no_lin ~fuel new_eqs (t : Term.t) v phi =
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
          solve_normalized_term_eq_no_lin_ ~fuel new_eqs t v phi


    (** same as {!solve_normalized_eq_no_lin} but also adds linear to [phi.linear_eqs] *)
    and solve_normalized_term_eq ~fuel new_eqs (t : Term.t) v phi =
      Debug.p "solve_normalized_term_eq: %a=%a in %a@\n" (Term.pp_no_paren Var.pp) t Var.pp v
        (pp_with_pp_var Var.pp) phi ;
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
                solve_normalized_term_eq_no_lin_ ~fuel new_eqs (Term.Linear l') v' phi
              in
              solve_normalized_lin_eq ~fuel new_eqs l'
                (normalize_linear phi (LinArith.of_var v'))
                phi )
      | Const c ->
          (* same as above but constants ([c]) are always normalized so it's simpler *)
          let* phi, new_eqs = solve_normalized_term_eq_no_lin_ ~fuel new_eqs t v phi in
          solve_normalized_lin_eq ~fuel new_eqs (LinArith.of_q c) (LinArith.of_var v) phi
      | _ ->
          solve_normalized_term_eq_no_lin ~fuel new_eqs t v phi


    and merge_vars ~fuel new_eqs v1 v2 phi =
      Debug.p "merge_vars: %a=%a@\n" Var.pp v1 Var.pp v2 ;
      let var_eqs, subst_opt = VarUF.union phi.var_eqs v1 v2 in
      let phi = {phi with var_eqs} in
      match subst_opt with
      | None ->
          (* we already knew the equality *)
          Sat (phi, new_eqs)
      | Some (v_old, v_new) -> (
          (* new equality [v_old = v_new]: we need to update a potential [v_old = l] to be [v_new =
             l], and if [v_new = l'] was known we need to also explore the consequences of [l = l'] *)
          (* NOTE: we try to maintain the invariant that for all [x=l] in [phi.linear_eqs], [x ∉
             vars(l)]. We also try to stay as close as possible (without going back and re-normalizing
             every linear equality every time we learn new equalities) to the invariant that the
             domain and the range of [phi.linear_eqs] mention distinct variables. This is to speed up
             normalization steps: when the stronger invariant holds we can normalize in one step (in
             [normalize_linear_eqs]). *)
          let v_new = (v_new :> Var.t) in
          L.d_printfln "new eq: %a = %a" Var.pp v_old Var.pp v_new ;
          let new_eqs = Equal (v_old, v_new) :: new_eqs in
          let phi, l_new =
            match Var.Map.find_opt v_new phi.linear_eqs with
            | None ->
                (phi, None)
            | Some l ->
                if LinArith.has_var v_old l then
                  let l_new = LinArith.subst v_old v_new l in
                  let linear_eqs = Var.Map.add v_new l_new phi.linear_eqs in
                  ({phi with linear_eqs}, Some l_new)
                else (phi, Some l)
          in
          let phi, l_old =
            match Var.Map.find_opt v_old phi.linear_eqs with
            | None ->
                (phi, None)
            | Some l_old ->
                (* [l_old] has no [v_old] or [v_new] by invariant so no need to subst, unlike for
                   [l_new] above: variables in [l_old] are strictly greater than [v_old], and [v_new]
                   is smaller than [v_old] *)
                ({phi with linear_eqs= Var.Map.remove v_old phi.linear_eqs}, Some l_old)
          in
          (* NOTE: we don't propagate new variable equalities to the tableau eagerly at the moment *)
          match (l_old, l_new) with
          | None, None | None, Some _ ->
              Sat (phi, new_eqs)
          | Some l, None ->
              let new_eqs = add_lin_eq_to_new_eqs v_new l new_eqs in
              Sat ({phi with linear_eqs= Var.Map.add v_new l phi.linear_eqs}, new_eqs)
          | Some l1, Some l2 ->
              (* no need to consume fuel here as we can only go through this branch finitely many
                 times because there are finitely many variables in a given formula *)
              (* TODO: we may want to keep the "simpler" representative for [v_new] between [l1] and [l2] *)
              solve_normalized_lin_eq ~fuel new_eqs l1 l2 phi )


    (* Assumes [w] is restricted, [l] is normalized. This is called [addlineq] in \[2\]. *)
    let solve_tableau_eq ~fuel new_eqs w l phi =
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
          solve_tableau_restricted_eq ~fuel new_eqs w l phi


    (** an arbitrary value *)
    let base_fuel = 10

    let solve_lin_ineq new_eqs l1 l2 phi =
      (* [l1 ≤ l2] becomes [(l2-l1) ≥ 0], encoded as [l = w] with [w] a fresh restricted variable *)
      let l = LinArith.subtract l2 l1 |> normalize_linear phi |> normalize_restricted phi in
      let w = Var.mk_fresh_restricted () in
      solve_tableau_eq ~fuel:base_fuel new_eqs w l phi


    let solve_lin_eq new_eqs t1 t2 phi =
      solve_normalized_lin_eq ~fuel:base_fuel new_eqs (normalize_linear phi t1)
        (normalize_linear phi t2) phi


    let and_var_linarith v l (phi, new_eqs) = solve_lin_eq new_eqs l (LinArith.of_var v) phi

    let normalize_linear_eqs (phi0, new_eqs) =
      Var.Map.fold
        (fun v l acc ->
          let* changed, ((_, new_eqs) as phi_new_eqs) = acc in
          let l' = normalize_linear phi0 l in
          let+ phi', new_eqs' = and_var_linarith v l' phi_new_eqs in
          let changed', new_eqs' =
            if phys_equal l l' then (changed, new_eqs) else (true, new_eqs')
          in
          (changed', (phi', new_eqs')) )
        phi0.linear_eqs
        (Sat (false, ({phi0 with linear_eqs= Var.Map.empty}, new_eqs)))


    let normalize_atom phi (atom : Atom.t) =
      let atom' = Atom.map_terms atom ~f:(fun t -> normalize_var_const phi t) in
      Atom.eval atom'
      |> sat_of_eval_result ~on_unsat:(fun () ->
             L.d_printfln "UNSAT atom: %a" (Atom.pp_with_pp_var Var.pp) atom' )


    let and_var_term ~fuel v t (phi, new_eqs) =
      Debug.p "and_var_term: %a=%a in %a@\n" Var.pp v (Term.pp_no_paren Var.pp) t
        (pp_with_pp_var Var.pp) phi ;
      let* (t' : Term.t) = normalize_var_const phi t |> Atom.eval_term in
      let v' = (get_repr phi v :> Var.t) in
      (* check if unsat given what we know of [v'] and [t'], in other words be at least as
         complete as general atoms *)
      let* _ =
        Atom.eval (Equal (t', normalize_var_const phi (Var v')))
        |> sat_of_eval_result ~on_unsat:(fun () ->
               L.d_printfln "UNSAT atom: %a == %a" (Term.pp_no_paren Var.pp) t' Var.pp v' )
      in
      solve_normalized_term_eq ~fuel new_eqs t' v' phi


    let normalize_term_eqs (phi0, new_eqs0) =
      Term.VarMap.fold
        (fun t v acc_sat_unsat ->
          let* new_lin, ((_phi, new_eqs) as phi_new_eqs) = acc_sat_unsat in
          let+ phi', new_eqs' = and_var_term ~fuel:base_fuel v t phi_new_eqs in
          let new_lin, new_eqs' =
            if phys_equal phi'.linear_eqs phi0.linear_eqs then (new_lin, new_eqs)
            else (true, new_eqs')
          in
          (new_lin, (phi', new_eqs')) )
        phi0.term_eqs
        (Sat (false, ({phi0 with term_eqs= Term.VarMap.empty}, new_eqs0)))


    (** return [(new_linear_equalities, phi ∧ atom)], where [new_linear_equalities] is [true] if
        [phi.linear_eqs] was changed as a result *)
    let and_atom atom (phi, new_eqs) =
      normalize_atom phi atom
      >>= function
      | None ->
          Sat (false, (phi, new_eqs))
      | Some (Atom.Equal (Linear _, Linear _)) ->
          assert false
      | Some (Atom.Equal (Linear l, Const c) | Atom.Equal (Const c, Linear l)) ->
          (* NOTE: {!normalize_atom} calls {!Atom.eval}, which normalizes linear equalities so
             they end up only on one side, hence only this match case is needed to detect linear
             equalities *)
          let+ phi', new_eqs = solve_lin_eq new_eqs l (LinArith.of_q c) phi in
          (true, (phi', new_eqs))
      | Some (Atom.Equal (Linear l, t) | Atom.Equal (t, Linear l))
        when Option.is_some (LinArith.get_as_var l) ->
          let v = Option.value_exn (LinArith.get_as_var l) in
          let+ phi_new_eqs' = solve_normalized_term_eq ~fuel:base_fuel new_eqs t v phi in
          (false, phi_new_eqs')
      | Some (Atom.LessEqual (Linear l, Const c)) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q c) phi in
          (true, (phi', new_eqs))
      | Some (Atom.LessEqual (Const c, Linear l)) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q c) l phi in
          (true, (phi', new_eqs))
      | Some (Atom.LessThan (Linear l, Const c)) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs l (LinArith.of_q (Q.sub c Q.one)) phi in
          (true, (phi', new_eqs))
      | Some (Atom.LessThan (Const c, Linear l)) ->
          let+ phi', new_eqs = solve_lin_ineq new_eqs (LinArith.of_q (Q.add c Q.one)) l phi in
          (true, (phi', new_eqs))
      | Some atom' ->
          Sat (false, ({phi with atoms= Atom.Set.add atom' phi.atoms}, new_eqs))


    let normalize_atoms (phi, new_eqs) =
      let atoms0 = phi.atoms in
      let init = Sat (false, ({phi with atoms= Atom.Set.empty}, new_eqs)) in
      IContainer.fold_of_pervasives_set_fold Atom.Set.fold atoms0 ~init ~f:(fun acc atom ->
          let* changed, phi_new_eqs = acc in
          let+ changed', phi_new_eqs = and_atom atom phi_new_eqs in
          (changed || changed', phi_new_eqs) )


    let rec normalize_with_fuel ~fuel phi_new_eqs0 =
      if fuel < 0 then (
        L.d_printfln "ran out of fuel when normalizing" ;
        Sat phi_new_eqs0 )
      else
        (* NOTE: [phi.tableau] is eagerly normalized so no further normalization needed here *)
        let* new_linear_eqs, phi_new_eqs' =
          let* new_linear_eqs_from_linear, phi_new_eqs = normalize_linear_eqs phi_new_eqs0 in
          if new_linear_eqs_from_linear && fuel > 0 then
            (* do another round of linear normalization early if we will renormalize again anyway;
               no need to first normalize term_eqs and atoms w.r.t. a linear relation that's going
               to change and trigger a recompute of these anyway *)
            Sat (true, phi_new_eqs)
          else
            let* new_linear_eqs_from_terms, phi_new_eqs = normalize_term_eqs phi_new_eqs in
            let+ new_linear_eqs_from_atoms, phi_new_eqs = normalize_atoms phi_new_eqs in
            ( new_linear_eqs_from_linear || new_linear_eqs_from_terms || new_linear_eqs_from_atoms
            , phi_new_eqs )
        in
        if new_linear_eqs then (
          L.d_printfln "new linear equalities, consuming fuel (from %d)" fuel ;
          normalize_with_fuel ~fuel:(fuel - 1) phi_new_eqs' )
        else Sat phi_new_eqs'


    (* interface *)

    let normalize phi_new_eqs = normalize_with_fuel ~fuel:base_fuel phi_new_eqs

    let and_atom atom phi_new_eqs =
      let phi_new_eqs' = and_atom atom phi_new_eqs >>| snd in
      Debug.p "and_atom %a -> %a@\n" (Atom.pp_with_pp_var Var.pp) atom
        (SatUnsat.pp (pp_with_pp_var Var.pp))
        (SatUnsat.map fst phi_new_eqs') ;
      phi_new_eqs'


    let and_var_term v t phi_new_eqs = and_var_term ~fuel:base_fuel v t phi_new_eqs

    let and_var_var v1 v2 (phi, new_eqs) = merge_vars ~fuel:base_fuel new_eqs v1 v2 phi

    let implies_atom phi atom =
      (* [φ ⊢ a] iff [φ ∧ ¬a] is inconsistent *)
      match and_atom (Atom.nnot atom) (phi, []) with Sat _ -> false | Unsat -> true
  end
end

(** Instead of a single formula, distinguish what we have observed to be true (coming from
    assignments) from what we have assumed to be true (coming from conditionals). This lets us delay
    reporting certain errors until no assumptions are needed (i.e. the issue is certain to arise
    regardless of context). *)
type t =
  { known: Formula.t  (** all the things we know to be true for sure *)
  ; pruned: Atom.Set.t  (** collection of conditions that have to be true along the path *)
  ; both: Formula.t  (** [both = known ∧ pruned], allows us to detect contradictions *) }
[@@deriving yojson_of]

let compare phi1 phi2 =
  if phys_equal phi1 phi2 then 0
  else [%compare: Atom.Set.t * Formula.t] (phi1.pruned, phi1.known) (phi2.pruned, phi2.known)


let equal = [%compare.equal: t]

let ttrue = {known= Formula.ttrue; pruned= Atom.Set.empty; both= Formula.ttrue}

let pp_with_pp_var pp_var fmt {known; pruned; both} =
  F.fprintf fmt "@[known=%a,@;pruned=%a,@;both=%a@]" (Formula.pp_with_pp_var pp_var) known
    (Atom.Set.pp_with_pp_var pp_var) pruned (Formula.pp_with_pp_var pp_var) both


let pp = pp_with_pp_var Var.pp

let and_known_atom atom phi =
  let open SatUnsat.Import in
  let* known, _ = Formula.Normalizer.and_atom atom (phi.known, []) in
  let+ both, new_eqs = Formula.Normalizer.and_atom atom (phi.both, []) in
  ({phi with known; both}, new_eqs)


let and_mk_atom mk_atom op1 op2 phi =
  let atom = mk_atom (Term.of_operand op1) (Term.of_operand op2) in
  and_known_atom atom phi


let and_equal = and_mk_atom Atom.equal

let and_equal_instanceof v1 v2 t phi =
  let atom = Atom.equal (Var v1) (IsInstanceOf (v2, t)) in
  and_known_atom atom phi


let and_is_int v phi =
  let atom = Atom.equal (IsInt (Var v)) Term.one in
  and_known_atom atom phi


let and_less_equal = and_mk_atom Atom.less_equal

let and_less_than = and_mk_atom Atom.less_than

let and_equal_unop v (op : Unop.t) x phi =
  and_known_atom (Equal (Var v, Term.of_unop op (Term.of_operand x))) phi


let and_equal_binop v (bop : Binop.t) x y phi =
  and_known_atom (Equal (Var v, Term.of_binop bop (Term.of_operand x) (Term.of_operand y))) phi


let prune_binop ~negated (bop : Binop.t) x y phi =
  let open SatUnsat.Import in
  let tx = Term.of_operand x in
  let ty = Term.of_operand y in
  let t = Term.of_binop bop tx ty in
  let atom = if negated then Atom.Equal (t, Term.zero) else Atom.NotEqual (t, Term.zero) in
  let* both, new_eqs = Formula.Normalizer.and_atom atom (phi.both, []) in
  let+ pruned =
    (* Use [both] to normalize [atom] here to take previous [prune]s into account. This shouldn't
       change whether [known |- pruned] overall, which is what we'll want to ultimately check in
       {!has_no_assumptions}. *)
    Formula.Normalizer.normalize_atom phi.both atom
    >>| Option.fold ~init:phi.pruned ~f:(fun pruned atom -> Atom.Set.add atom pruned)
  in
  ({phi with pruned; both}, new_eqs)


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


  let really_simplify tenv ~get_dynamic_type phi =
    let simplify_term (t : Term.t) =
      match t with
      | IsInstanceOf (v, typ) -> (
        match evaluate_instanceof tenv ~get_dynamic_type v typ with None -> t | Some t' -> t' )
      | t ->
          t
    in
    let simplify_atom atom = Atom.map_subterms ~f:simplify_term atom in
    let open SatUnsat.Import in
    let old_term_eqs = phi.both.term_eqs in
    let old_atoms = phi.both.atoms in
    let both = {phi.both with term_eqs= Term.VarMap.empty; atoms= Atom.Set.empty} in
    let* both, new_eqs =
      let f t v acc_both =
        let* acc_both in
        let t = simplify_term t in
        Formula.Normalizer.and_var_term v t acc_both
      in
      Term.VarMap.fold f old_term_eqs (Sat (both, []))
    in
    let+ both, new_eqs =
      let f atom acc_both =
        let* acc_both in
        let atom = simplify_atom atom in
        Formula.Normalizer.and_atom atom acc_both
      in
      Atom.Set.fold f old_atoms (Sat (both, new_eqs))
    in
    ({phi with both}, new_eqs)


  let has_instanceof phi =
    let in_term (t : Term.t) = match t with IsInstanceOf _ -> true | _ -> false in
    let in_atom atom = Atom.exists_subterm atom ~f:in_term in
    Term.VarMap.exists (fun t _v -> in_term t) phi.both.term_eqs
    || Atom.Set.exists in_atom phi.both.atoms


  let simplify tenv ~get_dynamic_type phi =
    if has_instanceof phi then really_simplify tenv ~get_dynamic_type phi else Sat (phi, [])
end

let normalize tenv ~get_dynamic_type phi =
  let open SatUnsat.Import in
  let* phi, new_eqs = DynamicTypes.simplify tenv ~get_dynamic_type phi in
  let* both, new_eqs = Formula.Normalizer.normalize (phi.both, new_eqs) in
  let* known, _ = Formula.Normalizer.normalize (phi.known, new_eqs) in
  let+ pruned =
    Atom.Set.fold
      (fun atom pruned_sat ->
        let* pruned = pruned_sat in
        match Formula.Normalizer.normalize_atom known atom with
        | Unsat ->
            Unsat
        | Sat None ->
            (* normalized to [true] *) pruned_sat
        | Sat (Some atom) ->
            Sat (Atom.Set.add atom pruned) )
      phi.pruned (Sat Atom.Set.empty)
  in
  ({both; known; pruned}, new_eqs)


(** translate each variable in [phi_foreign] according to [f] then incorporate each fact into [phi0] *)
let and_fold_subst_variables phi0 ~up_to_f:phi_foreign ~init ~f:f_var =
  let f_subst acc v =
    let acc', v' = f_var acc v in
    (acc', VarSubst v')
  in
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat -> raise Contradiction | Sat x -> x
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
  let and_term_eqs term_eqs_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_map_fold Term.VarMap.fold term_eqs_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) (t_foreign, v_foreign) ->
        let acc_f, t = Term.fold_subst_variables t_foreign ~init:acc_f ~f:f_subst in
        let acc_f, v = f_var acc_f v_foreign in
        let phi_new_eqs = Formula.Normalizer.and_var_term v t phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  let and_atoms atoms_foreign acc_phi_new_eqs =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold atoms_foreign ~init:acc_phi_new_eqs
      ~f:(fun (acc_f, phi_new_eqs) atom_foreign ->
        let acc_f, atom = Atom.fold_subst_variables atom_foreign ~init:acc_f ~f:f_subst in
        let phi_new_eqs = Formula.Normalizer.and_atom atom phi_new_eqs |> sat_value_exn in
        (acc_f, phi_new_eqs) )
  in
  let and_ phi_foreign acc phi =
    try
      Sat
        ( and_var_eqs phi_foreign.Formula.var_eqs (acc, (phi, []))
        |> and_linear_eqs phi_foreign.Formula.linear_eqs
        |> and_term_eqs phi_foreign.Formula.term_eqs
        |> and_atoms phi_foreign.Formula.atoms )
    with Contradiction -> Unsat
  in
  let open SatUnsat.Import in
  let* acc, (both, new_eqs) = and_ phi_foreign.both init phi0.both in
  let* acc, (known, _) = and_ phi_foreign.known acc phi0.known in
  let and_pruned pruned_foreign acc_pruned =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold pruned_foreign ~init:acc_pruned
      ~f:(fun (acc_f, pruned) atom_foreign ->
        let acc_f, atom = Atom.fold_subst_variables atom_foreign ~init:acc_f ~f:f_subst in
        let atom_opt = Formula.Normalizer.normalize_atom known atom |> sat_value_exn in
        let pruned =
          Option.fold atom_opt ~init:pruned ~f:(fun pruned atom -> Atom.Set.add atom pruned)
        in
        (acc_f, pruned) )
  in
  let+ acc, pruned =
    try Sat (and_pruned phi_foreign.pruned (acc, phi0.pruned)) with Contradiction -> Unsat
  in
  (acc, {known; pruned; both}, new_eqs)


module QuantifierElimination : sig
  val eliminate_vars : keep:Var.Set.t -> t -> t SatUnsat.t
  (** [eliminate_vars ~keep φ] substitutes every variable [x] in [φ] with [x'] whenever [x'] is a
      distinguished representative of the equivalence class of [x] in [φ] such that
      [x' ∈ keep_pre ∪ keep_post] *)
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
            raise Contradiction
        | Sat None ->
            new_map
        | Sat (Some (x'', l'')) ->
            Var.Map.add x'' l'' new_map )
      linear_eqs Var.Map.empty


  let subst_var_atoms subst atoms =
    Atom.Set.fold
      (fun atom atoms ->
        let atom' = Atom.subst_variables ~f:(targetted_subst_var subst) atom in
        Atom.Set.add atom' atoms )
      atoms Atom.Set.empty


  let subst_var_formula subst {Formula.var_eqs; linear_eqs; tableau; term_eqs; atoms} =
    { Formula.var_eqs= VarUF.apply_subst subst var_eqs
    ; linear_eqs= subst_var_linear_eqs subst linear_eqs
    ; term_eqs= Term.VarMap.apply_var_subst subst term_eqs
    ; tableau= subst_var_linear_eqs subst tableau
    ; atoms= subst_var_atoms subst atoms }


  let subst_var subst phi =
    { known= subst_var_formula subst phi.known
    ; pruned= subst_var_atoms subst phi.pruned
    ; both= subst_var_formula subst phi.both }


  let eliminate_vars ~keep phi =
    let subst =
      VarUF.reorient phi.known.var_eqs ~should_keep:(fun x ->
          (* TODO: we could probably be less coarse than keeping *all* restricted variables. This
             ensures that substituting in the tableau doesn't introduce unrestricted variables in
             the tableau. Also, getting rid of restricted variables risks losing information: if
             [x=u] with [u] restricted then [x≥0]. *)
          Var.is_restricted x || Var.Set.mem x keep )
    in
    try Sat (subst_var subst phi) with Contradiction -> Unsat
end

module DeadVariables = struct
  (** Intermediate step of [simplify]: build an (undirected) graph between variables where an edge
      between two variables means that they appear together in an atom, a linear equation, or an
      equivalence class. *)
  let build_var_graph phi =
    (* pretty naive representation of an undirected graph: a map where a vertex maps to the set of
       destination vertices and each edge has its symmetric in the map *)
    (* unused but can be useful for debugging *)
    let _pp_graph fmt graph =
      Caml.Hashtbl.iter (fun v vs -> F.fprintf fmt "%a->{%a}" Var.pp v Var.Set.pp vs) graph
    in
    (* 16 because why not *)
    let graph = Caml.Hashtbl.create 16 in
    (* add edges between all pairs of [vs] *)
    let add_all vs =
      (* add [src->vs] to [graph] (but not the symmetric edges) *)
      let add_set graph src vs =
        let dest =
          match Caml.Hashtbl.find_opt graph src with
          | None ->
              vs
          | Some dest0 ->
              Var.Set.union vs dest0
        in
        Caml.Hashtbl.replace graph src dest
      in
      Var.Set.iter (fun v -> add_set graph v vs) vs
    in
    Container.iter ~fold:VarUF.fold_congruences phi.Formula.var_eqs
      ~f:(fun ((repr : VarUF.repr), vs) -> add_all (Var.Set.add (repr :> Var.t) vs)) ;
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
    Term.VarMap.iter
      (fun t v -> union_vars_of_term t (Var.Set.singleton v) |> add_all)
      phi.Formula.term_eqs ;
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
      let[@warning "-8"] (v :: rest) = !new_vs in
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
      same atom of the formula. *)
  let eliminate ~can_be_pruned ~keep phi =
    (* We only consider [phi.both] when building the relation. Considering [phi.known] and
       [phi.pruned] as well could lead to us keeping more variables around, but that's not necessarily
       a good idea. Ignoring them means we err on the side of reporting potentially slightly more
       issues than we would otherwise, as some atoms in [phi.pruned] may vanish unfairly as a
       result. *)
    let var_graph = build_var_graph phi.both in
    let vars_to_keep = get_reachable_from var_graph keep in
    L.d_printfln "Reachable vars: %a" Var.Set.pp vars_to_keep ;
    let simplify_phi phi =
      let var_eqs =
        VarUF.filter_morphism ~f:(fun x -> Var.Set.mem x vars_to_keep) phi.Formula.var_eqs
      in
      let linear_eqs =
        Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.Formula.linear_eqs
      in
      let tableau = Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.Formula.tableau in
      let term_eqs =
        Term.VarMap.filter
          (fun t v -> Var.Set.mem v vars_to_keep && not (Term.has_var_notin vars_to_keep t))
          phi.Formula.term_eqs
      in
      (* discard atoms which have variables *not* in [vars_to_keep], which in particular is enough
         to guarantee that *none* of their variables are in [vars_to_keep] thanks to transitive
         closure on the graph above *)
      let atoms =
        Atom.Set.filter (fun atom -> not (Atom.has_var_notin vars_to_keep atom)) phi.Formula.atoms
      in
      {Formula.var_eqs; linear_eqs; term_eqs; tableau; atoms}
    in
    let known = simplify_phi phi.known in
    let both = simplify_phi phi.both in
    let pruned =
      (* discard atoms that callers have no way of influencing, i.e. more or less those that do not
         contain variables related to variables in the pre *)
      let closed_prunable_vars = get_reachable_from var_graph can_be_pruned in
      Atom.Set.filter (fun atom -> not (Atom.has_var_notin closed_prunable_vars atom)) phi.pruned
    in
    Sat ({known; pruned; both}, vars_to_keep)
end

let simplify tenv ~get_dynamic_type ~can_be_pruned ~keep phi =
  let open SatUnsat.Import in
  let* phi, new_eqs = normalize tenv ~get_dynamic_type phi in
  L.d_printfln_escaped "@[Simplifying %a@,wrt %a (keep), with prunables=%a@]" pp phi Var.Set.pp keep
    Var.Set.pp can_be_pruned ;
  (* get rid of as many variables as possible *)
  let* phi = QuantifierElimination.eliminate_vars ~keep phi in
  (* TODO: doing [QuantifierElimination.eliminate_vars; DeadVariables.eliminate] a few times may
     eliminate even more variables *)
  let+ phi, live_vars = DeadVariables.eliminate ~can_be_pruned ~keep phi in
  (phi, live_vars, new_eqs)


let is_known_zero phi v =
  Var.Map.find_opt (VarUF.find phi.both.var_eqs v :> Var.t) phi.both.linear_eqs
  |> Option.exists ~f:LinArith.is_zero


(** test if [phi.known ⊢ phi.pruned] *)
let has_no_assumptions phi =
  Atom.Set.for_all (fun atom -> Formula.Normalizer.implies_atom phi.known atom) phi.pruned


let get_known_var_repr phi v = (Formula.Normalizer.get_repr phi.known v :> Var.t)

let get_both_var_repr phi v = (Formula.Normalizer.get_repr phi.both v :> Var.t)
