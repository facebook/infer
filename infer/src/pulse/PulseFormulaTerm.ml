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
module Z = ZSafe
module LinArith = PulseFormulaLinArit
open SatUnsat.Import

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
      F.fprintf fmt "%a mod %a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
  | BitAnd (t1, t2) ->
      F.fprintf fmt "%a&%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
  | BitOr (t1, t2) ->
      F.fprintf fmt "%a|%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
  | BitShiftLeft (t1, t2) ->
      F.fprintf fmt "%a<<%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
  | BitShiftRight (t1, t2) ->
      F.fprintf fmt "%a>>%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
  | BitXor (t1, t2) ->
      F.fprintf fmt "%a xor %a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
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

let of_unop (unop : Unop.t) t = match unop with Neg -> Minus t | BNot -> BitNot t | LNot -> Not t

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
  let exception FoundUnsat of SatUnsat.unsat_info in
  try
    Sat
      (map_direct_subterms t ~f:(fun t' ->
           match f t' with
           | Unsat unsat_info ->
               raise_notrace (FoundUnsat unsat_info)
           | Sat t'' ->
               t'' ) )
  with FoundUnsat unsat_info -> Unsat unsat_info


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
      let reason () = F.asprintf "Undefined result when evaluating %a" (pp_no_paren Var.pp) t in
      Unsat {reason; source= __POS__}
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
      let reason () = F.asprintf "Undefined result when simplifying %a" (pp_no_paren Var.pp) t in
      Unsat {reason; source= __POS__}
  | Some (Const q) when not (Q.is_rational q) ->
      let reason () =
        F.asprintf "Non-rational result %a when simplifying %a" Q.pp_print q (pp_no_paren Var.pp) t
      in
      Unsat {reason; source= __POS__}
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
  include Stdlib.Map.Make (struct
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

(* PULSEINF: Needed to track term sets for infinite loops *)
module Set = struct
  include Stdlib.Set.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let pp_with_pp_var ?filter pp_var fmt atoms =
    if is_empty atoms then F.pp_print_string fmt "(empty)"
    else
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_set_fold fold)
        ?filter
        (fun fmt atom -> F.fprintf fmt "{%a}" (pp_no_paren pp_var) atom)
        fmt atoms


  let yojson_of_t atoms = `List (List.map (elements atoms) ~f:yojson_of_t)
end
