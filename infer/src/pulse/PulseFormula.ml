(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Var = PulseAbstractValue

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of Var.t

(** Expressive term structure to be able to express all of SIL, but the main smarts of the formulas
    are for the equality between variables and linear arithmetic subsets. Terms (and atoms, below)
    are kept as a last-resort for when outside that fragment. *)
module Term = struct
  type t =
    | Const of Q.t
    | Var of Var.t
    | Add of t * t
    | Minus of t
    | LessThan of t * t
    | LessEqual of t * t
    | Equal of t * t
    | NotEqual of t * t
    | Mult of t * t
    | Div of t * t
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
  [@@deriving compare]

  let equal_syntax = [%compare.equal: t]

  let needs_paren = function
    | Const c when Q.geq c Q.zero && Z.equal (Q.den c) Z.one ->
        (* nonnegative integer *)
        false
    | Const _ ->
        (* negative and/or a fraction *) true
    | Var _ ->
        false
    | Minus _
    | BitNot _
    | Not _
    | Add _
    | Mult _
    | Div _
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
    | NotEqual _ ->
        true


  let rec pp_paren pp_var ~needs_paren fmt t =
    if needs_paren t then F.fprintf fmt "(%a)" (pp_no_paren pp_var) t else pp_no_paren pp_var fmt t


  and pp_no_paren pp_var fmt = function
    | Var v ->
        pp_var fmt v
    | Const c ->
        Q.pp_print fmt c
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
    | Div (t1, t2) ->
        F.fprintf fmt "%a÷%a" (pp_paren pp_var ~needs_paren) t1 (pp_paren pp_var ~needs_paren) t2
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


  let of_intlit i = Const (Q.of_bigint (IntLit.to_big_int i))

  let of_operand = function AbstractValueOperand v -> Var v | LiteralOperand i -> of_intlit i

  let one = Const Q.one

  let zero = Const Q.zero

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
    | Div ->
        Div (t1, t2)
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


  let is_zero = function Const c -> Q.equal c Q.zero | _ -> false

  let is_non_zero_const = function Const c -> not (Q.equal c Q.zero) | _ -> false

  (** Fold [f] on the strict sub-terms of [t], if any. Preserve physical equality if [f] does. *)
  let fold_map_direct_subterms t ~init ~f =
    match t with
    | Var _ | Const _ ->
        (init, t)
    | Minus t_not | BitNot t_not | Not t_not ->
        let acc, t_not' = f init t_not in
        let t' =
          if phys_equal t_not t_not' then t
          else
            match t with
            | Minus _ ->
                Minus t_not'
            | BitNot _ ->
                BitNot t_not'
            | Not _ ->
                Not t_not'
            | Var _
            | Const _
            | Add _
            | Mult _
            | Div _
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
            | NotEqual _ ->
                assert false
        in
        (acc, t')
    | Add (t1, t2)
    | Mult (t1, t2)
    | Div (t1, t2)
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
            match t with
            | Add _ ->
                Add (t1', t2')
            | Mult _ ->
                Mult (t1', t2')
            | Div _ ->
                Div (t1', t2')
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
            | Var _ | Const _ | Minus _ | BitNot _ | Not _ ->
                assert false
        in
        (acc, t')


  let map_direct_subterms t ~f =
    fold_map_direct_subterms t ~init:() ~f:(fun () t' -> ((), f t')) |> snd


  let rec fold_map_variables t ~init ~f =
    match t with
    | Var v ->
        let acc, t_v = f init v in
        let t' = match t_v with Var v' when Var.equal v v' -> t | _ -> t_v in
        (acc, t')
    | _ ->
        fold_map_direct_subterms t ~init ~f:(fun acc t' -> fold_map_variables t' ~init:acc ~f)


  let fold_variables t ~init ~f =
    fold_map_variables t ~init ~f:(fun acc v -> (f acc v, Var v)) |> fst


  let iter_variables t ~f = fold_variables t ~init:() ~f:(fun () v -> f v)

  let map_variables t ~f = fold_map_variables t ~init:() ~f:(fun () v -> ((), f v)) |> snd

  let has_var_notin vars t =
    Container.exists t ~iter:iter_variables ~f:(fun v -> not (Var.Set.mem v vars))
end

(** Basically boolean terms, used to build the part of a formula that is not equalities between
    linear arithmetic. *)
module Atom = struct
  type t =
    | LessEqual of Term.t * Term.t
    | LessThan of Term.t * Term.t
    | Equal of Term.t * Term.t
    | NotEqual of Term.t * Term.t
  [@@deriving compare]

  type atom = t

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


  let pp = pp_with_pp_var Var.pp

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

  let term_of_eval_result = function
    | True ->
        Term.one
    | False ->
        Term.zero
    | Atom atom ->
        to_term atom


  (* Many simplifications are still TODO *)
  let rec eval_term t =
    let open Term in
    let t_norm_subterms = map_direct_subterms ~f:eval_term t in
    match t_norm_subterms with
    | Var _ | Const _ ->
        t
    | Minus (Minus t) ->
        (* [--t = t] *)
        t
    | Minus (Const c) ->
        (* [-c = -1*c] *)
        Const (Q.(mul minus_one) c)
    | BitNot (BitNot t) ->
        (* [~~t = t] *)
        t
    | Not (Const c) when Q.equal c Q.zero ->
        (* [!0 = 1]  *)
        one
    | Not (Const c) when Q.equal c Q.one ->
        (* [!1 = 0]  *)
        zero
    | Add (Const c1, Const c2) ->
        (* constants *)
        Const (Q.add c1 c2)
    | Add (Const c, t) when Q.equal c Q.zero ->
        (* [0 + t = t] *)
        t
    | Add (t, Const c) when Q.equal c Q.zero ->
        (* [t + 0 = t] *)
        t
    | Mult (Const c, t) when Q.equal c Q.one ->
        (* [1 × t = t] *)
        t
    | Mult (t, Const c) when Q.equal c Q.one ->
        (* [t × 1 = t] *)
        t
    | Mult (Const c, _) when Q.equal c Q.zero ->
        (* [0 × t = 0] *)
        zero
    | Mult (_, Const c) when Q.equal c Q.zero ->
        (* [t × 0 = 0] *)
        zero
    | Div (Const c, _) when Q.equal c Q.zero ->
        (* [0 / t = 0] *)
        zero
    | Div (t, Const c) when Q.equal c Q.one ->
        (* [t / 1 = t] *)
        t
    | Div (t, Const c) when Q.equal c Q.minus_one ->
        (* [t / (-1) = -t] *)
        eval_term (Minus t)
    | Div (Minus t1, Minus t2) ->
        (* [(-t1) / (-t2) = t1 / t2] *)
        eval_term (Div (t1, t2))
    | Mod (Const c, _) when Q.equal c Q.zero ->
        (* [0 % t = 0] *)
        zero
    | Mod (_, Const q) when Q.equal q Q.one ->
        (* [t % 1 = 0] *)
        zero
    | Mod (t1, t2) when equal_syntax t1 t2 ->
        (* [t % t = 0] *)
        zero
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
    (* terms that are atoms can be simplified in [eval_atom] *)
    | LessEqual (t1, t2) ->
        eval_atom (LessEqual (t1, t2) : atom) |> term_of_eval_result
    | LessThan (t1, t2) ->
        eval_atom (LessThan (t1, t2) : atom) |> term_of_eval_result
    | Equal (t1, t2) ->
        eval_atom (Equal (t1, t2) : atom) |> term_of_eval_result
    | NotEqual (t1, t2) ->
        eval_atom (NotEqual (t1, t2) : atom) |> term_of_eval_result
    | _ ->
        t_norm_subterms


  (** This assumes that the terms in the atom have been normalized/evaluated already.

      TODO: probably a better way to implement this would be to rely entirely on
      [eval_term (term_of_atom (atom))], possibly implementing it as something about observing the
      sign/zero-ness of [t1 - t2]. *)
  and eval_atom (atom : t) =
    let t1, t2 = get_terms atom in
    match (t1, t2) with
    | Const c1, Const c2 -> (
      match atom with
      | Equal _ ->
          eval_result_of_bool (Q.equal c1 c2)
      | NotEqual _ ->
          eval_result_of_bool (not (Q.equal c1 c2))
      | LessEqual _ ->
          eval_result_of_bool (Q.leq c1 c2)
      | LessThan _ ->
          eval_result_of_bool (Q.lt c1 c2) )
    | _ ->
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


  let eval (atom : t) = map_terms atom ~f:eval_term |> eval_atom

  let fold_map_variables a ~init ~f =
    fold_map_terms a ~init ~f:(fun acc t -> Term.fold_map_variables t ~init:acc ~f)


  let has_var_notin vars atom =
    let t1, t2 = get_terms atom in
    Term.has_var_notin vars t1 || Term.has_var_notin vars t2


  module Set = Caml.Set.Make (struct
    type nonrec t = t [@@deriving compare]
  end)
end

(** "normalized" is not to be taken too seriously, it just means *some* normalization was applied
    that could result in discovering something is unsatisfiable *)
type 'a normalized = Unsat | Sat of 'a

module SatUnsatMonad = struct
  let map_normalized f norm = match norm with Unsat -> Unsat | Sat phi -> Sat (f phi)

  let ( >>| ) phi f = map_normalized f phi

  let ( let+ ) phi f = map_normalized f phi

  let bind_normalized f norm = match norm with Unsat -> Unsat | Sat phi -> f phi

  let ( >>= ) x f = bind_normalized f x

  let ( let* ) phi f = bind_normalized f phi
end

(** Linear Arithmetic*)
module LinArith : sig
  (** linear combination of variables, eg [2·x + 3/4·y + 12] *)
  type t

  val pp : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

  val is_zero : t -> bool

  val add : t -> t -> t

  val minus : t -> t

  val subtract : t -> t -> t

  val solve_eq : t -> t -> (Var.t * t) option normalized
  (** [solve_eq l1 l2] is [Sat (Some (x, l))] if [l1=l2 <=> x=l], [Sat None] if [l1 = l2] is always
      true, and [Unsat] if it is always false *)

  val of_var : Var.t -> t

  val of_intlit : IntLit.t -> t

  val of_operand : operand -> t

  val get_as_const : t -> Q.t option
  (** [get_as_const l] is [Some c] if [l=c], else [None] *)

  val get_as_var : t -> Var.t option
  (** [get_as_var l] is [Some x] if [l=x], else [None] *)

  val has_var : Var.t -> t -> bool

  val subst : Var.t -> Var.t -> t -> t

  val subst_vars : f:(Var.t -> t) -> t -> t

  val get_variables : t -> Var.t Seq.t

  val fold_map_variables : t -> init:'a -> f:('a -> Var.t -> 'a * Var.t) -> 'a * t
end = struct
  (** invariant: the representation is always "canonical": coefficients cannot be [Q.zero] *)
  type t = Q.t Var.Map.t * Q.t

  let pp pp_var fmt (vs, c) =
    if Var.Map.is_empty vs then Q.pp_print fmt c
    else
      let pp_c fmt c =
        if Q.equal c Q.zero then ()
        else
          let plusminus, c_pos = if Q.geq c Q.zero then ('+', c) else ('-', Q.neg c) in
          F.fprintf fmt " %c%a" plusminus Q.pp_print c_pos
      in
      let pp_coeff fmt q =
        if Q.equal q Q.one then ()
        else if Q.equal q Q.minus_one then F.pp_print_string fmt "-"
        else F.fprintf fmt "%a·" Q.pp_print q
      in
      let pp_vs fmt vs =
        Pp.collection ~sep:" + "
          ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
          ~pp_item:(fun fmt (v, q) -> F.fprintf fmt "%a%a" pp_coeff q pp_var v)
          fmt vs
      in
      F.fprintf fmt "@[<h>%a%a@]" pp_vs vs pp_c c


  let add (vs1, c1) (vs2, c2) =
    ( Var.Map.union
        (fun _v c1 c2 ->
          let c = Q.add c1 c2 in
          if Q.equal c Q.zero then None else Some c )
        vs1 vs2
    , Q.add c1 c2 )


  let minus (vs, c) = (Var.Map.map (fun c -> Q.neg c) vs, Q.neg c)

  let subtract l1 l2 = add l1 (minus l2)

  let zero = (Var.Map.empty, Q.zero)

  let is_zero (vs, c) = Q.equal c Q.zero && Var.Map.is_empty vs

  let mult q ((vs, c) as l) =
    if Q.equal q Q.zero then (* needed for correction: coeffs cannot be zero *) zero
    else if Q.equal q Q.one then (* purely an optimisation *) l
    else (Var.Map.map (fun c -> Q.mul q c) vs, Q.mul q c)


  let solve_eq_zero (vs, c) =
    match Var.Map.min_binding_opt vs with
    | None ->
        if Q.equal c Q.zero then Sat None else Unsat
    | Some (x, coeff) ->
        let d = Q.neg coeff in
        let vs' =
          Var.Map.fold
            (fun v' coeff' vs' ->
              if Var.equal v' x then vs' else Var.Map.add v' (Q.div coeff' d) vs' )
            vs Var.Map.empty
        in
        let c' = Q.div c d in
        Sat (Some (x, (vs', c')))


  let solve_eq l1 l2 = solve_eq_zero (subtract l1 l2)

  let of_var v = (Var.Map.singleton v Q.one, Q.zero)

  let of_intlit i = (Var.Map.empty, IntLit.to_big_int i |> Q.of_bigint)

  let of_operand = function AbstractValueOperand v -> of_var v | LiteralOperand i -> of_intlit i

  let get_as_const (vs, c) = if Var.Map.is_empty vs then Some c else None

  let get_as_var (vs, c) =
    if Q.equal c Q.zero then
      match Var.Map.is_singleton_or_more vs with
      | Singleton (x, cx) when Q.equal cx Q.one ->
          Some x
      | _ ->
          None
    else None


  let has_var x (vs, _) = Var.Map.mem x vs

  let subst x y ((vs, c) as l) =
    match Var.Map.find_opt x vs with
    | None ->
        l
    | Some cx ->
        let vs' = Var.Map.remove x vs |> Var.Map.add y cx in
        (vs', c)


  let subst_vars ~f (vs, c) = Var.Map.fold (fun v q l -> mult q (f v) |> add l) vs (Var.Map.empty, c)

  let fold_map_variables (vs_foreign, c) ~init ~f =
    let acc_f, vs =
      Var.Map.fold
        (fun v_foreign q0 (acc_f, vs) ->
          let acc_f, v = f acc_f v_foreign in
          let vs =
            match Var.Map.find_opt v vs with
            | None ->
                Var.Map.add v q0 vs
            | Some q ->
                let q' = Q.add q q0 in
                if Q.equal q' Q.zero then Var.Map.remove v vs else Var.Map.add v q vs
          in
          (acc_f, vs) )
        vs_foreign (init, Var.Map.empty)
    in
    (acc_f, (vs, c))


  let get_variables (vs, _) = Var.Map.to_seq vs |> Seq.map fst
end

module VarUF =
  UnionFind.Make
    (struct
      type t = Var.t [@@deriving compare]

      let is_simpler_than (v1 : Var.t) (v2 : Var.t) = (v1 :> int) < (v2 :> int)
    end)
    (Var.Set)

type t =
  { var_eqs: VarUF.t  (** equality relation between variables *)
  ; linear_eqs: LinArith.t Var.Map.t
        (** equalities of the form [x = l] where [l] is from linear arithmetic *)
  ; atoms: Atom.Set.t  (** not always normalized w.r.t. [var_eqs] and [linear_eqs] *) }

let ttrue = {var_eqs= VarUF.empty; linear_eqs= Var.Map.empty; atoms= Atom.Set.empty}

let pp_with_pp_var pp_var fmt phi =
  let pp_linear_eqs fmt m =
    if Var.Map.is_empty m then F.pp_print_string fmt "true (no linear)"
    else
      Pp.collection ~sep:" ∧ "
        ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
        ~pp_item:(fun fmt (v, l) -> F.fprintf fmt "%a = %a" pp_var v (LinArith.pp pp_var) l)
        fmt m
  in
  let pp_atoms fmt atoms =
    if Atom.Set.is_empty atoms then F.pp_print_string fmt "true (no atoms)"
    else
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_set_fold Atom.Set.fold)
        ~pp_item:(fun fmt atom -> F.fprintf fmt "{%a}" (Atom.pp_with_pp_var pp_var) atom)
        fmt atoms
  in
  F.fprintf fmt "[@[<hv>%a@ &&@ %a@ &&@ %a@]]"
    (VarUF.pp ~pp_empty:(fun fmt -> F.pp_print_string fmt "true (no var=var)") pp_var)
    phi.var_eqs pp_linear_eqs phi.linear_eqs pp_atoms phi.atoms


let pp = pp_with_pp_var Var.pp

(** module that breaks invariants more often that the rest, with an interface that is safer to use *)
module Normalizer : sig
  val and_var_linarith : Var.t -> LinArith.t -> t -> t normalized

  val and_var_var : Var.t -> Var.t -> t -> t normalized

  val normalize : t -> t normalized
end = struct
  (* Use the monadic notations when normalizing formulas. *)
  open SatUnsatMonad

  (** OVERVIEW: the best way to think about this is as a half-assed Shostak technique.

      The [var_eqs] and [linear_eqs] parts of a formula are kept in a normal form of sorts. We apply
      some deduction every time a new equality is discovered. Where this is incomplete is that 1) we
      don't insist on normalizing the linear part of the relation always, and 2) we stop discovering
      new consequences of facts after some fixed number of steps (the [fuel] argument of some of the
      functions of this module).

      Normalizing more than 1) happens on [normalize], where we rebuild the linear equality relation
      (the equalities between variables are always "normalized" thanks to the union-find data
      structure).

      For 2), there is no mitigation as saturating the consequences of what we know implies keeping
      track of *all* the consequences (to avoid diverging by re-discovering the same facts over and
      over), which would be expensive.

      There is also an interaction between equality classes and linear equalities [x = l], as each
      such key [x] in the [linear_eqs] map is (or was at some point) the representative of its
      class. Unlike (non-diverging...) Shostak techniques, we do not try very hard to normalize the
      [l] in the linear equalities.

      Disclaimer: It could be that this half-assedness is premature optimisation and that we could
      afford much more completeness. *)

  (** the canonical representative of a given variable *)
  let get_repr phi x = VarUF.find phi.var_eqs x

  (** substitute vars in [l] *once* with their linear form to discover more simplification
      opportunities *)
  let apply phi l =
    LinArith.subst_vars l ~f:(fun v ->
        let repr = (get_repr phi v :> Var.t) in
        match Var.Map.find_opt repr phi.linear_eqs with
        | None ->
            LinArith.of_var repr
        | Some l' ->
            l' )


  let rec solve_eq ~fuel t1 t2 phi =
    LinArith.solve_eq t1 t2
    >>= function None -> Sat phi | Some (x, l) -> merge_var_linarith ~fuel x l phi


  and merge_var_linarith ~fuel v l phi =
    let v = (get_repr phi v :> Var.t) in
    let l = apply phi l in
    match LinArith.get_as_var l with
    | Some v' ->
        merge_vars ~fuel (v :> Var.t) v' phi
    | None -> (
      match Var.Map.find_opt (v :> Var.t) phi.linear_eqs with
      | None ->
          (* this is probably dodgy as nothing guarantees that [l] does not mention [v] *)
          Sat {phi with linear_eqs= Var.Map.add (v :> Var.t) l phi.linear_eqs}
      | Some l' ->
          (* This is the only step that consumes fuel: discovering an equality [l = l']: because we
             do not record these anywhere (except when there consequence can be recorded as [y =
             l''] or [y = y'], we could potentially discover the same equality over and over and
             diverge otherwise *)
          if fuel > 0 then solve_eq ~fuel:(fuel - 1) l l' phi
          else (* [fuel = 0]: give up simplifying further for fear of diverging *) Sat phi )


  and merge_vars ~fuel v1 v2 phi =
    let var_eqs, subst_opt = VarUF.union phi.var_eqs v1 v2 in
    let phi = {phi with var_eqs} in
    match subst_opt with
    | None ->
        (* we already knew the equality *)
        Sat phi
    | Some (v_old, v_new) -> (
        (* new equality [v_old = v_new]: we need to update a potential [v_old = l] to be [v_new =
           l], and if [v_new = l'] was known we need to also explore the consequences of [l = l'] *)
        (* NOTE: we try to maintain the invariant that for all [x=l] in [phi.linear_eqs], [x ∉
           vars(l)], because other Shostak techniques do so (in fact, they impose a stricter
           condition that the domain and the range of [phi.linear_eqs] mention distinct variables),
           but some other steps of the reasoning may break that. Not sure why we bother. *)
        let v_new = (v_new :> Var.t) in
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
        match (l_old, l_new) with
        | None, None | None, Some _ ->
            Sat phi
        | Some l, None ->
            Sat {phi with linear_eqs= Var.Map.add v_new l phi.linear_eqs}
        | Some l1, Some l2 ->
            (* no need to consume fuel here as we can only go through this branch finitely many
               times because there are finitely many variables in a given formula *)
            (* TODO: we may want to keep the "simpler" representative for [v_new] between [l1] and [l2] *)
            solve_eq ~fuel l1 l2 phi )


  (** an arbitrary value *)
  let fuel = 5

  let and_var_linarith v l phi = merge_var_linarith ~fuel v l phi

  let and_var_var v1 v2 phi = merge_vars ~fuel v1 v2 phi

  let normalize_linear_eqs phi0 =
    (* reconstruct the relation from scratch *)
    Var.Map.fold
      (fun v l phi -> bind_normalized (and_var_linarith v (apply phi0 l)) phi)
      phi0.linear_eqs
      (Sat {phi0 with linear_eqs= Var.Map.empty})


  let normalize_atom phi (atom : Atom.t) =
    let normalize_term phi t =
      Term.map_variables t ~f:(fun v ->
          let v_canon = (VarUF.find phi.var_eqs v :> Var.t) in
          let q_opt =
            let open Option.Monad_infix in
            Var.Map.find_opt v_canon phi.linear_eqs >>= LinArith.get_as_const
          in
          match q_opt with None -> Var v_canon | Some q -> Const q )
    in
    let atom' = Atom.map_terms atom ~f:(fun t -> normalize_term phi t) in
    match Atom.eval atom' with
    | True ->
        Sat None
    | False ->
        L.d_printfln "Contradiction detected! %a ~> %a is False" Atom.pp atom Atom.pp atom' ;
        Unsat
    | Atom atom ->
        Sat (Some atom)


  let normalize_atoms phi =
    (* TODO: normalizing an atom may produce a new linear arithmetic or variable equality fact, we
       should detect that and feed it back to the rest of the formula *)
    let+ atoms =
      IContainer.fold_of_pervasives_set_fold Atom.Set.fold phi.atoms ~init:(Sat Atom.Set.empty)
        ~f:(fun facts atom ->
          let* facts = facts in
          normalize_atom phi atom
          >>| function None -> facts | Some atom' -> Atom.Set.add atom' facts )
    in
    {phi with atoms}


  let normalize phi = normalize_linear_eqs phi >>= normalize_atoms
end

let and_equal op1 op2 phi =
  match (op1, op2) with
  | LiteralOperand i1, LiteralOperand i2 ->
      if IntLit.eq i1 i2 then Sat phi else Unsat
  | AbstractValueOperand v, LiteralOperand i | LiteralOperand i, AbstractValueOperand v ->
      Normalizer.and_var_linarith v (LinArith.of_intlit i) phi
  | AbstractValueOperand v1, AbstractValueOperand v2 ->
      Normalizer.and_var_var v1 v2 phi


let and_atom atom phi = {phi with atoms= Atom.Set.add atom phi.atoms}

let and_less_equal op1 op2 phi =
  match (op1, op2) with
  | LiteralOperand i1, LiteralOperand i2 ->
      if IntLit.leq i1 i2 then Sat phi else Unsat
  | _ ->
      Sat (and_atom (LessEqual (Term.of_operand op1, Term.of_operand op2)) phi)


let and_less_than op1 op2 phi =
  match (op1, op2) with
  | LiteralOperand i1, LiteralOperand i2 ->
      if IntLit.lt i1 i2 then Sat phi else Unsat
  | _ ->
      Sat (and_atom (LessThan (Term.of_operand op1, Term.of_operand op2)) phi)


let and_equal_unop v (op : Unop.t) x phi =
  match op with
  | Neg ->
      Normalizer.and_var_linarith v LinArith.(minus (of_operand x)) phi
  | BNot | LNot ->
      Sat (and_atom (Equal (Term.Var v, Term.of_unop op (Term.of_operand x))) phi)


let and_equal_binop v (bop : Binop.t) x y phi =
  let and_linear_eq l = Normalizer.and_var_linarith v l phi in
  match bop with
  | PlusA _ | PlusPI ->
      LinArith.(add (of_operand x) (of_operand y)) |> and_linear_eq
  | MinusA _ | MinusPI | MinusPP ->
      LinArith.(subtract (of_operand x) (of_operand y)) |> and_linear_eq
  (* TODO: some of the below could become linear arithmetic after simplifications (e.g. up to constants) *)
  | Mult _
  | Div
  | Mod
  | Shiftlt
  | Shiftrt
  | BAnd
  | BXor
  | BOr
  (* TODO: (most) logical operators should be translated into the formula structure *)
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Ne
  | LAnd
  | LOr ->
      Sat
        (and_atom
           (Equal (Term.Var v, Term.of_binop bop (Term.of_operand x) (Term.of_operand y)))
           phi)


let prune_binop ~negated (bop : Binop.t) x y phi =
  let atom op x y =
    let tx = Term.of_operand x in
    let ty = Term.of_operand y in
    let atom : Atom.t =
      match op with
      | `LessThan ->
          LessThan (tx, ty)
      | `LessEqual ->
          LessEqual (tx, ty)
      | `NotEqual ->
          NotEqual (tx, ty)
    in
    Sat (and_atom atom phi)
  in
  match (bop, negated) with
  | Eq, false | Ne, true ->
      and_equal x y phi
  | Ne, false | Eq, true ->
      atom `NotEqual x y
  | Lt, false | Ge, true ->
      atom `LessThan x y
  | Le, false | Gt, true ->
      atom `LessEqual x y
  | Ge, false | Lt, true ->
      atom `LessEqual y x
  | Gt, false | Le, true ->
      atom `LessThan y x
  | LAnd, _
  | LOr, _
  | Mult _, _
  | Div, _
  | Mod, _
  | Shiftlt, _
  | Shiftrt, _
  | BAnd, _
  | BXor, _
  | BOr, _
  | PlusA _, _
  | PlusPI, _
  | MinusA _, _
  | MinusPI, _
  | MinusPP, _ ->
      Sat phi


let normalize phi = Normalizer.normalize phi

(** translate each variable in [phi_foreign] according to [f] then incorporate each fact into [phi0] *)
let and_fold_map_variables phi0 ~up_to_f:phi_foreign ~init ~f =
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction in
  let sat_value_exn (norm : 'a normalized) =
    match norm with Unsat -> raise Contradiction | Sat x -> x
  in
  let and_var_eqs acc =
    VarUF.fold_congruences phi_foreign.var_eqs ~init:acc
      ~f:(fun (acc_f, phi) (repr_foreign, vs_foreign) ->
        let acc_f, repr = f acc_f (repr_foreign :> Var.t) in
        IContainer.fold_of_pervasives_set_fold Var.Set.fold vs_foreign ~init:(acc_f, phi)
          ~f:(fun (acc_f, phi) v_foreign ->
            let acc_f, v = f acc_f v_foreign in
            let phi = Normalizer.and_var_var repr v phi |> sat_value_exn in
            (acc_f, phi) ) )
  in
  let and_linear_eqs acc =
    IContainer.fold_of_pervasives_map_fold Var.Map.fold phi_foreign.linear_eqs ~init:acc
      ~f:(fun (acc_f, phi) (v_foreign, l_foreign) ->
        let acc_f, v = f acc_f v_foreign in
        let acc_f, l = LinArith.fold_map_variables l_foreign ~init:acc_f ~f in
        let phi = Normalizer.and_var_linarith v l phi |> sat_value_exn in
        (acc_f, phi) )
  in
  let and_atoms acc =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold phi_foreign.atoms ~init:acc
      ~f:(fun (acc_f, phi) atom_foreign ->
        let acc_f, atom =
          Atom.fold_map_variables atom_foreign ~init:acc_f ~f:(fun acc_f v ->
              let acc_f, v' = f acc_f v in
              (acc_f, Term.Var v') )
        in
        let phi = and_atom atom phi in
        (acc_f, phi) )
  in
  try Sat (and_var_eqs (init, phi0) |> and_linear_eqs |> and_atoms) with Contradiction -> Unsat


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
  Container.iter ~fold:VarUF.fold_congruences phi.var_eqs ~f:(fun ((repr : VarUF.repr), vs) ->
      add_all (Var.Set.add (repr :> Var.t) vs) ) ;
  Var.Map.iter
    (fun v l ->
      LinArith.get_variables l
      |> Seq.fold_left (fun vs v -> Var.Set.add v vs) (Var.Set.singleton v)
      |> add_all )
    phi.linear_eqs ;
  (* add edges between all pairs of variables appearing in [t1] or [t2] (yes this is quadratic in
     the number of variables of these terms) *)
  let add_from_terms t1 t2 =
    (* compute [vs U vars(t)] *)
    let union_vars_of_term t vs =
      Term.fold_variables t ~init:vs ~f:(fun vs v -> Var.Set.add v vs)
    in
    union_vars_of_term t1 Var.Set.empty |> union_vars_of_term t2 |> add_all
  in
  Atom.Set.iter
    (fun atom ->
      let t1, t2 = Atom.get_terms atom in
      add_from_terms t1 t2 )
    phi.atoms ;
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


let simplify ~keep phi =
  let open SatUnsatMonad in
  let+ phi = normalize phi in
  L.d_printfln "Simplifying %a wrt {%a}" pp phi Var.Set.pp keep ;
  (* Get rid of atoms when they contain only variables that do not appear in atoms mentioning
         variables in [keep], or variables appearing in atoms together with variables in [keep], and
         so on. In other words, the variables to keep are all the ones transitively reachable from
         variables in [keep] in the graph connecting two variables whenever they appear together in
         a same atom of the formula. *)
  let var_graph = build_var_graph phi in
  let vars_to_keep = get_reachable_from var_graph keep in
  L.d_printfln "Reachable vars: {%a}" Var.Set.pp vars_to_keep ;
  (* discard atoms which have variables *not* in [vars_to_keep], which in particular is enough
         to guarantee that *none* of their variables are in [vars_to_keep] thanks to transitive
         closure on the graph above *)
  let var_eqs = VarUF.filter_not_in_closed_set ~keep:vars_to_keep phi.var_eqs in
  let linear_eqs = Var.Map.filter (fun v _ -> Var.Set.mem v vars_to_keep) phi.linear_eqs in
  let atoms = Atom.Set.filter (fun atom -> not (Atom.has_var_notin vars_to_keep atom)) phi.atoms in
  {var_eqs; linear_eqs; atoms}


let is_known_zero phi v =
  Var.Map.find_opt (VarUF.find phi.var_eqs v :> Var.t) phi.linear_eqs
  |> Option.exists ~f:LinArith.is_zero
