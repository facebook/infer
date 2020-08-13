(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module AbstractValue = PulseAbstractValue

module Term = struct
  type t =
    | Const of Const.t
    | Var of AbstractValue.t
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
    | Const (Cint i) when IntLit.isnegative i ->
        true
    | Const (Cfloat _) ->
        true
    | Const (Cint _ | Cfun _ | Cstr _ | Cclass _) ->
        false
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
        Const.pp Pp.text fmt c
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


  let pp_with_pp_var pp_var fmt t = pp_no_paren pp_var fmt t

  let pp fmt t = pp_with_pp_var AbstractValue.pp fmt t

  let of_absval v = Var v

  let of_intlit i = Const (Cint i)

  let one = of_intlit IntLit.one

  let zero = of_intlit IntLit.zero

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


  let is_zero = function Const c -> Const.iszero_int_float c | _ -> false

  let is_non_zero_const = function Const c -> not (Const.iszero_int_float c) | _ -> false

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
        let acc, v' = f init v in
        let t' = if phys_equal v v' then t else Var v' in
        (acc, t')
    | _ ->
        fold_map_direct_subterms t ~init ~f:(fun acc t' -> fold_map_variables t' ~init:acc ~f)
end

(** Basically boolean terms, used to build formulas. *)
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


  let pp = pp_with_pp_var AbstractValue.pp

  let nnot = function
    | LessEqual (t1, t2) ->
        LessThan (t2, t1)
    | LessThan (t1, t2) ->
        LessEqual (t2, t1)
    | Equal (t1, t2) ->
        NotEqual (t1, t2)
    | NotEqual (t1, t2) ->
        Equal (t1, t2)


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


  (* other simplifications TODO:
     - push Minus inwards
     - (t1+i1)+((i2+t2)+i3) -> (t1+t2)+(i1+i2+i3): need to flatten trees of additions (and Minus)
     - same for multiplications, possibly others too
  *)
  let rec eval_term t =
    let open Term in
    let t_norm_subterms = map_direct_subterms ~f:eval_term t in
    match t_norm_subterms with
    | Var _ | Const _ ->
        t
    | Minus (Minus t) ->
        (* [--t = t] *)
        t
    | Minus (Const (Cint i)) ->
        (* [-i = -1*i] *)
        Const (Cint (IntLit.(mul minus_one) i))
    | BitNot (BitNot t) ->
        (* [~~t = t] *)
        t
    | Not (Const c) when Const.iszero_int_float c ->
        (* [!0 = 1]  *)
        one
    | Not (Const c) when Const.isone_int_float c ->
        (* [!1 = 0]  *)
        zero
    | Add (Const (Cint i1), Const (Cint i2)) ->
        (* constants *)
        Const (Cint (IntLit.add i1 i2))
    | Add (Const c, t) when Const.iszero_int_float c ->
        (* [0 + t = t] *)
        t
    | Add (t, Const c) when Const.iszero_int_float c ->
        (* [t + 0 = t] *)
        t
    | Mult (Const c, t) when Const.isone_int_float c ->
        (* [1 × t = t] *)
        t
    | Mult (t, Const c) when Const.isone_int_float c ->
        (* [t × 1 = t] *)
        t
    | Mult (Const c, _) when Const.iszero_int_float c ->
        (* [0 × t = 0] *)
        zero
    | Mult (_, Const c) when Const.iszero_int_float c ->
        (* [t × 0 = 0] *)
        zero
    | Div (Const c, _) when Const.iszero_int_float c ->
        (* [0 / t = 0] *)
        zero
    | Div (t, Const c) when Const.isone_int_float c ->
        (* [t / 1 = t] *)
        t
    | Div (t, Const c) when Const.isminusone_int_float c ->
        (* [t / (-1) = -t] *)
        eval_term (Minus t)
    | Div (Minus t1, Minus t2) ->
        (* [(-t1) / (-t2) = t1 / t2] *)
        eval_term (Div (t1, t2))
    | Mod (Const c, _) when Const.iszero_int_float c ->
        (* [0 % t = 0] *)
        zero
    | Mod (_, Const (Cint i)) when IntLit.isone i ->
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
    | Const (Cint i1), Const (Cint i2) -> (
      match atom with
      | Equal _ ->
          eval_result_of_bool (IntLit.eq i1 i2)
      | NotEqual _ ->
          eval_result_of_bool (IntLit.neq i1 i2)
      | LessEqual _ ->
          eval_result_of_bool (IntLit.leq i1 i2)
      | LessThan _ ->
          eval_result_of_bool (IntLit.lt i1 i2) )
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


  module Set = Caml.Set.Make (struct
    type nonrec t = t [@@deriving compare]
  end)
end

module UnionFind = UnionFind.Make (struct
  type t = Term.t [@@deriving compare]

  let is_simpler_than (t1 : Term.t) (t2 : Term.t) =
    match (t1, t2) with
    | Const _, _ ->
        true
    | _, Const _ ->
        false
    | Var _, _ ->
        true
    | _, Var _ ->
        false
    | _ ->
        false
end)

(** The main datatype is either in a normal form [True | False | NormalForm _], or in a
    not-yet-normalized form [Atom _ | And _], or a mix of both.

    Note the absence of disjunction and negation: negations are interpreted eagerly and
    under-approximately by only remembering the first produced disjunct, and disjunctions are kept
    in {!Term.t} form. *)
type t =
  | True
  | False
  | NormalForm of
      { congruences: UnionFind.t
            (** equality relation between terms represented by a union-find data structure with
                canonical representatives for each class of congruent terms *)
      ; facts: Atom.Set.t
            (** atoms not of the form [Equal _], normalized with respect to the congruence relation
                and the {!Atom.eval} function *) }
  | And of t * t
  | Atom of Atom.t

let ffalse = False

let is_literal_false = function False -> true | _ -> false

let ttrue = True

let rec pp_with_pp_var pp_var fmt = function
  | True ->
      F.fprintf fmt "true"
  | False ->
      F.fprintf fmt "false"
  | Atom atom ->
      Atom.pp_with_pp_var pp_var fmt atom
  | NormalForm {congruences; facts} ->
      let pp_collection ~fold ~sep ~pp_item fmt coll =
        let pp_coll_aux is_first item =
          F.fprintf fmt "@[<h>%s%a@]" (if is_first then "" else sep) pp_item item ;
          (* is_first not true anymore *) false
        in
        F.fprintf fmt "@[<hv>%t@]" (fun _fmt -> fold coll ~init:true ~f:pp_coll_aux |> ignore)
      in
      let term_pp_paren = Term.pp_paren pp_var ~needs_paren:Term.needs_paren in
      let pp_ts_or_repr repr fmt ts =
        if UnionFind.Set.is_empty ts then term_pp_paren fmt repr
        else
          pp_collection ~sep:"="
            ~fold:(IContainer.fold_of_pervasives_set_fold UnionFind.Set.fold)
            ~pp_item:term_pp_paren fmt ts
      in
      let pp_congruences fmt congruences =
        let is_empty = ref true in
        pp_collection ~sep:" ∧ " ~fold:UnionFind.fold_congruences fmt congruences
          ~pp_item:(fun fmt ((repr : UnionFind.repr), ts) ->
            is_empty := false ;
            F.fprintf fmt "%a=%a" term_pp_paren (repr :> Term.t) (pp_ts_or_repr (repr :> Term.t)) ts ) ;
        if !is_empty then pp_with_pp_var pp_var fmt True
      in
      let pp_atoms fmt atoms =
        if Atom.Set.is_empty atoms then pp_with_pp_var pp_var fmt True
        else
          pp_collection ~sep:"∧"
            ~fold:(IContainer.fold_of_pervasives_set_fold Atom.Set.fold)
            ~pp_item:(fun fmt atom -> F.fprintf fmt "{%a}" (Atom.pp_with_pp_var pp_var) atom)
            fmt atoms
      in
      F.fprintf fmt "[@[<hv>%a@ &&@ %a@]]" pp_congruences congruences pp_atoms facts
  | And (phi1, phi2) ->
      F.fprintf fmt "{%a}∧{%a}" (pp_with_pp_var pp_var) phi1 (pp_with_pp_var pp_var) phi2


let pp = pp_with_pp_var AbstractValue.pp

module NormalForm : sig
  val of_formula : t -> t
  (** This computes equivalence classes between terms induced by the given conjunctive formula, then
      symbolically evaluates the resulting terms and atoms to form a [NormalForm _] term equivalent
      to the input formula, or [True] or [False]. *)

  val to_formula : UnionFind.t -> Atom.Set.t -> t
  (** transforms a congruence relation and set of atoms into a formula without [NormalForm _]
      sub-formulas *)
end = struct
  (* NOTE: throughout this module some cases that are never supposed to happen at the moment are
     handled nonetheless to avoid hassle and surprises in the future. *)

  let to_formula uf facts =
    let phi = Atom.Set.fold (fun atom phi -> And (Atom atom, phi)) facts True in
    let phi =
      UnionFind.fold_congruences uf ~init:phi ~f:(fun conjuncts (repr, terms) ->
          L.d_printf "@\nEquivalence class of %a: " Term.pp (repr :> Term.t) ;
          UnionFind.Set.fold
            (fun t conjuncts ->
              L.d_printf "%a," Term.pp t ;
              if phys_equal t (repr :> Term.t) then conjuncts
              else And (Atom (Equal ((repr :> Term.t), t)), conjuncts) )
            terms conjuncts )
    in
    L.d_ln () ;
    phi


  (** used for quickly detecting contradictions *)
  exception Contradiction

  (** normalize term by replacing every (sub-)term by its canonical representative *)
  let rec apply_term uf t =
    match (UnionFind.find_opt uf t :> Term.t option) with
    | None ->
        (* no representative found for [t], look for substitution opportunities in its sub-terms *)
        Term.map_direct_subterms t ~f:(fun t' -> apply_term uf t')
    | Some t_repr ->
        t_repr


  let apply_atom uf (atom : Atom.t) =
    let atom' = Atom.map_terms atom ~f:(fun t -> apply_term uf t) in
    match Atom.eval atom' with
    | True ->
        None
    | False ->
        (* early exit on contradictions *)
        L.d_printfln "Contradiction detected! %a ~> %a is False" Atom.pp atom Atom.pp atom' ;
        raise_notrace Contradiction
    | Atom atom ->
        Some atom


  (** normalize atomes by replacing every (sub-)term by their canonical representative then
      symbolically evaluating the result *)
  let normalize_atoms uf ~add_to:facts0 atoms =
    List.fold atoms ~init:facts0 ~f:(fun facts atom ->
        match apply_atom uf atom with None -> facts | Some atom' -> Atom.Set.add atom' facts )


  (** Extract [NormalForm _] from the given formula and return the formula without that part
      (replaced with [True]). If there are several [NormalForm _] sub-formulas, return only one of
      them and leave the others in. *)
  let split_normal_form phi =
    let rec find_normal_form normal_form phi =
      match phi with
      | NormalForm _ when Option.is_some normal_form ->
          (normal_form, phi)
      | NormalForm {congruences; facts} ->
          (Some (congruences, facts), ttrue)
      | True | False | Atom _ ->
          (normal_form, phi)
      | And (phi1, phi2) ->
          let normal_form, phi1' = find_normal_form normal_form phi1 in
          let normal_form, phi2' = find_normal_form normal_form phi2 in
          let phi' =
            if phys_equal phi1' phi && phys_equal phi2' phi2 then phi else And (phi1', phi2')
          in
          (normal_form, phi')
    in
    find_normal_form None phi


  (** split the given formula into [(uf, facts, atoms)] where [phi] is equivalent to
      [uf ∧ facts ∧ atoms], [facts] are in normal form w.r.t. [uf], but [atoms] are not and need
      to be normalized *)
  let rec gather_congruences_and_facts ((uf, facts, atoms) as acc) phi =
    match phi with
    | True ->
        acc
    | False ->
        raise Contradiction
    | Atom (Equal _ as atom) ->
        (* Normalize the terms of the equality w.r.t. the equalities we have discovered so far. Note
           that we don't go back and normalize the existing equalities w.r.t. the new atom, which is
           dodgy. Doing so could have adverse perf implications.

           Note also that other (non-[Equal]) atoms are not yet normalized, this will happen after
           [gather_congruences_and_facts] has run. *)
        apply_atom uf atom
        |> Option.value_map ~default:acc ~f:(function
             | Atom.Equal (t1, t2) ->
                 let uf = UnionFind.union uf t1 t2 in
                 (* change facts into atoms when the equality relation changes so they will be normalized
                    again later using the new equality relation *)
                 let atoms_with_facts =
                   Atom.Set.fold (fun atom atoms -> atom :: atoms) facts atoms
                 in
                 (uf, Atom.Set.empty, atoms_with_facts)
             | atom ->
                 (uf, facts, atom :: atoms) )
    | Atom atom ->
        (uf, facts, atom :: atoms)
    | And (phi1, phi2) ->
        let acc' = gather_congruences_and_facts acc phi1 in
        gather_congruences_and_facts acc' phi2
    | NormalForm {congruences; facts} ->
        gather_congruences_and_facts acc (to_formula congruences facts)


  let of_formula phi =
    (* start from a pre-existing normal form if any *)
    let (uf, facts), phi =
      match split_normal_form phi with
      | Some uf_facts, phi ->
          (uf_facts, phi)
      | None, phi ->
          ((UnionFind.empty, Atom.Set.empty), phi)
    in
    try
      let uf, facts, new_facts = gather_congruences_and_facts (uf, facts, []) phi in
      let facts = normalize_atoms uf ~add_to:facts new_facts in
      NormalForm {congruences= uf; facts}
    with Contradiction -> ffalse
end

let mk_less_than t1 t2 = Atom (LessThan (t1, t2))

let mk_less_equal t1 t2 = Atom (LessEqual (t1, t2))

let mk_equal t1 t2 = Atom (Equal (t1, t2))

let mk_not_equal t1 t2 = Atom (NotEqual (t1, t2))

(** propagates literal [False] *)
let aand phi1 phi2 =
  if is_literal_false phi1 || is_literal_false phi2 then ffalse else And (phi1, phi2)


let rec nnot phi =
  match phi with
  | True ->
      False
  | False ->
      True
  | Atom a ->
      Atom (Atom.nnot a)
  | NormalForm {congruences; facts} ->
      NormalForm.to_formula congruences facts |> nnot
  | And (phi1, _phi2) ->
      (* HACK/TODO: this keeps only one disjunct of the negation, which is ok for
         under-approximation even though it's quite incomplete (especially for [And (True,
         phi)]!). We could work harder at disjunctions if that proves to be an issue. *)
      nnot phi1


(** Detects terms that look like formulas, but maybe the logic in here would be better in
    [Atom.eval_term] to avoid duplicating reasoning steps. *)
let rec of_term (t : Term.t) =
  match t with
  | And (t1, t2) ->
      aand (of_term t1) (of_term t2)
  | LessThan (t1, t2) ->
      mk_less_than t1 t2
  | LessEqual (t1, t2) ->
      mk_less_equal t1 t2
  | Equal (t1, t2) ->
      mk_equal t1 t2
  | NotEqual (t1, t2) ->
      mk_not_equal t1 t2
  | Const (Cint i) ->
      if IntLit.iszero i then ffalse else ttrue
  | Const (Cfloat f) ->
      if Float.equal f Float.zero then ffalse else ttrue
  | Const (Cstr _ | Cfun _ | Cclass _) ->
      ttrue
  | Mult (t1, t2) ->
      (* [t1 × t2 ≠ 0] iff [t1 ≠ 0] && [t2 ≠ 0] *)
      aand (of_term t1) (of_term t2)
  | Div (t1, t2) when Term.equal_syntax t1 t2 ->
      (* [t ÷ t = 1] *)
      ttrue
  | Div (t1, t2) ->
      (* [t1 ÷ t2 ≠ 0] iff [t1 ≠ 0 ∧ t1 ≥ t2] *)
      aand (of_term t1) (mk_less_equal t2 t1)
  | Not t ->
      nnot (of_term t)
  | Minus t ->
      (* [-t ≠ 0] iff [t ≠ 0] *)
      of_term t
  | Add (t1, Minus t2) | Add (Minus t1, t2) ->
      (* [t1 - t2 ≠ 0] iff [t1 ≠ t2] *)
      mk_not_equal t1 t2
  | Or _
  | Add _
  | Var _
  | Mod _
  | BitAnd _
  | BitOr _
  | BitNot _
  | BitShiftLeft _
  | BitShiftRight _
  | BitXor _ ->
      (* default case: we don't know how to change the term itself into a formula so we represent
         the fact that [t] is "true" by [t ≠ 0] *)
      Atom (NotEqual (t, Term.zero))


let of_term_binop bop t1 t2 =
  (* be brutal and convert to a term, then trust that [of_term] will restore the formula structure
     as the conversion is lossless *)
  Term.of_binop bop t1 t2 |> of_term


let normalize phi = NormalForm.of_formula phi

let simplify ~keep:_ phi = (* TODO: actually remove variables not in [keep] *) normalize phi

let rec fold_map_variables phi ~init ~f =
  match phi with
  | True | False ->
      (init, phi)
  | NormalForm {congruences; facts} ->
      NormalForm.to_formula congruences facts |> fold_map_variables ~init ~f
  | Atom atom ->
      let acc, atom' = Atom.fold_map_variables atom ~init ~f in
      let phi' = if phys_equal atom atom' then phi else Atom atom' in
      (acc, phi')
  | And (phi1, phi2) ->
      let acc, phi1' = fold_map_variables phi1 ~init ~f in
      let acc, phi2' = fold_map_variables phi2 ~init:acc ~f in
      if phys_equal phi1' phi1 && phys_equal phi2' phi2 then (acc, phi)
      else (acc, And (phi1', phi2'))
