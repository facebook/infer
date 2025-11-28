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
module Term = PulseFormulaTerm
open SatUnsat.Import

(** Basically boolean terms, used to build the part of a formula that is not equalities between
    linear arithmetic. *)

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

(** [atoms_of_term ~negated t] is [Some [atom1; ..; atomN]] if [t] (or [¬t] if [negated]) is (mostly
    syntactically) equivalent to [atom1 ∧ .. ∧ atomN]. For example
    [atoms_of_term ~negated:false (Equal (Or (x, Not y), 0))] should be
    [[Equal (x, 0); NotEqual (y, 0)]]. When the term [y] is known as a boolean, it generates a
    preciser atom [Equal (y, 1)].

    [is_neq_zero] is a function that can tell if a term is known to be [≠0], and [force_to_atom] can
    be used to force converting the term into an atom even if it does not make it simpler or
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
      let reason () =
        F.asprintf "UNSAT atom according to eval_const_shallow: %a" (pp_with_pp_var Var.pp) atom
      in
      Unsat {reason; source= __POS__}
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
            let reason () =
              F.asprintf "UNSAT atom according to eval_syntactically_equal_terms: %a"
                (pp_with_pp_var Var.pp) atom
            in
            Unsat {reason; source= __POS__}
        | Atom _atom ->
            (* HACK: [eval_syntactically_equal_terms] return [Atom _] only when it didn't manage
                 to normalize anything *)
            Sat None )
      | Some atoms ->
          Debug.p "Found that %a is equivalent to embedded atoms %a@\n" (pp_with_pp_var Var.pp) atom
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
    | Unsat unsat_info ->
        SatUnsat.log_unsat unsat_info ;
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
    let exception FoundUnsat of SatUnsat.unsat_info in
    try
      Sat
        (map_terms atom ~f:(fun t ->
             match eval_term ~is_neq_zero t with
             | Sat t' ->
                 t'
             | Unsat unsat_info ->
                 raise_notrace (FoundUnsat unsat_info) ) )
    with FoundUnsat unsat_info -> Unsat unsat_info
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
  include Stdlib.Set.Make (struct
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

module Map = struct
  include Stdlib.Map.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let pp pp_binding fmt atoms =
    if is_empty atoms then F.pp_print_string fmt "(empty)"
    else
      Pp.collection ~sep:"∧"
        ~fold:(IContainer.fold_of_pervasives_map_fold fold)
        pp_binding fmt atoms


  (* TODO: ignores values for now *)
  let yojson_of_t _yojson_of_value atoms = bindings atoms |> [%yojson_of: (t * _) list]
end
