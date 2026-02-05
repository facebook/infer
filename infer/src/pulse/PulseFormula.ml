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
open SatUnsat.Import
module LinArith = PulseFormulaLinArit
module Tableau = PulseFormulaTableau
module Term = PulseFormulaTerm
module Atom = PulseFormulaAtom
module VarUF = UnionFind.Make (Var) (Var.Set) (Var.Map)
module InstanceOf = PulseFormulaInstanceOf
module Formula = PulseFormulaPhi

let pp_var_map ?filter ~arrow pp_val pp_var fmt var_map =
  Pp.collection ~sep:"@;∧ "
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    ?filter
    (fun fmt (v, value) -> F.fprintf fmt "%a%s%a" pp_var v arrow pp_val value)
    fmt var_map


type new_eq = Formula.new_eq = EqZero of Var.t | Equal of Var.t * Var.t

let pp_new_eq = Formula.pp_new_eq

type new_eqs = new_eq RevList.t

type t =
  { conditions: int Atom.Map.t
        (** collection of conditions that have been assumed (via [PRUNE] CFG nodes) along the path.
            Note that these conditions are *not* normalized w.r.t. [phi]: [phi] already contains
            them so normalization w.r.t. [phi] would make them trivially true most of the time.

            Each condition is associated with the *call depth* at which the condition was added: 0
            if the condition is from the current function, 1 if it's in a direct callee, N if the
            condition appears after a call chain of length N. *)
  ; phi: Formula.t
        (** the arithmetic constraints of the current symbolic state; true in both the pre and post
            since abstract values [Var.t] have immutable semantics *) }
[@@deriving compare, equal, yojson_of]

let pp_conditions pp_var fmt conditions =
  (Atom.Map.pp (fun fmt (atom, depth) ->
       if Config.debug_level_analysis < 3 then
         F.fprintf fmt "{%a}" (Atom.pp_with_pp_var pp_var) atom
       else F.fprintf fmt "{%a@%d}" (Atom.pp_with_pp_var pp_var) atom depth ) )
    fmt conditions


let add_condition (atom, depth) conditions =
  Atom.Map.update atom
    (fun depth0_opt -> Some (min (Option.value depth0_opt ~default:Int.max_value) depth))
    conditions


let add_conditions (atoms, depth) conditions =
  List.fold atoms ~init:conditions ~f:(fun conditions atom ->
      add_condition (atom, depth) conditions )


let ttrue = {conditions= Atom.Map.empty; phi= Formula.ttrue}

(* added pulse-infinite *)
let extract_path_cond (var : t) : int Atom.Map.t = var.conditions

let extract_term_cond (var : t) = Formula.get_terminal_conds var.phi

let extract_term_cond2 (var : t) = Formula.get_terminal_terms var.phi

let map_is_empty (conds : int Atom.Map.t) = Atom.Map.is_empty conds

let set_is_empty (conds : Atom.Set.t) = Atom.Set.is_empty conds

let termset_is_empty (conds : Term.Set.t) = Term.Set.is_empty conds

let formula_is_empty (var : t) =
  map_is_empty (extract_path_cond var)
  && set_is_empty (extract_term_cond var)
  && termset_is_empty (extract_term_cond2 var)


let pp_with_pp_var pp_var fmt {conditions; phi} =
  let pp_conditions fmt conditions =
    if Atom.Map.is_empty conditions then F.pp_print_string fmt "(empty)"
    else pp_conditions pp_var fmt conditions
  in
  F.fprintf fmt "@[<hv>conditions: %a@;phi: %a@]" pp_conditions conditions
    (Formula.pp_with_pp_var pp_var) phi


let pp = pp_with_pp_var Var.pp

module Intervals = struct
  let interval_and_var_of_operand phi = function
    | Term.AbstractValueOperand v ->
        (Some (Formula.get_repr phi v :> Var.t), Var.Map.find_opt v phi.Formula.intervals)
    | Term.ConstOperand (Cint i) ->
        (None, Some (CItv.equal_to i))
    | Term.ConstOperand (Cfun _ | Cstr _ | Cfloat _ | Cclass _) ->
        (None, None)
    | Term.FunctionApplicationOperand _ ->
        (None, None)


  let interval_of_operand phi operand = interval_and_var_of_operand phi operand |> snd

  let update_formula formula intervals =
    {formula with phi= Formula.set_intervals intervals formula.phi}


  let incorporate_new_eqs new_eqs formula =
    Debug.p "Intervals.incorporate_new_eqs %a@\n" Formula.pp_new_eqs new_eqs ;
    RevList.to_list new_eqs
    |> SatUnsat.list_fold ~init:formula.phi.intervals ~f:(fun intervals new_eq ->
           Debug.p "intervals incorporating %a@\n" Formula.pp_new_eq new_eq ;
           match new_eq with
           | Formula.EqZero v ->
               Formula.add_interval_ v (CItv.equal_to IntLit.zero) intervals
           | Formula.Equal (v_old, v_new) -> (
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


  let update_formula_for_infinite_loop_checker ~need_atom binop op1 op2 formula =
    let binop_unknown (binop : Binop.t) =
      match binop with Eq | Ne | Le | Lt | Gt | Ge -> false | _ -> true
    in
    if phys_equal need_atom false || binop_unknown binop then formula
    else
      let atom_to_binop (binop : Binop.t) =
        match binop with
        | Eq ->
            (false, Atom.equal)
        | Ne ->
            (false, Atom.not_equal)
        | Le ->
            (false, Atom.less_equal)
        | Lt ->
            (false, Atom.less_than)
        | Gt ->
            (true, Atom.less_than)
        | Ge ->
            (true, Atom.less_equal)
        | _ ->
            L.die InternalError
              "PULSEINF: and_binop: Wrong argument to [mk_atom_of_binop]: %a -- this should never \
               happen "
              Binop.pp binop
      in
      let inv, op = atom_to_binop binop in
      let invcond = inv in
      (* Handle non-termination cases with while (x == x) by translating (x == x) to (0 == 0) *)
      (* If this is not done, each x has a new version per iteration and we have a false negative *)
      let opcond =
        match (Term.of_operand op1, Term.of_operand op2) with
        | Term.Var v1, Term.Var v2 ->
            phys_equal v1 v2
        | _, _ ->
            false
      in
      let swapcond = match (binop : Binop.t) with Eq -> true | _ -> false in
      let swapterm = Atom.equal Term.zero Term.zero in
      let atom =
        if opcond && swapcond then swapterm
        else if invcond then op (Term.of_operand op2) (Term.of_operand op1)
        else op (Term.of_operand op1) (Term.of_operand op2)
      in
      let newphi = Formula.and_termcond_atoms formula.phi [atom] in
      {formula with phi= newphi}


  let and_binop ~negated binop op1 op2 ?(need_atom = false) (formula, new_eqs) =
    let v1_opt, i1_opt = interval_and_var_of_operand formula.phi op1 in
    let v2_opt, i2_opt = interval_and_var_of_operand formula.phi op2 in
    match CItv.abduce_binop_is_true ~negated binop i1_opt i2_opt with
    | Unsatisfiable ->
        let reason () =
          F.asprintf "%s(%a %a %a) UNSAT according to concrete intervals"
            (if negated then "not" else "")
            (Pp.option CItv.pp) i1_opt Binop.pp binop (Pp.option CItv.pp) i2_opt
        in
        Unsat {reason; source= __POS__}
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
        let formula =
          if Config.pulse_experimental_infinite_loop_checker then
            update_formula_for_infinite_loop_checker ~need_atom binop op1 op2 formula
          else formula
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
        let reason () =
          F.asprintf "%a=%a=%a UNSAT according to concrete intervals" Var.pp v (Pp.option CItv.pp)
            citv_caller_opt CItv.pp citv_callee
        in
        Unsat {reason; source= __POS__}
    | Satisfiable (Some abduce_caller, _abduce_callee) ->
        let+ phi = Formula.add_interval v abduce_caller phi in
        (phi, new_eqs)
    | Satisfiable (None, _) ->
        Sat (phi, new_eqs)
end

let and_atom atom formula ~add_term =
  let open SatUnsat.Import in
  let* phi, new_eqs = Formula.Normalizer.and_atom atom (formula.phi, RevList.empty) ~add_term in
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
  let+ formula, new_eqs' = and_atom atom formula ~add_term:false in
  (formula, RevList.append new_eqs new_eqs')


let and_equal op1 op2 formula = and_mk_atom Eq op1 op2 formula

let and_equal_vars v1 v2 formula =
  and_equal (AbstractValueOperand v1) (AbstractValueOperand v2) formula


let and_not_equal = and_mk_atom Ne

let and_is_int v formula =
  let atom = Atom.equal (IsInt (Var v)) Term.one in
  and_atom atom formula ~add_term:false


let and_less_equal = and_mk_atom Le

let and_less_than = and_mk_atom Lt

let and_equal_unop v (op : Unop.t) x formula =
  let* formula = Intervals.unop v op x formula in
  and_atom (Equal (Var v, Term.of_unop op (Term.of_operand x))) formula ~add_term:false


let and_equal_binop v (bop : Binop.t) x y formula =
  let* formula = Intervals.binop v bop x y formula in
  and_atom
    (Equal (Var v, Term.of_binop bop (Term.of_operand x) (Term.of_operand y)))
    formula ~add_term:false


let and_equal_string_concat v x y formula =
  and_atom
    (Equal (Var v, StringConcat (Term.of_operand x, Term.of_operand y)))
    formula ~add_term:false


let prune_atom ~depth atom (formula, new_eqs) ~add_term =
  (* Use [phi] to normalize [atom] here to take previous [prune]s into account. *)
  Debug.p "prune atom %a in %a@\n" (Atom.pp_with_pp_var Var.pp) atom pp formula ;
  let* normalized_atoms = Formula.Normalizer.normalize_atom formula.phi atom in
  let* phi, new_eqs =
    Formula.Normalizer.and_normalized_atoms (formula.phi, new_eqs) normalized_atoms
      ~orig_atom:[atom] ~add_term
  in
  (* Sticking this call in slightly hopefully *)
  let* phi, new_eqs = Formula.Normalizer.propagate_atom atom (phi, new_eqs) in
  let conditions =
    List.fold normalized_atoms ~init:formula.conditions ~f:(fun conditions atom ->
        add_condition (atom, depth) conditions )
  in
  let+ formula = Intervals.incorporate_new_eqs new_eqs {phi; conditions} in
  (formula, new_eqs)


let prune_atoms ~depth atoms formula_new_eqs =
  (* dont add atom on that path as it would be doubly added by prune_binop/and_binop then *)
  SatUnsat.list_fold atoms ~init:formula_new_eqs ~f:(fun formula_new_eqs atom ->
      prune_atom ~depth atom formula_new_eqs ~add_term:false )


let and_path_flush formula =
  let phi = Formula.and_path_flush formula.phi in
  {formula with phi}


let infinite_loop_checker_prune_binop bop tx ty t formula =
  (* Check for cases like while (x == x) by rewriting them to while (0 == 0) *)
  let opcond = match (tx, ty) with Term.Var v1, Term.Var v2 -> phys_equal v1 v2 | _, _ -> false in
  let swapcond = match (bop : Binop.t) with Eq -> true | _ -> false in
  let swapterm = Term.of_binop Eq Term.zero Term.zero in
  let atom = if opcond && swapcond then swapterm else t in
  let phi = Formula.and_termcond_binop formula.phi atom in
  {formula with phi}


let prune_binop ?(depth = 0) ~negated (bop : Binop.t) ?(need_atom = false) x y formula =
  let tx = Term.of_operand x in
  let ty = Term.of_operand y in
  let t = Term.of_binop bop tx ty in
  let formula =
    if Config.pulse_experimental_infinite_loop_checker then
      infinite_loop_checker_prune_binop bop tx ty t formula
    else formula
  in
  let atoms =
    Option.value_exn
      (Atom.atoms_of_term ~is_neq_zero:(Formula.is_neq_zero formula.phi) ~force_to_atom:true
         ~negated t )
  in
  (* NOTE: [Intervals.and_binop] may tip off the rest of the formula about the new equality so it's
     important to do [prune_atoms] *first* otherwise it might become trivial. For instance adding [x
     = 4] would prune [4 = 4] and so not add anything to [formula.conditions] instead of adding [x =
     4]. *)
  prune_atoms ~depth atoms (formula, RevList.empty)
  >>= Intervals.and_binop ~negated bop x y ~need_atom


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
      Debug.p "known non zero of %a is %b@\n" Var.pp v known_non_zero ;
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
        (Typ.pp_full Pp.text) t Var.pp v
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
    and_atom
      (Atom.equal (Var v1) (IsInstanceOf {var= v2; typ= t; nullable}))
      formula ~add_term:false
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
            ~add_term:false
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
    (acc', Term.VarSubst v')
  in
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction of unsat_info in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat unsat_info -> raise_notrace (Contradiction unsat_info) | Sat x -> x
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
        let phi_new_eqs =
          Formula.Normalizer.and_atom atom phi_new_eqs ~add_term:false |> sat_value_exn
        in
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
    with Contradiction unsat_info -> Unsat unsat_info
  in
  let open SatUnsat.Import in
  let+ acc, (phi, new_eqs) = and_ formula_foreign.phi init formula0.phi in
  (acc, {formula0 with phi}, new_eqs)


let and_conditions_fold_subst_variables phi0 ~up_to_f:phi_foreign ~init ~f:f_var =
  Debug.p "START and_conditions_fold_subst_variables" ;
  let f_subst acc v =
    let acc', v' = f_var acc v in
    (acc', Term.VarSubst v')
  in
  (* propagate [Unsat] faster using this exception *)
  let exception Contradiction of unsat_info in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat unsat_info -> raise_notrace (Contradiction unsat_info) | Sat x -> x
  in
  let add_conditions conditions_foreign init =
    IContainer.fold_of_pervasives_map_fold Atom.Map.fold conditions_foreign ~init
      ~f:(fun (acc_f, phi_new_eqs) (atom_foreign, depth) ->
        let acc_f, atom = Atom.fold_subst_variables atom_foreign ~init:acc_f ~f_subst in
        let phi_new_eqs =
          prune_atom ~depth:(depth + 1) atom phi_new_eqs ~add_term:true |> sat_value_exn
        in
        (acc_f, phi_new_eqs) )
  in
  try
    let acc, (phi, new_eqs) = add_conditions phi_foreign.conditions (init, (phi0, RevList.empty)) in
    Debug.p "END and_conditions_fold_subst_variables" ;
    Sat (acc, phi, new_eqs)
  with Contradiction unsat_info -> Unsat unsat_info


module QuantifierElimination : sig
  val eliminate_vars : precondition_vocabulary:Var.Set.t -> keep:Var.Set.t -> t -> t SatUnsat.t
  (** [eliminate_vars ~precondition_vocabulary ~keep formula] substitutes every variable [x] in
      [formula.phi] with [x'] whenever [x'] is a distinguished representative of the equivalence
      class of [x] in [formula.phi] such that [x' ∈ keep_pre ∪ keep_post]. It also similarly
      substitutes variables in [formula.conditions] that are not in [precondition_vocabulary]. *)
end = struct
  exception Contradiction of unsat_info

  let subst_var_linear_eqs subst linear_eqs =
    Var.Map.fold
      (fun x l new_map ->
        let x' = Term.subst_f subst x in
        let l' = LinArith.subst_variables ~f:(Term.targetted_subst_var subst) l in
        match LinArith.solve_eq (LinArith.of_var x') l' with
        | Unsat unsat_info ->
            L.d_printfln "Contradiction found: %a=%a became %a=%a with is Unsat" Var.pp x
              (LinArith.pp Var.pp) l Var.pp x' (LinArith.pp Var.pp) l' ;
            raise_notrace (Contradiction unsat_info)
        | Sat None ->
            new_map
        | Sat (Some (x'', l'')) ->
            Var.Map.add x'' l'' new_map )
      linear_eqs Var.Map.empty


  (* used for both intervals and type constraints *)
  let subst_key_var_only subst intervals =
    Var.Map.fold
      (fun x citv new_map ->
        let x' = Term.subst_f subst x in
        (* concrete intervals/types have no variables inside them *)
        Var.Map.add x' citv new_map )
      intervals Var.Map.empty


  let subst_var_atoms subst atoms =
    Atom.Set.map (fun atom -> Atom.subst_variables ~f:(Term.targetted_subst_var subst) atom) atoms


  let subst_var_atoms_for_conditions ~precondition_vocabulary subst atoms =
    Atom.Map.fold
      (fun atom depth atoms' ->
        let changed, atom' =
          Atom.fold_subst_variables atom ~init:false ~f_subst:(fun changed v ->
              if Var.Set.mem v precondition_vocabulary then (changed, VarSubst v)
              else
                (* [v] is not mentioned in the precondition but maybe call sites can still affect its
                   truth value if it is related to other values from the precondition via other
                   atoms. Substitute [v] by its canonical representative otherwise the atom will
                   become truly dead as it will mention a variable that is not to be kept. *)
                let v' = Term.subst_f subst v in
                ((not (Var.equal v v')) || changed, VarSubst v') )
        in
        if changed then
          match Atom.eval ~is_neq_zero:(fun _ -> false) atom' with
          | Unsat unsat_info ->
              raise_notrace (Contradiction unsat_info)
          | Sat atoms'' ->
              add_conditions (atoms'', depth) atoms'
        else add_condition (atom', depth) atoms' )
      atoms Atom.Map.empty


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
    with Contradiction unsat_info -> Unsat unsat_info
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
      Stdlib.Hashtbl.iter (fun v vs -> F.fprintf fmt "%a->{%a}" Var.pp v Var.Set.pp vs) graph
    in
    (* 16 because why not *)
    let graph = Stdlib.Hashtbl.create 16 in
    (* add [src->vs] to [graph] (but not the symmetric edges) *)
    let add_set src vs =
      let dest =
        match Stdlib.Hashtbl.find_opt graph src with
        | None ->
            vs
        | Some dest0 ->
            Var.Set.union vs dest0
      in
      Stdlib.Hashtbl.replace graph src dest
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
    let reachable = Stdlib.Hashtbl.create (Var.Set.cardinal vs) in
    Var.Set.iter (fun v -> Stdlib.Hashtbl.add reachable v ()) vs ;
    (* Do a Dijkstra-style graph transitive closure in [graph] starting from [vs]. At each step,
       [new_vs] contains the variables to explore next. Iterative to avoid blowing the stack. *)
    let new_vs = ref (Var.Set.elements vs) in
    while not (List.is_empty !new_vs) do
      (* pop [new_vs] *)
      let[@warning "-partial-match"] (v :: rest) = !new_vs in
      new_vs := rest ;
      Stdlib.Hashtbl.find_opt graph v
      |> Option.iter ~f:(fun vs' ->
             Var.Set.iter
               (fun v' ->
                 if not (Stdlib.Hashtbl.mem reachable v') then (
                   (* [v'] seen for the first time: we need to explore it *)
                   Stdlib.Hashtbl.replace reachable v' () ;
                   new_vs := v' :: !new_vs ) )
               vs' )
    done ;
    Stdlib.Hashtbl.to_seq_keys reachable |> Var.Set.of_seq


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
      L.d_printfln "closed_prunable_vars: %a" Var.Set.pp_hov closed_prunable_vars ;
      Atom.Map.filter
        (fun atom _depth -> not (Atom.has_var_notin closed_prunable_vars atom))
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


(** translate each variable in [formula_foreign] according to [f] then prove that each translated
    fact is implied by [formula0] *)
let implies_conditions_up_to ~subst:subst0 formula0 ~implies:formula_foreign =
  let subst_map = ref subst0 in
  let subst v =
    match Var.Map.find_opt v !subst_map with
    | Some v' ->
        v'
    | None ->
        let v' = Var.mk_fresh () in
        subst_map := Var.Map.add v v' !subst_map ;
        v'
  in
  let f_subst v =
    let v' = subst v in
    Term.VarSubst v'
  in
  (* HEURISTIC: all RHS atoms that transitively relate RHS variables that have been unified to
     variables from the LHS should be conjoined to the LHS as they are likely to reflect expressions
     constructed from these variables and not facts to be established in a universal way.

     Example for the program [while(x+2>0) { x++; }]:

     We enter the loop with some value for [x] that gets abstracted away:
     {[
       α(Init) = P0 = x |-> v
     ]}

     After going through the body once, we reach the back edge in the CFG with the assertion:

     {[
       P1= x|->v' * v''=v+2 * v'=v+1 * v''>0
     ]}

     We then ask whether this implies [P0], together with the conditions needed to execute the loop
     body once more, i.e. the pure part of [P1]. Since variables (abstract values) are immutable,
     [P1] pure facts make sense for [P0]. However, we don't want to establish [P0] exactly: we just
     need to establish that there exists instantiations of the variables that make [P0 ∧ pure(P1)]
     true, i.e. existentially quantify. The entailment question becomes:

     {[
       x↦v' ∧ v'' = v+2 ∧ v' = v+1 ∧ v''>0 ⊢ ∃v,v',v''. x↦v ∧ v'=v+1 ∧ v''=v+2 ∧ v''>0
     ]}

     Let's rename existentially bound variables for clarity:

     {[
       x↦v' ∧ v'' = v+2 ∧ v' = v+1 ∧ v''>0 ⊢ ∃w,w',w''. x↦w ∧ w'=w+1 ∧ w''=w+2 ∧ w''>0
     ]}

     The step before calling [implies] was heap unification, which will instantiate [w] with [v']:

     {[
       x↦v' ∧ v'' = v+2 ∧ v' = v+1 ∧ v''>0 ⊢ ∃w',w''. x↦v' ∧ w'=v'+1 ∧ w''=v'+2 ∧ w''>0
     ]}

     So, the question asked to this function, [implies], is, after removing the matching spatial [↦]
     predicates:

     {[
       v'' = v+2 ∧ v' = v+1 ∧ v''>0 ⊢ ∃w',w''. w'=v'+1 ∧ w''=v'+2 ∧ w''>0
     ]}

     
     Now the HEURISTIC part is to remark that [w'] and [w''] are bound to terms about [v'], so they
     can be conjoined to LHS instead first, then we'll try to prove each remaining condition atom
     (here only [w''>0]) is implied:

     {[
       v'' = v+2 ∧ v' = v+1 ∧ v''>0 ∧ w'=v'+1 ∧ w''=v'+2  ⊢ w''>0
     ]}
     v]
 *)
  let vars_to_existentially_quantify =
    DeadVariables.get_reachable_from
      (DeadVariables.build_var_graph formula0.phi)
      (Var.Map.to_seq !subst_map |> Seq.map fst |> Var.Set.of_seq)
  in
  subst_map :=
    Var.Set.fold
      (fun v subst_map ->
        if Var.Map.mem v subst_map then subst_map else Var.Map.add v (Var.mk_fresh ()) subst_map )
      vars_to_existentially_quantify !subst_map ;
  (* propagate non-implication and contradictions faster using these exceptions *)
  let exception NotImplied of Atom.t in
  let exception Contradiction of unsat_info in
  let sat_value_exn (norm : 'a SatUnsat.t) =
    match norm with Unsat unsat_info -> raise_notrace (Contradiction unsat_info) | Sat x -> x
  in
  (* TODO: instead of matching atoms/terms with two or more existentially quantified variables,
     match "bindings": variables that are just intermediate variables representing a term ultimately
     built from variables in [subst0]. *)
  let should_be_conjoined acc v =
    match acc with
    | `FoundNone ->
        `FoundOne v
    | `FoundTwo ->
        `FoundTwo
    | `FoundOne v' ->
        if Var.equal v v' then acc else `FoundTwo
  in
  let and_term_eqs phi_foreign phi =
    IContainer.fold_of_pervasives_map_fold Formula.term_eqs_fold phi_foreign ~init:phi
      ~f:(fun phi (t_foreign, v_foreign) ->
        match
          Term.fold_variables t_foreign
            ~init:(should_be_conjoined `FoundNone v_foreign)
            ~f:should_be_conjoined
        with
        | `FoundTwo ->
            let t = Term.subst_variables t_foreign ~f:f_subst in
            let phi, _new_eqs =
              Formula.Normalizer.and_var_term (subst v_foreign) t (phi, RevList.empty)
              |> sat_value_exn
            in
            phi
        | _ ->
            phi )
  in
  let and_atoms atoms_foreign phi =
    IContainer.fold_of_pervasives_set_fold Atom.Set.fold atoms_foreign ~init:phi
      ~f:(fun phi atom_foreign ->
        match Atom.fold_variables atom_foreign ~init:`FoundNone ~f:should_be_conjoined with
        | `FoundTwo ->
            let atom = Atom.subst_variables atom_foreign ~f:f_subst in
            let phi, _new_eqs =
              Formula.Normalizer.and_atom atom (phi, RevList.empty) ~add_term:false |> sat_value_exn
            in
            phi
        | _ ->
            phi )
  in
  let assert_atom phi atom =
    match Formula.Normalizer.and_atom (Atom.nnot atom) (phi, RevList.empty) ~add_term:false with
    | Unsat unsat_info ->
        L.d_printfln_escaped "implies %a by %a" (Atom.pp_with_pp_var Var.pp) atom
          SatUnsat.pp_unsat_info unsat_info ;
        ()
    | Sat _ ->
        raise (NotImplied atom)
  in
  let implies_atoms phi atoms_foreign =
    Stdlib.Seq.iter
      (fun atom_foreign -> Atom.subst_variables atom_foreign ~f:f_subst |> assert_atom phi)
      atoms_foreign
  in
  let implies_terms phi terms =
    Stdlib.Seq.concat_map
      (fun term ->
        Option.value_exn
          (Atom.atoms_of_term ~is_neq_zero:(Formula.is_neq_zero phi) ~force_to_atom:true
             ~negated:false term )
        |> ListLabels.to_seq )
      terms
    |> implies_atoms phi
  in
  try
    (* first use the HEURISTIC above to add some of the formula as facts *)
    let phi =
      and_term_eqs formula_foreign.phi formula0.phi |> and_atoms formula_foreign.phi.atoms
    in
    (* try to imply each atom in the conditions *)
    implies_atoms phi (formula_foreign.conditions |> Atom.Map.to_seq |> Seq.map fst) ;
    implies_terms phi (formula_foreign.phi.term_conditions2 |> Term.Set.to_seq) ;
    Ok ()
  with
  | NotImplied atom ->
      Error (`NotImplied atom)
  | Contradiction unsat_info ->
      Error (`Contradiction unsat_info)


let is_known_non_pointer formula v = Formula.is_non_pointer formula.phi v

let is_manifest ~is_allocated formula =
  Atom.Map.for_all
    (fun atom depth ->
      let is_ground = not @@ Term.has_var_notin Var.Set.empty @@ Atom.to_term atom in
      is_ground
      || ((not (Language.curr_language_is Erlang)) && Int.equal depth 0)
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
let subst_find_or_new ~default subst addr_callee =
  match Var.Map.find_opt addr_callee subst with
  | None ->
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_caller = Var.mk_fresh_same_kind addr_callee in
      L.d_printfln "new subst %a <-> %a (fresh)" Var.pp addr_callee Var.pp addr_caller ;
      let addr_hist_fresh = (addr_caller, default) in
      (Var.Map.add addr_callee addr_hist_fresh subst, fst addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, fst addr_hist_caller)


let and_callee_formula ~default ~subst formula ~callee:formula_callee =
  let* subst, formula, new_eqs =
    and_conditions_fold_subst_variables formula ~up_to_f:formula_callee
      ~f:(subst_find_or_new ~default) ~init:subst
  in
  let+ subst, formula, new_eqs' =
    and_fold_subst_variables ~up_to_f:formula_callee ~f:(subst_find_or_new ~default) ~init:subst
      formula
  in
  (subst, formula, RevList.append new_eqs' new_eqs)


let fold_variables {conditions; phi} ~init ~f =
  let init =
    let f atom _depth acc = Atom.fold_variables atom ~init:acc ~f in
    Atom.Map.fold f conditions init
  in
  Formula.fold_variables phi ~init ~f


(** careful with the axe! *)
let assert_sat = function
  | Sat x ->
      x
  | Unsat unsat_info ->
      SatUnsat.log_unsat unsat_info ;
      L.die InternalError "did not expect UNSAT here: %s" (unsat_info.reason ())


let absval_of_int formula i =
  match Formula.get_term_eq formula.phi (Term.of_intlit i) with
  | Some v ->
      (formula, v)
  | None ->
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
       ; atoms_occurrences= _
       ; term_conditions= _
       ; term_conditions2= _ }
       [@warning "+missing-record-field-pattern"] ) =
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
  if not (Atom.Map.is_empty conditions) then
    F.fprintf fmt "@;∧ %a" (pp_conditions pp_var) conditions


let join_conditions conditions_lhs conditions_rhs =
  let conditions_join =
    Atom.Map.merge
      (fun _atom depth1 depth2 ->
        (* keep only atoms present on both sides, with the min of their call depths *)
        Option.both depth1 depth2 |> Option.map ~f:(fun (depth1, depth2) -> Int.min depth1 depth2) )
      conditions_lhs conditions_rhs
  in
  let atoms_not_in ~not_in:atoms_not_in atoms =
    Atom.Map.merge
      (fun _atom atom_depth not_in ->
        match (atom_depth, not_in) with
        | Some _, None ->
            atom_depth
        | None, _ | Some _, Some _ ->
            None )
      atoms atoms_not_in
    |> Atom.Map.bindings |> List.map ~f:fst
  in
  let kill_conditions_lhs = atoms_not_in ~not_in:conditions_join conditions_lhs in
  let kill_conditions_rhs = atoms_not_in ~not_in:conditions_join conditions_rhs in
  (conditions_join, kill_conditions_lhs, kill_conditions_rhs)


(* This relies on the idea that two formulas for the same procedure must be different only because
   the path conditions are different. All other variables not involved in the path conditions are
   the results of being created fresh to hold some intermediate values created by the program and so
   can be handled with a conjunction (since they will appear only on one side).

   Given that, the strategy is to compute the conditions that are common to both sides, and those
   that are present only on one side need to be removed from the formulas. The removal of facts is
   done brutally by forgetting all facts involving any variable present in these conditions. This
   should be a valid over-approximation.

   NOTE: It would be good to know which variables were created fresh in [phi1] and in [phi2] and
   automatically keep information about these, since they cannot appear in the other formula and
   their valuation doesn't matter for the formula where they don't belong. Instead, we rely on
   conditions to tell which variables need to be forgotten about but that is over-approximate (and
   fragile: it could be that some consequences are not completely cleaned up this way). *)
let join {conditions= conditions_lhs; phi= phi_lhs} {conditions= conditions_rhs; phi= phi_rhs} =
  let conditions_join, killed_conditions_lhs, killed_conditions_rhs =
    join_conditions conditions_lhs conditions_rhs
  in
  let phi_lhs = Formula.remove_conditions_for_join killed_conditions_lhs phi_lhs phi_rhs in
  let phi_rhs = Formula.remove_conditions_for_join killed_conditions_rhs phi_rhs phi_lhs in
  let phi_join = Formula.join phi_lhs phi_rhs in
  {conditions= conditions_join; phi= phi_join}


type path_stamp = {path_cond: int Atom.Map.t; atom_set: Atom.Set.t; term_set: Term.Set.t}
[@@deriving compare, equal]

let pp_path_cond fmt cond =
  if Atom.Map.is_empty cond then F.pp_print_string fmt "(empty)" else pp_conditions Var.pp fmt cond


let pp_atom_set fmt set =
  if Atom.Set.is_empty set then F.pp_print_string fmt "(empty)"
  else Atom.Set.pp_with_pp_var Var.pp fmt set


let pp_term_set fmt set =
  if Term.Set.is_empty set then F.pp_print_string fmt "(empty)"
  else Term.Set.pp_with_pp_var Var.pp fmt set


let extract_path_stamp formula =
  let path_cond = extract_path_cond formula in
  let atom_set = extract_term_cond formula in
  let term_set = extract_term_cond2 formula in
  {path_cond; atom_set; term_set}


let is_empty_path_stamp {path_cond; atom_set; term_set} =
  Atom.Map.is_empty path_cond && Atom.Set.is_empty atom_set && Term.Set.is_empty term_set


let pp_path_stamp fmt {path_cond; atom_set; term_set} =
  F.fprintf fmt "{@[<v>path_cond=%a@;atom_set=%a@;term_set=%a@]}" pp_path_cond path_cond pp_atom_set
    atom_set pp_term_set term_set


type function_symbol = Term.function_symbol = Unknown of Var.t | Procname of Procname.t
[@@deriving compare, equal]

type operand = Term.operand =
  | AbstractValueOperand of Var.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: function_symbol; actuals: Var.t list}
[@@deriving compare, equal]

let pp_operand = Term.pp_operand
