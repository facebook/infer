(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Symbolic Execution *)

module L = Logging
module F = Format

let rec fldlist_assoc fld = function
  | [] -> raise Not_found
  | (fld', x, _):: l -> if Sil.fld_equal fld fld' then x else fldlist_assoc fld l

let rec unroll_type tenv typ off =
  match (typ, off) with
  | Sil.Tvar _, _ ->
      let typ' = Tenv.expand_type tenv typ in
      unroll_type tenv typ' off
  | Sil.Tstruct { Sil.instance_fields; static_fields }, Sil.Off_fld (fld, _) ->
      begin
        try fldlist_assoc fld (instance_fields @ static_fields)
        with Not_found ->
          L.d_strln ".... Invalid Field Access ....";
          L.d_strln ("Fld : " ^ Ident.fieldname_to_string fld);
          L.d_str "Type : "; Sil.d_typ_full typ; L.d_ln ();
          raise (Exceptions.Bad_footprint __POS__)
      end
  | Sil.Tarray (typ', _), Sil.Off_index _ ->
      typ'
  | _, Sil.Off_index (Sil.Const (Sil.Cint i)) when Sil.Int.iszero i ->
      typ
  | _ ->
      L.d_strln ".... Invalid Field Access ....";
      L.d_str "Fld : "; Sil.d_offset off; L.d_ln ();
      L.d_str "Type : "; Sil.d_typ_full typ; L.d_ln ();
      assert false

(** Given a node, returns a list of pvar of blocks that have been nullified in the block. *)
let get_blocks_nullified node =
  let null_blocks = IList.flatten(IList.map (fun i -> match i with
      | Sil.Nullify(pvar, _, true) when Sil.is_block_pvar pvar -> [pvar]
      | _ -> []) (Cfg.Node.get_instrs node)) in
  null_blocks

(** Given a proposition and an objc block checks whether by existentially quantifying
    captured variables in the block we obtain a leak. *)
let check_block_retain_cycle tenv caller_pname prop block_nullified =
  let mblock = Pvar.get_name block_nullified in
  let block_pname = Procname.mangled_objc_block (Mangled.to_string mblock) in
  let block_captured =
    match AttributesTable.load_attributes block_pname with
    | Some attributes ->
        fst (IList.split attributes.ProcAttributes.captured)
    | None ->
        [] in
  let prop' = Cfg.remove_seed_captured_vars_block block_captured prop in
  let prop'' = Prop.prop_rename_fav_with_existentials prop' in
  let _ : Prop.normal Prop.t = Abs.abstract_junk ~original_prop: prop caller_pname tenv prop'' in
  ()

(** Apply function [f] to the expression at position [offlist] in [strexp].
    If not found, expand [strexp] and apply [f] to [None].
    The routine should maintain the invariant that strexp and typ correspond to
    each other exactly, without involving any re - interpretation of some type t
    as the t array. The [fp_root] parameter indicates whether the kind of the
    root expression of the corresponding pointsto predicate is a footprint identifier.
    The function can expand a list of higher - order [hpara_psto] predicates, if
    the list is stored at [offlist] in [strexp] initially. The expanded list
    is returned as a part of the result. All these happen under [p], so that it
    is sound to call the prover with [p]. Finally, before running this function,
    the tool should run strexp_extend_value in rearrange.ml for the same strexp
    and offlist, so that all the necessary extensions of strexp are done before
    this function. If the tool follows this protocol, it will never hit the assert
    false cases for field and array accesses. *)
let rec apply_offlist
    pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist
    (f: Sil.exp option -> Sil.exp) inst lookup_inst =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let pp_error () =
    L.d_strln ".... Invalid Field ....";
    L.d_str "strexp : "; Sil.d_sexp strexp; L.d_ln ();
    L.d_str "offlist : "; Sil.d_offset_list offlist; L.d_ln ();
    L.d_str "type : "; Sil.d_typ_full typ; L.d_ln ();
    L.d_str "prop : "; Prop.d_prop p; L.d_ln (); L.d_ln () in
  match offlist, strexp with
  | [], Sil.Eexp (e, inst_curr) ->
      let inst_is_uninitialized = function
        | Sil.Ialloc ->
            (* java allocation initializes with default values *)
            !Config.curr_language <> Config.Java
        | Sil.Iinitial -> true
        | _ -> false in
      let is_hidden_field () =
        match State.get_instr () with
        | Some (Sil.Letderef (_, Sil.Lfield (_, fieldname, _), _, _)) ->
            Ident.fieldname_is_hidden fieldname
        | _ -> false in
      let inst_new = match inst with
        | Sil.Ilookup when inst_is_uninitialized inst_curr && not (is_hidden_field()) ->
            (* we are in a lookup of an uninitialized value *)
            lookup_inst := Some inst_curr;
            let alloc_attribute_opt =
              if inst_curr = Sil.Iinitial then None
              else Prop.get_undef_attribute p root_lexp in
            let deref_str = Localise.deref_str_uninitialized alloc_attribute_opt in
            let err_desc = Errdesc.explain_memory_access deref_str p (State.get_loc ()) in
            let exn = (Exceptions.Uninitialized_value (err_desc, __POS__)) in
            let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
            Reporting.log_warning pname ~pre: pre_opt exn;
            Sil.update_inst inst_curr inst
        | Sil.Ilookup -> (* a lookup does not change an inst unless it is inst_initial *)
            lookup_inst := Some inst_curr;
            inst_curr
        | _ -> Sil.update_inst inst_curr inst in
      let e' = f (Some e) in
      (e', Sil.Eexp (e', inst_new), typ, None)
  | [], Sil.Estruct (fesl, inst') ->
      if not nullify_struct then (f None, Sil.Estruct (fesl, inst'), typ, None)
      else if fp_root then (pp_error(); assert false)
      else
        begin
          L.d_strln "WARNING: struct assignment treated as nondeterministic assignment";
          (f None, Prop.create_strexp_of_type (Some tenv) Prop.Fld_init typ inst, typ, None)
        end
  | [], Sil.Earray _ ->
      let offlist' = (Sil.Off_index Sil.exp_zero):: offlist in
      apply_offlist
        pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist' f inst lookup_inst
  | (Sil.Off_fld _):: _, Sil.Earray _ ->
      let offlist_new = Sil.Off_index(Sil.exp_zero) :: offlist in
      apply_offlist
        pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist_new f inst lookup_inst
  | (Sil.Off_fld (fld, fld_typ)):: offlist', Sil.Estruct (fsel, inst') ->
      begin
        let typ' = Tenv.expand_type tenv typ in
        let struct_typ =
          match typ' with
          | Sil.Tstruct struct_typ ->
              struct_typ
          | _ -> assert false in
        let t' = unroll_type tenv typ (Sil.Off_fld (fld, fld_typ)) in
        try
          let _, se' = IList.find (fun fse -> Ident.fieldname_equal fld (fst fse)) fsel in
          let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist
              pdesc tenv p fp_root nullify_struct
              (root_lexp, se', t') offlist' f inst lookup_inst in
          let replace_fse fse = if Sil.fld_equal fld (fst fse) then (fld, res_se') else fse in
          let res_se = Sil.Estruct (IList.map replace_fse fsel, inst') in
          let replace_fta (f, t, a) = if Sil.fld_equal fld f then (fld, res_t', a) else (f, t, a) in
          let instance_fields' = IList.map replace_fta struct_typ.Sil.instance_fields in
          let res_t =
            Sil.Tstruct { struct_typ with Sil.instance_fields = instance_fields' } in
          (res_e', res_se, res_t, res_pred_insts_op')
        with Not_found ->
          pp_error();
          assert false
          (* This case should not happen. The rearrangement should
             have materialized all the accessed cells. *)
      end
  | (Sil.Off_fld _):: _, _ ->
      pp_error();
      assert false

  | (Sil.Off_index idx):: offlist', Sil.Earray (size, esel, inst1) ->
      let nidx = Prop.exp_normalize_prop p idx in
      begin
        let typ' = Tenv.expand_type tenv typ in
        let t', size' = match typ' with Sil.Tarray (t', size') -> (t', size') | _ -> assert false in
        try
          let idx_ese', se' = IList.find (fun ese -> Prover.check_equal p nidx (fst ese)) esel in
          let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist
              pdesc tenv p fp_root nullify_struct
              (root_lexp, se', t') offlist' f inst lookup_inst in
          let replace_ese ese =
            if Sil.exp_equal idx_ese' (fst ese)
            then (idx_ese', res_se')
            else ese in
          let res_se = Sil.Earray(size, IList.map replace_ese esel, inst1) in
          let res_t = Sil.Tarray(res_t', size') in
          (res_e', res_se, res_t, res_pred_insts_op')
        with Not_found ->
          (* return a nondeterministic value if the index is not found after rearrangement *)
          L.d_str "apply_offlist: index "; Sil.d_exp idx;
          L.d_strln " not materialized -- returning nondeterministic value";
          let res_e' = Sil.Var (Ident.create_fresh Ident.kprimed) in
          (res_e', strexp, typ, None)
      end
  | (Sil.Off_index _):: _, _ ->
      pp_error();
      raise (Exceptions.Internal_error (Localise.verbatim_desc "Array out of bounds in Symexec"))
(* This case should not happen. The rearrangement should
   have materialized all the accessed cells. *)

(** Given [lexp |-> se: typ], if the location [offlist] exists in [se],
    function [ptsto_lookup p (lexp, se, typ) offlist id] returns a tuple.
    The first component of the tuple is an expression at position [offlist] in [se].
    The second component is an expansion of the predicate [lexp |-> se: typ],
    where the entity at [offlist] in [se] is expanded if the entity is a list of
    higher - order parameters [hpara_psto]. If this expansion happens,
    the last component of the tuple is a list of pi - sigma pairs obtained
    by instantiating the [hpara_psto] list. Otherwise, the last component is None.
    All these steps happen under [p]. So, we can call a prover with [p].
    Finally, before running this function, the tool should run strexp_extend_value
    in rearrange.ml for the same se and offlist, so that all the necessary
    extensions of se are done before this function. *)
let ptsto_lookup pdesc tenv p (lexp, se, typ, st) offlist id =
  let f =
    function Some exp -> exp | None -> Sil.Var id in
  let fp_root =
    match lexp with Sil.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let e', se', typ', pred_insts_op' =
    apply_offlist
      pdesc tenv p fp_root false (lexp, se, typ) offlist f Sil.inst_lookup lookup_inst in
  let lookup_uninitialized = (* true if we have looked up an uninitialized value *)
    match !lookup_inst with
    | Some (Sil.Iinitial | Sil.Ialloc | Sil.Ilookup) -> true
    | _ -> false in
  let ptsto' = Prop.mk_ptsto lexp se' (Sil.Sizeof (typ', st)) in
  (e', ptsto', pred_insts_op', lookup_uninitialized)

(** [ptsto_update p (lexp,se,typ) offlist exp] takes
    [lexp |-> se: typ], and updates [se] by replacing the
    expression at [offlist] with [exp]. Then, it returns
    the updated pointsto predicate. If [lexp |-> se: typ] gets
    expanded during this update, the generated pi - sigma list from
    the expansion gets returned, and otherwise, None is returned.
    All these happen under the proposition [p], so it is ok call
    prover with [p]. Finally, before running this function,
    the tool should run strexp_extend_value in rearrange.ml for the same
    se and offlist, so that all the necessary extensions of se are done
    before this function. *)
let ptsto_update pdesc tenv p (lexp, se, typ, st) offlist exp =
  let f _ = exp in
  let fp_root =
    match lexp with Sil.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let _, se', typ', pred_insts_op' =
    let pos = State.get_path_pos () in
    apply_offlist
      pdesc tenv p fp_root true (lexp, se, typ) offlist f (State.get_inst_update pos) lookup_inst in
  let ptsto' = Prop.mk_ptsto lexp se' (Sil.Sizeof (typ', st)) in
  (ptsto', pred_insts_op')

let update_iter iter pi sigma =
  let iter' = Prop.prop_iter_update_current_by_list iter sigma in
  IList.fold_left (Prop.prop_iter_add_atom false) iter' pi

(** Precondition: se should not include hpara_psto
    that could mean nonempty heaps. *)
let rec execute_nullify_se = function
  | Sil.Eexp _ ->
      Sil.Eexp (Sil.exp_zero, Sil.inst_nullify)
  | Sil.Estruct (fsel, _) ->
      let fsel' = IList.map (fun (fld, se) -> (fld, execute_nullify_se se)) fsel in
      Sil.Estruct (fsel', Sil.inst_nullify)
  | Sil.Earray (size, esel, _) ->
      let esel' = IList.map (fun (idx, se) -> (idx, execute_nullify_se se)) esel in
      Sil.Earray (size, esel', Sil.inst_nullify)

(** Do pruning for conditional [if (e1 != e2) ] if [positive] is true
    and [(if (e1 == e2)] if [positive] is false *)
let prune_ne ~positive e1 e2 prop =
  let is_inconsistent =
    if positive then Prover.check_equal prop e1 e2
    else Prover.check_disequal prop e1 e2 in
  if is_inconsistent then Propset.empty
  else
    let conjoin = if positive then Prop.conjoin_neq else Prop.conjoin_eq in
    let new_prop = conjoin ~footprint: (!Config.footprint) e1 e2 prop in
    if Prover.check_inconsistency new_prop then Propset.empty
    else Propset.singleton new_prop

(** Do pruning for conditional "if ([e1] CMP [e2])" if [positive] is
    true and "if (!([e1] CMP [e2]))" if [positive] is false, where CMP
    is "<" if [is_strict] is true and "<=" if [is_strict] is false.
*)
let prune_ineq ~is_strict ~positive prop e1 e2 =
  if Sil.exp_equal e1 e2 then
    if (positive && not is_strict) || (not positive && is_strict) then
      Propset.singleton prop
    else Propset.empty
  else
    (* build the pruning condition and its negation, as explained in
       the comment above *)
    (* build [e1] CMP [e2] *)
    let cmp = if is_strict then Sil.Lt else Sil.Le in
    let e1_cmp_e2 = Sil.BinOp (cmp, e1, e2) in
    (* build !([e1] CMP [e2]) *)
    let dual_cmp = if is_strict then Sil.Le else Sil.Lt in
    let not_e1_cmp_e2 = Sil.BinOp (dual_cmp, e2, e1) in
    (* take polarity into account *)
    let (prune_cond, not_prune_cond) =
      if positive then (e1_cmp_e2, not_e1_cmp_e2)
      else (not_e1_cmp_e2, e1_cmp_e2) in
    let is_inconsistent = Prover.check_atom prop (Prop.mk_inequality not_prune_cond) in
    if is_inconsistent then Propset.empty
    else
      let footprint = !Config.footprint in
      let prop_with_ineq = Prop.conjoin_eq ~footprint prune_cond Sil.exp_one prop in
      Propset.singleton prop_with_ineq

let rec prune ~positive condition prop =
  match condition with
  | Sil.Var _ | Sil.Lvar _ ->
      prune_ne ~positive condition Sil.exp_zero prop
  | Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
      if positive then Propset.empty else Propset.singleton prop
  | Sil.Const (Sil.Cint _) | Sil.Sizeof _ | Sil.Const (Sil.Cstr _) | Sil.Const (Sil.Cclass _) ->
      if positive then Propset.singleton prop else Propset.empty
  | Sil.Const _ ->
      assert false
  | Sil.Cast (_, condition') ->
      prune ~positive condition' prop
  | Sil.UnOp (Sil.LNot, condition', _) ->
      prune ~positive:(not positive) condition' prop
  | Sil.UnOp _ ->
      assert false
  | Sil.BinOp (Sil.Eq, e, Sil.Const (Sil.Cint i))
  | Sil.BinOp (Sil.Eq, Sil.Const (Sil.Cint i), e) when Sil.Int.iszero i && not (Sil.Int.isnull i) ->
      prune ~positive:(not positive) e prop
  | Sil.BinOp (Sil.Eq, e1, e2) ->
      prune_ne ~positive:(not positive) e1 e2 prop
  | Sil.BinOp (Sil.Ne, e, Sil.Const (Sil.Cint i))
  | Sil.BinOp (Sil.Ne, Sil.Const (Sil.Cint i), e) when Sil.Int.iszero i && not (Sil.Int.isnull i) ->
      prune ~positive e prop
  | Sil.BinOp (Sil.Ne, e1, e2) ->
      prune_ne ~positive e1 e2 prop
  | Sil.BinOp (Sil.Ge, e2, e1) | Sil.BinOp (Sil.Le, e1, e2) ->
      prune_ineq ~is_strict:false ~positive prop e1 e2
  | Sil.BinOp (Sil.Gt, e2, e1) | Sil.BinOp (Sil.Lt, e1, e2) ->
      prune_ineq ~is_strict:true ~positive prop e1 e2
  | Sil.BinOp (Sil.LAnd, condition1, condition2) ->
      let pruner = if positive then prune_inter else prune_union in
      pruner ~positive condition1 condition2 prop
  | Sil.BinOp (Sil.LOr, condition1, condition2) ->
      let pruner = if positive then prune_union else prune_inter in
      pruner ~positive condition1 condition2 prop
  | Sil.BinOp _ | Sil.Lfield _ | Sil.Lindex _ ->
      prune_ne ~positive condition Sil.exp_zero prop

and prune_inter ~positive condition1 condition2 prop =
  let res = ref Propset.empty in
  let pset1 = prune ~positive condition1 prop in
  let do_p p =
    res := Propset.union (prune ~positive condition2 p) !res in
  Propset.iter do_p pset1;
  !res

and prune_union ~positive condition1 condition2 prop =
  let pset1 = prune ~positive condition1 prop in
  let pset2 = prune ~positive condition2 prop in
  Propset.union pset1 pset2

let dangerous_functions =
  let dangerous_list = ["gets"] in
  ref ((IList.map Procname.from_string_c_fun) dangerous_list)

let check_inherently_dangerous_function caller_pname callee_pname =
  if IList.exists (Procname.equal callee_pname) !dangerous_functions then
    let exn =
      Exceptions.Inherently_dangerous_function
        (Localise.desc_inherently_dangerous_function callee_pname) in
    let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop caller_pname) in
    Reporting.log_warning caller_pname ~pre: pre_opt exn

let proc_is_defined proc_name =
  match AttributesTable.load_attributes proc_name with
  | Some attributes ->
      attributes.ProcAttributes.is_defined
  | None ->
      false

let call_should_be_skipped callee_pname summary =
  (* check skip flag *)
  Specs.get_flag callee_pname proc_flag_skip <> None
  (* skip abstract methods *)
  || summary.Specs.attributes.ProcAttributes.is_abstract
  (* treat calls with no specs as skip functions in angelic mode *)
  || (!Config.angelic_execution && Specs.get_specs_from_payload summary == [])

(** In case of constant string dereference, return the result immediately *)
let check_constant_string_dereference lexp =
  let string_lookup s n =
    let c = try Char.code (String.get s (Sil.Int.to_int n)) with Invalid_argument _ -> 0 in
    Sil.exp_int (Sil.Int.of_int c) in
  match lexp with
  | Sil.BinOp(Sil.PlusPI, Sil.Const (Sil.Cstr s), e)
  | Sil.Lindex (Sil.Const (Sil.Cstr s), e) ->
      let value = match e with
        | Sil.Const (Sil.Cint n)
          when Sil.Int.geq n Sil.Int.zero &&
               Sil.Int.leq n (Sil.Int.of_int (String.length s)) ->
            string_lookup s n
        | _ -> Sil.exp_get_undefined false in
      Some value
  | Sil.Const (Sil.Cstr s) ->
      Some (string_lookup s Sil.Int.zero)
  | _ -> None

(** Normalize an expression and check for arithmetic problems *)
let check_arith_norm_exp pname exp prop =
  match Prop.find_arithmetic_problem (State.get_path_pos ()) prop exp with
  | Some (Prop.Div0 div), prop' ->
      let desc = Errdesc.explain_divide_by_zero div (State.get_node ()) (State.get_loc ()) in
      let exn = Exceptions.Divide_by_zero (desc, __POS__) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn;
      Prop.exp_normalize_prop prop exp, prop'
  | Some (Prop.UminusUnsigned (e, typ)), prop' ->
      let desc =
        Errdesc.explain_unary_minus_applied_to_unsigned_expression
          e typ (State.get_node ()) (State.get_loc ()) in
      let exn = Exceptions.Unary_minus_applied_to_unsigned_expression (desc, __POS__) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn;
      Prop.exp_normalize_prop prop exp, prop'
  | None, prop' -> Prop.exp_normalize_prop prop exp, prop'

(** Check if [cond] is testing for NULL a pointer already dereferenced *)
let check_already_dereferenced pname cond prop =
  let find_hpred lhs =
    try Some (IList.find (function
        | Sil.Hpointsto (e, _, _) -> Sil.exp_equal e lhs
        | _ -> false) (Prop.get_sigma prop))
    with Not_found -> None in
  let rec is_check_zero = function
    | Sil.Var id ->
        Some id
    | Sil.UnOp(Sil.LNot, e, _) ->
        is_check_zero e
    | Sil.BinOp ((Sil.Eq | Sil.Ne), Sil.Const Sil.Cint i, Sil.Var id)
    | Sil.BinOp ((Sil.Eq | Sil.Ne), Sil.Var id, Sil.Const Sil.Cint i) when Sil.Int.iszero i ->
        Some id
    | _ -> None in
  let dereferenced_line = match is_check_zero cond with
    | Some id ->
        (match find_hpred (Prop.exp_normalize_prop prop (Sil.Var id)) with
         | Some (Sil.Hpointsto (_, se, _)) ->
             (match Tabulation.find_dereference_without_null_check_in_sexp se with
              | Some n -> Some (id, n)
              | None -> None)
         | _ -> None)
    | None ->
        None in
  match dereferenced_line with
  | Some (id, (n, _)) ->
      let desc =
        Errdesc.explain_null_test_after_dereference
          (Sil.Var id) (State.get_node ()) n (State.get_loc ()) in
      let exn =
        (Exceptions.Null_test_after_dereference (desc, __POS__)) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn
  | None -> ()

(** Check whether symbolic execution de-allocated a stack variable or a constant string,
    raising an exception in that case *)
let check_deallocate_static_memory prop_after =
  let check_deallocated_attribute = function
    | Sil.Lvar pv, Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra)
      when Pvar.is_local pv || Pvar.is_global pv ->
        let freed_desc = Errdesc.explain_deallocate_stack_var pv ra in
        raise (Exceptions.Deallocate_stack_variable freed_desc)
    | Sil.Const (Sil.Cstr s), Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra) ->
        let freed_desc = Errdesc.explain_deallocate_constant_string s ra in
        raise (Exceptions.Deallocate_static_memory freed_desc)
    | _ -> () in
  let exp_att_list = Prop.get_all_attributes prop_after in
  IList.iter check_deallocated_attribute exp_att_list;
  prop_after

let method_exists right_proc_name methods =
  if !Config.curr_language = Config.Java then
    IList.exists (fun meth_name -> Procname.equal right_proc_name meth_name) methods
  else (* ObjC/C++ case : The attribute map will only exist when we have code for the method or
          the method has been called directly somewhere. It can still be that this is not the
          case but we have a model for the method. *)
    match AttributesTable.load_attributes right_proc_name with
    | Some attrs -> attrs.ProcAttributes.is_defined
    | None -> Specs.summary_exists_in_models right_proc_name

let resolve_method tenv class_name proc_name =
  let found_class =
    let visited = ref Typename.Set.empty in
    let rec resolve class_name =
      visited := Typename.Set.add class_name !visited;
      let right_proc_name =
        Procname.replace_class proc_name (Typename.name class_name) in
      match Tenv.lookup tenv class_name with
      | Some { Sil.csu = Csu.Class _; def_methods; superclasses } ->
          if method_exists right_proc_name def_methods then
            Some right_proc_name
          else
            (match superclasses with
             | super_classname:: _ ->
                 if not (Typename.Set.mem super_classname !visited)
                 then resolve super_classname
                 else None
             | _ -> None)
      | _ -> None in
    resolve class_name in
  match found_class with
  | None ->
      Logging.d_strln
        ("Couldn't find method in the hierarchy of type "^(Typename.name class_name));
      proc_name
  | Some proc_name ->
      proc_name

let resolve_typename prop receiver_exp =
  let typexp_opt =
    let rec loop = function
      | [] -> None
      | Sil.Hpointsto(e, _, typexp) :: _ when Sil.exp_equal e receiver_exp -> Some typexp
      | _ :: hpreds -> loop hpreds in
    loop (Prop.get_sigma prop) in
  match typexp_opt with
  | Some (Sil.Sizeof (Sil.Tstruct { Sil.struct_name = None }, _)) -> None
  | Some (Sil.Sizeof (Sil.Tstruct { Sil.csu = Csu.Class ck; struct_name = Some name }, _)) ->
      Some (Typename.TN_csu (Csu.Class ck, name))
  | _ -> None

(** If the dynamic type of the receiver actual T_actual is a subtype of the reciever type T_formal
    in the signature of [pname], resolve [pname] to T_actual.[pname]. *)
let resolve_virtual_pname tenv prop actuals callee_pname call_flags : Procname.t list =
  let resolve receiver_exp pname prop = match resolve_typename prop receiver_exp with
    | Some class_name -> resolve_method tenv class_name pname
    | None -> pname in
  let get_receiver_typ pname fallback_typ =
    match pname with
    | Procname.Java pname_java ->
        begin
          match Tenv.proc_extract_declaring_class_typ tenv pname_java with
          | Some struct_typ -> Sil.Tptr (Tstruct struct_typ, Pk_pointer)
          | None -> fallback_typ
        end
    | _ ->
        fallback_typ in
  let receiver_types_equal pname actual_receiver_typ =
    (* the type of the receiver according to the function signature *)
    let formal_receiver_typ = get_receiver_typ pname actual_receiver_typ in
    Sil.typ_equal formal_receiver_typ actual_receiver_typ in
  let do_resolve called_pname receiver_exp actual_receiver_typ =
    if receiver_types_equal called_pname actual_receiver_typ
    then resolve receiver_exp called_pname prop
    else called_pname in
  match actuals with
  | _ when not (call_flags.Sil.cf_virtual || call_flags.Sil.cf_interface) ->
      (* if this is not a virtual or interface call, there's no need for resolution *)
      [callee_pname]
  | (receiver_exp, actual_receiver_typ) :: _ ->
      if !Config.curr_language <> Config.Java then
        (* default mode for Obj-C/C++/Java virtual calls: resolution only *)
        [do_resolve callee_pname receiver_exp actual_receiver_typ]
      else if Config.sound_dynamic_dispatch then
        let targets =
          if call_flags.Sil.cf_virtual
          then
            (* virtual call--either [called_pname] or an override in some subtype may be called *)
            callee_pname :: call_flags.Sil.cf_targets
          else
            (* interface call--[called_pname] has no implementation), we don't want to consider *)
            call_flags.Sil.cf_targets in (* interface call, don't want to consider *)
        (* return true if (receiver typ of [target_pname]) <: [actual_receiver_typ] *)
        let may_dispatch_to target_pname =
          let target_receiver_typ = get_receiver_typ target_pname actual_receiver_typ in
          Prover.Subtyping_check.check_subtype tenv target_receiver_typ actual_receiver_typ in
        let resolved_pname = do_resolve callee_pname receiver_exp actual_receiver_typ in
        let feasible_targets = IList.filter may_dispatch_to targets in
        (* make sure [resolved_pname] is not a duplicate *)
        if IList.mem Procname.equal resolved_pname feasible_targets
        then feasible_targets
        else resolved_pname :: feasible_targets
      else
        begin
          match call_flags.Sil.cf_targets with
          | target :: _ when call_flags.Sil.cf_interface &&
                             receiver_types_equal callee_pname actual_receiver_typ ->
              (* "production mode" of dynamic dispatch for Java: unsound, but faster. the handling
                 is restricted to interfaces: if we can't resolve an interface call, we pick the
                 first implementation of the interface and call it *)
              [target]
          | _ ->
              (* default mode for Java virtual calls: resolution only *)
              [do_resolve callee_pname receiver_exp actual_receiver_typ]
        end
  | _ -> failwith "A virtual call must have a receiver"


(** Resolve the name of the procedure to call based on the type of the arguments *)
let resolve_java_pname tenv prop args pname_java call_flags : Procname.java =
  let resolve_from_args resolved_pname_java args =
    let parameters = Procname.java_get_parameters resolved_pname_java in
    if IList.length args <> IList.length parameters then
      resolved_pname_java
    else
      let resolved_params =
        IList.fold_left2
          (fun accu (arg_exp, _) name ->
             match resolve_typename prop arg_exp with
             | Some class_name ->
                 (Procname.split_classname (Typename.name class_name)) :: accu
             | None -> name :: accu)
          [] args (Procname.java_get_parameters resolved_pname_java) |> IList.rev in
      Procname.java_replace_parameters resolved_pname_java resolved_params in
  let resolved_pname_java, other_args =
    match args with
    | [] ->
        pname_java, []
    | (first_arg, _) :: other_args when call_flags.Sil.cf_virtual ->
        let resolved =
          begin
            match resolve_typename prop first_arg with
            | Some class_name ->
                begin
                  match resolve_method tenv class_name (Procname.Java pname_java) with
                  | Procname.Java resolved_pname_java ->
                      resolved_pname_java
                  | _ ->
                      pname_java
                end
            | None ->
                pname_java
          end in
        resolved, other_args
    | _ :: other_args when Procname.is_constructor (Procname.Java pname_java) ->
        pname_java, other_args
    | args ->
        pname_java, args in
  resolve_from_args resolved_pname_java other_args


(** Resolve the procedure name and run the analysis of the resolved procedure
    if not already analyzed *)
let resolve_and_analyze
    tenv caller_pdesc prop args callee_proc_name call_flags : Procname.t * Specs.summary option =
  (* TODO (#9333890): Fix conflict with method overloading by encoding in the procedure name
     whether the method is defined or generated by the specialization *)
  let analyze_ondemand resolved_pname : unit =
    if Procname.equal resolved_pname callee_proc_name then
      Ondemand.analyze_proc_name ~propagate_exceptions:true caller_pdesc callee_proc_name
    else
      (* Create the type sprecialized procedure description and analyze it directly *)
      Option.may
        (fun specialized_pdesc ->
           Ondemand.analyze_proc_desc ~propagate_exceptions:true caller_pdesc specialized_pdesc)
        (match Ondemand.get_proc_desc resolved_pname with
         | Some resolved_proc_desc ->
             Some resolved_proc_desc
         | None ->
             begin
               Option.map
                 (fun callee_proc_desc ->
                    Cfg.specialize_types callee_proc_desc resolved_pname args)
                 (Ondemand.get_proc_desc callee_proc_name)
             end) in
  let resolved_pname = match callee_proc_name with
    | Procname.Java callee_proc_name_java ->
        Procname.Java
          (resolve_java_pname tenv prop args callee_proc_name_java call_flags)
    | _ ->
        callee_proc_name in
  analyze_ondemand resolved_pname;
  resolved_pname, Specs.get_summary resolved_pname


(** recognize calls to the constructor java.net.URL and splits the argument string
    to be only the protocol.  *)
let call_constructor_url_update_args pname actual_params =
  let url_pname =
    Procname.Java
      (Procname.java
         ((Some "java.net"), "URL") None "<init>"
         [(Some "java.lang"), "String"] Procname.Non_Static) in
  if (Procname.equal url_pname pname) then
    (match actual_params with
     | [this; (Sil.Const (Sil.Cstr s), atype)] ->
         let parts = Str.split (Str.regexp_string "://") s in
         (match parts with
          | frst:: _ ->
              if frst = "http" ||
                 frst = "ftp" ||
                 frst = "https" ||
                 frst = "mailto" ||
                 frst = "jar"
              then
                [this; (Sil.Const (Sil.Cstr frst), atype)]
              else actual_params
          | _ -> actual_params)
     | [this; _, atype] -> [this; (Sil.Const (Sil.Cstr "file"), atype)]
     | _ -> actual_params)
  else actual_params


(* This method handles ObjC method calls, in particular the fact that calling a method with nil *)
(* returns nil. The exec_call function is either standard call execution or execution of ObjC *)
(* getters and setters using a builtin. *)
let handle_objc_method_call actual_pars actual_params pre tenv ret_ids pdesc callee_pname loc
    path exec_call =
  let path_description =
    "Message " ^
    (Procname.to_simplified_string callee_pname) ^
    " with receiver nil returns nil." in
  let receiver = (match actual_pars with
      | (e, _):: _ -> e
      | _ -> raise
               (Exceptions.Internal_error
                  (Localise.verbatim_desc
                     "In Objective-C instance method call there should be a receiver."))) in
  let is_receiver_null =
    match actual_pars with
    | (e, _) :: _
      when Sil.exp_equal e Sil.exp_zero ||
           Option.is_some (Prop.get_objc_null_attribute pre e) -> true
    | _ -> false in
  let add_objc_null_attribute_or_nullify_result prop =
    match ret_ids with
    | [ret_id] ->
        (match Prop.find_equal_formal_path receiver prop with
         | Some info ->
             Prop.add_or_replace_exp_attribute prop (Sil.Var ret_id) (Sil.Aobjc_null info)
         | None -> Prop.conjoin_eq (Sil.Var ret_id) Sil.exp_zero prop)
    | _ -> prop in
  if is_receiver_null
  then (* objective-c instance method with a null receiver just return objc_null(res) *)
    let path = Paths.Path.add_description path path_description in
    L.d_strln
      ("Object-C method " ^
       Procname.to_string callee_pname ^
       " called with nil receiver. Returning 0/nil");
    (* We wish to nullify the result. However, in some cases,
       we want to add the attribute OBJC_NULL to it so that we *)
    (* can keep track of how this object became null,
       so that in a NPE we can separate it into a different error type *)
    [(add_objc_null_attribute_or_nullify_result pre, path)]
  else
    let res = exec_call tenv ret_ids pdesc callee_pname loc actual_params pre path in
    let is_undef =
      Option.is_some (Prop.get_undef_attribute pre receiver) in
    if !Config.footprint && not is_undef then
      let res_null = (* returns: (objc_null(res) /\ receiver=0) or an empty list of results *)
        let pre_with_attr_or_null = add_objc_null_attribute_or_nullify_result pre in
        let propset = prune_ne ~positive:false receiver Sil.exp_zero pre_with_attr_or_null in
        if Propset.is_empty propset then []
        else
          let prop = IList.hd (Propset.to_proplist propset) in
          let path = Paths.Path.add_description path path_description in
          [(prop, path)] in
      res_null @ res
    else res (* Not known if receiver = 0 and not footprint. Standard tabulation *)

let normalize_params pdesc prop actual_params =
  let norm_arg (p, args) (e, t) =
    let e', p' = check_arith_norm_exp pdesc e p in
    (p', (e', t) :: args) in
  let prop, args = IList.fold_left norm_arg (prop, []) actual_params in
  (prop, IList.rev args)

let do_error_checks node_opt instr pname pdesc = match node_opt with
  | Some node ->
      if !Config.curr_language = Config.Java then
        PrintfArgs.check_printf_args_ok node instr pname pdesc
  | None ->
      ()

let add_strexp_to_footprint strexp abducted_pv typ prop =
  let abducted_lvar = Sil.Lvar abducted_pv in
  let lvar_pt_fpvar =
    let sizeof_exp = Sil.Sizeof (typ, Sil.Subtype.subtypes) in
    Prop.mk_ptsto abducted_lvar strexp sizeof_exp in
  let sigma_fp = Prop.get_sigma_footprint prop in
  Prop.normalize (Prop.replace_sigma_footprint (lvar_pt_fpvar :: sigma_fp) prop)

let add_to_footprint abducted_pv typ prop =
  let fresh_fp_var = Sil.Var (Ident.create_fresh Ident.kfootprint) in
  let prop' = add_strexp_to_footprint (Sil.Eexp (fresh_fp_var, Sil.Inone)) abducted_pv typ prop in
  prop', fresh_fp_var

(* the current abduction mechanism treats struct values differently than all other types. abduction
   on struct values adds a a struct whose fields are initialized to fresh footprint vars to the
   footprint. regular abduction just adds a fresh footprint value of the correct type to the
   footprint. we can get rid of this special case if we fix the abduction on struct values *)
let add_struct_value_to_footprint tenv abducted_pv typ prop =
  let struct_strexp =
    Prop.create_strexp_of_type (Some tenv) Prop.Fld_init typ Sil.inst_none in
  let prop' = add_strexp_to_footprint struct_strexp abducted_pv typ prop in
  prop', struct_strexp

let add_constraints_on_retval pdesc prop ret_exp typ callee_pname callee_loc =
  if Procname.is_infer_undefined callee_pname then prop
  else
    let is_rec_call pname = (* TODO: (t7147096) extend this to detect mutual recursion *)
      Procname.equal pname (Cfg.Procdesc.get_proc_name pdesc) in
    let already_has_abducted_retval p abducted_ret_pv =
      IList.exists
        (fun hpred -> match hpred with
           | Sil.Hpointsto (Sil.Lvar pv, _, _) -> Pvar.equal pv abducted_ret_pv
           | _ -> false)
        (Prop.get_sigma_footprint p) in
    (* find an hpred [abducted] |-> A in [prop] and add [exp] = A to prop *)
    let bind_exp_to_abducted_val exp_to_bind abducted prop =
      let bind_exp prop = function
        | Sil.Hpointsto (Sil.Lvar pv, Sil.Eexp (rhs, _), _)
          when Pvar.equal pv abducted ->
            Prop.conjoin_eq exp_to_bind rhs prop
        | _ -> prop in
      IList.fold_left bind_exp prop (Prop.get_sigma prop) in
    (* To avoid obvious false positives, assume skip functions do not return null pointers *)
    let add_ret_non_null exp typ prop =
      match typ with
      | Sil.Tptr _ -> Prop.conjoin_neq exp Sil.exp_zero prop
      | _ -> prop in
    let add_tainted_post ret_exp callee_pname prop =
      Prop.add_or_replace_exp_attribute prop ret_exp (Sil.Ataint callee_pname) in

    if !Config.angelic_execution && not (is_rec_call callee_pname) then
      (* introduce a fresh program variable to allow abduction on the return value *)
      let abducted_ret_pv = Pvar.mk_abducted_ret callee_pname callee_loc in
      (* prevent introducing multiple abducted retvals for a single call site in a loop *)
      if already_has_abducted_retval prop abducted_ret_pv then prop
      else
        let prop' =
          if !Config.footprint then
            let (prop', fresh_fp_var) = add_to_footprint abducted_ret_pv typ prop in
            Prop.conjoin_eq ~footprint: true ret_exp fresh_fp_var prop'
          else
            (* bind return id to the abducted value pointed to by the pvar we introduced *)
            bind_exp_to_abducted_val ret_exp abducted_ret_pv prop in
        let prop'' = add_ret_non_null ret_exp typ prop' in
        if !Config.taint_analysis && Taint.returns_tainted callee_pname None then
          add_tainted_post ret_exp { Sil.taint_source = callee_pname; taint_kind = Unknown } prop''
        else prop''
    else add_ret_non_null ret_exp typ prop

let execute_letderef ?(report_deref_errors=true) pname pdesc tenv id rhs_exp typ loc prop_ =
  let execute_letderef_ pdesc tenv id loc acc_in iter =
    let iter_ren = Prop.prop_iter_make_id_primed id iter in
    let prop_ren = Prop.prop_iter_to_prop iter_ren in
    match Prop.prop_iter_current iter_ren with
    | (Sil.Hpointsto(lexp, strexp, Sil.Sizeof (typ, st)), offlist) ->
        let contents, new_ptsto, pred_insts_op, lookup_uninitialized =
          ptsto_lookup pdesc tenv prop_ren (lexp, strexp, typ, st) offlist id in
        let update acc (pi, sigma) =
          let pi' = Sil.Aeq (Sil.Var(id), contents):: pi in
          let sigma' = new_ptsto:: sigma in
          let iter' = update_iter iter_ren pi' sigma' in
          let prop' = Prop.prop_iter_to_prop iter' in
          let prop'' =
            if lookup_uninitialized then
              Prop.add_or_replace_exp_attribute prop' (Sil.Var id) (Sil.Adangling Sil.DAuninit)
            else prop' in
          prop'' :: acc in
        begin
          match pred_insts_op with
          | None -> update acc_in ([],[])
          | Some pred_insts -> IList.rev (IList.fold_left update acc_in pred_insts)
        end
    | (Sil.Hpointsto _, _) ->
        Errdesc.warning_err loc "no offset access in execute_letderef -- treating as skip@.";
        (Prop.prop_iter_to_prop iter_ren) :: acc_in
    | _ ->
        (* The implementation of this case means that we
           ignore this dereferencing operator. When the analyzer treats
           numerical information and arrays more precisely later, we
           should change the implementation here. *)
        assert false in
  try
    let n_rhs_exp, prop = check_arith_norm_exp pname rhs_exp prop_ in
    let n_rhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_rhs_exp in
    match check_constant_string_dereference n_rhs_exp' with
    | Some value ->
        [Prop.conjoin_eq (Sil.Var id) value prop]
    | None ->
        let exp_get_undef_attr exp =
          let fold_undef_pname callee_opt attr =
            if Option.is_none callee_opt && Sil.attr_is_undef attr then Some attr
            else callee_opt in
          IList.fold_left fold_undef_pname None (Prop.get_exp_attributes prop exp) in
        let prop' =
          if !Config.angelic_execution then
            (* when we try to deref an undefined value, add it to the footprint *)
            match exp_get_undef_attr n_rhs_exp' with
            | Some (Sil.Aundef (callee_pname, callee_loc, _)) ->
                add_constraints_on_retval pdesc prop n_rhs_exp' typ callee_pname callee_loc
            | _ -> prop
          else prop in
        let iter_list =
          Rearrange.rearrange ~report_deref_errors pdesc tenv n_rhs_exp' typ prop' loc in
        IList.rev (IList.fold_left (execute_letderef_ pdesc tenv id loc) [] iter_list)
  with Rearrange.ARRAY_ACCESS ->
    if (!Config.array_level = 0) then assert false
    else
      let undef = Sil.exp_get_undefined false in
      [Prop.conjoin_eq (Sil.Var id) undef prop_]

let execute_set ?(report_deref_errors=true) pname pdesc tenv lhs_exp typ rhs_exp loc prop_ =
  let execute_set_ pdesc tenv rhs_exp acc_in iter =
    let (lexp, strexp, typ, st, offlist) =
      match Prop.prop_iter_current iter with
      | (Sil.Hpointsto(lexp, strexp, Sil.Sizeof (typ, st)), offlist) ->
          (lexp, strexp, typ, st, offlist)
      | _ -> assert false in
    let p = Prop.prop_iter_to_prop iter in
    let new_ptsto, pred_insts_op =
      ptsto_update pdesc tenv p (lexp, strexp, typ, st) offlist rhs_exp in
    let update acc (pi, sigma) =
      let sigma' = new_ptsto:: sigma in
      let iter' = update_iter iter pi sigma' in
      let prop' = Prop.prop_iter_to_prop iter' in
      prop' :: acc in
    match pred_insts_op with
    | None -> update acc_in ([],[])
    | Some pred_insts -> IList.fold_left update acc_in pred_insts in
  try
    let n_lhs_exp, prop_' = check_arith_norm_exp pname lhs_exp prop_ in
    let n_rhs_exp, prop = check_arith_norm_exp pname rhs_exp prop_' in
    let prop = Prop.replace_objc_null prop n_lhs_exp n_rhs_exp in
    let n_lhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_lhs_exp in
    let iter_list = Rearrange.rearrange ~report_deref_errors pdesc tenv n_lhs_exp' typ prop loc in
    IList.rev (IList.fold_left (execute_set_ pdesc tenv n_rhs_exp) [] iter_list)
  with Rearrange.ARRAY_ACCESS ->
    if (!Config.array_level = 0) then assert false
    else [prop_]

(** Execute [instr] with a symbolic heap [prop].*)
let rec sym_exec tenv current_pdesc _instr (prop_: Prop.normal Prop.t) path
  : (Prop.normal Prop.t * Paths.Path.t) list =
  let current_pname = Cfg.Procdesc.get_proc_name current_pdesc in
  State.set_instr _instr; (* mark instruction last seen *)
  State.set_prop_tenv_pdesc prop_ tenv current_pdesc; (* mark prop,tenv,pdesc last seen *)
  SymOp.pay(); (* pay one symop *)
  let ret_old_path pl = (* return the old path unchanged *)
    IList.map (fun p -> (p, path)) pl in
  let instr = match _instr with
    | Sil.Call (ret, exp, par, loc, call_flags) ->
        let exp' = Prop.exp_normalize_prop prop_ exp in
        let instr' = match exp' with
          | Sil.Const (Sil.Cclosure c) ->
              let proc_exp = Sil.Const (Sil.Cfun c.name) in
              let proc_exp' = Prop.exp_normalize_prop prop_ proc_exp in
              let par' = IList.map (fun (id_exp, _, typ) -> (id_exp, typ)) c.captured_vars in
              Sil.Call (ret, proc_exp', par' @ par, loc, call_flags)
          | _ ->
              Sil.Call (ret, exp', par, loc, call_flags) in
        instr'
    | _ -> _instr in
  let skip_call prop path callee_pname loc ret_ids ret_typ_opt actual_args =
    let exn = Exceptions.Skip_function (Localise.desc_skip_function callee_pname) in
    Reporting.log_info current_pname exn;
    L.d_strln
      ("Undefined function " ^ Procname.to_string callee_pname
       ^ ", returning undefined value.");
    (match Specs.get_summary current_pname with
     | None -> ()
     | Some summary ->
         Specs.CallStats.trace
           summary.Specs.stats.Specs.call_stats callee_pname loc
           (Specs.CallStats.CR_skip) !Config.footprint);
    unknown_or_scan_call ~is_scan:false ret_typ_opt Builtin.{
        pdesc= current_pdesc; instr; tenv; prop_= prop; path; ret_ids; args= actual_args;
        proc_name= callee_pname; loc; } in
  let call_args prop_ proc_name args ret_ids loc = {
    Builtin.pdesc = current_pdesc; instr; tenv; prop_; path; ret_ids; args; proc_name; loc; } in
  match instr with
  | Sil.Letderef (id, rhs_exp, typ, loc) ->
      execute_letderef current_pname current_pdesc tenv id rhs_exp typ loc prop_
      |> ret_old_path
  | Sil.Set (lhs_exp, typ, rhs_exp, loc) ->
      execute_set current_pname current_pdesc tenv lhs_exp typ rhs_exp loc prop_
      |> ret_old_path
  | Sil.Prune (cond, loc, true_branch, ik) ->
      let prop__ = Prop.nullify_exp_with_objc_null prop_ cond in
      let check_condition_always_true_false () =
        let report_condition_always_true_false i =
          let skip_loop = match ik with
            | Sil.Ik_while | Sil.Ik_for ->
                not (Sil.Int.iszero i) (* skip wile(1) and for (;1;) *)
            | Sil.Ik_dowhile ->
                true (* skip do..while *)
            | Sil.Ik_land_lor ->
                true (* skip subpart of a condition obtained from compilation of && and || *)
            | _ -> false in
          true_branch && not skip_loop in
        (* in comparisons, nil is translated as (void * ) 0 rather than 0 *)
        let is_comparison_to_nil = function
          | Sil.Cast ((Sil.Tptr (Sil.Tvoid, _)), exp) ->
              !Config.curr_language = Config.C_CPP && Sil.exp_is_zero exp
          | _ -> false in
        match Prop.exp_normalize_prop Prop.prop_emp cond with
        | Sil.Const (Sil.Cint i) when report_condition_always_true_false i ->
            let node = State.get_node () in
            let desc = Errdesc.explain_condition_always_true_false i cond node loc in
            let exn =
              Exceptions.Condition_always_true_false (desc, not (Sil.Int.iszero i), __POS__) in
            let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop current_pname) in
            Reporting.log_warning current_pname ~pre: pre_opt exn
        | Sil.BinOp ((Sil.Eq | Sil.Ne), lhs, rhs)
          when true_branch && !Config.footprint && not (is_comparison_to_nil rhs) ->
            (* iOS: check that NSNumber *'s are not used in conditionals without comparing to nil *)
            let lhs_normal = Prop.exp_normalize_prop prop__ lhs in
            let is_nsnumber = function
              | Sil.Tvar (Typename.TN_csu (Csu.Class _, name)) ->
                  Mangled.to_string name = "NSNumber"
              | _ -> false in
            let lhs_is_ns_ptr () =
              IList.exists
                (function
                  | Sil.Hpointsto (_, Sil.Eexp (exp, _), Sil.Sizeof (Sil.Tptr (typ, _), _)) ->
                      Sil.exp_equal exp lhs_normal && is_nsnumber typ
                  | _ -> false)
                (Prop.get_sigma prop__) in
            if not (Sil.exp_is_zero lhs_normal) && lhs_is_ns_ptr () then
              let node = State.get_node () in
              let desc = Errdesc.explain_bad_pointer_comparison lhs node loc in
              let exn = Exceptions.Bad_pointer_comparison (desc, __POS__) in
              Reporting.log_warning current_pname exn
        | _ -> () in
      if not !Config.report_runtime_exceptions then
        check_already_dereferenced current_pname cond prop__;
      check_condition_always_true_false ();
      let n_cond, prop = check_arith_norm_exp current_pname cond prop__ in
      ret_old_path (Propset.to_proplist (prune ~positive:true n_cond prop))
  | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname), args, loc, _)
    when Builtin.is_registered callee_pname ->
      let sym_exe_builtin = Builtin.get callee_pname in
      sym_exe_builtin (call_args prop_ callee_pname args ret_ids loc)
  | Sil.Call (ret_ids,
              Sil.Const (Sil.Cfun ((Procname.Java callee_pname_java) as callee_pname)),
              actual_params, loc, call_flags)
    when Config.lazy_dynamic_dispatch ->
      let norm_prop, norm_args = normalize_params current_pname prop_ actual_params in
      let exec_skip_call skipped_pname ret_type =
        skip_call norm_prop path skipped_pname loc ret_ids (Some ret_type) norm_args in
      let resolved_pname, summary_opt =
        resolve_and_analyze tenv current_pdesc norm_prop norm_args callee_pname call_flags in
      begin
        match summary_opt with
        | None ->
            let ret_typ =
              match Tenv.proc_extract_return_typ tenv callee_pname_java with
              | Some (Sil.Tstruct _ as typ) -> Sil.Tptr (typ, Pk_pointer)
              | Some typ -> typ
              | None -> Sil.Tvoid in
            exec_skip_call resolved_pname ret_typ
        | Some summary when call_should_be_skipped resolved_pname summary ->
            exec_skip_call resolved_pname summary.Specs.attributes.ProcAttributes.ret_type
        | Some summary ->
            proc_call summary (call_args prop_ callee_pname norm_args ret_ids loc)
      end

  | Sil.Call (ret_ids,
              Sil.Const (Sil.Cfun ((Procname.Java callee_pname_java) as callee_pname)),
              actual_params, loc, call_flags) ->
      do_error_checks (Paths.Path.curr_node path) instr current_pname current_pdesc;
      let norm_prop, norm_args = normalize_params current_pname prop_ actual_params in
      let url_handled_args =
        call_constructor_url_update_args callee_pname norm_args in
      let resolved_pnames =
        resolve_virtual_pname tenv norm_prop url_handled_args callee_pname call_flags in
      let exec_one_pname pname =
        Ondemand.analyze_proc_name ~propagate_exceptions:true current_pdesc pname;
        let exec_skip_call ret_type =
          skip_call norm_prop path pname loc ret_ids (Some ret_type) url_handled_args in
        match Specs.get_summary pname with
        | None ->
            let ret_typ =
              match Tenv.proc_extract_return_typ tenv callee_pname_java with
              | Some (Sil.Tstruct _ as typ) -> Sil.Tptr (typ, Pk_pointer)
              | Some typ -> typ
              | None -> Sil.Tvoid in
            exec_skip_call ret_typ
        | Some summary when call_should_be_skipped pname summary ->
            exec_skip_call summary.Specs.attributes.ProcAttributes.ret_type
        | Some summary ->
            proc_call summary (call_args norm_prop pname url_handled_args ret_ids loc) in
      IList.fold_left (fun acc pname -> exec_one_pname pname @ acc) [] resolved_pnames

  | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname), actual_params, loc, call_flags) ->
      (** Generic fun call with known name *)
      let (prop_r, n_actual_params) = normalize_params current_pname prop_ actual_params in
      let resolved_pname =
        match resolve_virtual_pname tenv prop_r n_actual_params callee_pname call_flags with
        | resolved_pname :: _ -> resolved_pname
        | [] -> callee_pname in

      Ondemand.analyze_proc_name ~propagate_exceptions:true current_pdesc resolved_pname;

      let callee_pdesc_opt = Ondemand.get_proc_desc resolved_pname in

      let ret_typ_opt = Option.map Cfg.Procdesc.get_ret_type callee_pdesc_opt in
      let sentinel_result =
        if !Config.curr_language = Config.C_CPP then
          check_variadic_sentinel_if_present
            (call_args prop_r callee_pname actual_params ret_ids loc)
        else [(prop_r, path)] in
      let do_call (prop, path) =
        let attrs_opt = Option.map Cfg.Procdesc.get_attributes callee_pdesc_opt in
        let objc_property_accessor =
          match attrs_opt with
          | Some attrs -> attrs.ProcAttributes.objc_accessor
          | None -> None in
        let summary = Specs.get_summary resolved_pname in
        let should_skip resolved_pname summary =
          match summary with
          | None -> true
          | Some summary -> call_should_be_skipped resolved_pname summary in
        if should_skip resolved_pname summary then
          (* If it's an ObjC getter or setter, call the builtin rather than skipping *)
          match objc_property_accessor with
          | Some objc_property_accessor ->
              handle_objc_method_call
                n_actual_params n_actual_params prop tenv ret_ids
                current_pdesc callee_pname loc path
                (sym_exec_objc_accessor objc_property_accessor ret_typ_opt)
          | None ->
              skip_call prop path resolved_pname loc ret_ids ret_typ_opt n_actual_params
        else
          proc_call (Option.get summary)
            (call_args prop resolved_pname n_actual_params ret_ids loc) in
      IList.flatten (IList.map do_call sentinel_result)
  | Sil.Call (ret_ids, fun_exp, actual_params, loc, call_flags) -> (** Call via function pointer *)
      let (prop_r, n_actual_params) = normalize_params current_pname prop_ actual_params in
      if call_flags.Sil.cf_is_objc_block then
        Rearrange.check_call_to_objc_block_error current_pdesc prop_r fun_exp loc;
      Rearrange.check_dereference_error current_pdesc prop_r fun_exp loc;
      if call_flags.Sil.cf_noreturn then begin
        L.d_str "Unknown function pointer with noreturn attribute ";
        Sil.d_exp fun_exp; L.d_strln ", diverging.";
        diverge prop_r path
      end else begin
        L.d_str "Unknown function pointer "; Sil.d_exp fun_exp;
        L.d_strln ", returning undefined value.";
        let callee_pname = Procname.from_string_c_fun "__function_pointer__" in
        unknown_or_scan_call ~is_scan:false None Builtin.{
            pdesc= current_pdesc; instr; tenv; prop_= prop_r; path; ret_ids; args= n_actual_params;
            proc_name= callee_pname; loc; }
      end
  | Sil.Nullify (pvar, _, deallocate) ->
      begin
        let eprop = Prop.expose prop_ in
        match IList.partition
                (function
                  | Sil.Hpointsto (Sil.Lvar pvar', _, _) -> Pvar.equal pvar pvar'
                  | _ -> false) (Prop.get_sigma eprop) with
        | [Sil.Hpointsto(e, se, typ)], sigma' ->
            let sigma'' = match deallocate with
              | false ->
                  let se' = execute_nullify_se se in
                  Sil.Hpointsto(e, se', typ):: sigma'
              | true -> sigma' in
            let eprop_res = Prop.replace_sigma sigma'' eprop in
            ret_old_path [Prop.normalize eprop_res]
        | _ -> assert false
      end
  | Sil.Abstract _ ->
      let node = State.get_node () in
      let blocks_nullified = get_blocks_nullified node in
      IList.iter (check_block_retain_cycle tenv current_pname prop_) blocks_nullified;
      if Prover.check_inconsistency prop_
      then
        ret_old_path []
      else
        ret_old_path
          [Abs.remove_redundant_array_elements current_pname tenv
             (Abs.abstract current_pname tenv prop_)]
  | Sil.Remove_temps (temps, _) ->
      ret_old_path [Prop.exist_quantify (Sil.fav_from_list temps) prop_]
  | Sil.Declare_locals (ptl, _) ->
      let sigma_locals =
        let add_None (x, y) = (x, Sil.Sizeof (y, Sil.Subtype.exact), None) in
        let sigma_locals () =
          IList.map
            (Prop.mk_ptsto_lvar (Some tenv) Prop.Fld_init Sil.inst_initial)
            (IList.map add_None ptl) in
        run_in_re_execution_mode (* no footprint vars for locals *)
          sigma_locals () in
      let sigma' = Prop.get_sigma prop_ @ sigma_locals in
      let prop' = Prop.normalize (Prop.replace_sigma sigma' prop_) in
      ret_old_path [prop']
  | Sil.Stackop _ -> (* this should be handled at the propset level *)
      assert false
and diverge prop path =
  State.add_diverging_states (Paths.PathSet.from_renamed_list [(prop, path)]); (* diverge *)
  []

(** Symbolic execution of a sequence of instructions.
    If errors occur and [mask_errors] is true, just treat as skip. *)
and instrs ?(mask_errors=false) tenv pdesc instrs ppl =
  let exe_instr instr (p, path) =
    L.d_str "Executing Generated Instruction "; Sil.d_instr instr; L.d_ln ();
    try sym_exec tenv pdesc instr p path
    with exn when SymOp.exn_not_failure exn && mask_errors ->
      let err_name, _, ml_source, _ , _, _, _ = Exceptions.recognize_exception exn in
      let loc = (match ml_source with
          | Some ml_loc -> "at " ^ (L.ml_loc_to_string ml_loc)
          | None -> "") in
      L.d_warning
        ("Generated Instruction Failed with: " ^
         (Localise.to_string err_name)^loc ); L.d_ln();
      [(p, path)] in
  let f plist instr = IList.flatten (IList.map (exe_instr instr) plist) in
  IList.fold_left f ppl instrs

and add_constraints_on_actuals_by_ref tenv prop actuals_by_ref callee_pname callee_loc =
  (* replace an hpred of the form actual_var |-> _ with new_hpred in prop *)
  let replace_actual_hpred actual_var new_hpred prop =
    let sigma' =
      IList.map
        (function
          | Sil.Hpointsto (lhs, _, _) when Sil.exp_equal lhs actual_var -> new_hpred
          | hpred -> hpred)
        (Prop.get_sigma prop) in
    Prop.normalize (Prop.replace_sigma sigma' prop) in
  if !Config.angelic_execution then
    let add_actual_by_ref_to_footprint prop (actual, actual_typ) =
      match actual with
      | Sil.Lvar actual_pv ->
          (* introduce a fresh program variable to allow abduction on the return value *)
          let abducted_ref_pv =
            Pvar.mk_abducted_ref_param callee_pname actual_pv callee_loc in
          let already_has_abducted_retval p =
            IList.exists
              (fun hpred -> match hpred with
                 | Sil.Hpointsto (Sil.Lvar pv, _, _) -> Pvar.equal pv abducted_ref_pv
                 | _ -> false)
              (Prop.get_sigma_footprint p) in
          (* prevent introducing multiple abducted retvals for a single call site in a loop *)
          if already_has_abducted_retval prop then prop
          else
          if !Config.footprint then
            let prop', abduced_strexp = match actual_typ with
              | Sil.Tptr ((Sil.Tstruct _) as typ, _) ->
                  (* for struct types passed by reference, do abduction on the fields of the
                     struct *)
                  add_struct_value_to_footprint tenv abducted_ref_pv typ prop
              | Sil.Tptr (typ, _) ->
                  (* for pointer types passed by reference, do abduction directly on the pointer *)
                  let (prop', fresh_fp_var) =
                    add_to_footprint abducted_ref_pv typ prop in
                  prop', Sil.Eexp (fresh_fp_var, Sil.Inone)
              | typ ->
                  failwith
                    ("No need for abduction on non-pointer type " ^
                     (Sil.typ_to_string typ)) in
            (* replace [actual] |-> _ with [actual] |-> [fresh_fp_var] *)
            let filtered_sigma =
              IList.map
                (function
                  | Sil.Hpointsto (lhs, _, typ_exp) when Sil.exp_equal lhs actual ->
                      Sil.Hpointsto (lhs, abduced_strexp, typ_exp)
                  | hpred -> hpred)
                (Prop.get_sigma prop') in
            Prop.normalize (Prop.replace_sigma filtered_sigma prop')
          else
            (* bind actual passed by ref to the abducted value pointed to by the synthetic pvar *)
            let prop' =
              let filtered_sigma =
                IList.filter
                  (function
                    | Sil.Hpointsto (lhs, _, _) when Sil.exp_equal lhs actual ->
                        false
                    | _ -> true)
                  (Prop.get_sigma prop) in
              Prop.normalize (Prop.replace_sigma filtered_sigma prop) in
            IList.fold_left
              (fun p hpred ->
                 match hpred with
                 | Sil.Hpointsto (Sil.Lvar pv, rhs, texp) when Pvar.equal pv abducted_ref_pv ->
                     let new_hpred = Sil.Hpointsto (actual, rhs, texp) in
                     Prop.normalize (Prop.replace_sigma (new_hpred :: (Prop.get_sigma prop')) p)
                 | _ -> p)
              prop'
              (Prop.get_sigma prop')
      | _ -> assert false in
    IList.fold_left add_actual_by_ref_to_footprint prop actuals_by_ref
  else
    (* non-angelic mode; havoc each var passed by reference by assigning it to a fresh id *)
    let havoc_actual_by_ref (actual, actual_typ) prop =
      let actual_pt_havocd_var =
        let havocd_var = Sil.Var (Ident.create_fresh Ident.kprimed) in
        let sizeof_exp = Sil.Sizeof (Sil.typ_strip_ptr actual_typ, Sil.Subtype.subtypes) in
        Prop.mk_ptsto actual (Sil.Eexp (havocd_var, Sil.Inone)) sizeof_exp in
      replace_actual_hpred actual actual_pt_havocd_var prop in
    IList.fold_left (fun p var -> havoc_actual_by_ref var p) prop actuals_by_ref

and check_untainted exp caller_pname callee_pname prop =
  match Prop.get_taint_attribute prop exp with
  | Some (Sil.Ataint source_pname) ->
      let err_desc =
        Errdesc.explain_tainted_value_reaching_sensitive_function
          prop
          exp
          source_pname
          callee_pname
          (State.get_loc ()) in
      let exn =
        Exceptions.Tainted_value_reaching_sensitive_function
          (err_desc, __POS__) in
      Reporting.log_warning caller_pname exn;
      Prop.add_or_replace_exp_attribute prop exp (Sil.Auntaint)
  | _ ->
      if !Config.footprint then
        let untaint_attr = Sil.Const (Sil.Cattribute (Sil.Auntaint)) in
        (* add untained(n_lexp) to the footprint *)
        Prop.conjoin_neq ~footprint:true exp untaint_attr prop
      else prop

(** execute a call for an unknown or scan function *)
and unknown_or_scan_call ~is_scan ret_type_option
    { Builtin.tenv; pdesc; prop_= pre; path; ret_ids;
      args= actual_pars; proc_name= callee_pname; loc; } =
  let remove_file_attribute prop =
    let do_exp p (e, _) =
      let do_attribute q = function
        | Sil.Aresource res_action as res
          when res_action.Sil.ra_res = Sil.Rfile ->
            Prop.remove_attribute res q
        | _ -> q in
      IList.fold_left do_attribute p (Prop.get_exp_attributes p e) in
    IList.fold_left do_exp prop actual_pars in
  let add_tainted_pre prop actuals caller_pname callee_pname =
    if !Config.taint_analysis then
      match Taint.accepts_sensitive_params callee_pname None with
      | [] -> prop
      | param_nums ->
          let check_taint_if_nums_match (prop_acc, param_num) (actual_exp, _actual_typ) =
            let prop_acc' =
              if IList.exists (fun num -> num = param_num) param_nums
              then check_untainted actual_exp caller_pname callee_pname prop_acc
              else prop_acc in
            prop_acc', param_num + 1 in
          IList.fold_left
            check_taint_if_nums_match
            (prop, 0)
            actuals
          |> fst
    else prop in
  let actuals_by_ref =
    IList.filter
      (function
        | Sil.Lvar _, Sil.Tptr _ -> true
        | _ -> false)
      actual_pars in
  let pre_final =
    (* in Java, assume that skip functions close resources passed as params *)
    let pre_1 = if !Config.curr_language = Config.Java then remove_file_attribute pre else pre in
    let pre_2 = match ret_ids, ret_type_option with
      | [ret_id], Some ret_typ ->
          add_constraints_on_retval pdesc pre_1 (Sil.Var ret_id) ret_typ callee_pname loc
      | _ -> pre_1 in
    let pre_3 = add_constraints_on_actuals_by_ref tenv pre_2 actuals_by_ref callee_pname loc in
    let caller_pname = Cfg.Procdesc.get_proc_name pdesc in
    add_tainted_pre pre_3 actual_pars caller_pname callee_pname in
  if is_scan (* if scan function, don't mark anything with undef attributes *)
  then [(Tabulation.remove_constant_string_class pre_final, path)]
  else
    (* otherwise, add undefined attribute to retvals and actuals passed by ref *)
    let exps_to_mark =
      let ret_exps = IList.map (fun ret_id -> Sil.Var ret_id) ret_ids in
      IList.fold_left
        (fun exps_to_mark (exp, _) -> exp :: exps_to_mark) ret_exps actuals_by_ref in
    let path_pos = State.get_path_pos () in
    [(Prop.mark_vars_as_undefined pre_final exps_to_mark callee_pname loc path_pos, path)]

and check_variadic_sentinel
    ?(fails_on_nil = false) n_formals  (sentinel, null_pos)
    { Builtin.pdesc; tenv; prop_; path; args; proc_name; loc; }
  =
  (* from clang's lib/Sema/SemaExpr.cpp: *)
  (* "nullPos" is the number of formal parameters at the end which *)
  (* effectively count as part of the variadic arguments.  This is *)
  (* useful if you would prefer to not have *any* formal parameters, *)
  (* but the language forces you to have at least one. *)
  let first_var_arg_pos = if null_pos > n_formals then 0 else n_formals - null_pos in
  let nargs = IList.length args in
  (* sentinels start counting from the last argument to the function *)
  let sentinel_pos = nargs - sentinel - 1 in
  let mk_non_terminal_argsi (acc, i) a =
    if i < first_var_arg_pos || i >= sentinel_pos then (acc, i +1)
    else ((a, i):: acc, i +1) in
  (* IList.fold_left reverses the arguments *)
  let non_terminal_argsi = fst (IList.fold_left mk_non_terminal_argsi ([], 0) args) in
  let check_allocated result ((lexp, typ), i) =
    (* simulate a Letderef for [lexp] *)
    let tmp_id_deref = Ident.create_fresh Ident.kprimed in
    let letderef = Sil.Letderef (tmp_id_deref, lexp, typ, loc) in
    try
      instrs tenv pdesc [letderef] result
    with e when SymOp.exn_not_failure e ->
      if not fails_on_nil then
        let deref_str = Localise.deref_str_nil_argument_in_variadic_method proc_name nargs i in
        let err_desc =
          Errdesc.explain_dereference ~use_buckets: true ~is_premature_nil: true
            deref_str prop_ loc in
        raise (Exceptions.Premature_nil_termination (err_desc, __POS__))
      else
        raise e in
  (* IList.fold_left reverses the arguments back so that we report an *)
  (* error on the first premature nil argument *)
  IList.fold_left check_allocated [(prop_, path)] non_terminal_argsi

and check_variadic_sentinel_if_present
    ({ Builtin.prop_; path; proc_name; } as builtin_args) =
  match Specs.proc_resolve_attributes proc_name with
  | None ->
      [(prop_, path)]
  | Some callee_attributes ->
      match Sil.get_sentinel_func_attribute_value
              callee_attributes.ProcAttributes.func_attributes with
      | None -> [(prop_, path)]
      | Some sentinel_arg ->
          let formals = callee_attributes.ProcAttributes.formals in
          check_variadic_sentinel (IList.length formals) sentinel_arg builtin_args

and sym_exec_objc_getter field_name ret_typ_opt tenv ret_ids pdesc pname loc args prop =
  L.d_strln ("No custom getter found. Executing the ObjC builtin getter with ivar "^
             (Ident.fieldname_to_string field_name)^".");
  let ret_id =
    match ret_ids with
    | [ret_id] -> ret_id
    | _ -> assert false in
  let ret_typ =
    match ret_typ_opt with
    | Some ret_typ -> ret_typ
    | None -> assert false in
  match args with
  | [(lexp, typ)] ->
      let typ' = (match Tenv.expand_type tenv typ with
          | Sil.Tstruct _ as s -> s
          | Sil.Tptr (t, _) -> Tenv.expand_type tenv t
          | _ -> assert false) in
      let field_access_exp = Sil.Lfield (lexp, field_name, typ') in
      execute_letderef
        ~report_deref_errors:false pname pdesc tenv ret_id field_access_exp ret_typ loc prop
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

and sym_exec_objc_setter field_name _ tenv _ pdesc pname loc args prop =
  L.d_strln ("No custom setter found. Executing the ObjC builtin setter with ivar "^
             (Ident.fieldname_to_string field_name)^".");
  match args with
  | (lexp1, typ1) :: (lexp2, typ2)::_ ->
      let typ1' = (match Tenv.expand_type tenv typ1 with
          | Sil.Tstruct _ as s -> s
          | Sil.Tptr (t, _) -> Tenv.expand_type tenv t
          | _ -> assert false) in
      let field_access_exp = Sil.Lfield (lexp1, field_name, typ1') in
      execute_set ~report_deref_errors:false pname pdesc tenv field_access_exp typ2 lexp2 loc prop
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

and sym_exec_objc_accessor property_accesor ret_typ_opt tenv ret_ids pdesc _ loc args prop path
  : Builtin.ret_typ =
  let f_accessor =
    match property_accesor with
    | ProcAttributes.Objc_getter field_name -> sym_exec_objc_getter field_name
    | ProcAttributes.Objc_setter field_name -> sym_exec_objc_setter field_name in
  (* we want to execute in the context of the current procedure, not in the context of callee_pname,
     since this is the procname of the setter/getter method *)
  let cur_pname = Cfg.Procdesc.get_proc_name pdesc in
  f_accessor ret_typ_opt tenv ret_ids pdesc cur_pname loc args prop
  |> IList.map (fun p -> (p, path))

(** Perform symbolic execution for a function call *)
and proc_call summary {Builtin.pdesc; tenv; prop_= pre; path; ret_ids; args= actual_pars; loc; } =
  let caller_pname = Cfg.Procdesc.get_proc_name pdesc in
  let callee_pname = Specs.get_proc_name summary in
  let ret_typ = Specs.get_ret_type summary in
  let check_return_value_ignored () =
    (* check if the return value of the call is ignored, and issue a warning *)
    let is_ignored = match ret_typ, ret_ids with
      | Sil.Tvoid, _ -> false
      | Sil.Tint _, _ when not (proc_is_defined callee_pname) ->
          (* if the proc returns Tint and is not defined, *)
          (* don't report ignored return value *)
          false
      | _, [] -> true
      | _, [id] -> Errdesc.id_is_assigned_then_dead (State.get_node ()) id
      | _ -> false in
    if is_ignored
    && Specs.get_flag callee_pname proc_flag_ignore_return = None then
      let err_desc = Localise.desc_return_value_ignored callee_pname loc in
      let exn = (Exceptions.Return_value_ignored (err_desc, __POS__)) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop caller_pname) in
      Reporting.log_warning caller_pname ~pre: pre_opt exn in
  check_inherently_dangerous_function caller_pname callee_pname;
  begin
    let formal_types = IList.map (fun (_, typ) -> typ) (Specs.get_formals summary) in
    let rec comb actual_pars formal_types =
      match actual_pars, formal_types with
      | [], [] -> actual_pars
      | (e, t_e):: etl', _:: tl' ->
          (e, t_e) :: comb etl' tl'
      | _,[] ->
          Errdesc.warning_err
            (State.get_loc ())
            "likely use of variable-arguments function, or function prototype missing@.";
          L.d_warning
            "likely use of variable-arguments function, or function prototype missing";
          L.d_ln();
          L.d_str "actual parameters: "; Sil.d_exp_list (IList.map fst actual_pars); L.d_ln ();
          L.d_str "formal parameters: "; Sil.d_typ_list formal_types; L.d_ln ();
          actual_pars
      | [], _ ->
          L.d_str ("**** ERROR: Procedure " ^ Procname.to_string callee_pname);
          L.d_strln (" mismatch in the number of parameters ****");
          L.d_str "actual parameters: "; Sil.d_exp_list (IList.map fst actual_pars); L.d_ln ();
          L.d_str "formal parameters: "; Sil.d_typ_list formal_types; L.d_ln ();
          raise (Exceptions.Wrong_argument_number __POS__) in
    let actual_params = comb actual_pars formal_types in
    (* Actual parameters are associated to their formal
       parameter type if there are enough formal parameters, and
       to their actual type otherwise. The latter case happens
       with variable - arguments functions *)
    check_return_value_ignored ();
    (* In case we call an objc instance method we add and extra spec *)
    (* were the receiver is null and the semantics of the call is nop*)
    let callee_attrs = Specs.get_attributes summary in
    if (!Config.curr_language <> Config.Java) && !Config.objc_method_call_semantics &&
       (Specs.get_attributes summary).ProcAttributes.is_objc_instance_method then
      handle_objc_method_call actual_pars actual_params pre tenv ret_ids pdesc callee_pname loc
        path (Tabulation.exe_function_call callee_attrs)
    else  (* non-objective-c method call. Standard tabulation *)
      Tabulation.exe_function_call
        callee_attrs tenv ret_ids pdesc callee_pname loc actual_params pre path
  end

(** perform symbolic execution for a single prop, and check for junk *)
and sym_exec_wrapper handle_exn tenv pdesc instr ((prop: Prop.normal Prop.t), path)
  : Paths.PathSet.t =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let prop_primed_to_normal p = (** Rename primed vars with fresh normal vars, and return them *)
    let fav = Prop.prop_fav p in
    Sil.fav_filter_ident fav Ident.is_primed;
    let ids_primed = Sil.fav_to_list fav in
    let ids_primed_normal =
      IList.map (fun id -> (id, Ident.create_fresh Ident.knormal)) ids_primed in
    let ren_sub =
      Sil.sub_of_list (IList.map
                         (fun (id1, id2) -> (id1, Sil.Var id2)) ids_primed_normal) in
    let p' = Prop.normalize (Prop.prop_sub ren_sub p) in
    let fav_normal = Sil.fav_from_list (IList.map snd ids_primed_normal) in
    p', fav_normal in
  let prop_normal_to_primed fav_normal p = (* rename given normal vars to fresh primed *)
    if Sil.fav_to_list fav_normal = [] then p
    else Prop.exist_quantify fav_normal p in
  try
    let pre_process_prop p =
      let p', fav =
        if Sil.instr_is_auxiliary instr
        then p, Sil.fav_new ()
        else prop_primed_to_normal p in
      let p'' =
        let map_res_action e ra = (* update the vpath in resource attributes *)
          let vpath, _ = Errdesc.vpath_find p' e in
          { ra with Sil.ra_vpath = vpath } in
        Prop.attribute_map_resource p' map_res_action in
      p'', fav in
    let post_process_result fav_normal p path =
      let p' = prop_normal_to_primed fav_normal p in
      State.set_path path None;
      let node_has_abstraction node =
        let instr_is_abstraction = function
          | Sil.Abstract _ -> true
          | _ -> false in
        IList.exists instr_is_abstraction (Cfg.Node.get_instrs node) in
      let curr_node = State.get_node () in
      match Cfg.Node.get_kind curr_node with
      | Cfg.Node.Prune_node _ when not (node_has_abstraction curr_node) ->
          (* don't check for leaks in prune nodes, unless there is abstraction anyway,*)
          (* but force them into either branch *)
          p'
      | _ ->
          check_deallocate_static_memory (Abs.abstract_junk ~original_prop: p pname tenv p') in
    L.d_str "Instruction "; Sil.d_instr instr; L.d_ln ();
    let prop', fav_normal = pre_process_prop prop in
    let res_list =
      run_with_abs_val_equal_zero (* no exp abstraction during sym exe *)
        (fun () -> sym_exec tenv pdesc instr prop' path)
        () in
    let res_list_nojunk =
      IList.map
        (fun (p, path) -> (post_process_result fav_normal p path, path))
        res_list in
    let results =
      IList.map
        (fun (p, path) -> (Prop.prop_rename_primed_footprint_vars p, path))
        res_list_nojunk in
    L.d_strln "Instruction Returns";
    Propgraph.d_proplist prop (IList.map fst results); L.d_ln ();
    State.mark_instr_ok ();
    Paths.PathSet.from_renamed_list results
  with exn when Exceptions.handle_exception exn && !Config.footprint ->
    handle_exn exn; (* calls State.mark_instr_fail *)
    if !Config.nonstop
    then
      (* in nonstop mode treat the instruction as skip *)
      (Paths.PathSet.from_renamed_list [(prop, path)])
    else
      Paths.PathSet.empty

(** {2 Lifted Abstract Transfer Functions} *)

let node handle_exn tenv node (pset : Paths.PathSet.t) : Paths.PathSet.t =
  let pdesc = Cfg.Node.get_proc_desc node in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let exe_instr_prop instr p tr (pset1: Paths.PathSet.t) =
    let pset2 =
      if Tabulation.prop_is_exn pname p && not (Sil.instr_is_auxiliary instr)
         && Cfg.Node.get_kind node <> Cfg.Node.exn_handler_kind
         (* skip normal instructions if an exception was thrown,
            unless this is an exception handler node *)
      then
        begin
          L.d_str "Skipping instr "; Sil.d_instr instr; L.d_strln " due to exception";
          Paths.PathSet.from_renamed_list [(p, tr)]
        end
      else sym_exec_wrapper handle_exn tenv pdesc instr (p, tr) in
    Paths.PathSet.union pset2 pset1 in
  let exe_instr_pset (pset, stack) instr = (** handle a single instruction at the set level *)
    let pp_stack_instr pset' =
      L.d_str "Stack Instruction "; Sil.d_instr instr; L.d_ln ();
      L.d_strln "Stack Instruction Returns";
      Propset.d Prop.prop_emp (Paths.PathSet.to_propset pset'); L.d_ln () in
    match instr, stack with
    | Sil.Stackop (Sil.Push, _), _ ->
        pp_stack_instr pset;
        (pset, pset :: stack)
    | Sil.Stackop (Sil.Swap, _), (pset':: stack') ->
        pp_stack_instr pset';
        (pset', pset:: stack')
    | Sil.Stackop (Sil.Pop, _), (pset':: stack') ->
        let pset'' = Paths.PathSet.union pset pset' in
        pp_stack_instr pset'';
        (pset'', stack')
    | Sil.Stackop _, _ -> (* should not happen *)
        assert false
    | _ ->
        let pset' = Paths.PathSet.fold (exe_instr_prop instr) pset Paths.PathSet.empty in
        (pset', stack) in
  let stack = [] in
  let instrs = Cfg.Node.get_instrs node in
  let pset', stack' = IList.fold_left exe_instr_pset (pset, stack) instrs in
  if stack' != [] then assert false; (* final stack must be empty *)
  pset'
