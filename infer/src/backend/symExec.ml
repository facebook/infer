(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Symbolic Execution *)

module L = Logging
module F = Format

let rec fldlist_assoc fld = function
  | []
   -> raise Not_found
  | (fld', x, _) :: l
   -> if Typ.Fieldname.equal fld fld' then x else fldlist_assoc fld l

let unroll_type tenv (typ: Typ.t) (off: Sil.offset) =
  let fail fld_to_string fld =
    L.d_strln ".... Invalid Field Access ...." ;
    L.d_str ("Fld : " ^ fld_to_string fld) ;
    L.d_ln () ;
    L.d_str "Type : " ;
    Typ.d_full typ ;
    L.d_ln () ;
    raise (Exceptions.Bad_footprint __POS__)
  in
  match (typ.desc, off) with
  | Tstruct name, Off_fld (fld, _) -> (
    match Tenv.lookup tenv name with
    | Some {fields; statics} -> (
      try fldlist_assoc fld (fields @ statics)
      with Not_found -> fail Typ.Fieldname.to_string fld )
    | None
     -> fail Typ.Fieldname.to_string fld )
  | Tarray (typ', _, _), Off_index _
   -> typ'
  | _, Off_index Const Cint i when IntLit.iszero i
   -> typ
  | _
   -> fail Sil.offset_to_string off

(** Given a node, returns a list of pvar of blocks that have been nullified in the block. *)
let get_blocks_nullified node =
  let null_blocks =
    List.concat_map
      ~f:(fun i ->
        match i with Sil.Nullify (pvar, _) when Sil.is_block_pvar pvar -> [pvar] | _ -> [])
      (Procdesc.Node.get_instrs node)
  in
  null_blocks

(** Given a proposition and an objc block checks whether by existentially quantifying
    captured variables in the block we obtain a leak. *)
let check_block_retain_cycle tenv caller_pname prop block_nullified =
  let mblock = Pvar.get_name block_nullified in
  let block_pname = Typ.Procname.mangled_objc_block (Mangled.to_string mblock) in
  let block_captured =
    match AttributesTable.load_attributes ~cache:true block_pname with
    | Some attributes
     -> fst (List.unzip attributes.ProcAttributes.captured)
    | None
     -> []
  in
  let prop' = Prop.remove_seed_captured_vars_block tenv block_captured prop in
  let prop'' = Prop.prop_rename_fav_with_existentials tenv prop' in
  let _ : Prop.normal Prop.t = Abs.abstract_junk ~original_prop:prop caller_pname tenv prop'' in
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
let rec apply_offlist pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist
    (f: Exp.t option -> Exp.t) inst lookup_inst =
  let pname = Procdesc.get_proc_name pdesc in
  let pp_error () =
    L.d_strln ".... Invalid Field ...." ;
    L.d_str "strexp : " ;
    Sil.d_sexp strexp ;
    L.d_ln () ;
    L.d_str "offlist : " ;
    Sil.d_offset_list offlist ;
    L.d_ln () ;
    L.d_str "type : " ;
    Typ.d_full typ ;
    L.d_ln () ;
    L.d_str "prop : " ;
    Prop.d_prop p ;
    L.d_ln () ;
    L.d_ln ()
  in
  match (offlist, strexp, typ.Typ.desc) with
  | [], Sil.Eexp (e, inst_curr), _
   -> let inst_is_uninitialized = function
        | Sil.Ialloc
         -> (* java allocation initializes with default values *)
            !Config.curr_language <> Config.Java
        | Sil.Iinitial
         -> true
        | _
         -> false
      in
      let is_hidden_field () =
        match State.get_instr () with
        | Some Sil.Load (_, Exp.Lfield (_, fieldname, _), _, _)
         -> Typ.Fieldname.is_hidden fieldname
        | _
         -> false
      in
      let inst_new =
        match inst with
        | Sil.Ilookup when inst_is_uninitialized inst_curr && not (is_hidden_field ())
         -> (* we are in a lookup of an uninitialized value *)
            lookup_inst := Some inst_curr ;
            let alloc_attribute_opt =
              if Sil.equal_inst inst_curr Sil.Iinitial then None
              else Attribute.get_undef tenv p root_lexp
            in
            let deref_str = Localise.deref_str_uninitialized alloc_attribute_opt in
            let err_desc = Errdesc.explain_memory_access tenv deref_str p (State.get_loc ()) in
            let exn = Exceptions.Uninitialized_value (err_desc, __POS__) in
            Reporting.log_warning_deprecated pname exn ; Sil.update_inst inst_curr inst
        | Sil.Ilookup
         -> (* a lookup does not change an inst unless it is inst_initial *)
            lookup_inst := Some inst_curr ;
            inst_curr
        | _
         -> Sil.update_inst inst_curr inst
      in
      let e' = f (Some e) in
      (e', Sil.Eexp (e', inst_new), typ, None)
  | [], Sil.Estruct (fesl, inst'), _
   -> if not nullify_struct then (f None, Sil.Estruct (fesl, inst'), typ, None)
      else if fp_root then (
        pp_error () ;
        assert false )
      else (
        L.d_strln "WARNING: struct assignment treated as nondeterministic assignment" ;
        (f None, Prop.create_strexp_of_type tenv Prop.Fld_init typ None inst, typ, None) )
  | [], Sil.Earray _, _
   -> let offlist' = Sil.Off_index Exp.zero :: offlist in
      apply_offlist pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist' f inst
        lookup_inst
  | (Sil.Off_fld _) :: _, Sil.Earray _, _
   -> let offlist_new = Sil.Off_index Exp.zero :: offlist in
      apply_offlist pdesc tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist_new f inst
        lookup_inst
  | (Sil.Off_fld (fld, fld_typ)) :: offlist', Sil.Estruct (fsel, inst'), Typ.Tstruct name -> (
    match Tenv.lookup tenv name with
    | Some ({fields} as struct_typ)
     -> (
        let t' = unroll_type tenv typ (Sil.Off_fld (fld, fld_typ)) in
        match List.find ~f:(fun fse -> Typ.Fieldname.equal fld (fst fse)) fsel with
        | Some (_, se')
         -> let res_e', res_se', res_t', res_pred_insts_op' =
              apply_offlist pdesc tenv p fp_root nullify_struct (root_lexp, se', t') offlist' f
                inst lookup_inst
            in
            let replace_fse fse =
              if Typ.Fieldname.equal fld (fst fse) then (fld, res_se') else fse
            in
            let res_se = Sil.Estruct (List.map ~f:replace_fse fsel, inst') in
            let replace_fta (f, t, a) =
              if Typ.Fieldname.equal fld f then (fld, res_t', a) else (f, t, a)
            in
            let fields' = List.map ~f:replace_fta fields in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            (res_e', res_se, typ, res_pred_insts_op')
        | None
         -> (* This case should not happen. The rearrangement should
                 have materialized all the accessed cells. *)
            pp_error () ;
            assert false )
    | None
     -> pp_error () ;
        assert false )
  | (Sil.Off_fld _) :: _, _, _
   -> pp_error () ;
      assert false
  | (Sil.Off_index idx) :: offlist', Sil.Earray (len, esel, inst1), Typ.Tarray (t', len', stride')
   -> (
      let nidx = Prop.exp_normalize_prop tenv p idx in
      match List.find ~f:(fun ese -> Prover.check_equal tenv p nidx (fst ese)) esel with
      | Some (idx_ese', se')
       -> let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist pdesc tenv p fp_root nullify_struct (root_lexp, se', t') offlist' f inst
              lookup_inst
          in
          let replace_ese ese =
            if Exp.equal idx_ese' (fst ese) then (idx_ese', res_se') else ese
          in
          let res_se = Sil.Earray (len, List.map ~f:replace_ese esel, inst1) in
          let res_t = Typ.mk ~default:typ (Tarray (res_t', len', stride')) in
          (res_e', res_se, res_t, res_pred_insts_op')
      | None
       -> (* return a nondeterministic value if the index is not found after rearrangement *)
          L.d_str "apply_offlist: index " ;
          Sil.d_exp idx ;
          L.d_strln " not materialized -- returning nondeterministic value" ;
          let res_e' = Exp.Var (Ident.create_fresh Ident.kprimed) in
          (res_e', strexp, typ, None) )
  | (Sil.Off_index _) :: _, _, _
   -> (* This case should not happen. The rearrangement should
         have materialized all the accessed cells. *)
      pp_error () ;
      raise (Exceptions.Internal_error (Localise.verbatim_desc "Array out of bounds in Symexec"))

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
let ptsto_lookup pdesc tenv p (lexp, se, sizeof) offlist id =
  let f = function Some exp -> exp | None -> Exp.Var id in
  let fp_root = match lexp with Exp.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let e', se', typ', pred_insts_op' =
    apply_offlist pdesc tenv p fp_root false (lexp, se, sizeof.Exp.typ) offlist f Sil.inst_lookup
      lookup_inst
  in
  let lookup_uninitialized =
    (* true if we have looked up an uninitialized value *)
    match !lookup_inst with Some (Sil.Iinitial | Sil.Ialloc | Sil.Ilookup) -> true | _ -> false
  in
  let ptsto' = Prop.mk_ptsto tenv lexp se' (Exp.Sizeof {sizeof with typ= typ'}) in
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
let ptsto_update pdesc tenv p (lexp, se, sizeof) offlist exp =
  let f _ = exp in
  let fp_root = match lexp with Exp.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let _, se', typ', pred_insts_op' =
    let pos = State.get_path_pos () in
    apply_offlist pdesc tenv p fp_root true (lexp, se, sizeof.Exp.typ) offlist f
      (State.get_inst_update pos) lookup_inst
  in
  let ptsto' = Prop.mk_ptsto tenv lexp se' (Exp.Sizeof {sizeof with typ= typ'}) in
  (ptsto', pred_insts_op')

let update_iter iter pi sigma =
  let iter' = Prop.prop_iter_update_current_by_list iter sigma in
  List.fold ~f:(Prop.prop_iter_add_atom false) ~init:iter' pi

(** Precondition: se should not include hpara_psto
    that could mean nonempty heaps. *)
let rec execute_nullify_se = function
  | Sil.Eexp _
   -> Sil.Eexp (Exp.zero, Sil.inst_nullify)
  | Sil.Estruct (fsel, _)
   -> let fsel' = List.map ~f:(fun (fld, se) -> (fld, execute_nullify_se se)) fsel in
      Sil.Estruct (fsel', Sil.inst_nullify)
  | Sil.Earray (len, esel, _)
   -> let esel' = List.map ~f:(fun (idx, se) -> (idx, execute_nullify_se se)) esel in
      Sil.Earray (len, esel', Sil.inst_nullify)

(** Do pruning for conditional [if (e1 != e2) ] if [positive] is true
    and [(if (e1 == e2)] if [positive] is false *)
let prune_ne tenv ~positive e1 e2 prop =
  let is_inconsistent =
    if positive then Prover.check_equal tenv prop e1 e2 else Prover.check_disequal tenv prop e1 e2
  in
  if is_inconsistent then Propset.empty
  else
    let conjoin = if positive then Prop.conjoin_neq else Prop.conjoin_eq in
    let new_prop = conjoin tenv ~footprint:!Config.footprint e1 e2 prop in
    if Prover.check_inconsistency tenv new_prop then Propset.empty
    else Propset.singleton tenv new_prop

(** Do pruning for conditional "if ([e1] CMP [e2])" if [positive] is
    true and "if (!([e1] CMP [e2]))" if [positive] is false, where CMP
    is "<" if [is_strict] is true and "<=" if [is_strict] is false.
*)
let prune_ineq tenv ~is_strict ~positive prop e1 e2 =
  if Exp.equal e1 e2 then
    if positive && not is_strict || not positive && is_strict then Propset.singleton tenv prop
    else Propset.empty
  else
    (* build the pruning condition and its negation, as explained in
       the comment above *)
    (* build [e1] CMP [e2] *)
    let cmp = if is_strict then Binop.Lt else Binop.Le in
    let e1_cmp_e2 = Exp.BinOp (cmp, e1, e2) in
    (* build !([e1] CMP [e2]) *)
    let dual_cmp = if is_strict then Binop.Le else Binop.Lt in
    let not_e1_cmp_e2 = Exp.BinOp (dual_cmp, e2, e1) in
    (* take polarity into account *)
    let prune_cond, not_prune_cond =
      if positive then (e1_cmp_e2, not_e1_cmp_e2) else (not_e1_cmp_e2, e1_cmp_e2)
    in
    let is_inconsistent = Prover.check_atom tenv prop (Prop.mk_inequality tenv not_prune_cond) in
    if is_inconsistent then Propset.empty
    else
      let footprint = !Config.footprint in
      let prop_with_ineq = Prop.conjoin_eq tenv ~footprint prune_cond Exp.one prop in
      Propset.singleton tenv prop_with_ineq

let rec prune tenv ~positive condition prop =
  match condition with
  | Exp.Var _ | Exp.Lvar _
   -> prune_ne tenv ~positive condition Exp.zero prop
  | Exp.Const Const.Cint i when IntLit.iszero i
   -> if positive then Propset.empty else Propset.singleton tenv prop
  | Exp.Const (Const.Cint _ | Const.Cstr _ | Const.Cclass _) | Exp.Sizeof _
   -> if positive then Propset.singleton tenv prop else Propset.empty
  | Exp.Const _
   -> assert false
  | Exp.Cast (_, condition')
   -> prune tenv ~positive condition' prop
  | Exp.UnOp (Unop.LNot, condition', _)
   -> prune tenv ~positive:(not positive) condition' prop
  | Exp.UnOp _
   -> assert false
  | Exp.BinOp (Binop.Eq, e, Exp.Const Const.Cint i) when IntLit.iszero i && not (IntLit.isnull i)
   -> prune tenv ~positive:(not positive) e prop
  | Exp.BinOp (Binop.Eq, Exp.Const Const.Cint i, e) when IntLit.iszero i && not (IntLit.isnull i)
   -> prune tenv ~positive:(not positive) e prop
  | Exp.BinOp (Binop.Eq, e1, e2)
   -> prune_ne tenv ~positive:(not positive) e1 e2 prop
  | Exp.BinOp (Binop.Ne, e, Exp.Const Const.Cint i) when IntLit.iszero i && not (IntLit.isnull i)
   -> prune tenv ~positive e prop
  | Exp.BinOp (Binop.Ne, Exp.Const Const.Cint i, e) when IntLit.iszero i && not (IntLit.isnull i)
   -> prune tenv ~positive e prop
  | Exp.BinOp (Binop.Ne, e1, e2)
   -> prune_ne tenv ~positive e1 e2 prop
  | Exp.BinOp (Binop.Ge, e2, e1) | Exp.BinOp (Binop.Le, e1, e2)
   -> prune_ineq tenv ~is_strict:false ~positive prop e1 e2
  | Exp.BinOp (Binop.Gt, e2, e1) | Exp.BinOp (Binop.Lt, e1, e2)
   -> prune_ineq tenv ~is_strict:true ~positive prop e1 e2
  | Exp.BinOp (Binop.LAnd, condition1, condition2)
   -> let pruner = if positive then prune_inter tenv else prune_union tenv in
      pruner ~positive condition1 condition2 prop
  | Exp.BinOp (Binop.LOr, condition1, condition2)
   -> let pruner = if positive then prune_union tenv else prune_inter tenv in
      pruner ~positive condition1 condition2 prop
  | Exp.BinOp _ | Exp.Lfield _ | Exp.Lindex _
   -> prune_ne tenv ~positive condition Exp.zero prop
  | Exp.Exn _
   -> assert false
  | Exp.Closure _
   -> assert false

and prune_inter tenv ~positive condition1 condition2 prop =
  let res = ref Propset.empty in
  let pset1 = prune tenv ~positive condition1 prop in
  let do_p p = res := Propset.union (prune tenv ~positive condition2 p) !res in
  Propset.iter do_p pset1 ; !res

and prune_union tenv ~positive condition1 condition2 prop =
  let pset1 = prune tenv ~positive condition1 prop in
  let pset2 = prune tenv ~positive condition2 prop in
  Propset.union pset1 pset2

let dangerous_functions =
  let dangerous_list = ["gets"] in
  ref (List.map ~f:Typ.Procname.from_string_c_fun dangerous_list)

let check_inherently_dangerous_function caller_pname callee_pname =
  if List.exists ~f:(Typ.Procname.equal callee_pname) !dangerous_functions then
    let exn =
      Exceptions.Inherently_dangerous_function
        (Localise.desc_inherently_dangerous_function callee_pname)
    in
    Reporting.log_warning_deprecated caller_pname exn

let call_should_be_skipped callee_summary =
  (* check skip flag *)
  Specs.get_flag callee_summary ProcAttributes.proc_flag_skip <> None
  (* skip abstract methods *)
  || callee_summary.Specs.attributes.ProcAttributes.is_abstract
  (* treat calls with no specs as skip functions in angelic mode *)
  || Config.angelic_execution && List.is_empty (Specs.get_specs_from_payload callee_summary)

(** In case of constant string dereference, return the result immediately *)
let check_constant_string_dereference lexp =
  let string_lookup s n =
    let c =
      try Char.to_int s.[IntLit.to_int n]
      with Invalid_argument _ -> 0
    in
    Exp.int (IntLit.of_int c)
  in
  match lexp with
  | Exp.BinOp (Binop.PlusPI, Exp.Const Const.Cstr s, e) | Exp.Lindex (Exp.Const Const.Cstr s, e)
   -> let value =
        match e with
        | Exp.Const Const.Cint n
          when IntLit.geq n IntLit.zero && IntLit.leq n (IntLit.of_int (String.length s))
         -> string_lookup s n
        | _
         -> Exp.get_undefined false
      in
      Some value
  | Exp.Const Const.Cstr s
   -> Some (string_lookup s IntLit.zero)
  | _
   -> None

(** Normalize an expression and check for arithmetic problems *)
let check_arith_norm_exp tenv pname exp prop =
  match Attribute.find_arithmetic_problem tenv (State.get_path_pos ()) prop exp with
  | Some Attribute.Div0 div, prop'
   -> let desc = Errdesc.explain_divide_by_zero tenv div (State.get_node ()) (State.get_loc ()) in
      let exn = Exceptions.Divide_by_zero (desc, __POS__) in
      Reporting.log_warning_deprecated pname exn ; (Prop.exp_normalize_prop tenv prop exp, prop')
  | Some Attribute.UminusUnsigned (e, typ), prop'
   -> let desc =
        Errdesc.explain_unary_minus_applied_to_unsigned_expression tenv e typ (State.get_node ())
          (State.get_loc ())
      in
      let exn = Exceptions.Unary_minus_applied_to_unsigned_expression (desc, __POS__) in
      Reporting.log_warning_deprecated pname exn ; (Prop.exp_normalize_prop tenv prop exp, prop')
  | None, prop'
   -> (Prop.exp_normalize_prop tenv prop exp, prop')

(** Check if [cond] is testing for NULL a pointer already dereferenced *)
let check_already_dereferenced tenv pname cond prop =
  let find_hpred lhs =
    List.find
      ~f:(function Sil.Hpointsto (e, _, _) -> Exp.equal e lhs | _ -> false)
      prop.Prop.sigma
  in
  let rec is_check_zero = function
    | Exp.Var id
     -> Some id
    | Exp.UnOp (Unop.LNot, e, _)
     -> is_check_zero e
    | Exp.BinOp ((Binop.Eq | Binop.Ne), Exp.Const Const.Cint i, Exp.Var id)
    | Exp.BinOp ((Binop.Eq | Binop.Ne), Exp.Var id, Exp.Const Const.Cint i)
      when IntLit.iszero i
     -> Some id
    | _
     -> None
  in
  let dereferenced_line =
    match is_check_zero cond with
    | Some id -> (
      match find_hpred (Prop.exp_normalize_prop tenv prop (Exp.Var id)) with
      | Some Sil.Hpointsto (_, se, _) -> (
        match Tabulation.find_dereference_without_null_check_in_sexp se with
        | Some n
         -> Some (id, n)
        | None
         -> None )
      | _
       -> None )
    | None
     -> None
  in
  match dereferenced_line with
  | Some (id, (n, _))
   -> let desc =
        Errdesc.explain_null_test_after_dereference tenv (Exp.Var id) (State.get_node ()) n
          (State.get_loc ())
      in
      let exn = Exceptions.Null_test_after_dereference (desc, __POS__) in
      Reporting.log_warning_deprecated pname exn
  | None
   -> ()

(** Check whether symbolic execution de-allocated a stack variable or a constant string,
    raising an exception in that case *)
let check_deallocate_static_memory prop_after =
  let check_deallocated_attribute = function
    | Sil.Apred (Aresource ({ra_kind= Rrelease} as ra), [(Lvar pv)])
      when Pvar.is_local pv || Pvar.is_global pv
     -> let freed_desc = Errdesc.explain_deallocate_stack_var pv ra in
        raise (Exceptions.Deallocate_stack_variable freed_desc)
    | Sil.Apred (Aresource ({ra_kind= Rrelease} as ra), [(Const Cstr s)])
     -> let freed_desc = Errdesc.explain_deallocate_constant_string s ra in
        raise (Exceptions.Deallocate_static_memory freed_desc)
    | _
     -> ()
  in
  let exp_att_list = Attribute.get_all prop_after in
  List.iter ~f:check_deallocated_attribute exp_att_list ; prop_after

let method_exists right_proc_name methods =
  if Config.curr_language_is Config.Java then
    List.exists ~f:(fun meth_name -> Typ.Procname.equal right_proc_name meth_name) methods
  else
    (* ObjC/C++ case : The attribute map will only exist when we have code for the method or
          the method has been called directly somewhere. It can still be that this is not the
          case but we have a model for the method. *)
    match AttributesTable.load_attributes ~cache:true right_proc_name with
    | Some attrs
     -> attrs.ProcAttributes.is_defined
    | None
     -> Specs.summary_exists_in_models right_proc_name

let resolve_method tenv class_name proc_name =
  let found_class =
    let visited = ref Typ.Name.Set.empty in
    let rec resolve (class_name: Typ.Name.t) =
      visited := Typ.Name.Set.add class_name !visited ;
      let right_proc_name = Typ.Procname.replace_class proc_name class_name in
      match Tenv.lookup tenv class_name with
      | Some {methods; supers} when Typ.Name.is_class class_name
       -> (
          if method_exists right_proc_name methods then Some right_proc_name
          else
            match supers with
            | super_classname :: _
             -> if not (Typ.Name.Set.mem super_classname !visited) then resolve super_classname
                else None
            | _
             -> None )
      | _
       -> None
    in
    resolve class_name
  in
  match found_class with
  | None
   -> Logging.d_strln ("Couldn't find method in the hierarchy of type " ^ Typ.Name.name class_name) ;
      proc_name
  | Some proc_name
   -> proc_name

let resolve_typename prop receiver_exp =
  let typexp_opt =
    let rec loop = function
      | []
       -> None
      | (Sil.Hpointsto (e, _, typexp)) :: _ when Exp.equal e receiver_exp
       -> Some typexp
      | _ :: hpreds
       -> loop hpreds
    in
    loop prop.Prop.sigma
  in
  match typexp_opt with Some Exp.Sizeof {typ= {desc= Tstruct name}} -> Some name | _ -> None

(** If the dynamic type of the receiver actual T_actual is a subtype of the reciever type T_formal
    in the signature of [pname], resolve [pname] to T_actual.[pname]. *)
let resolve_virtual_pname tenv prop actuals callee_pname call_flags : Typ.Procname.t list =
  let resolve receiver_exp pname prop =
    match resolve_typename prop receiver_exp with
    | Some class_name
     -> resolve_method tenv class_name pname
    | None
     -> pname
  in
  let get_receiver_typ pname fallback_typ =
    match pname with
    | Typ.Procname.Java pname_java
     -> (
        let name = Typ.Procname.java_get_class_type_name pname_java in
        match Tenv.lookup tenv name with
        | Some _
         -> Typ.mk (Typ.Tptr (Typ.mk (Tstruct name), Pk_pointer))
        | None
         -> fallback_typ )
    | _
     -> fallback_typ
  in
  let receiver_types_equal pname actual_receiver_typ =
    (* the type of the receiver according to the function signature *)
    let formal_receiver_typ = get_receiver_typ pname actual_receiver_typ in
    Typ.equal formal_receiver_typ actual_receiver_typ
  in
  let do_resolve called_pname receiver_exp actual_receiver_typ =
    if receiver_types_equal called_pname actual_receiver_typ then
      resolve receiver_exp called_pname prop
    else called_pname
  in
  match actuals with
  | _ when not (call_flags.CallFlags.cf_virtual || call_flags.CallFlags.cf_interface)
   -> (* if this is not a virtual or interface call, there's no need for resolution *)
      [callee_pname]
  | (receiver_exp, actual_receiver_typ) :: _
   -> (
      if !Config.curr_language <> Config.Java then
        (* default mode for Obj-C/C++/Java virtual calls: resolution only *)
        [do_resolve callee_pname receiver_exp actual_receiver_typ]
      else if Config.dynamic_dispatch = `Sound then
        let targets =
          if call_flags.CallFlags.cf_virtual then
            (* virtual call--either [called_pname] or an override in some subtype may be called *)
            callee_pname :: call_flags.CallFlags.cf_targets
          else
            (* interface call--[called_pname] has no implementation), we don't want to consider *)
            call_flags.CallFlags.cf_targets
          (* interface call, don't want to consider *)
        in
        (* return true if (receiver typ of [target_pname]) <: [actual_receiver_typ] *)
        let may_dispatch_to target_pname =
          let target_receiver_typ = get_receiver_typ target_pname actual_receiver_typ in
          Prover.Subtyping_check.check_subtype tenv target_receiver_typ actual_receiver_typ
        in
        let resolved_pname = do_resolve callee_pname receiver_exp actual_receiver_typ in
        let feasible_targets = List.filter ~f:may_dispatch_to targets in
        (* make sure [resolved_pname] is not a duplicate *)
        if List.mem ~equal:Typ.Procname.equal feasible_targets resolved_pname then feasible_targets
        else resolved_pname :: feasible_targets
      else
        let resolved_target = do_resolve callee_pname receiver_exp actual_receiver_typ in
        match call_flags.CallFlags.cf_targets with
        | target :: _
          when call_flags.CallFlags.cf_interface
               && receiver_types_equal callee_pname actual_receiver_typ
               && Typ.Procname.equal resolved_target callee_pname
         -> (* "production mode" of dynamic dispatch for Java: unsound, but faster. the handling
                 is restricted to interfaces: if we can't resolve an interface call, we pick the
                 first implementation of the interface and call it *)
            [target]
        | _
         -> (* default mode for Java virtual calls: resolution only *)
            [resolved_target] )
  | _
   -> failwith "A virtual call must have a receiver"

(** Resolve the name of the procedure to call based on the type of the arguments *)
let resolve_java_pname tenv prop args pname_java call_flags : Typ.Procname.java =
  let resolve_from_args resolved_pname_java args =
    let parameters = Typ.Procname.java_get_parameters resolved_pname_java in
    if List.length args <> List.length parameters then resolved_pname_java
    else
      let resolved_params =
        List.fold2_exn
          ~f:(fun accu (arg_exp, _) name ->
            match resolve_typename prop arg_exp with
            | Some class_name
             -> Typ.Procname.split_classname (Typ.Name.name class_name) :: accu
            | None
             -> name :: accu)
          ~init:[] args (Typ.Procname.java_get_parameters resolved_pname_java)
        |> List.rev
      in
      Typ.Procname.java_replace_parameters resolved_pname_java resolved_params
  in
  let resolved_pname_java, other_args =
    match args with
    | []
     -> (pname_java, [])
    | (first_arg, _) :: other_args when call_flags.CallFlags.cf_virtual
     -> let resolved =
          match resolve_typename prop first_arg with
          | Some class_name -> (
            match resolve_method tenv class_name (Typ.Procname.Java pname_java) with
            | Typ.Procname.Java resolved_pname_java
             -> resolved_pname_java
            | _
             -> pname_java )
          | None
           -> pname_java
        in
        (resolved, other_args)
    | _ :: other_args when Typ.Procname.is_constructor (Typ.Procname.Java pname_java)
     -> (pname_java, other_args)
    | args
     -> (pname_java, args)
  in
  resolve_from_args resolved_pname_java other_args

(** Resolve the procedure name and run the analysis of the resolved procedure
    if not already analyzed *)
let resolve_and_analyze tenv caller_pdesc prop args callee_proc_name call_flags
    : Typ.Procname.t * Specs.summary option =
  (* TODO (#15748878): Fix conflict with method overloading by encoding in the procedure name
     whether the method is defined or generated by the specialization *)
  let analyze_ondemand resolved_pname : Specs.summary option =
    if Typ.Procname.equal resolved_pname callee_proc_name then
      Ondemand.analyze_proc_name caller_pdesc callee_proc_name
    else
      (* Create the type sprecialized procedure description and analyze it directly *)
      let analyze specialized_pdesc = Ondemand.analyze_proc_desc caller_pdesc specialized_pdesc in
      let resolved_proc_desc_option =
        match Ondemand.get_proc_desc resolved_pname with
        | Some resolved_proc_desc
         -> Some resolved_proc_desc
        | None
         -> Option.map
              ~f:(fun callee_proc_desc ->
                Cfg.specialize_types callee_proc_desc resolved_pname args)
              (Ondemand.get_proc_desc callee_proc_name)
      in
      Option.bind resolved_proc_desc_option ~f:analyze
  in
  let resolved_pname =
    match callee_proc_name with
    | Typ.Procname.Java callee_proc_name_java
     -> Typ.Procname.Java (resolve_java_pname tenv prop args callee_proc_name_java call_flags)
    | _
     -> callee_proc_name
  in
  (resolved_pname, analyze_ondemand resolved_pname)

(** recognize calls to the constructor java.net.URL and splits the argument string
    to be only the protocol.  *)
let call_constructor_url_update_args pname actual_params =
  let url_pname =
    Typ.Procname.Java
      (Typ.Procname.java (Typ.Name.Java.from_string "java.net.URL") None "<init>"
         [(Some "java.lang", "String")] Typ.Procname.Non_Static)
  in
  if Typ.Procname.equal url_pname pname then
    match actual_params with
    | [this; (Exp.Const Const.Cstr s, atype)]
     -> (
        let parts = Str.split (Str.regexp_string "://") s in
        match parts with
        | frst :: _
         -> if String.equal frst "http" || String.equal frst "ftp" || String.equal frst "https"
               || String.equal frst "mailto" || String.equal frst "jar"
            then [this; (Exp.Const (Const.Cstr frst), atype)]
            else actual_params
        | _
         -> actual_params )
    | [this; (_, atype)]
     -> [this; (Exp.Const (Const.Cstr "file"), atype)]
    | _
     -> actual_params
  else actual_params

let receiver_self receiver prop =
  List.exists
    ~f:(fun hpred ->
      match hpred with
      | Sil.Hpointsto (Exp.Lvar pv, Sil.Eexp (e, _), _)
       -> Exp.equal e receiver && Pvar.is_seed pv
          && Mangled.equal (Pvar.get_name pv) (Mangled.from_string "self")
      | _
       -> false)
    prop.Prop.sigma

(* When current ObjC method is an initializer and the method call is also an initializer,
   and the receiver is self, i.e. the call is [super init], then we want to assume that it
   can return null, regardless of code or annotations, so that the next statement should be
   a check for null, which is considered good practice.  *)
let force_objc_init_return_nil pdesc callee_pname tenv ret_id pre path receiver =
  let current_pname = Procdesc.get_proc_name pdesc in
  if Typ.Procname.is_constructor callee_pname && receiver_self receiver pre && !Config.footprint
     && Typ.Procname.is_constructor current_pname
  then
    match ret_id with
    | Some (ret_id, _)
     -> let propset = prune_ne tenv ~positive:false (Exp.Var ret_id) Exp.zero pre in
        if Propset.is_empty propset then []
        else
          let prop = List.hd_exn (Propset.to_proplist propset) in
          [(prop, path)]
    | _
     -> []
  else []

(* This method is used to handle the special semantics of ObjC instance method calls. *)
(* res = [obj foo] *)
(*  1. We know that obj is null, then we return null *)
(*  2. We don't know, but obj could be null, we return both options, *)
(* (obj = null, res = null), (obj != null, res = [obj foo]) *)
(*  We want the same behavior even when we are going to skip the function. *)
let handle_objc_instance_method_call_or_skip pdesc tenv actual_pars path callee_pname pre ret_id
    res =
  let path_description =
    "Message " ^ Typ.Procname.to_simplified_string callee_pname ^ " with receiver nil returns nil."
  in
  let receiver =
    match actual_pars with
    | (e, _) :: _
     -> e
    | _
     -> raise
          (Exceptions.Internal_error
             (Localise.verbatim_desc
                "In Objective-C instance method call there should be a receiver."))
  in
  let is_receiver_null =
    match actual_pars with
    | (e, _) :: _ when Exp.equal e Exp.zero || Option.is_some (Attribute.get_objc_null tenv pre e)
     -> true
    | _
     -> false
  in
  let add_objc_null_attribute_or_nullify_result prop =
    match ret_id with
    | Some (ret_id, _) -> (
      match Attribute.find_equal_formal_path tenv receiver prop with
      | Some vfs
       -> Attribute.add_or_replace tenv prop (Apred (Aobjc_null, [Exp.Var ret_id; vfs]))
      | None
       -> Prop.conjoin_eq tenv (Exp.Var ret_id) Exp.zero prop )
    | _
     -> prop
  in
  if is_receiver_null then
    (* objective-c instance method with a null receiver just return objc_null(res). *)
    let path = Paths.Path.add_description path path_description in
    L.d_strln
      ( "Object-C method " ^ Typ.Procname.to_string callee_pname
      ^ " called with nil receiver. Returning 0/nil" ) ;
    (* We wish to nullify the result. However, in some cases,
       we want to add the attribute OBJC_NULL to it so that we *)
    (* can keep track of how this object became null,
       so that in a NPE we can separate it into a different error type *)
    [(add_objc_null_attribute_or_nullify_result pre, path)]
  else
    match force_objc_init_return_nil pdesc callee_pname tenv ret_id pre path receiver with
    | []
     -> if !Config.footprint && Option.is_none (Attribute.get_undef tenv pre receiver)
           && not (Rearrange.is_only_pt_by_fld_or_param_nonnull pdesc tenv pre receiver)
        then
          let res_null =
            (* returns: (objc_null(res) /\ receiver=0) or an empty list of results *)
            let pre_with_attr_or_null = add_objc_null_attribute_or_nullify_result pre in
            let propset = prune_ne tenv ~positive:false receiver Exp.zero pre_with_attr_or_null in
            if Propset.is_empty propset then []
            else
              let prop = List.hd_exn (Propset.to_proplist propset) in
              let path = Paths.Path.add_description path path_description in
              [(prop, path)]
          in
          List.append res_null (res ())
        else res ()
        (* Not known if receiver = 0 and not footprint. Standard tabulation *)
    | res_null
     -> List.append res_null (res ())

(* This method handles ObjC instance method calls, in particular the fact that calling a method *)
(* with nil returns nil. The exec_call function is either standard call execution or execution *)
(* of ObjC getters and setters using a builtin. *)
let handle_objc_instance_method_call actual_pars actual_params pre tenv ret_id pdesc callee_pname
    loc path exec_call =
  let res () = exec_call tenv ret_id pdesc callee_pname loc actual_params pre path in
  handle_objc_instance_method_call_or_skip pdesc tenv actual_pars path callee_pname pre ret_id res

let normalize_params tenv pdesc prop actual_params =
  let norm_arg (p, args) (e, t) =
    let e', p' = check_arith_norm_exp tenv pdesc e p in
    (p', (e', t) :: args)
  in
  let prop, args = List.fold ~f:norm_arg ~init:(prop, []) actual_params in
  (prop, List.rev args)

let add_strexp_to_footprint tenv strexp abduced_pv typ prop =
  let abduced_lvar = Exp.Lvar abduced_pv in
  let lvar_pt_fpvar =
    let sizeof_exp =
      Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes}
    in
    Prop.mk_ptsto tenv abduced_lvar strexp sizeof_exp
  in
  let sigma_fp = prop.Prop.sigma_fp in
  Prop.normalize tenv (Prop.set prop ~sigma_fp:(lvar_pt_fpvar :: sigma_fp))

let add_to_footprint tenv abduced_pv typ prop =
  let fresh_fp_var = Exp.Var (Ident.create_fresh Ident.kfootprint) in
  let prop' =
    add_strexp_to_footprint tenv (Sil.Eexp (fresh_fp_var, Sil.Inone)) abduced_pv typ prop
  in
  (prop', fresh_fp_var)

(* the current abduction mechanism treats struct values differently than all other types. abduction
   on struct values adds a a struct whose fields are initialized to fresh footprint vars to the
   footprint. regular abduction just adds a fresh footprint value of the correct type to the
   footprint. we can get rid of this special case if we fix the abduction on struct values *)
let add_struct_value_to_footprint tenv abduced_pv typ prop =
  let struct_strexp = Prop.create_strexp_of_type tenv Prop.Fld_init typ None Sil.inst_none in
  let prop' = add_strexp_to_footprint tenv struct_strexp abduced_pv typ prop in
  (prop', struct_strexp)

let add_constraints_on_retval tenv pdesc prop ret_exp ~has_nullable_annot typ callee_pname
    callee_loc =
  if Typ.Procname.is_infer_undefined callee_pname then prop
  else
    let is_rec_call pname =
      (* TODO: (t7147096) extend this to detect mutual recursion *)
      Typ.Procname.equal pname (Procdesc.get_proc_name pdesc)
    in
    let lookup_abduced_expression p abduced_ret_pv =
      List.find_map
        ~f:(fun hpred ->
          match hpred with
          | Sil.Hpointsto (Exp.Lvar pv, _, exp) when Pvar.equal pv abduced_ret_pv
           -> Some exp
          | _
           -> None)
        p.Prop.sigma_fp
    in
    (* find an hpred [abduced] |-> A in [prop] and add [exp] = A to prop *)
    let bind_exp_to_abduced_val exp_to_bind abduced prop =
      let bind_exp prop = function
        | Sil.Hpointsto (Exp.Lvar pv, Sil.Eexp (rhs, _), _) when Pvar.equal pv abduced
         -> Prop.conjoin_eq tenv exp_to_bind rhs prop
        | _
         -> prop
      in
      List.fold ~f:bind_exp ~init:prop prop.Prop.sigma
    in
    (* To avoid obvious false positives, assume skip functions do not return null pointers *)
    let add_ret_non_null exp typ prop =
      if has_nullable_annot then prop
        (* don't assume nonnull if the procedure is annotated with @Nullable *)
      else
        match typ.Typ.desc with Typ.Tptr _ -> Prop.conjoin_neq tenv exp Exp.zero prop | _ -> prop
    in
    if Config.angelic_execution && not (is_rec_call callee_pname) then
      (* introduce a fresh program variable to allow abduction on the return value *)
      let prop_with_abduced_var =
        let abduced_ret_pv =
          (* in Java, always re-use the same abduced ret var to prevent false alarms with repeated method calls *)
          let loc = if Typ.Procname.is_java callee_pname then Location.dummy else callee_loc in
          Pvar.mk_abduced_ret callee_pname loc
        in
        if !Config.footprint then
          match lookup_abduced_expression prop abduced_ret_pv with
          | None
           -> let p, fp_var = add_to_footprint tenv abduced_ret_pv typ prop in
              Prop.conjoin_eq tenv ~footprint:true ret_exp fp_var p
          | Some exp
           -> Prop.conjoin_eq tenv ~footprint:true ret_exp exp prop
        else
          (* bind return id to the abduced value pointed to by the pvar we introduced *)
          bind_exp_to_abduced_val ret_exp abduced_ret_pv prop
      in
      add_ret_non_null ret_exp typ prop_with_abduced_var
    else add_ret_non_null ret_exp typ prop

let execute_load ?(report_deref_errors= true) pname pdesc tenv id rhs_exp typ loc prop_ =
  let execute_load_ pdesc tenv id loc acc_in iter =
    let iter_ren = Prop.prop_iter_make_id_primed tenv id iter in
    let prop_ren = Prop.prop_iter_to_prop tenv iter_ren in
    match Prop.prop_iter_current tenv iter_ren with
    | Sil.Hpointsto (lexp, strexp, Exp.Sizeof sizeof_data), offlist
     -> (
        let contents, new_ptsto, pred_insts_op, lookup_uninitialized =
          ptsto_lookup pdesc tenv prop_ren (lexp, strexp, sizeof_data) offlist id
        in
        let update acc (pi, sigma) =
          let pi' = Sil.Aeq (Exp.Var id, contents) :: pi in
          let sigma' = new_ptsto :: sigma in
          let iter' = update_iter iter_ren pi' sigma' in
          let prop' = Prop.prop_iter_to_prop tenv iter' in
          let prop'' =
            if lookup_uninitialized then
              Attribute.add_or_replace tenv prop' (Apred (Adangling DAuninit, [Exp.Var id]))
            else prop'
          in
          prop'' :: acc
        in
        match pred_insts_op with
        | None
         -> update acc_in ([], [])
        | Some pred_insts
         -> List.rev (List.fold ~f:update ~init:acc_in pred_insts) )
    | Sil.Hpointsto _, _
     -> Errdesc.warning_err loc "no offset access in execute_load -- treating as skip@." ;
        Prop.prop_iter_to_prop tenv iter_ren :: acc_in
    | _
     -> (* The implementation of this case means that we
           ignore this dereferencing operator. When the analyzer treats
           numerical information and arrays more precisely later, we
           should change the implementation here. *)
        assert false
  in
  try
    let n_rhs_exp, prop = check_arith_norm_exp tenv pname rhs_exp prop_ in
    let n_rhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_rhs_exp in
    match check_constant_string_dereference n_rhs_exp' with
    | Some value
     -> [Prop.conjoin_eq tenv (Exp.Var id) value prop]
    | None ->
      try
        let iter_list =
          Rearrange.rearrange ~report_deref_errors pdesc tenv n_rhs_exp' typ prop loc
        in
        List.rev (List.fold ~f:(execute_load_ pdesc tenv id loc) ~init:[] iter_list)
      with Exceptions.Symexec_memory_error _ ->
        (* This should normally be a real alarm and should not be caught but currently happens
           when the normalization drops hpreds of the form ident |-> footprint var. *)
        let undef = Exp.get_undefined !Config.footprint in
        [Prop.conjoin_eq tenv (Exp.Var id) undef prop]
  with Rearrange.ARRAY_ACCESS ->
    if Int.equal Config.array_level 0 then assert false
    else
      let undef = Exp.get_undefined false in
      [Prop.conjoin_eq tenv (Exp.Var id) undef prop_]

let load_ret_annots pname =
  match AttributesTable.load_attributes ~cache:true pname with
  | Some attrs
   -> let ret_annots, _ = attrs.ProcAttributes.method_annotation in
      ret_annots
  | None
   -> Annot.Item.empty

let execute_store ?(report_deref_errors= true) pname pdesc tenv lhs_exp typ rhs_exp loc prop_ =
  let execute_store_ pdesc tenv rhs_exp acc_in iter =
    let lexp, strexp, sizeof, offlist =
      match Prop.prop_iter_current tenv iter with
      | Sil.Hpointsto (lexp, strexp, Exp.Sizeof sizeof), offlist
       -> (lexp, strexp, sizeof, offlist)
      | _
       -> assert false
    in
    let p = Prop.prop_iter_to_prop tenv iter in
    let new_ptsto, pred_insts_op =
      ptsto_update pdesc tenv p (lexp, strexp, sizeof) offlist rhs_exp
    in
    let update acc (pi, sigma) =
      let sigma' = new_ptsto :: sigma in
      let iter' = update_iter iter pi sigma' in
      let prop' = Prop.prop_iter_to_prop tenv iter' in
      prop' :: acc
    in
    match pred_insts_op with
    | None
     -> update acc_in ([], [])
    | Some pred_insts
     -> List.fold ~f:update ~init:acc_in pred_insts
  in
  try
    let n_lhs_exp, prop_' = check_arith_norm_exp tenv pname lhs_exp prop_ in
    let n_rhs_exp, prop = check_arith_norm_exp tenv pname rhs_exp prop_' in
    let prop = Attribute.replace_objc_null tenv prop n_lhs_exp n_rhs_exp in
    let n_lhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_lhs_exp in
    let iter_list = Rearrange.rearrange ~report_deref_errors pdesc tenv n_lhs_exp' typ prop loc in
    List.rev (List.fold ~f:(execute_store_ pdesc tenv n_rhs_exp) ~init:[] iter_list)
  with Rearrange.ARRAY_ACCESS -> if Int.equal Config.array_level 0 then assert false else [prop_]

(** Execute [instr] with a symbolic heap [prop].*)
let rec sym_exec tenv current_pdesc _instr (prop_: Prop.normal Prop.t) path
    : (Prop.normal Prop.t * Paths.Path.t) list =
  let current_pname = Procdesc.get_proc_name current_pdesc in
  State.set_instr _instr ;
  (* mark instruction last seen *)
  State.set_prop_tenv_pdesc prop_ tenv current_pdesc ;
  (* mark prop,tenv,pdesc last seen *)
  SymOp.pay () ;
  (* pay one symop *)
  let ret_old_path pl =
    (* return the old path unchanged *)
    List.map ~f:(fun p -> (p, path)) pl
  in
  let instr =
    match _instr with
    | Sil.Call (ret, exp, par, loc, call_flags)
     -> let exp' = Prop.exp_normalize_prop tenv prop_ exp in
        let instr' =
          match exp' with
          | Exp.Closure c
           -> let proc_exp = Exp.Const (Const.Cfun c.name) in
              let proc_exp' = Prop.exp_normalize_prop tenv prop_ proc_exp in
              let par' = List.map ~f:(fun (id_exp, _, typ) -> (id_exp, typ)) c.captured_vars in
              Sil.Call (ret, proc_exp', par' @ par, loc, call_flags)
          | _
           -> Sil.Call (ret, exp', par, loc, call_flags)
        in
        instr'
    | _
     -> _instr
  in
  let skip_call ?(is_objc_instance_method= false) prop path callee_pname ret_annots loc ret_id
      ret_typ_opt actual_args =
    let skip_res () =
      let exn = Exceptions.Skip_function (Localise.desc_skip_function callee_pname) in
      Reporting.log_info_deprecated current_pname exn ;
      L.d_strln
        ( "Undefined function " ^ Typ.Procname.to_string callee_pname
        ^ ", returning undefined value." ) ;
      ( match Specs.get_summary current_pname with
      | None
       -> ()
      | Some summary
       -> Specs.CallStats.trace summary.Specs.stats.Specs.call_stats callee_pname loc
            Specs.CallStats.CR_skip !Config.footprint ) ;
      unknown_or_scan_call ~is_scan:false ret_typ_opt ret_annots
        (Builtin.
          { pdesc= current_pdesc
          ; instr
          ; tenv
          ; prop_= prop
          ; path
          ; ret_id
          ; args= actual_args
          ; proc_name= callee_pname
          ; loc })
    in
    if is_objc_instance_method then
      handle_objc_instance_method_call_or_skip current_pdesc tenv actual_args path callee_pname
        prop ret_id skip_res
    else skip_res ()
  in
  let call_args prop_ proc_name args ret_id loc =
    {Builtin.pdesc= current_pdesc; instr; tenv; prop_; path; ret_id; args; proc_name; loc}
  in
  match instr with
  | Sil.Load (id, rhs_exp, typ, loc)
   -> execute_load current_pname current_pdesc tenv id rhs_exp typ loc prop_ |> ret_old_path
  | Sil.Store (lhs_exp, typ, rhs_exp, loc)
   -> execute_store current_pname current_pdesc tenv lhs_exp typ rhs_exp loc prop_ |> ret_old_path
  | Sil.Prune (cond, loc, true_branch, ik)
   -> let prop__ = Attribute.nullify_exp_with_objc_null tenv prop_ cond in
      let check_condition_always_true_false () =
        if !Config.curr_language <> Config.Clang || Config.report_condition_always_true_in_clang
        then
          let report_condition_always_true_false i =
            let skip_loop =
              match ik with
              | Sil.Ik_while | Sil.Ik_for
               -> not (IntLit.iszero i) (* skip wile(1) and for (;1;) *)
              | Sil.Ik_dowhile
               -> true (* skip do..while *)
              | Sil.Ik_land_lor
               -> true (* skip subpart of a condition obtained from compilation of && and || *)
              | _
               -> false
            in
            true_branch && not skip_loop
          in
          match Prop.exp_normalize_prop tenv Prop.prop_emp cond with
          | Exp.Const Const.Cint i when report_condition_always_true_false i
           -> let node = State.get_node () in
              let desc = Errdesc.explain_condition_always_true_false tenv i cond node loc in
              let exn =
                Exceptions.Condition_always_true_false (desc, not (IntLit.iszero i), __POS__)
              in
              Reporting.log_warning_deprecated current_pname exn
          | _
           -> ()
      in
      if not Config.tracing then check_already_dereferenced tenv current_pname cond prop__ ;
      check_condition_always_true_false () ;
      let n_cond, prop = check_arith_norm_exp tenv current_pname cond prop__ in
      ret_old_path (Propset.to_proplist (prune tenv ~positive:true n_cond prop))
  | Sil.Call (ret_id, Exp.Const Const.Cfun callee_pname, actual_params, loc, call_flags) -> (
    match Builtin.get callee_pname with
    | Some exec_builtin
     -> exec_builtin (call_args prop_ callee_pname actual_params ret_id loc)
    | None ->
      match callee_pname with
      | Java callee_pname_java when Config.dynamic_dispatch = `Lazy
       -> (
          let norm_prop, norm_args' = normalize_params tenv current_pname prop_ actual_params in
          let norm_args = call_constructor_url_update_args callee_pname norm_args' in
          let exec_skip_call skipped_pname ret_annots ret_type =
            skip_call norm_prop path skipped_pname ret_annots loc ret_id (Some ret_type) norm_args
          in
          let resolved_pname, resolved_summary_opt =
            resolve_and_analyze tenv current_pdesc norm_prop norm_args callee_pname call_flags
          in
          match resolved_summary_opt with
          | None
           -> let ret_typ = Typ.java_proc_return_typ callee_pname_java in
              let ret_annots = load_ret_annots callee_pname in
              exec_skip_call resolved_pname ret_annots ret_typ
          | Some resolved_summary when call_should_be_skipped resolved_summary
           -> let proc_attrs = resolved_summary.Specs.attributes in
              let ret_annots, _ = proc_attrs.ProcAttributes.method_annotation in
              exec_skip_call resolved_pname ret_annots proc_attrs.ProcAttributes.ret_type
          | Some resolved_summary
           -> proc_call resolved_summary (call_args prop_ callee_pname norm_args ret_id loc) )
      | Java callee_pname_java
       -> let norm_prop, norm_args = normalize_params tenv current_pname prop_ actual_params in
          let url_handled_args = call_constructor_url_update_args callee_pname norm_args in
          let resolved_pnames =
            resolve_virtual_pname tenv norm_prop url_handled_args callee_pname call_flags
          in
          let exec_one_pname pname =
            let exec_skip_call ret_annots ret_type =
              skip_call norm_prop path pname ret_annots loc ret_id (Some ret_type) url_handled_args
            in
            match Ondemand.analyze_proc_name current_pdesc pname with
            | None
             -> let ret_typ = Typ.java_proc_return_typ callee_pname_java in
                let ret_annots = load_ret_annots callee_pname in
                exec_skip_call ret_annots ret_typ
            | Some callee_summary when call_should_be_skipped callee_summary
             -> let proc_attrs = callee_summary.Specs.attributes in
                let ret_annots, _ = proc_attrs.ProcAttributes.method_annotation in
                exec_skip_call ret_annots proc_attrs.ProcAttributes.ret_type
            | Some callee_summary
             -> let handled_args = call_args norm_prop pname url_handled_args ret_id loc in
                proc_call callee_summary handled_args
          in
          List.fold ~f:(fun acc pname -> exec_one_pname pname @ acc) ~init:[] resolved_pnames
      | _
       -> (* Generic fun call with known name *)
          let prop_r, n_actual_params = normalize_params tenv current_pname prop_ actual_params in
          let resolved_pname =
            match resolve_virtual_pname tenv prop_r n_actual_params callee_pname call_flags with
            | resolved_pname :: _
             -> resolved_pname
            | []
             -> callee_pname
          in
          let resolved_summary_opt = Ondemand.analyze_proc_name current_pdesc resolved_pname in
          let callee_pdesc_opt = Ondemand.get_proc_desc resolved_pname in
          let ret_typ_opt = Option.map ~f:Procdesc.get_ret_type callee_pdesc_opt in
          let sentinel_result =
            if Config.curr_language_is Config.Clang then
              check_variadic_sentinel_if_present
                (call_args prop_r callee_pname actual_params ret_id loc)
            else [(prop_r, path)]
          in
          let do_call (prop, path) =
            if Option.value_map ~f:call_should_be_skipped ~default:true resolved_summary_opt then
              (* If it's an ObjC getter or setter, call the builtin rather than skipping *)
              let attrs_opt =
                let attr_opt = Option.map ~f:Procdesc.get_attributes callee_pdesc_opt in
                match (attr_opt, resolved_pname) with
                | Some attrs, Typ.Procname.ObjC_Cpp _
                 -> Some attrs
                | None, Typ.Procname.ObjC_Cpp _
                 -> AttributesTable.load_attributes ~cache:true resolved_pname
                | _
                 -> None
              in
              let objc_property_accessor_ret_typ_opt =
                match attrs_opt with
                | Some attrs -> (
                  match attrs.ProcAttributes.objc_accessor with
                  | Some objc_accessor
                   -> Some (objc_accessor, attrs.ProcAttributes.ret_type)
                  | None
                   -> None )
                | None
                 -> None
              in
              match objc_property_accessor_ret_typ_opt with
              | Some (objc_property_accessor, ret_typ)
               -> handle_objc_instance_method_call n_actual_params n_actual_params prop tenv ret_id
                    current_pdesc callee_pname loc path
                    (sym_exec_objc_accessor objc_property_accessor ret_typ)
              | None
               -> let ret_annots =
                    match resolved_summary_opt with
                    | Some summ
                     -> let ret_annots, _ =
                          summ.Specs.attributes.ProcAttributes.method_annotation
                        in
                        ret_annots
                    | None
                     -> load_ret_annots resolved_pname
                  in
                  let is_objc_instance_method =
                    match attrs_opt with
                    | Some attrs
                     -> attrs.ProcAttributes.is_objc_instance_method
                    | None
                     -> false
                  in
                  skip_call ~is_objc_instance_method prop path resolved_pname ret_annots loc ret_id
                    ret_typ_opt n_actual_params
            else
              proc_call (Option.value_exn resolved_summary_opt)
                (call_args prop resolved_pname n_actual_params ret_id loc)
          in
          List.concat_map ~f:do_call sentinel_result )
  | Sil.Call (ret_id, fun_exp, actual_params, loc, call_flags)
   -> (* Call via function pointer *)
      let prop_r, n_actual_params = normalize_params tenv current_pname prop_ actual_params in
      if call_flags.CallFlags.cf_is_objc_block
         && not (Rearrange.is_only_pt_by_fld_or_param_nonnull current_pdesc tenv prop_r fun_exp)
      then Rearrange.check_call_to_objc_block_error tenv current_pdesc prop_r fun_exp loc ;
      Rearrange.check_dereference_error tenv current_pdesc prop_r fun_exp loc ;
      if call_flags.CallFlags.cf_noreturn then (
        L.d_str "Unknown function pointer with noreturn attribute " ;
        Sil.d_exp fun_exp ;
        L.d_strln ", diverging." ;
        diverge prop_r path )
      else (
        L.d_str "Unknown function pointer " ;
        Sil.d_exp fun_exp ;
        L.d_strln ", returning undefined value." ;
        let callee_pname = Typ.Procname.from_string_c_fun "__function_pointer__" in
        unknown_or_scan_call ~is_scan:false None Annot.Item.empty
          (Builtin.
            { pdesc= current_pdesc
            ; instr
            ; tenv
            ; prop_= prop_r
            ; path
            ; ret_id
            ; args= n_actual_params
            ; proc_name= callee_pname
            ; loc }) )
  | Sil.Nullify (pvar, _)
   -> (
      let eprop = Prop.expose prop_ in
      match
        List.partition_tf
          ~f:(function
              | Sil.Hpointsto (Exp.Lvar pvar', _, _) -> Pvar.equal pvar pvar' | _ -> false)
          eprop.Prop.sigma
      with
      | [(Sil.Hpointsto (e, se, typ))], sigma'
       -> let sigma'' =
            let se' = execute_nullify_se se in
            Sil.Hpointsto (e, se', typ) :: sigma'
          in
          let eprop_res = Prop.set eprop ~sigma:sigma'' in
          ret_old_path [Prop.normalize tenv eprop_res]
      | [], _
       -> ret_old_path [prop_]
      | _
       -> L.internal_error "Pvar %a appears on the LHS of >1 heap predicate!@." (Pvar.pp Pp.text)
            pvar ;
          assert false )
  | Sil.Abstract _
   -> let node = State.get_node () in
      let blocks_nullified = get_blocks_nullified node in
      List.iter ~f:(check_block_retain_cycle tenv current_pname prop_) blocks_nullified ;
      if Prover.check_inconsistency tenv prop_ then ret_old_path []
      else
        ret_old_path
          [ Abs.remove_redundant_array_elements current_pname tenv
              (Abs.abstract current_pname tenv prop_) ]
  | Sil.Remove_temps (temps, _)
   -> ret_old_path [Prop.exist_quantify tenv (Sil.fav_from_list temps) prop_]
  | Sil.Declare_locals (ptl, _)
   -> let sigma_locals =
        let add_None (x, typ) =
          (x, Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}, None)
        in
        let sigma_locals () =
          List.map ~f:(Prop.mk_ptsto_lvar tenv Prop.Fld_init Sil.inst_initial)
            (List.map ~f:add_None ptl)
        in
        Config.run_in_re_execution_mode (* no footprint vars for locals *)
                                        sigma_locals ()
      in
      let sigma' = prop_.Prop.sigma @ sigma_locals in
      let prop' = Prop.normalize tenv (Prop.set prop_ ~sigma:sigma') in
      ret_old_path [prop']

and diverge prop path =
  State.add_diverging_states (Paths.PathSet.from_renamed_list [(prop, path)]) ;
  (* diverge *)
  []

(** Symbolic execution of a sequence of instructions.
    If errors occur and [mask_errors] is true, just treat as skip. *)
and instrs ?(mask_errors= false) tenv pdesc instrs ppl =
  let exe_instr instr (p, path) =
    L.d_str "Executing Generated Instruction " ;
    Sil.d_instr instr ;
    L.d_ln () ;
    try sym_exec tenv pdesc instr p path
    with exn when SymOp.exn_not_failure exn && mask_errors ->
      let err_name, _, ml_source, _, _, _, _ = Exceptions.recognize_exception exn in
      let loc =
        match ml_source with Some ml_loc -> "at " ^ L.ml_loc_to_string ml_loc | None -> ""
      in
      L.d_warning ("Generated Instruction Failed with: " ^ Localise.to_issue_id err_name ^ loc) ;
      L.d_ln () ;
      [(p, path)]
  in
  let f plist instr = List.concat_map ~f:(exe_instr instr) plist in
  List.fold ~f ~init:ppl instrs

and add_constraints_on_actuals_by_ref tenv prop actuals_by_ref callee_pname callee_loc =
  (* replace an hpred of the form actual_var |-> _ with new_hpred in prop *)
  let replace_actual_hpred actual_var new_hpred prop =
    let sigma' =
      List.map
        ~f:(function
            | Sil.Hpointsto (lhs, _, _) when Exp.equal lhs actual_var -> new_hpred | hpred -> hpred)
        prop.Prop.sigma
    in
    Prop.normalize tenv (Prop.set prop ~sigma:sigma')
  in
  let add_actual_by_ref_to_footprint prop (actual, actual_typ, actual_index) =
    let abduced =
      match actual with
      | Exp.Lvar _ | Exp.Var _
       -> Pvar.mk_abduced_ref_param callee_pname actual_index callee_loc
      | _
       -> failwithf "Unexpected variable expression %a" Exp.pp actual
    in
    let already_has_abduced_retval p =
      List.exists
        ~f:(fun hpred ->
          match hpred with
          | Sil.Hpointsto (Exp.Lvar pv, _, _)
           -> Pvar.equal pv abduced
          | _
           -> false)
        p.Prop.sigma_fp
    in
    (* prevent introducing multiple abduced retvals for a single call site in a loop *)
    if already_has_abduced_retval prop then prop
    else if !Config.footprint then
      let prop', abduced_strexp =
        match actual_typ.Typ.desc with
        | Typ.Tptr (({desc= Tstruct _} as typ), _)
         -> (* for struct types passed by reference, do abduction on the fields of the
               struct *)
            add_struct_value_to_footprint tenv abduced typ prop
        | Typ.Tptr (typ, _)
         -> (* for pointer types passed by reference, do abduction directly on the pointer *)
            let prop', fresh_fp_var = add_to_footprint tenv abduced typ prop in
            (prop', Sil.Eexp (fresh_fp_var, Sil.Inone))
        | _
         -> failwith ("No need for abduction on non-pointer type " ^ Typ.to_string actual_typ)
      in
      let filtered_sigma =
        List.map
          ~f:(function
              | Sil.Hpointsto (lhs, _, typ_exp) when Exp.equal lhs actual
               -> Sil.Hpointsto (lhs, abduced_strexp, typ_exp)
              | hpred
               -> hpred)
          prop'.Prop.sigma
      in
      Prop.normalize tenv (Prop.set prop' ~sigma:filtered_sigma)
    else
      (* bind actual passed by ref to the abduced value pointed to by the synthetic pvar *)
      let prop' =
        let filtered_sigma =
          List.filter
            ~f:(function
                | Sil.Hpointsto (lhs, _, _) when Exp.equal lhs actual -> false | _ -> true)
            prop.Prop.sigma
        in
        Prop.normalize tenv (Prop.set prop ~sigma:filtered_sigma)
      in
      List.fold
        ~f:(fun p hpred ->
          match hpred with
          | Sil.Hpointsto (Exp.Lvar pv, rhs, texp) when Pvar.equal pv abduced
           -> let new_hpred = Sil.Hpointsto (actual, rhs, texp) in
              Prop.normalize tenv (Prop.set p ~sigma:(new_hpred :: prop'.Prop.sigma))
          | _
           -> p)
        ~init:prop' prop'.Prop.sigma
  in
  (* non-angelic mode; havoc each var passed by reference by assigning it to a fresh id *)
  let havoc_actual_by_ref prop (actual, actual_typ, _) =
    let actual_pt_havocd_var =
      let havocd_var = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let sizeof_exp =
        Exp.Sizeof
          { typ= Typ.strip_ptr actual_typ
          ; nbytes= None
          ; dynamic_length= None
          ; subtype= Subtype.subtypes }
      in
      Prop.mk_ptsto tenv actual (Sil.Eexp (havocd_var, Sil.Inone)) sizeof_exp
    in
    replace_actual_hpred actual actual_pt_havocd_var prop
  in
  let do_actual_by_ref =
    if Config.angelic_execution then add_actual_by_ref_to_footprint else havoc_actual_by_ref
  in
  let non_const_actuals_by_ref =
    let is_not_const (e, _, i) =
      match AttributesTable.load_attributes ~cache:true callee_pname with
      | Some attrs
       -> let is_const = List.mem ~equal:Int.equal attrs.ProcAttributes.const_formals i in
          if is_const then (
            L.d_str (Printf.sprintf "Not havocing const argument number %d: " i) ;
            Sil.d_exp e ;
            L.d_ln () ) ;
          not is_const
      | None
       -> true
    in
    List.filter ~f:is_not_const actuals_by_ref
  in
  List.fold ~f:do_actual_by_ref ~init:prop non_const_actuals_by_ref

(** execute a call for an unknown or scan function *)
and unknown_or_scan_call ~is_scan ret_type_option ret_annots
    {Builtin.tenv; pdesc; prop_= pre; path; ret_id; args; proc_name= callee_pname; loc; instr} =
  let remove_file_attribute prop =
    let do_exp p (e, _) =
      let do_attribute q atom =
        match atom with
        | Sil.Apred ((Aresource {ra_res= Rfile} as res), _)
         -> Attribute.remove_for_attr tenv q res
        | _
         -> q
      in
      List.fold ~f:do_attribute ~init:p (Attribute.get_for_exp tenv p e)
    in
    let filtered_args =
      match (args, instr) with
      | _ :: other_args, Sil.Call (_, _, _, _, {CallFlags.cf_virtual}) when cf_virtual
       -> (* Do not remove the file attribute on the reciver for virtual calls *)
          other_args
      | _
       -> args
    in
    List.fold ~f:do_exp ~init:prop filtered_args
  in
  let should_abduce_param_value pname =
    let open Typ.Procname in
    match pname with
    | Java _
     -> (* FIXME (T19882766): we need to disable this for Java because it breaks too many tests *)
        false
    | ObjC_Cpp _
     -> (* FIXME: we need to work around a frontend hack for std::shared_ptr
         * to silent some of the uninitialization warnings *)
        if String.is_suffix ~suffix:"_std__shared_ptr" (Typ.Procname.to_string callee_pname) then
          false
        else true
    | _
     -> true
  in
  let actuals_by_ref =
    List.filter_mapi
      ~f:(fun i actual ->
        match actual with
        | (Exp.Lvar _ as e), ({Typ.desc= Tptr _} as t)
         -> Some (e, t, i)
        | (Exp.Var _ as e), ({Typ.desc= Tptr _} as t) when should_abduce_param_value callee_pname
         -> Some (e, t, i)
        | _
         -> None)
      args
  in
  let has_nullable_annot = Annotations.ia_is_nullable ret_annots in
  let pre_final =
    (* in Java, assume that skip functions close resources passed as params *)
    let pre_1 = if Typ.Procname.is_java callee_pname then remove_file_attribute pre else pre in
    let pre_2 =
      match (ret_id, ret_type_option) with
      | Some (ret_id, _), Some ret_typ
       -> (* TODO(jjb): Should this use the type of ret_id, or ret_type from the procedure type? *)
          add_constraints_on_retval tenv pdesc pre_1 (Exp.Var ret_id) ret_typ ~has_nullable_annot
            callee_pname loc
      | _
       -> pre_1
    in
    add_constraints_on_actuals_by_ref tenv pre_2 actuals_by_ref callee_pname loc
  in
  if is_scan (* if scan function, don't mark anything with undef attributes *) then
    [(Tabulation.remove_constant_string_class tenv pre_final, path)]
  else
    (* otherwise, add undefined attribute to retvals and actuals passed by ref *)
    let exps_to_mark =
      let ret_exps = Option.value_map ~f:(fun (id, _) -> [Exp.Var id]) ~default:[] ret_id in
      List.fold
        ~f:(fun exps_to_mark (exp, _, _) -> exp :: exps_to_mark)
        ~init:ret_exps actuals_by_ref
    in
    let prop_with_undef_attr =
      let path_pos = State.get_path_pos () in
      Attribute.mark_vars_as_undefined tenv pre_final exps_to_mark callee_pname ret_annots loc
        path_pos
    in
    let reason = "function or method not found" in
    let skip_path = Paths.Path.add_skipped_call path callee_pname reason in
    [(prop_with_undef_attr, skip_path)]

and check_variadic_sentinel ?(fails_on_nil= false) n_formals (sentinel, null_pos)
    {Builtin.pdesc; tenv; prop_; path; args; proc_name; loc} =
  (* from clang's lib/Sema/SemaExpr.cpp: *)
  (* "nullPos" is the number of formal parameters at the end which *)
  (* effectively count as part of the variadic arguments.  This is *)
  (* useful if you would prefer to not have *any* formal parameters, *)
  (* but the language forces you to have at least one. *)
  let first_var_arg_pos = if null_pos > n_formals then 0 else n_formals - null_pos in
  let nargs = List.length args in
  (* sentinels start counting from the last argument to the function *)
  let sentinel_pos = nargs - sentinel - 1 in
  let mk_non_terminal_argsi (acc, i) a =
    if i < first_var_arg_pos || i >= sentinel_pos then (acc, i + 1) else ((a, i) :: acc, i + 1)
  in
  (* fold_left reverses the arguments *)
  let non_terminal_argsi = fst (List.fold ~f:mk_non_terminal_argsi ~init:([], 0) args) in
  let check_allocated result ((lexp, typ), i) =
    (* simulate a Load for [lexp] *)
    let tmp_id_deref = Ident.create_fresh Ident.kprimed in
    let load_instr = Sil.Load (tmp_id_deref, lexp, typ, loc) in
    try instrs tenv pdesc [load_instr] result
    with e when SymOp.exn_not_failure e ->
      if not fails_on_nil then
        let deref_str = Localise.deref_str_nil_argument_in_variadic_method proc_name nargs i in
        let err_desc =
          Errdesc.explain_dereference tenv ~use_buckets:true ~is_premature_nil:true deref_str prop_
            loc
        in
        raise (Exceptions.Premature_nil_termination (err_desc, __POS__))
      else raise e
  in
  (* fold_left reverses the arguments back so that we report an *)
  (* error on the first premature nil argument *)
  List.fold ~f:check_allocated ~init:[(prop_, path)] non_terminal_argsi

and check_variadic_sentinel_if_present ({Builtin.prop_; path; proc_name} as builtin_args) =
  match Specs.proc_resolve_attributes proc_name with
  | None
   -> [(prop_, path)]
  | Some callee_attributes ->
    match
      PredSymb.get_sentinel_func_attribute_value callee_attributes.ProcAttributes.func_attributes
    with
    | None
     -> [(prop_, path)]
    | Some sentinel_arg
     -> let formals = callee_attributes.ProcAttributes.formals in
        check_variadic_sentinel (List.length formals) sentinel_arg builtin_args

and sym_exec_objc_getter field_name ret_typ tenv ret_id pdesc pname loc args prop =
  L.d_strln
    ( "No custom getter found. Executing the ObjC builtin getter with ivar "
    ^ Typ.Fieldname.to_string field_name ^ "." ) ;
  let ret_id = match ret_id with Some (ret_id, _) -> ret_id | None -> assert false in
  match args with
  | [(lexp, ({Typ.desc= Tstruct _} as typ | {desc= Tptr (({desc= Tstruct _} as typ), _)}))]
   -> let field_access_exp = Exp.Lfield (lexp, field_name, typ) in
      execute_load ~report_deref_errors:false pname pdesc tenv ret_id field_access_exp ret_typ loc
        prop
  | _
   -> raise (Exceptions.Wrong_argument_number __POS__)

and sym_exec_objc_setter field_name _ tenv _ pdesc pname loc args prop =
  L.d_strln
    ( "No custom setter found. Executing the ObjC builtin setter with ivar "
    ^ Typ.Fieldname.to_string field_name ^ "." ) ;
  match args with
  | (lexp1, ({Typ.desc= Tstruct _} as typ1 | {Typ.desc= Tptr (typ1, _)})) :: (lexp2, typ2) :: _
   -> let field_access_exp = Exp.Lfield (lexp1, field_name, typ1) in
      execute_store ~report_deref_errors:false pname pdesc tenv field_access_exp typ2 lexp2 loc
        prop
  | _
   -> raise (Exceptions.Wrong_argument_number __POS__)

and sym_exec_objc_accessor property_accesor ret_typ tenv ret_id pdesc _ loc args prop path
    : Builtin.ret_typ =
  let f_accessor =
    match property_accesor with
    | ProcAttributes.Objc_getter field_name
     -> sym_exec_objc_getter field_name
    | ProcAttributes.Objc_setter field_name
     -> sym_exec_objc_setter field_name
  in
  (* we want to execute in the context of the current procedure, not in the context of callee_pname,
     since this is the procname of the setter/getter method *)
  let cur_pname = Procdesc.get_proc_name pdesc in
  f_accessor ret_typ tenv ret_id pdesc cur_pname loc args prop |> List.map ~f:(fun p -> (p, path))

(** Perform symbolic execution for a function call *)
and proc_call callee_summary
    {Builtin.pdesc; tenv; prop_= pre; path; ret_id; args= actual_pars; loc} =
  let caller_pname = Procdesc.get_proc_name pdesc in
  let callee_pname = Specs.get_proc_name callee_summary in
  check_inherently_dangerous_function caller_pname callee_pname ;
  let formal_types = List.map ~f:snd (Specs.get_formals callee_summary) in
  let rec comb actual_pars formal_types =
    match (actual_pars, formal_types) with
    | [], []
     -> actual_pars
    | (e, t_e) :: etl', _ :: tl'
     -> (e, t_e) :: comb etl' tl'
    | _, []
     -> Errdesc.warning_err (State.get_loc ())
          "likely use of variable-arguments function, or function prototype missing@." ;
        L.d_warning "likely use of variable-arguments function, or function prototype missing" ;
        L.d_ln () ;
        L.d_str "actual parameters: " ;
        Sil.d_exp_list (List.map ~f:fst actual_pars) ;
        L.d_ln () ;
        L.d_str "formal parameters: " ;
        Typ.d_list formal_types ;
        L.d_ln () ;
        actual_pars
    | [], _
     -> L.d_str ("**** ERROR: Procedure " ^ Typ.Procname.to_string callee_pname) ;
        L.d_strln " mismatch in the number of parameters ****" ;
        L.d_str "actual parameters: " ;
        Sil.d_exp_list (List.map ~f:fst actual_pars) ;
        L.d_ln () ;
        L.d_str "formal parameters: " ;
        Typ.d_list formal_types ;
        L.d_ln () ;
        raise (Exceptions.Wrong_argument_number __POS__)
  in
  (* Actual parameters are associated to their formal
       parameter type if there are enough formal parameters, and
       to their actual type otherwise. The latter case happens
       with variable - arguments functions *)
  let actual_params = comb actual_pars formal_types in
  (* In case we call an objc instance method we add and extra spec *)
  (* were the receiver is null and the semantics of the call is nop*)
  (* let callee_attrs = Specs.get_attributes callee_summary in *)
  if !Config.curr_language <> Config.Java
     && (Specs.get_attributes callee_summary).ProcAttributes.is_objc_instance_method
  then
    handle_objc_instance_method_call actual_pars actual_params pre tenv ret_id pdesc callee_pname
      loc path (Tabulation.exe_function_call callee_summary)
  else
    (* non-objective-c method call. Standard tabulation *)
    Tabulation.exe_function_call callee_summary tenv ret_id pdesc callee_pname loc actual_params
      pre path

(** perform symbolic execution for a single prop, and check for junk *)
and sym_exec_wrapper handle_exn tenv pdesc instr ((prop: Prop.normal Prop.t), path)
    : Paths.PathSet.t =
  let pname = Procdesc.get_proc_name pdesc in
  let prop_primed_to_normal p =
    (* Rename primed vars with fresh normal vars, and return them *)
    let fav = Prop.prop_fav p in
    Sil.fav_filter_ident fav Ident.is_primed ;
    let ids_primed = Sil.fav_to_list fav in
    let ids_primed_normal =
      List.map ~f:(fun id -> (id, Ident.create_fresh Ident.knormal)) ids_primed
    in
    let ren_sub =
      Sil.subst_of_list (List.map ~f:(fun (id1, id2) -> (id1, Exp.Var id2)) ids_primed_normal)
    in
    let p' = Prop.normalize tenv (Prop.prop_sub ren_sub p) in
    let fav_normal = Sil.fav_from_list (List.map ~f:snd ids_primed_normal) in
    (p', fav_normal)
  in
  let prop_normal_to_primed fav_normal p =
    (* rename given normal vars to fresh primed *)
    if List.is_empty (Sil.fav_to_list fav_normal) then p else Prop.exist_quantify tenv fav_normal p
  in
  try
    let pre_process_prop p =
      let p', fav =
        if Sil.instr_is_auxiliary instr then (p, Sil.fav_new ()) else prop_primed_to_normal p
      in
      let p'' =
        let map_res_action e ra =
          (* update the vpath in resource attributes *)
          let vpath, _ = Errdesc.vpath_find tenv p' e in
          {ra with PredSymb.ra_vpath= vpath}
        in
        Attribute.map_resource tenv p' map_res_action
      in
      (p'', fav)
    in
    let post_process_result fav_normal p path =
      let p' = prop_normal_to_primed fav_normal p in
      State.set_path path None ;
      let node_has_abstraction node =
        let instr_is_abstraction = function Sil.Abstract _ -> true | _ -> false in
        List.exists ~f:instr_is_abstraction (Procdesc.Node.get_instrs node)
      in
      let curr_node = State.get_node () in
      match Procdesc.Node.get_kind curr_node with
      | Procdesc.Node.Prune_node _ when not (node_has_abstraction curr_node)
       -> (* don't check for leaks in prune nodes, unless there is abstraction anyway,*)
          (* but force them into either branch *)
          p'
      | _
       -> check_deallocate_static_memory (Abs.abstract_junk ~original_prop:p pname tenv p')
    in
    L.d_str "Instruction " ;
    Sil.d_instr instr ;
    L.d_ln () ;
    let prop', fav_normal = pre_process_prop prop in
    let res_list =
      Config.run_with_abs_val_equal_zero
        (* no exp abstraction during sym exe *)
          (fun () -> sym_exec tenv pdesc instr prop' path)
        ()
    in
    let res_list_nojunk =
      List.map ~f:(fun (p, path) -> (post_process_result fav_normal p path, path)) res_list
    in
    let results =
      List.map
        ~f:(fun (p, path) -> (Prop.prop_rename_primed_footprint_vars tenv p, path))
        res_list_nojunk
    in
    L.d_strln "Instruction Returns" ;
    Propgraph.d_proplist prop (List.map ~f:fst results) ;
    L.d_ln () ;
    State.mark_instr_ok () ;
    Paths.PathSet.from_renamed_list results
  with exn when Exceptions.handle_exception exn && !Config.footprint ->
    handle_exn exn ; (* calls State.mark_instr_fail *)
                     Paths.PathSet.empty

(** {2 Lifted Abstract Transfer Functions} *)

let node handle_exn tenv pdesc node (pset: Paths.PathSet.t) : Paths.PathSet.t =
  let pname = Procdesc.get_proc_name pdesc in
  let exe_instr_prop instr p tr (pset1: Paths.PathSet.t) =
    let pset2 =
      if Tabulation.prop_is_exn pname p && not (Sil.instr_is_auxiliary instr)
         && Procdesc.Node.get_kind node <> Procdesc.Node.exn_handler_kind
         (* skip normal instructions if an exception was thrown,
            unless this is an exception handler node *)
      then (
        L.d_str "Skipping instr " ;
        Sil.d_instr instr ;
        L.d_strln " due to exception" ;
        Paths.PathSet.from_renamed_list [(p, tr)] )
      else sym_exec_wrapper handle_exn tenv pdesc instr (p, tr)
    in
    Paths.PathSet.union pset2 pset1
  in
  let exe_instr_pset pset instr =
    Paths.PathSet.fold (exe_instr_prop instr) pset Paths.PathSet.empty
  in
  List.fold ~f:exe_instr_pset ~init:pset (Procdesc.Node.get_instrs node)
