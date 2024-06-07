(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Symbolic Execution *)

module L = Logging
module F = Format

let rec fldlist_assoc fld = function
  | [] ->
      raise Caml.Not_found
  | {Struct.name= fld'; typ= x} :: l ->
      if Fieldname.equal fld fld' then x else fldlist_assoc fld l


let unroll_type tenv (typ : Typ.t) (off : Predicates.offset) =
  let fail pp_fld fld =
    L.d_strln ".... Invalid Field Access ...." ;
    L.d_printfln "Fld : %a" pp_fld fld ;
    L.d_str "Type : " ;
    Typ.d_full typ ;
    L.d_ln () ;
    raise (Exceptions.Bad_footprint __POS__)
  in
  match (typ.desc, off) with
  | Tstruct name, Off_fld (fld, _) -> (
    match Tenv.lookup tenv name with
    | Some {fields; statics} -> (
      try fldlist_assoc fld (fields @ statics) with Caml.Not_found -> fail Fieldname.pp fld )
    | None ->
        fail Fieldname.pp fld )
  | Tarray {elt}, Off_index _ ->
      elt
  | _, Off_index (Const (Cint i)) when IntLit.iszero i ->
      typ
  | _ ->
      fail (Predicates.pp_offset Pp.text) off


(** Apply function [f] to the expression at position [offlist] in [strexp]. If not found, expand
    [strexp] and apply [f] to [None]. The routine should maintain the invariant that strexp and typ
    correspond to each other exactly, without involving any re - interpretation of some type t as
    the t array. The [fp_root] parameter indicates whether the kind of the root expression of the
    corresponding pointsto predicate is a footprint identifier. The function can expand a list of
    higher - order [hpara_psto] predicates, if the list is stored at [offlist] in [strexp]
    initially. The expanded list is returned as a part of the result. All these happen under [p], so
    that it is sound to call the prover with [p]. Finally, before running this function, the tool
    should run strexp_extend_value in rearrange.ml for the same strexp and offlist, so that all the
    necessary extensions of strexp are done before this function. If the tool follows this protocol,
    it will never hit the assert false cases for field and array accesses. *)
let rec apply_offlist analysis_data tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist
    (f : Exp.t option -> Exp.t) inst lookup_inst =
  let pp_error () =
    L.d_strln ".... Invalid Field ...." ;
    L.d_str "strexp : " ;
    Predicates.d_sexp strexp ;
    L.d_ln () ;
    L.d_str "offlist : " ;
    Predicates.d_offset_list offlist ;
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
  | [], Predicates.Eexp (e, inst_curr), _ ->
      let inst_new =
        match inst with
        | Predicates.Ilookup ->
            (* a lookup does not change an inst unless it is inst_initial *)
            lookup_inst := Some inst_curr ;
            inst_curr
        | _ ->
            Predicates.update_inst inst_curr inst
      in
      let e' = f (Some e) in
      (e', Predicates.Eexp (e', inst_new), typ, None)
  | [], Predicates.Estruct (fesl, inst'), _ ->
      if not nullify_struct then (f None, Predicates.Estruct (fesl, inst'), typ, None)
      else if fp_root then (
        pp_error () ;
        assert false )
      else (
        L.d_strln "WARNING: struct assignment treated as nondeterministic assignment" ;
        (f None, Prop.create_strexp_of_type tenv Prop.Fld_init typ None inst, typ, None) )
  | [], Predicates.Earray _, _ ->
      let offlist' = Predicates.Off_index Exp.zero :: offlist in
      apply_offlist analysis_data tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist' f
        inst lookup_inst
  | Predicates.Off_fld _ :: _, Predicates.Earray _, _ ->
      let offlist_new = Predicates.Off_index Exp.zero :: offlist in
      apply_offlist analysis_data tenv p fp_root nullify_struct (root_lexp, strexp, typ) offlist_new
        f inst lookup_inst
  | ( Predicates.Off_fld (fld, fld_typ) :: offlist'
    , Predicates.Estruct (fsel, inst')
    , Typ.Tstruct name ) -> (
    match Tenv.lookup tenv name with
    | Some ({fields} as struct_typ) -> (
        let t' = unroll_type tenv typ (Predicates.Off_fld (fld, fld_typ)) in
        match List.find ~f:(fun fse -> Fieldname.equal fld (fst fse)) fsel with
        | Some (_, se') ->
            let res_e', res_se', res_t', res_pred_insts_op' =
              apply_offlist analysis_data tenv p fp_root nullify_struct (root_lexp, se', t')
                offlist' f inst lookup_inst
            in
            let replace_fse fse = if Fieldname.equal fld (fst fse) then (fld, res_se') else fse in
            let res_se = Predicates.Estruct (List.map ~f:replace_fse fsel, inst') in
            let replace_fta {Struct.name= f; typ= t; annot= a} =
              if Fieldname.equal fld f then Struct.mk_field fld res_t' ~annot:a
              else Struct.mk_field f t ~annot:a
            in
            let fields' = List.map ~f:replace_fta fields in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            (res_e', res_se, typ, res_pred_insts_op')
        | None ->
            (* This case should not happen. The rearrangement should
               have materialized all the accessed cells. *)
            pp_error () ;
            assert false )
    | None ->
        pp_error () ;
        assert false )
  | Predicates.Off_fld _ :: _, _, _ ->
      pp_error () ;
      assert false
  | ( Predicates.Off_index idx :: offlist'
    , Predicates.Earray (len, esel, inst1)
    , Typ.Tarray {elt= t'; length= len'; stride= stride'} ) -> (
      let nidx = Prop.exp_normalize_prop tenv p idx in
      match List.find ~f:(fun ese -> Prover.check_equal tenv p nidx (fst ese)) esel with
      | Some (idx_ese', se') ->
          let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist analysis_data tenv p fp_root nullify_struct (root_lexp, se', t') offlist'
              f inst lookup_inst
          in
          let replace_ese ese = if Exp.equal idx_ese' (fst ese) then (idx_ese', res_se') else ese in
          let res_se = Predicates.Earray (len, List.map ~f:replace_ese esel, inst1) in
          let res_t = Typ.mk_array ~default:typ res_t' ?length:len' ?stride:stride' in
          (res_e', res_se, res_t, res_pred_insts_op')
      | None ->
          (* return a nondeterministic value if the index is not found after rearrangement *)
          L.d_str "apply_offlist: index " ;
          Exp.d_exp idx ;
          L.d_strln " not materialized -- returning nondeterministic value" ;
          let res_e' = Exp.Var (Ident.create_fresh Ident.kprimed) in
          (res_e', strexp, typ, None) )
  | Predicates.Off_index _ :: _, _, _ ->
      (* This case should not happen. The rearrangement should
         have materialized all the accessed cells. *)
      pp_error () ;
      raise (Exceptions.Internal_error (Localise.verbatim_desc "Array out of bounds in Symexec"))


(** Given [lexp |-> se: typ], if the location [offlist] exists in [se], function
    [ptsto_lookup p (lexp, se, typ) offlist id] returns a tuple. The first component of the tuple is
    an expression at position [offlist] in [se]. The second component is an expansion of the
    predicate [lexp |-> se: typ], where the entity at [offlist] in [se] is expanded if the entity is
    a list of higher - order parameters [hpara_psto]. If this expansion happens, the last component
    of the tuple is a list of pi - sigma pairs obtained by instantiating the [hpara_psto] list.
    Otherwise, the last component is None. All these steps happen under [p]. So, we can call a
    prover with [p]. Finally, before running this function, the tool should run strexp_extend_value
    in rearrange.ml for the same se and offlist, so that all the necessary extensions of se are done
    before this function. *)
let ptsto_lookup analysis_data tenv p (lexp, se, sizeof) offlist id =
  let f = function Some exp -> exp | None -> Exp.Var id in
  let fp_root = match lexp with Exp.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let e', se', typ', pred_insts_op' =
    apply_offlist analysis_data tenv p fp_root false (lexp, se, sizeof.Exp.typ) offlist f
      Predicates.inst_lookup lookup_inst
  in
  let lookup_uninitialized =
    (* true if we have looked up an uninitialized value *)
    match !lookup_inst with
    | Some (Predicates.Iinitial | Predicates.Ialloc | Predicates.Ilookup) ->
        true
    | _ ->
        false
  in
  let ptsto' = Prop.mk_ptsto tenv lexp se' (Exp.Sizeof {sizeof with typ= typ'}) in
  (e', ptsto', pred_insts_op', lookup_uninitialized)


(** [ptsto_update p (lexp,se,typ) offlist exp] takes [lexp |-> se: typ], and updates [se] by
    replacing the expression at [offlist] with [exp]. Then, it returns the updated pointsto
    predicate. If [lexp |-> se: typ] gets expanded during this update, the generated pi - sigma list
    from the expansion gets returned, and otherwise, None is returned. All these happen under the
    proposition [p], so it is ok call prover with [p]. Finally, before running this function, the
    tool should run strexp_extend_value in rearrange.ml for the same se and offlist, so that all the
    necessary extensions of se are done before this function. *)
let ptsto_update analysis_data tenv p (lexp, se, sizeof) offlist exp =
  let f _ = exp in
  let fp_root = match lexp with Exp.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let _, se', typ', pred_insts_op' =
    let pos = State.get_path_pos () in
    apply_offlist analysis_data tenv p fp_root true (lexp, se, sizeof.Exp.typ) offlist f
      (State.get_inst_update pos) lookup_inst
  in
  let ptsto' = Prop.mk_ptsto tenv lexp se' (Exp.Sizeof {sizeof with typ= typ'}) in
  (ptsto', pred_insts_op')


let update_iter iter pi sigma =
  let iter' = Prop.prop_iter_update_current_by_list iter sigma in
  List.fold ~f:(Prop.prop_iter_add_atom false) ~init:iter' pi


(** Precondition: se should not include hpara_psto that could mean nonempty heaps. *)
let rec execute_nullify_se = function
  | Predicates.Eexp _ ->
      Predicates.Eexp (Exp.zero, Predicates.inst_nullify)
  | Predicates.Estruct (fsel, _) ->
      let fsel' = List.map ~f:(fun (fld, se) -> (fld, execute_nullify_se se)) fsel in
      Predicates.Estruct (fsel', Predicates.inst_nullify)
  | Predicates.Earray (len, esel, _) ->
      let esel' = List.map ~f:(fun (idx, se) -> (idx, execute_nullify_se se)) esel in
      Predicates.Earray (len, esel', Predicates.inst_nullify)


(** Do pruning for conditional [if (e1 != e2)] if [positive] is true and [(if (e1 == e2)] if
    [positive] is false *)
let prune_ne tenv ~positive e1 e2 prop =
  let is_inconsistent =
    if positive then Prover.check_equal tenv prop e1 e2 else Prover.check_disequal tenv prop e1 e2
  in
  if is_inconsistent then Propset.empty
  else
    let conjoin = if positive then Prop.conjoin_neq else Prop.conjoin_eq in
    let new_prop = conjoin tenv ~footprint:!BiabductionConfig.footprint e1 e2 prop in
    if Prover.check_inconsistency tenv new_prop then Propset.empty
    else Propset.singleton tenv new_prop


(** Do pruning for conditional "if ([e1] CMP [e2])" if [positive] is true and "if (!([e1] CMP
    [e2]))" if [positive] is false, where CMP is "<" if [is_strict] is true and "<=" if [is_strict]
    is false. *)
let prune_ineq tenv ~is_strict ~positive prop e1 e2 =
  if Exp.equal e1 e2 then
    if (positive && not is_strict) || ((not positive) && is_strict) then Propset.singleton tenv prop
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
      let footprint = !BiabductionConfig.footprint in
      let prop_with_ineq = Prop.conjoin_eq tenv ~footprint prune_cond Exp.one prop in
      Propset.singleton tenv prop_with_ineq


let rec prune tenv ~positive condition prop =
  match Prop.exp_normalize_prop ~destructive:true tenv prop condition with
  | Exp.Var _ | Exp.Lvar _ ->
      prune_ne tenv ~positive condition Exp.zero prop
  | Exp.Const (Const.Cint i) when IntLit.iszero i ->
      if positive then Propset.empty else Propset.singleton tenv prop
  | Exp.Const (Const.Cint _ | Const.Cstr _ | Const.Cclass _) | Exp.Sizeof _ ->
      if positive then Propset.singleton tenv prop else Propset.empty
  | Exp.Const _ ->
      assert false
  | Exp.Cast (_, condition') ->
      prune tenv ~positive condition' prop
  | Exp.UnOp (Unop.LNot, condition', _) ->
      prune tenv ~positive:(not positive) condition' prop
  | Exp.UnOp _ ->
      assert false
  | Exp.BinOp (Binop.Eq, e, Exp.Const (Const.Cint i)) when IntLit.iszero i && not (IntLit.isnull i)
    ->
      prune tenv ~positive:(not positive) e prop
  | Exp.BinOp (Binop.Eq, Exp.Const (Const.Cint i), e) when IntLit.iszero i && not (IntLit.isnull i)
    ->
      prune tenv ~positive:(not positive) e prop
  | Exp.BinOp (Binop.Eq, e1, e2) ->
      prune_ne tenv ~positive:(not positive) e1 e2 prop
  | Exp.BinOp (Binop.Ne, e, Exp.Const (Const.Cint i)) when IntLit.iszero i && not (IntLit.isnull i)
    ->
      prune tenv ~positive e prop
  | Exp.BinOp (Binop.Ne, Exp.Const (Const.Cint i), e) when IntLit.iszero i && not (IntLit.isnull i)
    ->
      prune tenv ~positive e prop
  | Exp.BinOp (Binop.Ne, e1, e2) ->
      prune_ne tenv ~positive e1 e2 prop
  | Exp.BinOp (Binop.Ge, e2, e1) | Exp.BinOp (Binop.Le, e1, e2) ->
      prune_ineq tenv ~is_strict:false ~positive prop e1 e2
  | Exp.BinOp (Binop.Gt, e2, e1) | Exp.BinOp (Binop.Lt, e1, e2) ->
      prune_ineq tenv ~is_strict:true ~positive prop e1 e2
  | Exp.BinOp (Binop.LAnd, condition1, condition2) ->
      let pruner = if positive then prune_inter tenv else prune_union tenv in
      pruner ~positive condition1 condition2 prop
  | Exp.BinOp (Binop.LOr, condition1, condition2) ->
      let pruner = if positive then prune_union tenv else prune_inter tenv in
      pruner ~positive condition1 condition2 prop
  | Exp.BinOp _ | Exp.Lfield _ | Exp.Lindex _ ->
      prune_ne tenv ~positive condition Exp.zero prop
  | Exp.Exn _ ->
      assert false
  | Exp.Closure _ ->
      assert false


and prune_inter tenv ~positive condition1 condition2 prop =
  let res = ref Propset.empty in
  let pset1 = prune tenv ~positive condition1 prop in
  let do_p p = res := Propset.union (prune tenv ~positive condition2 p) !res in
  Propset.iter do_p pset1 ;
  !res


and prune_union tenv ~positive condition1 condition2 prop =
  let pset1 = prune tenv ~positive condition1 prop in
  let pset2 = prune tenv ~positive condition2 prop in
  Propset.union pset1 pset2


let dangerous_functions =
  let dangerous_list = ["gets"] in
  ref (List.map ~f:Procname.from_string_c_fun dangerous_list)


let check_inherently_dangerous_function {InterproceduralAnalysis.proc_desc; err_log} callee_pname =
  if List.exists ~f:(Procname.equal callee_pname) !dangerous_functions then
    let exn =
      Exceptions.Inherently_dangerous_function
        (Localise.desc_inherently_dangerous_function callee_pname)
    in
    BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn


let reason_to_skip ~callee_desc : string option =
  let reason_from_attributes attributes =
    if attributes.ProcAttributes.is_abstract then Some "abstract method"
    else if not attributes.ProcAttributes.is_defined then Some "method has no implementation"
    else None
  in
  let reason_from_pname pname =
    if Procname.is_method_in_objc_protocol pname then
      Some "no implementation found for method declared in Objective-C protocol"
    else None
  in
  match callee_desc with
  | `Summary (callee_pname, callee_summary) ->
      let attr_reason = Attributes.load_exn callee_pname |> reason_from_attributes in
      if Option.is_some attr_reason then attr_reason
      else if List.is_empty (BiabductionSummary.get_specs callee_summary) then
        Some "empty list of specs"
      else (* we are not skipping *) None
  | `ProcDesc procdesc ->
      let pname_reason = Procdesc.get_proc_name procdesc |> reason_from_pname in
      if Option.is_some pname_reason then pname_reason
      else
        let attr_reason = Procdesc.get_attributes procdesc |> reason_from_attributes in
        if Option.is_some attr_reason then attr_reason else Some "function or method not found"
  | `ProcName callee_pname ->
      let pname_reason = reason_from_pname callee_pname in
      if Option.is_some pname_reason then pname_reason else Some "method has no implementation"


(** In case of constant string dereference, return the result immediately *)
let check_constant_string_dereference lexp =
  let string_lookup s n =
    let c = try Char.to_int s.[IntLit.to_int_exn n] with Invalid_argument _ -> 0 in
    Exp.int (IntLit.of_int c)
  in
  match lexp with
  | Exp.BinOp (Binop.PlusPI, Exp.Const (Const.Cstr s), e) | Exp.Lindex (Exp.Const (Const.Cstr s), e)
    ->
      let value =
        match e with
        | Exp.Const (Const.Cint n)
          when IntLit.geq n IntLit.zero && IntLit.leq n (IntLit.of_int (String.length s)) ->
            string_lookup s n
        | _ ->
            Exp.get_undefined false
      in
      Some value
  | Exp.Const (Const.Cstr s) ->
      Some (string_lookup s IntLit.zero)
  | _ ->
      None


(** Normalize an expression and check for arithmetic problems *)
let check_arith_norm_exp {InterproceduralAnalysis.proc_desc; err_log; tenv} exp prop =
  match Attribute.find_arithmetic_problem tenv (State.get_path_pos ()) prop exp with
  | Some (Attribute.Div0 div), prop' ->
      let desc =
        Errdesc.explain_divide_by_zero tenv div (AnalysisState.get_node_exn ())
          (AnalysisState.get_loc_exn ())
      in
      let exn = Exceptions.Divide_by_zero (desc, __POS__) in
      BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn ;
      (Prop.exp_normalize_prop tenv prop exp, prop')
  | Some (Attribute.UminusUnsigned _), prop' | None, prop' ->
      (Prop.exp_normalize_prop tenv prop exp, prop')


let method_exists right_proc_name methods =
  if Language.curr_language_is Java || Language.curr_language_is CIL then
    List.exists ~f:(fun meth_name -> Procname.equal right_proc_name meth_name) methods
  else
    (* ObjC/C++ case : The attribute map will only exist when we have code for the method or
       the method has been called directly somewhere. It can still be that this is not the
       case but we have a model for the method. *)
    match Attributes.load right_proc_name with
    | Some attrs ->
        attrs.ProcAttributes.is_defined
    | None ->
        BiabductionModels.mem right_proc_name


let resolve_method tenv class_name proc_name =
  match Tenv.resolve_method ~method_exists tenv class_name proc_name |> fst with
  | None ->
      Logging.d_printfln "Couldn't find method in the hierarchy of type %s"
        (Typ.Name.name class_name) ;
      proc_name
  | Some method_info ->
      Tenv.MethodInfo.get_procname method_info


let resolve_typename prop receiver_exp =
  let typexp_opt =
    let rec loop = function
      | [] ->
          None
      | Predicates.Hpointsto (e, _, typexp) :: _ when Exp.equal e receiver_exp ->
          Some typexp
      | _ :: hpreds ->
          loop hpreds
    in
    loop prop.Prop.sigma
  in
  match typexp_opt with Some (Exp.Sizeof {typ= {desc= Tstruct name}}) -> Some name | _ -> None


(** If the dynamic type of the receiver actual T_actual is a subtype of the receiver type T_formal
    in the signature of [pname], resolve [pname] to T_actual.[pname]. *)
let resolve_virtual_pname tenv prop actuals callee_pname call_flags : Procname.t list =
  let resolve receiver_exp pname prop =
    match resolve_typename prop receiver_exp with
    | Some class_name ->
        resolve_method tenv class_name pname
    | None ->
        pname
  in
  let get_receiver_typ pname fallback_typ =
    match pname with
    | Procname.Java pname_java -> (
        let name = Procname.Java.get_class_type_name pname_java in
        match Tenv.lookup tenv name with
        | Some _ ->
            Typ.mk (Typ.Tptr (Typ.mk (Tstruct name), Pk_pointer))
        | None ->
            fallback_typ )
    | Procname.CSharp pname_csharp -> (
        let name = Procname.CSharp.get_class_type_name pname_csharp in
        match Tenv.lookup tenv name with
        | Some _ ->
            Typ.mk (Typ.Tptr (Typ.mk (Tstruct name), Pk_pointer))
        | None ->
            fallback_typ )
    | _ ->
        fallback_typ
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
  | _ when not (call_flags.CallFlags.cf_virtual || call_flags.CallFlags.cf_interface) ->
      (* if this is not a virtual or interface call, there's no need for resolution *)
      [callee_pname]
  | (receiver_exp, actual_receiver_typ) :: _ ->
      if not (Language.curr_language_is Java || Language.curr_language_is CIL) then
        (* default mode for Obj-C/C++/Java virtual calls: resolution only *)
        [do_resolve callee_pname receiver_exp actual_receiver_typ]
      else
        let resolved_target = do_resolve callee_pname receiver_exp actual_receiver_typ in
        [resolved_target]
  | _ ->
      L.(die InternalError) "A virtual call must have a receiver"


(** Resolve the name of the procedure to call based on the type of the arguments *)
let resolve_pname ~caller_loc tenv prop args pname call_flags : Procname.t =
  let resolve_from_args resolved_pname args =
    let resolved_parameters = Procname.get_parameters resolved_pname in
    let resolved_params =
      try
        List.fold2_exn
          ~f:(fun accu (arg_exp, _) name ->
            match resolve_typename prop arg_exp with
            | Some class_name ->
                Procname.parameter_of_name resolved_pname class_name :: accu
            | None ->
                name :: accu )
          ~init:[] args resolved_parameters
        |> List.rev
      with Invalid_argument _ ->
        let file = caller_loc.Location.file in
        L.(debug Analysis Medium)
          "Call mismatch: method %a has %i paramters but is called with %i arguments, in %a, %a@."
          Procname.pp pname (List.length resolved_parameters) (List.length args) SourceFile.pp file
          Location.pp caller_loc ;
        raise SpecializeProcdesc.UnmatchedParameters
    in
    Procname.replace_parameters resolved_params resolved_pname
  in
  let resolved_pname, other_args =
    let parameters = Procname.get_parameters pname in
    let match_parameters args = Int.equal (List.length args) (List.length parameters) in
    match args with
    | [] ->
        (pname, [])
    | (first_arg, _) :: other_args when call_flags.CallFlags.cf_virtual ->
        let resolved =
          match resolve_typename prop first_arg with
          | Some class_name ->
              resolve_method tenv class_name pname
          | None ->
              pname
        in
        (resolved, other_args)
    | _ :: other_args
      when match_parameters other_args (* Non-virtual call, e.g. constructors or private methods *)
      ->
        (pname, other_args)
    | args when match_parameters args (* Static call *) ->
        (pname, args)
    | args ->
        let file = caller_loc.Location.file in
        L.(debug Analysis Medium)
          "Call mismatch: method %a has %i paramters but is called with %i arguments, in %a, %a@."
          Procname.pp pname (List.length parameters) (List.length args) SourceFile.pp file
          Location.pp caller_loc ;
        raise SpecializeProcdesc.UnmatchedParameters
  in
  resolve_from_args resolved_pname other_args


let resolve_args prop args =
  List.map
    ~f:(fun ((arg_exp, arg_typ) as arg) ->
      match (resolve_typename prop arg_exp, arg_typ.Typ.desc) with
      | Some class_name, Tptr (({desc= Tstruct typename} as inner_typ), p) ->
          let resolved_arg_typ =
            if Typ.Name.equal class_name typename then arg_typ
            else
              let struct_typ = {inner_typ with desc= Tstruct class_name} in
              ({arg_typ with desc= Tptr (struct_typ, p)} : Typ.t)
          in
          (arg_exp, resolved_arg_typ)
      | _ ->
          arg )
    args


type resolve_and_analyze_result =
  { resolved_pname: Procname.t
  ; resolved_procdesc_opt: Procdesc.t option
  ; resolved_summary_opt: BiabductionSummary.t option }

(** Resolve the procedure name and run the analysis of the resolved procedure if not already
    analyzed *)
let resolve_and_analyze {InterproceduralAnalysis.analyze_dependency; proc_desc; tenv}
    ~has_clang_model prop args callee_proc_name call_flags : resolve_and_analyze_result =
  if has_clang_model then
    {resolved_pname= callee_proc_name; resolved_procdesc_opt= None; resolved_summary_opt= None}
  else
    (* TODO (#15748878): Fix conflict with method overloading by encoding in the procedure name
       whether the method is defined or generated by the specialization *)
    let analyze_ondemand resolved_pname =
      if Procname.equal resolved_pname callee_proc_name then
        ( Procdesc.load callee_proc_name
        , analyze_dependency callee_proc_name |> AnalysisResult.to_option )
      else
        (* Create the type specialized procedure description and analyze it directly *)
        let resolved_proc_desc_option =
          match Procdesc.load resolved_pname with
          | Some _ as resolved_proc_desc ->
              resolved_proc_desc
          | None ->
              let procdesc_opt = Procdesc.load callee_proc_name in
              Option.map procdesc_opt ~f:(fun callee_proc_desc ->
                  (* It is possible that the types of the arguments are not as precise as the type of
                     the objects in the heap, so we should update them to get the best results. *)
                  let resolved_args = resolve_args prop args in
                  SpecializeProcdesc.with_formals_types callee_proc_desc resolved_pname
                    resolved_args )
        in
        ( resolved_proc_desc_option
        , Option.bind resolved_proc_desc_option ~f:(fun _pdesc ->
              analyze_dependency resolved_pname |> AnalysisResult.to_option ) )
    in
    let resolved_pname =
      let caller_loc = Procdesc.get_loc proc_desc in
      resolve_pname ~caller_loc tenv prop args callee_proc_name call_flags
    in
    let resolved_procdesc_opt, resolved_summary_opt = analyze_ondemand resolved_pname in
    {resolved_pname; resolved_procdesc_opt; resolved_summary_opt}


(** recognize calls to the constructor java.net.URL and splits the argument string to be only the
    protocol. *)
let call_constructor_url_update_args =
  let url_pname =
    Procname.make_java
      ~class_name:(Typ.Name.Java.from_string "java.net.URL")
      ~return_type:None ~method_name:Procname.Java.constructor_method_name
      ~parameters:[StdTyp.Java.pointer_to_java_lang_string]
      ~kind:Procname.Java.Non_Static
  in
  let parts_delim = lazy (Str.regexp_string "://") in
  fun pname actual_params ->
    if Procname.equal url_pname pname then
      match actual_params with
      | [this; (Exp.Const (Const.Cstr s), atype)] -> (
          let parts = Str.split (Lazy.force parts_delim) s in
          match parts with
          | frst :: _ ->
              if
                String.equal frst "http" || String.equal frst "ftp" || String.equal frst "https"
                || String.equal frst "mailto" || String.equal frst "jar"
              then [this; (Exp.Const (Const.Cstr frst), atype)]
              else actual_params
          | _ ->
              actual_params )
      | [this; (_, atype)] ->
          [this; (Exp.Const (Const.Cstr "file"), atype)]
      | _ ->
          actual_params
    else actual_params


let receiver_self receiver prop =
  List.exists
    ~f:(fun hpred ->
      match hpred with
      | Predicates.Hpointsto (Lvar pv, Eexp (e, _), _) ->
          Exp.equal e receiver && Pvar.is_seed pv && Pvar.is_self pv
      | _ ->
          false )
    prop.Prop.sigma


(* When current ObjC method is an initializer and the method call is also an initializer,
   and the receiver is self, i.e. the call is [super init], then we want to assume that it
   can return null, regardless of code or annotations, so that the next statement should be
   a check for null, which is considered good practice. *)
let force_objc_init_return_nil pdesc callee_pname tenv ret_id pre path receiver =
  let current_pname = Procdesc.get_proc_name pdesc in
  if
    Procname.is_constructor callee_pname
    && receiver_self receiver pre && !BiabductionConfig.footprint
    && Procname.is_constructor current_pname
  then
    let propset = prune_ne tenv ~positive:false (Exp.Var ret_id) Exp.zero pre in
    if Propset.is_empty propset then []
    else
      let prop = List.hd_exn (Propset.to_proplist propset) in
      [(prop, path)]
  else []


(* This method is used to handle the special semantics of ObjC instance method calls. *)
(* res = [obj foo] *)
(*  1. We know that obj is null, then we return null *)
(*  2. We don't know, but obj could be null, we return both options, *)
(* (obj = null, res = null), (obj != null, res = [obj foo]) *)
(*  We want the same behavior even when we are going to skip the function. *)
let handle_objc_instance_method_call_or_skip pdesc tenv actual_pars path callee_pname pre ret_id res
    =
  let path_description =
    F.sprintf "Message %s with receiver nil returns nil."
      (Procname.to_simplified_string callee_pname)
  in
  let receiver =
    match actual_pars with
    | (e, _) :: _ ->
        e
    | _ ->
        raise
          (Exceptions.Internal_error
             (Localise.verbatim_desc
                "In Objective-C instance method call there should be a receiver." ) )
  in
  let is_receiver_null =
    match actual_pars with
    | (e, _) :: _ when Exp.equal e Exp.zero || Option.is_some (Attribute.get_objc_null tenv pre e)
      ->
        true
    | _ ->
        false
  in
  let add_objc_null_attribute_or_nullify_result prop =
    match Attribute.find_equal_formal_path tenv receiver prop with
    | Some vfs ->
        Attribute.add_or_replace tenv prop (Apred (Aobjc_null, [Exp.Var ret_id; vfs]))
    | None ->
        Prop.conjoin_eq tenv (Exp.Var ret_id) Exp.zero prop
  in
  if is_receiver_null then (
    (* objective-c instance method with a null receiver just return objc_null(res). *)
    let path = Paths.Path.add_description path path_description in
    L.d_printfln "Object-C method %a called with nil receiver. Returning 0/nil" Procname.pp
      callee_pname ;
    (* We wish to nullify the result. However, in some cases,
       we want to add the attribute OBJC_NULL to it so that we
       can keep track of how this object became null,
       so that in a NPE we can separate it into a different error type *)
    [(add_objc_null_attribute_or_nullify_result pre, path)] )
  else
    match force_objc_init_return_nil pdesc callee_pname tenv ret_id pre path receiver with
    | [] ->
        if
          !BiabductionConfig.footprint
          && Option.is_none (Attribute.get_undef tenv pre receiver)
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
        else res () (* Not known if receiver = 0 and not footprint. Standard tabulation *)
    | res_null ->
        List.append res_null (res ())


(* This method handles ObjC instance method calls, in particular the fact that calling a method *)
(* with nil returns nil. The exec_call function is either standard call execution or execution *)
(* of ObjC getters and setters using a builtin. *)
let handle_objc_instance_method_call actual_pars pre tenv ret_id pdesc callee_pname path res =
  handle_objc_instance_method_call_or_skip pdesc tenv actual_pars path callee_pname pre ret_id res


let normalize_params analysis_data prop actual_params =
  let norm_arg (p, args) (e, t) =
    let e', p' = check_arith_norm_exp analysis_data e p in
    (p', (e', t) :: args)
  in
  let prop, args = List.fold ~f:norm_arg ~init:(prop, []) actual_params in
  (prop, List.rev args)


let add_strexp_to_footprint tenv strexp abduced_pv typ prop =
  let abduced_lvar = Exp.Lvar abduced_pv in
  let lvar_pt_fpvar =
    let sizeof_exp =
      Exp.Sizeof
        {typ; nbytes= None; dynamic_length= None; subtype= Subtype.subtypes; nullable= false}
    in
    Prop.mk_ptsto tenv abduced_lvar strexp sizeof_exp
  in
  let sigma_fp = prop.Prop.sigma_fp in
  Prop.normalize tenv (Prop.set prop ~sigma_fp:(lvar_pt_fpvar :: sigma_fp))


let add_to_footprint tenv abduced_pv typ prop =
  let fresh_fp_var = Exp.Var (Ident.create_fresh Ident.kfootprint) in
  let prop' =
    add_strexp_to_footprint tenv (Eexp (fresh_fp_var, Predicates.Inone)) abduced_pv typ prop
  in
  (prop', fresh_fp_var)


(* the current abduction mechanism treats struct values differently than all other types. abduction
   on struct values adds a a struct whose fields are initialized to fresh footprint vars to the
   footprint. regular abduction just adds a fresh footprint value of the correct type to the
   footprint. we can get rid of this special case if we fix the abduction on struct values *)
let add_struct_value_to_footprint tenv abduced_pv typ prop =
  let struct_strexp = Prop.create_strexp_of_type tenv Prop.Fld_init typ None Predicates.inst_none in
  let prop' = add_strexp_to_footprint tenv struct_strexp abduced_pv typ prop in
  (prop', struct_strexp)


let is_rec_call callee_pname caller_pdesc =
  (* TODO: (t7147096) extend this to detect mutual recursion *)
  Procname.equal callee_pname (Procdesc.get_proc_name caller_pdesc)


let add_constraints_on_retval tenv pdesc prop ret_exp ~has_nonnull_annot typ callee_pname callee_loc
    =
  if Procname.is_infer_undefined callee_pname then prop
  else
    let lookup_abduced_expression p abduced_ret_pv =
      List.find_map
        ~f:(fun hpred ->
          match hpred with
          | Predicates.Hpointsto (Exp.Lvar pv, Eexp (exp, _), _) when Pvar.equal pv abduced_ret_pv
            ->
              Some exp
          | _ ->
              None )
        p.Prop.sigma_fp
    in
    (* find an hpred [abduced] |-> A in [prop] and add [exp] = A to prop *)
    let bind_exp_to_abduced_val exp_to_bind abduced prop =
      let bind_exp prop = function
        | Predicates.Hpointsto (Exp.Lvar pv, Eexp (rhs, _), _) when Pvar.equal pv abduced ->
            Prop.conjoin_eq tenv exp_to_bind rhs prop
        | _ ->
            prop
      in
      List.fold ~f:bind_exp ~init:prop prop.Prop.sigma
    in
    (* To avoid obvious false positives, assume skip functions do not return null pointers *)
    let add_ret_non_null exp typ prop =
      if has_nonnull_annot then
        match typ.Typ.desc with Typ.Tptr _ -> Prop.conjoin_neq tenv exp Exp.zero prop | _ -> prop
      else prop
    in
    if not (is_rec_call callee_pname pdesc) then
      (* introduce a fresh program variable to allow abduction on the return value *)
      let prop_with_abduced_var =
        let abduced_ret_pv =
          (* in Java, always re-use the same abduced ret var to prevent false alarms with repeated method calls *)
          let loc =
            if Procname.is_java callee_pname || Procname.is_csharp callee_pname then Location.dummy
            else callee_loc
          in
          Pvar.mk_abduced_ret callee_pname loc
        in
        if !BiabductionConfig.footprint then
          match lookup_abduced_expression prop abduced_ret_pv with
          | None ->
              let p, fp_var = add_to_footprint tenv abduced_ret_pv typ prop in
              Prop.conjoin_eq tenv ~footprint:true ret_exp fp_var p
          | Some exp ->
              Prop.conjoin_eq tenv ~footprint:true ret_exp exp prop
        else
          (* bind return id to the abduced value pointed to by the pvar we introduced *)
          bind_exp_to_abduced_val ret_exp abduced_ret_pv prop
      in
      add_ret_non_null ret_exp typ prop_with_abduced_var
    else add_ret_non_null ret_exp typ prop


let execute_load ?(report_deref_errors = true) ({InterproceduralAnalysis.tenv; _} as analysis_data)
    id rhs_exp typ loc prop_ =
  let execute_load_ acc_in iter =
    let iter_ren = Prop.prop_iter_make_id_primed tenv id iter in
    let prop_ren = Prop.prop_iter_to_prop tenv iter_ren in
    match Prop.prop_iter_current tenv iter_ren with
    | Predicates.Hpointsto (lexp, strexp, Exp.Sizeof sizeof_data), offlist -> (
        let contents, new_ptsto, pred_insts_op, lookup_uninitialized =
          ptsto_lookup analysis_data tenv prop_ren (lexp, strexp, sizeof_data) offlist id
        in
        let is_union_field =
          match rhs_exp with
          | Exp.Lfield (_, _, {Typ.desc= Tstruct name}) when Typ.Name.is_union name ->
              true
          | _ ->
              false
        in
        let update acc (pi, sigma) =
          let pi' = Predicates.Aeq (Exp.Var id, contents) :: pi in
          let sigma' = new_ptsto :: sigma in
          let iter' = update_iter iter_ren pi' sigma' in
          let prop' = Prop.prop_iter_to_prop tenv iter' in
          let prop'' =
            (* T30105165 remove `is_union_field` check after we improve union translation *)
            if lookup_uninitialized && not is_union_field then
              Attribute.add_or_replace tenv prop' (Apred (Adangling DAuninit, [Exp.Var id]))
            else prop'
          in
          prop'' :: acc
        in
        match pred_insts_op with
        | None ->
            update acc_in ([], [])
        | Some pred_insts ->
            List.rev (List.fold ~f:update ~init:acc_in pred_insts) )
    | Predicates.Hpointsto _, _ ->
        Errdesc.warning_err loc "no offset access in execute_load -- treating as skip@." ;
        Prop.prop_iter_to_prop tenv iter_ren :: acc_in
    | _ ->
        (* The implementation of this case means that we
           ignore this dereferencing operator. When the analyzer treats
           numerical information and arrays more precisely later, we
           should change the implementation here. *)
        assert false
  in
  try
    let n_rhs_exp, prop = check_arith_norm_exp analysis_data rhs_exp prop_ in
    let n_rhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_rhs_exp in
    match check_constant_string_dereference n_rhs_exp' with
    | Some value ->
        [Prop.conjoin_eq tenv (Exp.Var id) value prop]
    | None -> (
      try
        let iter_list =
          Rearrange.rearrange ~report_deref_errors analysis_data n_rhs_exp' typ prop loc
        in
        List.rev (List.fold ~f:execute_load_ ~init:[] iter_list)
      with Exceptions.Symexec_memory_error _ ->
        (* This should normally be a real alarm and should not be caught but currently happens
           when the normalization drops hpreds of the form ident |-> footprint var. *)
        let undef = Exp.get_undefined !BiabductionConfig.footprint in
        [Prop.conjoin_eq tenv (Exp.Var id) undef prop] )
  with Rearrange.ARRAY_ACCESS ->
    if Int.equal Config.biabduction_array_level 0 then assert false
    else
      let undef = Exp.get_undefined false in
      [Prop.conjoin_eq tenv (Exp.Var id) undef prop_]


let load_ret_annots pname =
  Attributes.load pname
  |> Option.value_map ~default:Annot.Item.empty ~f:(fun {ProcAttributes.ret_annots} -> ret_annots)


let execute_store ?(report_deref_errors = true) ({InterproceduralAnalysis.tenv; _} as analysis_data)
    lhs_exp typ rhs_exp loc prop_ =
  let execute_store_ analysis_data tenv rhs_exp acc_in iter =
    let lexp, strexp, sizeof, offlist =
      match Prop.prop_iter_current tenv iter with
      | Predicates.Hpointsto (lexp, strexp, Exp.Sizeof sizeof), offlist ->
          (lexp, strexp, sizeof, offlist)
      | _ ->
          assert false
    in
    let p = Prop.prop_iter_to_prop tenv iter in
    let new_ptsto, pred_insts_op =
      ptsto_update analysis_data tenv p (lexp, strexp, sizeof) offlist rhs_exp
    in
    let update acc (pi, sigma) =
      let sigma' = new_ptsto :: sigma in
      let iter' = update_iter iter pi sigma' in
      let prop' = Prop.prop_iter_to_prop tenv iter' in
      prop' :: acc
    in
    match pred_insts_op with
    | None ->
        update acc_in ([], [])
    | Some pred_insts ->
        List.fold ~f:update ~init:acc_in pred_insts
  in
  try
    let n_lhs_exp, prop_' = check_arith_norm_exp analysis_data lhs_exp prop_ in
    let n_rhs_exp, prop = check_arith_norm_exp analysis_data rhs_exp prop_' in
    let prop = Attribute.replace_objc_null tenv prop n_lhs_exp n_rhs_exp in
    let n_lhs_exp' = Prop.exp_collapse_consecutive_indices_prop typ n_lhs_exp in
    let iter_list =
      Rearrange.rearrange ~report_deref_errors analysis_data n_lhs_exp' typ prop loc
    in
    let prop_list =
      List.rev (List.fold ~f:(execute_store_ analysis_data tenv n_rhs_exp) ~init:[] iter_list)
    in
    prop_list
  with Rearrange.ARRAY_ACCESS ->
    if Int.equal Config.biabduction_array_level 0 then assert false else [prop_]


let is_variadic_procname callee_pname =
  Option.exists (Attributes.load callee_pname) ~f:(fun proc_attrs ->
      proc_attrs.ProcAttributes.is_clang_variadic )


let resolve_and_analyze_no_dynamic_dispatch {InterproceduralAnalysis.analyze_dependency; tenv}
    prop_r n_actual_params callee_pname call_flags =
  let resolved_pname =
    match resolve_virtual_pname tenv prop_r n_actual_params callee_pname call_flags with
    | resolved_pname :: _ ->
        resolved_pname
    | [] ->
        callee_pname
  in
  let resolved_summary_opt = analyze_dependency resolved_pname |> AnalysisResult.to_option in
  {resolved_pname; resolved_procdesc_opt= Procdesc.load resolved_pname; resolved_summary_opt}


let resolve_and_analyze_clang analysis_data prop_r n_actual_params callee_pname call_flags =
  if
    ((not (is_variadic_procname callee_pname)) && Procname.is_objc_method callee_pname)
    || Procname.is_objc_block callee_pname
    (* to be extended to other methods *)
  then
    try
      let has_clang_model = BiabductionModels.mem callee_pname in
      let resolve_and_analyze_result =
        resolve_and_analyze analysis_data ~has_clang_model prop_r n_actual_params callee_pname
          call_flags
      in
      (* It could be useful to specialize a model, but also it could cause a failure,
         because we don't have the correct fields in the tenv.
         In that case, default to the non-specialized spec for the model. *)
      let clang_model_specialized_failure =
        match resolve_and_analyze_result.resolved_summary_opt with
        | Some summary when has_clang_model ->
            List.is_empty (BiabductionSummary.get_specs summary)
        | None ->
            true
        | _ ->
            false
      in
      if clang_model_specialized_failure then
        resolve_and_analyze_no_dynamic_dispatch analysis_data prop_r n_actual_params callee_pname
          call_flags
      else resolve_and_analyze_result
    with SpecializeProcdesc.UnmatchedParameters ->
      resolve_and_analyze_no_dynamic_dispatch analysis_data prop_r n_actual_params callee_pname
        call_flags
  else
    resolve_and_analyze_no_dynamic_dispatch analysis_data prop_r n_actual_params callee_pname
      call_flags


let declare_locals_and_ret tenv pdesc (prop_ : Prop.normal Prop.t) =
  let sigma_locals_and_ret =
    let mk_ptsto pvar typ =
      let ptsto =
        ( pvar
        , Exp.Sizeof
            {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
        , None )
      in
      Prop.mk_ptsto_lvar tenv Prop.Fld_init Predicates.inst_initial ptsto
    in
    let sigma_locals_and_ret () =
      let pname = Procdesc.get_proc_name pdesc in
      let sigma_ret =
        let pvar = Procdesc.get_ret_var pdesc in
        let typ = Procdesc.get_ret_type pdesc in
        mk_ptsto pvar typ
      in
      let locals = Procdesc.get_locals pdesc in
      let sigma_locals =
        List.map locals ~f:(fun {ProcAttributes.name; typ} ->
            let pvar = Pvar.mk name pname in
            mk_ptsto pvar typ )
      in
      sigma_ret :: sigma_locals
    in
    BiabductionConfig.run_in_re_execution_mode
      (* no footprint vars for locals *)
      sigma_locals_and_ret ()
  in
  let sigma' = prop_.Prop.sigma @ sigma_locals_and_ret in
  let prop' = Prop.normalize tenv (Prop.set prop_ ~sigma:sigma') in
  prop'


(** Execute [instr] with a symbolic heap [prop].*)
let rec sym_exec
    ( {InterproceduralAnalysis.proc_desc= current_pdesc; analyze_dependency; err_log; tenv} as
      analysis_data ) instr_ (prop_ : Prop.normal Prop.t) path :
    (Prop.normal Prop.t * Paths.Path.t) list =
  AnalysisState.set_instr instr_ ;
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
    match instr_ with
    | Sil.Call (ret, exp, par, loc, call_flags) ->
        let exp' = Prop.exp_normalize_prop tenv prop_ exp in
        let instr' =
          match exp' with
          | Exp.Closure c ->
              let proc_exp = Exp.Const (Const.Cfun c.name) in
              let proc_exp' = Prop.exp_normalize_prop tenv prop_ proc_exp in
              let par' = List.map ~f:(fun (id_exp, _, typ, _) -> (id_exp, typ)) c.captured_vars in
              Sil.Call (ret, proc_exp', par' @ par, loc, call_flags)
          | Exp.Const (Const.Cfun callee_pname) when ObjCDispatchModels.is_model callee_pname -> (
            match ObjCDispatchModels.get_dispatch_closure_opt par with
            | Some (_cname, closure_exp, args) ->
                Sil.Call (ret, closure_exp, args, loc, call_flags)
            | None ->
                Sil.Call (ret, exp', par, loc, call_flags) )
          | _ ->
              Sil.Call (ret, exp', par, loc, call_flags)
        in
        instr'
    | _ ->
        instr_
  in
  let skip_call ?(is_objc_instance_method = false) ~reason prop path callee_pname ret_annots loc
      ret_id_typ ret_typ actual_args =
    let skip_res () =
      let exn = Exceptions.Skip_function (Localise.desc_skip_function callee_pname) in
      BiabductionReporting.log_issue_deprecated_using_state current_pdesc err_log exn ;
      L.d_printfln "Skipping function '%a': %s" Procname.pp callee_pname reason ;
      unknown_or_scan_call ~is_scan:false ~reason ret_typ ret_annots
        { Builtin.instr
        ; prop_= prop
        ; path
        ; ret_id_typ
        ; args= actual_args
        ; proc_name= callee_pname
        ; loc
        ; analysis_data }
    in
    if is_objc_instance_method then
      handle_objc_instance_method_call_or_skip current_pdesc tenv actual_args path callee_pname prop
        (fst ret_id_typ) skip_res
    else skip_res ()
  in
  let call_args prop_ proc_name args ret_id_typ loc =
    {Builtin.instr; prop_; path; ret_id_typ; args; proc_name; loc; analysis_data}
  in
  let root_type exp typ =
    match (exp : Exp.t) with
    | Lfield (_, f, _) ->
        Fieldname.get_class_name f |> Typ.mk_struct
    | _ ->
        typ
  in
  match instr with
  | Sil.Load {id; e= rhs_exp; typ; loc} ->
      let typ = root_type rhs_exp typ in
      execute_load analysis_data id rhs_exp typ loc prop_ |> ret_old_path
  | Sil.Store {e1= lhs_exp; typ; e2= rhs_exp; loc} ->
      let typ = root_type lhs_exp typ in
      execute_store analysis_data lhs_exp typ rhs_exp loc prop_ |> ret_old_path
  | Sil.Prune (cond, _, _, _) ->
      let prop__ = Attribute.nullify_exp_with_objc_null tenv prop_ cond in
      let n_cond, prop = check_arith_norm_exp analysis_data cond prop__ in
      ret_old_path (Propset.to_proplist (prune tenv ~positive:true n_cond prop))
  | Sil.Call (ret_id_typ, Exp.Const (Const.Cfun callee_pname), actual_params, loc, call_flags) -> (
    match Builtin.get callee_pname with
    | Some exec_builtin ->
        exec_builtin (call_args prop_ callee_pname actual_params ret_id_typ loc)
    | None -> (
      match callee_pname with
      | Java callee_pname_java -> (
          let norm_prop, norm_args' = normalize_params analysis_data prop_ actual_params in
          let norm_args = call_constructor_url_update_args callee_pname norm_args' in
          let exec_skip_call ~reason skipped_pname ret_annots ret_type =
            skip_call ~reason norm_prop path skipped_pname ret_annots loc ret_id_typ ret_type
              norm_args
          in
          let resolve_and_analyze_result =
            resolve_and_analyze ~has_clang_model:false analysis_data norm_prop norm_args
              callee_pname call_flags
          in
          let resolved_pname = resolve_and_analyze_result.resolved_pname in
          match resolve_and_analyze_result.resolved_summary_opt with
          | None ->
              let ret_typ = Procname.Java.get_return_typ callee_pname_java in
              let ret_annots = load_ret_annots callee_pname in
              exec_skip_call ~reason:"unknown method" resolved_pname ret_annots ret_typ
          | Some resolved_summary -> (
            match reason_to_skip ~callee_desc:(`Summary (resolved_pname, resolved_summary)) with
            | None ->
                proc_call resolved_pname resolved_summary
                  (call_args prop_ callee_pname norm_args ret_id_typ loc)
            | Some reason ->
                let ret_typ = Procname.Java.get_return_typ callee_pname_java in
                let ret_annots = load_ret_annots callee_pname in
                exec_skip_call ~reason resolved_pname ret_annots ret_typ ) )
      | CSharp callee_pname_csharp ->
          let norm_prop, norm_args = normalize_params analysis_data prop_ actual_params in
          let url_handled_args = call_constructor_url_update_args callee_pname norm_args in
          let resolved_pnames =
            resolve_virtual_pname tenv norm_prop url_handled_args callee_pname call_flags
          in
          let exec_one_pname pname =
            let exec_skip_call ~reason ret_annots ret_type =
              skip_call ~reason norm_prop path pname ret_annots loc ret_id_typ ret_type
                url_handled_args
            in
            match analyze_dependency pname with
            | Error _ ->
                let ret_typ = Procname.CSharp.get_return_typ callee_pname_csharp in
                let ret_annots = load_ret_annots callee_pname in
                exec_skip_call ~reason:"unknown method" ret_annots ret_typ
            | Ok callee_summary -> (
              match reason_to_skip ~callee_desc:(`Summary (callee_pname, callee_summary)) with
              | None ->
                  let handled_args = call_args norm_prop pname url_handled_args ret_id_typ loc in
                  proc_call callee_pname callee_summary handled_args
              | Some reason ->
                  let ret_typ = Procname.CSharp.get_return_typ callee_pname_csharp in
                  let ret_annots = load_ret_annots callee_pname in
                  exec_skip_call ~reason ret_annots ret_typ )
          in
          List.fold ~f:(fun acc pname -> exec_one_pname pname @ acc) ~init:[] resolved_pnames
      | _ ->
          (* Generic fun call with known name *)
          let prop_r, n_actual_params = normalize_params analysis_data prop_ actual_params in
          let resolve_and_analyze_result =
            resolve_and_analyze_clang analysis_data prop_r n_actual_params callee_pname call_flags
          in
          let resolved_pname = resolve_and_analyze_result.resolved_pname in
          let resolved_pdesc_opt = resolve_and_analyze_result.resolved_procdesc_opt in
          let resolved_summary_opt = resolve_and_analyze_result.resolved_summary_opt in
          Logging.d_printfln "Original callee %s" (Procname.to_unique_id callee_pname) ;
          Logging.d_printfln "Resolved callee %s" (Procname.to_unique_id resolved_pname) ;
          let sentinel_result =
            if Language.curr_language_is Clang then
              check_variadic_sentinel_if_present
                (call_args prop_r resolved_pname actual_params ret_id_typ loc)
            else [(prop_r, path)]
          in
          let do_call (prop, path) =
            let callee_desc =
              match (resolved_summary_opt, resolved_pdesc_opt) with
              | Some summary, _ ->
                  `Summary (resolved_pname, summary)
              | None, Some pdesc ->
                  `ProcDesc pdesc
              | None, None ->
                  `ProcName resolved_pname
            in
            match reason_to_skip ~callee_desc with
            | Some reason -> (
                let ret_annots = load_ret_annots resolved_pname in
                match resolved_pdesc_opt with
                | Some resolved_pdesc ->
                    let attrs = Procdesc.get_attributes resolved_pdesc in
                    let ret_type = attrs.ProcAttributes.ret_type in
                    let is_objc_instance_method =
                      ClangMethodKind.equal attrs.ProcAttributes.clang_method_kind
                        ClangMethodKind.OBJC_INSTANCE
                    in
                    skip_call ~is_objc_instance_method ~reason prop path resolved_pname ret_annots
                      loc ret_id_typ ret_type n_actual_params
                | None ->
                    skip_call ~reason prop path resolved_pname ret_annots loc ret_id_typ
                      (snd ret_id_typ) n_actual_params )
            | None ->
                proc_call resolved_pname
                  (Option.value_exn resolved_summary_opt)
                  (call_args prop resolved_pname n_actual_params ret_id_typ loc)
          in
          List.concat_map ~f:do_call sentinel_result ) )
  | Sil.Call (ret_id_typ, fun_exp, actual_params, loc, _) ->
      (* Call via function pointer *)
      let prop_r, n_actual_params = normalize_params analysis_data prop_ actual_params in
      Rearrange.check_dereference_error tenv current_pdesc prop_r fun_exp loc ;
      L.d_str "Unknown function pointer " ;
      Exp.d_exp fun_exp ;
      L.d_strln ", returning undefined value." ;
      let callee_pname = Procname.from_string_c_fun "__function_pointer__" in
      unknown_or_scan_call ~is_scan:false ~reason:"unresolved function pointer" (snd ret_id_typ)
        Annot.Item.empty
        { Builtin.analysis_data
        ; instr
        ; prop_= prop_r
        ; path
        ; ret_id_typ
        ; args= n_actual_params
        ; proc_name= callee_pname
        ; loc }
  | Sil.Metadata (Nullify (pvar, _)) -> (
      let eprop = Prop.expose prop_ in
      match
        List.partition_tf
          ~f:(function
            | Predicates.Hpointsto (Exp.Lvar pvar', _, _) -> Pvar.equal pvar pvar' | _ -> false )
          eprop.Prop.sigma
      with
      | [Predicates.Hpointsto (e, se, typ)], sigma' ->
          let sigma'' =
            let se' = execute_nullify_se se in
            Predicates.Hpointsto (e, se', typ) :: sigma'
          in
          let eprop_res = Prop.set eprop ~sigma:sigma'' in
          ret_old_path [Prop.normalize tenv eprop_res]
      | [], _ ->
          ret_old_path [prop_]
      | _ ->
          L.internal_error "Pvar %a appears on the LHS of >1 heap predicate!@." (Pvar.pp Pp.text)
            pvar ;
          assert false )
  | Sil.Metadata (Abstract _) ->
      if Prover.check_inconsistency tenv prop_ then ret_old_path []
      else
        ret_old_path
          [Abs.remove_redundant_array_elements analysis_data (Abs.abstract analysis_data prop_)]
  | Sil.Metadata (ExitScope (dead_vars, _)) ->
      let dead_ids = List.filter_map dead_vars ~f:Var.get_ident in
      ret_old_path [Prop.exist_quantify tenv dead_ids prop_]
  | Sil.Metadata
      (CatchEntry _ | EndBranches | Skip | TryEntry _ | TryExit _ | VariableLifetimeBegins _) ->
      ret_old_path [prop_]


and diverge prop path =
  State.add_diverging_states (Paths.PathSet.from_renamed_list [(prop, path)]) ;
  (* diverge *)
  []


(** Symbolic execution of a sequence of instructions. If errors occur and [mask_errors] is true,
    just treat as skip. *)
and instrs ?(mask_errors = false) analysis_data instrs ppl =
  let exe_instr instr (p, path) =
    L.d_str "Executing Generated Instruction " ;
    Sil.d_instr instr ;
    L.d_ln () ;
    try sym_exec analysis_data instr p path
    with exn ->
      IExn.reraise_if exn ~f:(fun () -> (not mask_errors) || not (Exception.exn_not_failure exn)) ;
      let error = Exceptions.recognize_exception exn in
      let loc =
        match error.ocaml_pos with
        | Some ocaml_pos ->
            "at " ^ L.ocaml_pos_to_string ocaml_pos
        | None ->
            ""
      in
      L.d_warning
        (F.sprintf "Generated Instruction Failed with: %s%s" error.issue_type.unique_id loc) ;
      L.d_ln () ;
      [(p, path)]
  in
  let f plist instr = List.concat_map ~f:(exe_instr instr) plist in
  Instrs.fold ~f ~init:ppl instrs


and add_constraints_on_actuals_by_ref tenv caller_pdesc prop actuals_by_ref callee_pname callee_loc
    =
  let add_actual_by_ref_to_footprint prop (actual, actual_typ, actual_index) =
    let abduced =
      match actual with
      | Exp.Lvar _ | Exp.Var _ ->
          Pvar.mk_abduced_ref_param callee_pname actual_index callee_loc
      | _ ->
          L.(die InternalError) "Unexpected variable expression %a" Exp.pp actual
    in
    let already_has_abduced_retval p =
      List.exists
        ~f:(fun hpred ->
          match hpred with
          | Predicates.Hpointsto (Exp.Lvar pv, _, _) ->
              Pvar.equal pv abduced
          | _ ->
              false )
        p.Prop.sigma_fp
    in
    (* prevent introducing multiple abduced retvals for a single call site in a loop *)
    if already_has_abduced_retval prop || is_rec_call callee_pname caller_pdesc then prop
    else if !BiabductionConfig.footprint then
      let prop', abduced_strexp =
        match actual_typ.Typ.desc with
        | Typ.Tptr (({desc= Tstruct _} as typ), _) ->
            (* for struct types passed by reference, do abduction on the fields of the
               struct *)
            add_struct_value_to_footprint tenv abduced typ prop
        | Typ.Tptr (typ, _) ->
            (* for pointer types passed by reference, do abduction directly on the pointer *)
            let prop', fresh_fp_var = add_to_footprint tenv abduced typ prop in
            (prop', Predicates.Eexp (fresh_fp_var, Predicates.Inone))
        | _ ->
            L.(die InternalError)
              "No need for abduction on non-pointer type %s" (Typ.to_string actual_typ)
      in
      let filtered_sigma =
        List.map
          ~f:(function
            | Predicates.Hpointsto (lhs, _, typ_exp) when Exp.equal lhs actual ->
                Predicates.Hpointsto (lhs, abduced_strexp, typ_exp)
            | hpred ->
                hpred )
          prop'.Prop.sigma
      in
      Prop.normalize tenv (Prop.set prop' ~sigma:filtered_sigma)
    else
      (* bind actual passed by ref to the abduced value pointed to by the synthetic pvar *)
      let prop' =
        let filtered_sigma =
          List.filter
            ~f:(function
              | Predicates.Hpointsto (lhs, _, _) when Exp.equal lhs actual -> false | _ -> true )
            prop.Prop.sigma
        in
        Prop.normalize tenv (Prop.set prop ~sigma:filtered_sigma)
      in
      List.fold
        ~f:(fun p hpred ->
          match hpred with
          | Predicates.Hpointsto (Exp.Lvar pv, rhs, texp) when Pvar.equal pv abduced ->
              let new_hpred = Predicates.Hpointsto (actual, rhs, texp) in
              Prop.normalize tenv (Prop.set p ~sigma:(new_hpred :: prop'.Prop.sigma))
          | _ ->
              p )
        ~init:prop' prop'.Prop.sigma
  in
  let non_const_actuals_by_ref =
    let is_not_const (e, _, i) =
      match Attributes.load callee_pname with
      | Some attrs ->
          let is_const = List.mem ~equal:Int.equal attrs.ProcAttributes.const_formals i in
          if is_const then (
            L.d_printf "Not havocing const argument number %d: " i ;
            Exp.d_exp e ;
            L.d_ln () ) ;
          not is_const
      | None ->
          true
    in
    List.filter ~f:is_not_const actuals_by_ref
  in
  List.fold ~f:add_actual_by_ref_to_footprint ~init:prop non_const_actuals_by_ref


(** execute a call for an unknown or scan function *)
and unknown_or_scan_call ~is_scan ~reason ret_typ ret_annots
    { Builtin.analysis_data= {proc_desc; tenv; _}
    ; prop_= pre
    ; path
    ; ret_id_typ
    ; args
    ; proc_name= callee_pname
    ; loc
    ; instr } =
  let remove_file_attribute prop =
    let do_exp p (e, _) =
      let do_attribute q atom =
        match atom with
        | Predicates.Apred ((Aresource {ra_res= Rfile} as res), _) ->
            Attribute.remove_for_attr tenv q res
        | _ ->
            q
      in
      List.fold ~f:do_attribute ~init:p (Attribute.get_for_exp tenv p e)
    in
    let filtered_args =
      match (args, instr) with
      | _ :: other_args, Sil.Call (_, _, _, _, {CallFlags.cf_virtual}) when cf_virtual ->
          (* Do not remove the file attribute on the reciver for virtual calls *)
          other_args
      | _ ->
          args
    in
    List.fold ~f:do_exp ~init:prop filtered_args
  in
  let should_abduce_param_value pname =
    let open Procname in
    match pname with
    | Java _ ->
        (* FIXME (T19882766): we need to disable this for Java because it breaks too many tests *)
        false
    | CSharp _ ->
        (* FIXME (T19882766): we need to disable this for Java because it breaks too many tests *)
        false
    | ObjC_Cpp cpp_name ->
        (* FIXME: we need to work around a frontend hack for std::shared_ptr
         * to silent some of the uninitialization warnings *)
        if
          String.is_suffix ~suffix:"_std__shared_ptr" (Procname.to_string callee_pname)
          (* Abduced parameters for the empty destructor body cause `Cannot star` *)
          || Procname.ObjC_Cpp.is_destructor cpp_name
        then false
        else true
    | _ ->
        true
  in
  let actuals_by_ref =
    List.filter_mapi
      ~f:(fun i actual ->
        match actual with
        | (Exp.Lvar _ as e), ({Typ.desc= Tptr _} as t) ->
            Some (e, t, i)
        | (Exp.Var _ as e), ({Typ.desc= Tptr _} as t) when should_abduce_param_value callee_pname ->
            Some (e, t, i)
        | _ ->
            None )
      args
  in
  let has_nonnull_annot = Annotations.ia_is_nonnull ret_annots in
  let pre_final =
    (* in Java, assume that skip functions close resources passed as params *)
    let pre_1 =
      if Procname.is_java callee_pname || Procname.is_csharp callee_pname then
        remove_file_attribute pre
      else pre
    in
    let pre_2 =
      (* TODO(jjb): Should this use the type of ret_id, or ret_type from the procedure type? *)
      add_constraints_on_retval tenv proc_desc pre_1
        (Exp.Var (fst ret_id_typ))
        ret_typ ~has_nonnull_annot callee_pname loc
    in
    add_constraints_on_actuals_by_ref tenv proc_desc pre_2 actuals_by_ref callee_pname loc
  in
  if is_scan (* if scan function, don't mark anything with undef attributes *) then
    [(Tabulation.remove_constant_string_class tenv pre_final, path)]
  else
    (* otherwise, add undefined attribute to retvals and actuals passed by ref *)
    let undefined_actuals_by_ref = List.map ~f:(fun (exp, _, _) -> exp) actuals_by_ref in
    let ret_exp = Exp.Var (fst ret_id_typ) in
    let prop_with_undef_attr =
      let path_pos = State.get_path_pos () in
      Attribute.mark_vars_as_undefined tenv pre_final ~ret_exp ~undefined_actuals_by_ref
        callee_pname ret_annots loc path_pos
    in
    let callee_loc_opt = Attributes.load callee_pname |> Option.map ~f:ProcAttributes.get_loc in
    let skip_path = Paths.Path.add_skipped_call path callee_pname reason callee_loc_opt in
    [(prop_with_undef_attr, skip_path)]


and check_variadic_sentinel ?(fails_on_nil = false) n_formals (sentinel, null_pos)
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; args; proc_name; loc} =
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
    let load_instr = Sil.Load {id= tmp_id_deref; e= lexp; typ; loc} in
    try instrs analysis_data (Instrs.singleton load_instr) result
    with e when Exception.exn_not_failure e ->
      IExn.reraise_if e ~f:(fun () -> fails_on_nil) ;
      let deref_str = Localise.deref_str_nil_argument_in_variadic_method proc_name nargs i in
      let err_desc =
        Errdesc.explain_dereference proc_name tenv ~use_buckets:true ~is_premature_nil:true
          deref_str prop_ loc
      in
      raise (Exceptions.Premature_nil_termination (err_desc, __POS__))
  in
  (* fold_left reverses the arguments back so that we report an *)
  (* error on the first premature nil argument *)
  List.fold ~f:check_allocated ~init:[(prop_, path)] non_terminal_argsi


and check_variadic_sentinel_if_present ({Builtin.prop_; path; proc_name} as builtin_args) =
  match Attributes.load proc_name with
  | Some callee_attributes -> (
    match callee_attributes.ProcAttributes.sentinel_attr with
    | Some sentinel ->
        let formals = callee_attributes.ProcAttributes.formals in
        check_variadic_sentinel (List.length formals) sentinel builtin_args
    | None ->
        [(prop_, path)] )
  | None ->
      [(prop_, path)]


(** Perform symbolic execution for a function call *)
and proc_call callee_pname callee_summary
    { Builtin.analysis_data= {tenv; proc_desc= caller_pdesc; _} as analysis_data
    ; prop_= pre
    ; path
    ; ret_id_typ
    ; args= actual_pars
    ; loc } =
  let callee_attributes = Attributes.load_exn callee_pname in
  check_inherently_dangerous_function analysis_data callee_pname ;
  let formal_types = List.map ~f:snd3 callee_attributes.ProcAttributes.formals in
  let rec comb actual_pars formal_types =
    match (actual_pars, formal_types) with
    | [], [] ->
        actual_pars
    | (e, t_e) :: etl', _ :: tl' ->
        (e, t_e) :: comb etl' tl'
    | _, [] ->
        Errdesc.warning_err (AnalysisState.get_loc_exn ())
          "likely use of variable-arguments function, or function prototype missing@." ;
        L.d_warning "likely use of variable-arguments function, or function prototype missing" ;
        L.d_ln () ;
        L.d_str "actual parameters: " ;
        Exp.d_list (List.map ~f:fst actual_pars) ;
        L.d_ln () ;
        L.d_str "formal parameters: " ;
        Typ.d_list formal_types ;
        L.d_ln () ;
        actual_pars
    | [], _ ->
        L.d_printfln "**** ERROR: Procedure %a mismatch in the number of parameters ****"
          Procname.pp callee_pname ;
        L.d_str "actual parameters: " ;
        Exp.d_list (List.map ~f:fst actual_pars) ;
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
  let actuals = comb actual_pars formal_types in
  (* In case we call an objc instance method we add an extra spec
     where the receiver is null and the semantics of the call is nop *)
  match (!Language.curr_language, callee_attributes.ProcAttributes.clang_method_kind) with
  | Language.Clang, ClangMethodKind.OBJC_INSTANCE ->
      handle_objc_instance_method_call actual_pars pre tenv (fst ret_id_typ) caller_pdesc
        callee_pname path (fun () ->
          Tabulation.exe_function_call analysis_data ~callee_attributes ~callee_pname
            ~callee_summary ~ret_id:(fst ret_id_typ) loc ~actuals pre path )
  | _ ->
      (* non-objective-c method call. Standard tabulation *)
      Tabulation.exe_function_call analysis_data ~callee_summary ~ret_id:(fst ret_id_typ)
        ~callee_pname ~callee_attributes loc ~actuals pre path


(** perform symbolic execution for a single prop, and check for junk *)
and sym_exec_wrapper ({InterproceduralAnalysis.tenv; _} as analysis_data) handle_exn instr
    ((prop : Prop.normal Prop.t), path) : Paths.PathSet.t =
  let prop_primed_to_normal p =
    (* Rename primed vars with fresh normal vars, and return them *)
    let ids_primed =
      Prop.free_vars p
      |> Sequence.filter ~f:Ident.is_primed
      |> Ident.hashqueue_of_sequence |> Ident.HashQueue.keys
    in
    let ids_primed_normal =
      List.map ~f:(fun id -> (id, Ident.create_fresh Ident.knormal)) ids_primed
    in
    let ren_sub =
      Predicates.subst_of_list
        (List.map ~f:(fun (id1, id2) -> (id1, Exp.Var id2)) ids_primed_normal)
    in
    let p' = Prop.normalize tenv (Prop.prop_sub ren_sub p) in
    let fav_normal = List.map ~f:snd ids_primed_normal in
    (p', fav_normal)
  in
  let prop_normal_to_primed fav_normal p =
    (* rename given normal vars to fresh primed *)
    if List.is_empty fav_normal then p else Prop.exist_quantify tenv fav_normal p
  in
  try
    let pre_process_prop p =
      let p', fav = if Sil.instr_is_auxiliary instr then (p, []) else prop_primed_to_normal p in
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
      (* Check for retain cycles after assignments and method calls *)
      ( match instr with
      | (Sil.Store _ | Sil.Call _) when !BiabductionConfig.footprint ->
          RetainCycles.report_cycle analysis_data p
      | _ ->
          () ) ;
      let node_has_abstraction node =
        let instr_is_abstraction = function Sil.Metadata (Abstract _) -> true | _ -> false in
        Instrs.exists ~f:instr_is_abstraction (ProcCfg.Exceptional.instrs node)
      in
      let curr_node = AnalysisState.get_node_exn () in
      match ProcCfg.Exceptional.Node.kind curr_node with
      | Procdesc.Node.Prune_node _ when not (node_has_abstraction curr_node) ->
          (* don't check for leaks in prune nodes, unless there is abstraction anyway,*)
          (* but force them into either branch *)
          p'
      | _ ->
          Abs.abstract_junk analysis_data p'
    in
    L.d_str "Instruction " ;
    Sil.d_instr instr ;
    L.d_ln () ;
    let prop', fav_normal = pre_process_prop prop in
    let res_list =
      BiabductionConfig.run_with_abs_val_equal_zero
        (* no exp abstraction during sym exe *)
          (fun () -> sym_exec analysis_data instr prop' path)
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
  with exn ->
    IExn.reraise_if exn ~f:(fun () ->
        (not !BiabductionConfig.footprint) || not (Exceptions.handle_exception exn) ) ;
    handle_exn exn ;
    (* calls State.mark_instr_fail *)
    Paths.PathSet.empty


(** {2 Lifted Abstract Transfer Functions} *)

let node handle_exn analysis_data proc_cfg (node : ProcCfg.Exceptional.Node.t)
    (pset : Paths.PathSet.t) : Paths.PathSet.t =
  let pname = Procdesc.get_proc_name (ProcCfg.Exceptional.proc_desc proc_cfg) in
  let exe_instr_prop instr p tr (pset1 : Paths.PathSet.t) =
    let pset2 =
      if
        Tabulation.prop_is_exn pname p
        && (not (Sil.instr_is_auxiliary instr))
        && not
             (Procdesc.Node.equal_nodekind
                (ProcCfg.Exceptional.Node.kind node)
                Procdesc.Node.exn_handler_kind )
        (* skip normal instructions if an exception was thrown, unless this is an exception
           handler node *)
      then (
        L.d_str "Skipping instr " ;
        Sil.d_instr instr ;
        L.d_strln " due to exception" ;
        Paths.PathSet.from_renamed_list [(p, tr)] )
      else sym_exec_wrapper analysis_data handle_exn instr (p, tr)
    in
    Paths.PathSet.union pset2 pset1
  in
  let exe_instr_pset pset instr =
    Paths.PathSet.fold (exe_instr_prop instr) pset Paths.PathSet.empty
  in
  Instrs.fold ~f:exe_instr_pset ~init:pset (ProcCfg.Exceptional.instrs node)
