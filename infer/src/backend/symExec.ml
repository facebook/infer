(*
* Copyright (c) 2009 - 2013 Monoidics ltd.
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Symbolic Execution *)

module L = Logging
module F = Format
open Utils

let rec idlist_assoc id = function
  | [] -> raise Not_found
  | (i, x):: l -> if Ident.equal i id then x else idlist_assoc id l

let rec fldlist_assoc fld = function
  | [] -> raise Not_found
  | (fld', x, a):: l -> if Sil.fld_equal fld fld' then x else fldlist_assoc fld l

let rec explist_assoc e = function
  | [] -> raise Not_found
  | (e', x):: l -> if Sil.exp_equal e e' then x else explist_assoc e l

let append_list_op list_op1 list_op2 =
  match list_op1, list_op2 with
  | None, _ -> list_op2
  | _, None -> list_op1
  | Some list1, Some list2 -> Some (list1@list2)

let reverse_list_op list_op =
  match list_op with
  | None -> None
  | Some list -> Some (list_rev list)

let rec unroll_type tenv typ off =
  match (typ, off) with
  | Sil.Tvar _, _ ->
      let typ' = Sil.expand_type tenv typ in
      unroll_type tenv typ' off
  | Sil.Tstruct (ftal, sftal, _, _, _, _, _), Sil.Off_fld (fld, _) ->
      begin
        try fldlist_assoc fld ftal
        with Not_found ->
            L.d_strln ".... Invalid Field Access ....";
            L.d_strln ("Fld : " ^ Ident.fieldname_to_string fld);
            L.d_str "Type : "; Sil.d_typ_full typ; L.d_ln ();
            raise (Exceptions.Bad_footprint (try assert false with Assert_failure x -> x))
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

(* This function has the same name the standard list_split in Utils.*)
(* Maybe it's better to change name as we open Utils.  *)
let list_split equal x xys =
  let (xy, xys') = list_partition (fun (x', _) -> equal x x') xys in
  match xy with
  | [] -> (xys', None)
  | [(x', y')] -> (xys', Some y')
  | _ -> assert false

(* Given a node, returns a list of pvar of blocks that have been nullified in the block *)
let get_nullified_block node =
  let null_blocks = list_flatten(list_map (fun i -> match i with
                | Sil.Nullify(pvar, _, true) when Sil.is_block_pvar pvar -> [pvar]
                | _ -> []) (Cfg.Node.get_instrs node)) in
  null_blocks

(* Given a proposition and an objc block checks whether by existentially quantifying *)
(* captured variables in the block we obtain a leak *)
let check_block_retain_cycle cfg tenv pname _prop block_nullified =
  let mblock = Sil.pvar_get_name block_nullified in
  let block_captured = (match Cfg.get_block_pdesc cfg mblock with
      | Some pd -> fst (Utils.list_split (Cfg.Procdesc.get_captured pd))
      | None -> []) in
  let _prop' = Cfg.remove_seed_captured_vars_block block_captured _prop in
  let _prop'' = Prop.prop_rename_fav_with_existentials _prop' in
  let _ = Abs.abstract_junk ~original_prop: _prop pname tenv _prop'' in
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
    footprint_part pdesc tenv p fp_root nullify_struct
    (root_lexp, strexp, typ) offlist (f: Sil.exp option -> Sil.exp) inst lookup_inst =
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
        | Sil.Ialloc -> !Sil.curr_language <> Sil.Java (* java allocation initializes with default values *)
        | Sil.Iinitial -> true
        | _ -> false in
      let is_hidden_field () =
        match State.get_instr () with
        | Some (Sil.Letderef (_, Sil.Lfield (_, fieldname, _), _, _)) ->
            Ident.fieldname_is_hidden fieldname
        | _ -> false in
      let inst_new = match inst with
        | Sil.Ilookup when inst_is_uninitialized inst_curr && not (is_hidden_field()) -> (* we are in a lookup of an uninitialized value *)
            lookup_inst := Some inst_curr;
            let alloc_attribute_opt =
              if inst_curr = Sil.Iinitial then None
              else Prop.get_resource_undef_attribute p root_lexp in
            let deref_str = Localise.deref_str_uninitialized alloc_attribute_opt in
            let err_desc = Errdesc.explain_memory_access deref_str p (State.get_loc ()) in
            let exn = (Exceptions.Uninitialized_value (err_desc, try assert false with Assert_failure x -> x)) in
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
        footprint_part pdesc tenv p fp_root nullify_struct
        (root_lexp, strexp, typ) offlist' f inst lookup_inst
  | (Sil.Off_fld (fld, _)):: offlist', Sil.Earray _ ->
      let offlist_new = Sil.Off_index(Sil.exp_zero) :: offlist in
      apply_offlist
        footprint_part pdesc tenv p fp_root nullify_struct
        (root_lexp, strexp, typ) offlist_new f inst lookup_inst
  | (Sil.Off_fld (fld, fld_typ)):: offlist', Sil.Estruct (fsel, inst') ->
      begin
        let typ' = Sil.expand_type tenv typ in
        let ftal, sftal, csu, nameo, supers, def_mthds, iann = match typ' with Sil.Tstruct (ftal, sftal, csu, nameo, supers, def_mthds, iann) -> ftal, sftal, csu, nameo, supers, def_mthds, iann | _ -> assert false in
        let t' = unroll_type tenv typ (Sil.Off_fld (fld, fld_typ)) in
        try
          let _, se' = list_find (fun fse -> Ident.fieldname_equal fld (fst fse)) fsel in
          let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist
              footprint_part pdesc tenv p fp_root nullify_struct
              (root_lexp, se', t') offlist' f inst lookup_inst in
          let replace_fse fse = if Sil.fld_equal fld (fst fse) then (fld, res_se') else fse in
          let res_se = Sil.Estruct (list_map replace_fse fsel, inst') in
          let replace_fta (f, t, a) = if Sil.fld_equal fld f then (fld, res_t', a) else (f, t, a) in
          let res_t = Sil.Tstruct (list_map replace_fta ftal, sftal, csu, nameo, supers, def_mthds, iann) in
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
        let typ' = Sil.expand_type tenv typ in
        let t', size' = match typ' with Sil.Tarray (t', size') -> (t', size') | _ -> assert false in
        try
          let idx_ese', se' = list_find (fun ese -> Prover.check_equal p nidx (fst ese)) esel in
          let res_e', res_se', res_t', res_pred_insts_op' =
            apply_offlist
              footprint_part pdesc tenv p fp_root nullify_struct
              (root_lexp, se', t') offlist' f inst lookup_inst in
          let replace_ese ese = if Sil.exp_equal idx_ese' (fst ese) then (idx_ese', res_se') else ese in
          let res_se = Sil.Earray(size, list_map replace_ese esel, inst1) in
          let res_t = Sil.Tarray(res_t', size') in
          (res_e', res_se, res_t, res_pred_insts_op')
        with Not_found -> (* return a nondeterministic value if the index is not found after rearrangement *)
            L.d_str "apply_offlist: index "; Sil.d_exp idx; L.d_strln " not materialized -- returning nondeterministic value";
            let res_e' = Sil.Var (Ident.create_fresh Ident.kprimed) in
            (res_e', strexp, typ, None)
      end
  | (Sil.Off_index idx):: offlist', _ ->
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
let ptsto_lookup footprint_part pdesc tenv p (lexp, se, typ, st) offlist id =
  let f =
    function Some exp -> exp | None -> Sil.Var id in
  let fp_root =
    match lexp with Sil.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let e', se', typ', pred_insts_op' =
    apply_offlist
      footprint_part pdesc tenv p fp_root false
      (lexp, se, typ) offlist f Sil.inst_lookup lookup_inst in
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
let ptsto_update footprint_part pdesc tenv p (lexp, se, typ, st) offlist exp =
  let f _ = exp in
  let fp_root =
    match lexp with Sil.Var id -> Ident.is_footprint id | _ -> false in
  let lookup_inst = ref None in
  let _, se', typ', pred_insts_op' =
    let pos = State.get_path_pos () in
    apply_offlist
      footprint_part pdesc tenv p fp_root true (lexp, se, typ)
      offlist f (State.get_inst_update pos) lookup_inst in
  let ptsto' = Prop.mk_ptsto lexp se' (Sil.Sizeof (typ', st)) in
  (ptsto', pred_insts_op')

let update_iter iter pi sigma =
  let iter' = Prop.prop_iter_update_current_by_list iter sigma in
  list_fold_left (Prop.prop_iter_add_atom false) iter' pi

let execute_letderef pdesc tenv id rhs_exp acc_in iter =
  let iter_ren = Prop.prop_iter_make_id_primed id iter in
  let prop_ren = Prop.prop_iter_to_prop iter_ren in
  match Prop.prop_iter_current iter_ren with
  | (Sil.Hpointsto(lexp, strexp, Sil.Sizeof (typ, st)), offlist) ->
      let contents, new_ptsto, pred_insts_op, lookup_uninitialized =
        ptsto_lookup false pdesc tenv prop_ren (lexp, strexp, typ, st) offlist id in
      let update acc (pi, sigma) =
        let pi' = Sil.Aeq (Sil.Var(id), contents):: pi in
        let sigma' = new_ptsto:: sigma in
        let iter' = update_iter iter_ren pi' sigma' in
        let prop' = Prop.prop_iter_to_prop iter' in
        let prop'' =
          if lookup_uninitialized
          then
            let check_attr_change att_old att_new = () in
            Prop.add_or_replace_exp_attribute check_attr_change prop' (Sil.Var id) (Sil.Adangling Sil.DAuninit)
          else prop' in
        prop'' :: acc in
      begin
        match pred_insts_op with
        | None -> update acc_in ([],[])
        | Some pred_insts -> list_rev (list_fold_left update acc_in pred_insts)
      end

  | (Sil.Hpointsto _, _) ->
      Errdesc.warning_err (State.get_loc ()) "no offset access in execute_letderef -- treating as skip@.";
      (Prop.prop_iter_to_prop iter_ren) :: acc_in
  (* The implementation of this case means that we
  ignore this dereferencing operator. When the analyzer treats
  numerical information and arrays more precisely later, we
  should change the implementation here. *)

  | _ -> assert false

let execute_set pdesc tenv rhs_exp acc_in iter =
  let (lexp, strexp, typ, st, offlist) =
    match Prop.prop_iter_current iter with
    | (Sil.Hpointsto(lexp, strexp, Sil.Sizeof (typ, st)), offlist) -> (lexp, strexp, typ, st, offlist)
    | _ -> assert false in
  let p = Prop.prop_iter_to_prop iter in
  let new_ptsto, pred_insts_op =
    ptsto_update false pdesc tenv p (lexp, strexp, typ, st) offlist rhs_exp in
  let update acc (pi, sigma) =
    let sigma' = new_ptsto:: sigma in
    let iter' = update_iter iter pi sigma' in
    let prop' = Prop.prop_iter_to_prop iter' in
    prop' :: acc in
  match pred_insts_op with
  | None -> update acc_in ([],[])
  | Some pred_insts -> list_fold_left update acc_in pred_insts

(** Module for builtin functions with their symbolic execution handler *)
module Builtin = struct
  type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list
  type sym_exe_builtin =
    Cfg.cfg -> Cfg.Procdesc.t -> Sil.instr -> Sil.tenv -> Prop.normal Prop.t -> Paths.Path.t ->
    Ident.t list -> (Sil.exp * Sil.typ) list -> Procname.t -> Sil.location -> ret_typ

  (* builtin function names for which we do symbolic execution *)
  let builtin_functions = Procname.Hash.create 4
  (* builtin plain function names: they match all the function names whose plain name is the given string *)
  let builtin_plain_functions = Hashtbl.create 4

  (* Check if the function is a builtin *)
  let is_registered name =
    Procname.Hash.mem builtin_functions name
    ||
    Hashtbl.mem builtin_plain_functions (Procname.to_string name)

  (* get the symbolic execution handler associated to the builtin function name *)
  let get_sym_exe_builtin name : sym_exe_builtin =
    try Procname.Hash.find builtin_functions name
    with Not_found ->
        try Hashtbl.find builtin_plain_functions (Procname.to_string name)
        with Not_found -> assert false

  (* register a builtin function name and symbolic execution handler *)
  let register proc_name_str (sym_exe_fun: sym_exe_builtin) =
    let proc_name = Procname.from_string proc_name_str in
    Procname.Hash.replace builtin_functions proc_name sym_exe_fun;
    proc_name

  (* register a builtin plain function name and symbolic execution handler *)
  let register_plain proc_name_str (sym_exe_fun: sym_exe_builtin) =
    let proc_name = Procname.from_string proc_name_str in
    Hashtbl.replace builtin_plain_functions proc_name_str sym_exe_fun;
    proc_name

  (* register a builtin [Procname.t] and symbolic execution handler *)
  let register_procname proc_name (sym_exe_fun: sym_exe_builtin) =
    Procname.Hash.replace builtin_functions proc_name sym_exe_fun

  (* register a builtin plain [Procname.t] and symbolic execution handler *)
  let register_plain_procname proc_name (sym_exe_fun: sym_exe_builtin) =
    Hashtbl.replace builtin_plain_functions (Procname.to_string proc_name) sym_exe_fun

  (** print the functions registered *)
  let pp_registered fmt () =
    let builtin_names = ref [] in
    Procname.Hash.iter (fun name _ -> builtin_names := name :: !builtin_names) builtin_functions;
    builtin_names := list_sort Procname.compare !builtin_names;
    let pp pname = Format.fprintf fmt "%a@\n" Procname.pp pname in
    Format.fprintf fmt "Registered builtins:@\n  @[";
    list_iter pp !builtin_names;
    Format.fprintf fmt "@]@."
end

(** print the builtin functions and exit *)
let print_builtins () =
  Builtin.pp_registered Format.std_formatter ();
  exit 0

(** Check if the function is a builtin *)
let function_is_builtin = Builtin.is_registered

(** Precondition: se should not include hpara_psto
that could mean nonempty heaps. *)
let rec execute_nullify_se = function
  | Sil.Eexp _ ->
      Sil.Eexp (Sil.exp_zero, Sil.inst_nullify)
  | Sil.Estruct (fsel, _) ->
      let fsel' = list_map (fun (fld, se) -> (fld, execute_nullify_se se)) fsel in
      Sil.Estruct (fsel', Sil.inst_nullify)
  | Sil.Earray (size, esel, inst) ->
      let esel' = list_map (fun (idx, se) -> (idx, execute_nullify_se se)) esel in
      Sil.Earray (size, esel', Sil.inst_nullify)

(** Do pruning for conditional [if (e1 != e2) ] if [positive] is true
and [(if (e1 == e2)] if [positive] is false *)
let prune_ne tenv positive e1 e2 prop =
  let is_inconsistent =
    if positive then Prover.check_equal prop e1 e2
    else Prover.check_disequal prop e1 e2 in
  if is_inconsistent then Propset.empty else
    let new_prop =
      if positive then Prop.conjoin_neq ~footprint: (!Config.footprint) e1 e2 prop
      else Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e2 prop
    in if Prover.check_inconsistency new_prop then Propset.empty
    else Propset.singleton new_prop

let rec prune_polarity tenv positive (condition : Sil.exp) (prop : Prop.normal Prop.t) = match condition with
  | Sil.Var _ | Sil.Lvar _ ->
      prune_ne tenv positive condition Sil.exp_zero prop
  | Sil.Const (Sil.Cint i) when Sil.Int.iszero i ->
      if positive then Propset.empty else Propset.singleton prop
  | Sil.Const (Sil.Cint _) | Sil.Sizeof _ | Sil.Const (Sil.Cstr _) | Sil.Const (Sil.Cclass _) ->
      if positive then Propset.singleton prop else Propset.empty
  | Sil.Const _ ->
      assert false
  | Sil.Cast (_, condition') ->
      prune_polarity tenv positive condition' prop
  | Sil.UnOp (Sil.LNot, condition', _) ->
      prune_polarity tenv (not positive) condition' prop
  | Sil.UnOp _ ->
      assert false
  | Sil.BinOp (Sil.Eq, e, Sil.Const (Sil.Cint i))
  | Sil.BinOp (Sil.Eq, Sil.Const (Sil.Cint i), e) when Sil.Int.iszero i && not (Sil.Int.isnull i) ->
      prune_polarity tenv (not positive) e prop
  | Sil.BinOp (Sil.Eq, e1, e2) ->
      prune_ne tenv (not positive) e1 e2 prop
  | Sil.BinOp (Sil.Ne, e, Sil.Const (Sil.Cint i))
  | Sil.BinOp (Sil.Ne, Sil.Const (Sil.Cint i), e) when Sil.Int.iszero i && not (Sil.Int.isnull i) ->
      prune_polarity tenv positive e prop
  | Sil.BinOp (Sil.Ne, e1, e2) ->
      prune_ne tenv positive e1 e2 prop
  | Sil.BinOp (Sil.Ge, e2, e1) | Sil.BinOp (Sil.Le, e1, e2) ->
  (* e1<=e2 Case. Encode it as (e1<=e2)=1  *)
      if Sil.exp_equal e1 e2 then
        if positive then Propset.singleton prop else Propset.empty
      else
        let e2_lt_e1 = Sil.BinOp (Sil.Lt, e2, e1) in (* e2 < e1 *)
        let e1_le_e2 = Sil.BinOp (Sil.Le, e1, e2) in (* e1 <= e2 *)
        let is_inconsistent =
          if positive then Prover.check_atom prop (Prop.mk_inequality e2_lt_e1) (* e2 < e1 *)
          else Prover.check_atom prop (Prop.mk_inequality e1_le_e2) (* e1 <= e2 *) in
        begin
          if is_inconsistent then
            Propset.empty
          else if positive then
            Propset.singleton
              (Prop.conjoin_eq ~footprint: (!Config.footprint) e1_le_e2 Sil.exp_one prop)
          else
            Propset.singleton
              (Prop.conjoin_eq ~footprint: (!Config.footprint) e2_lt_e1 Sil.exp_one prop)
        end
  | Sil.BinOp (Sil.Gt, e2, e1) | Sil.BinOp (Sil.Lt, e1, e2) ->
  (* e1 < e2 Case. Encode it as (e1<e2)=1  *)
      if Sil.exp_equal e1 e2 then
        if positive then Propset.empty else Propset.singleton prop
      else
        let e1_lt_e2 = Sil.BinOp(Sil.Lt, e1, e2) in (* e1 < e2 *)
        let e2_le_e1 = Sil.BinOp(Sil.Le, e2, e1) in (* e2 <= e1 *)
        let is_inconsistent =
          if positive then Prover.check_atom prop (Prop.mk_inequality e2_le_e1) (* e2 <= e1 *)
          else Prover.check_atom prop (Prop.mk_inequality e1_lt_e2) (* e1 < e2 *) in
        begin
          if is_inconsistent then
            Propset.empty
          else if positive then
            Propset.singleton
              (Prop.conjoin_eq ~footprint: (!Config.footprint) e1_lt_e2 Sil.exp_one prop)
          else
            Propset.singleton
              (Prop.conjoin_eq ~footprint: (!Config.footprint) e2_le_e1 Sil.exp_one prop)
        end
  | Sil.BinOp (Sil.LAnd, condition1, condition2) ->
      (if positive then prune_polarity_inter else prune_polarity_union) tenv positive condition1 condition2 prop
  | Sil.BinOp (Sil.LOr, condition1, condition2) ->
      (if positive then prune_polarity_union else prune_polarity_inter) tenv positive condition1 condition2 prop
  | Sil.BinOp _ | Sil.Lfield _ | Sil.Lindex _ ->
      prune_ne tenv positive condition Sil.exp_zero prop

and prune_polarity_inter tenv positive condition1 condition2 prop =
  let res = ref Propset.empty in
  let pset1 = prune_polarity tenv positive condition1 prop in
  let do_p p =
    res := Propset.union (prune_polarity tenv positive condition2 p) !res in
  Propset.iter do_p pset1;
  !res

and prune_polarity_union tenv positive condition1 condition2 prop =
  let pset1 = prune_polarity tenv positive condition1 prop in
  let pset2 = prune_polarity tenv positive condition2 prop in
  Propset.union pset1 pset2

let prune_prop tenv condition prop =
  match condition with
  | Sil.Const (Sil.Cint i) when Sil.Int.iszero i -> Propset.empty
  | Sil.Const (Sil.Cint _) -> Propset.singleton prop
  | _ -> prune_polarity tenv true condition prop

let dangerous_functions =
  let dangerous_list = ["gets"] in
  ref ((list_map Procname.from_string) dangerous_list)

let check_inherently_dangerous_function caller_pname callee_pname =
  if list_exists (Procname.equal callee_pname) !dangerous_functions then
    let exn = Exceptions.Inherently_dangerous_function (Localise.desc_inherently_dangerous_function callee_pname) in
    let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop caller_pname) in
    Reporting.log_warning caller_pname ~pre: pre_opt exn


let is_defined cfg pname =
  match Cfg.Procdesc.find_from_name cfg pname with
  | None -> false
  | Some pdesc -> Cfg.Procdesc.is_defined pdesc


let call_should_be_skipped callee_pname summary =
  (* skip all procedures in intra-precedural mode *)
  !Config.intraprocedural
  (* check skip flag *)
  || Specs.get_flag callee_pname proc_flag_skip <> None
  (* skip abstract methods *)
  || summary.Specs.attributes.Sil.is_abstract
  (* treat calls with no specs as skip functions in angelic mode *)
  || (!Config.angelic_execution && Specs.get_specs_from_payload summary == [])


let report_raise_memory_leak tenv msg hpred prop =
  L.d_strln msg;
  L.d_increase_indent 1;
  L.d_strln "PROP:";
  Prop.d_prop prop; L.d_ln ();
  L.d_strln "PREDICATE:";
  Prop.d_sigma [hpred];
  L.d_decrease_indent 1;
  L.d_ln ();
  let footprint_part = false in
  let resource = match Errdesc.hpred_is_open_resource prop hpred with
    | Some res -> res
    | None -> Sil.Rmemory Sil.Mmalloc in
  raise (Exceptions.Leak (footprint_part, prop, hpred, Errdesc.explain_leak tenv hpred prop None None, false, resource, try assert false with Assert_failure x -> x))

(** In case of constant string dereference, return the result immediately *)
let check_constant_string_dereference lexp =
  let string_lookup s n =
    let c = try Char.code (String.get s (Sil.Int.to_int n)) with Invalid_argument _ -> 0 in
    Sil.exp_int (Sil.Int.of_int c) in
  match lexp with
  | Sil.BinOp(Sil.PlusPI, Sil.Const (Sil.Cstr s), e)
  | Sil.Lindex (Sil.Const (Sil.Cstr s), e) ->
      let value = match e with
        | Sil.Const (Sil.Cint n) when Sil.Int.geq n Sil.Int.zero && Sil.Int.leq n (Sil.Int.of_int (String.length s)) ->
            string_lookup s n
        | _ -> Sil.exp_get_undefined false in
      Some value
  | Sil.Const (Sil.Cstr s) ->
      Some (string_lookup s Sil.Int.zero)
  | _ -> None

(** Normalize an expression and check for arithmetic problems *)
let exp_norm_check_arith pdesc prop exp =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  match Prop.find_arithmetic_problem (State.get_path_pos ()) prop exp with
  | Some (Prop.Div0 div), prop' ->
      let desc = Errdesc.explain_divide_by_zero div (State.get_node ()) (State.get_loc ()) in
      let exn = Exceptions.Divide_by_zero (desc, try assert false with Assert_failure x -> x) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn;
      Prop.exp_normalize_prop prop exp, prop'
  | Some (Prop.UminusUnsigned (e, typ)), prop' ->
      let desc = Errdesc.explain_unary_minus_applied_to_unsigned_expression e typ (State.get_node ()) (State.get_loc ()) in
      let exn = Exceptions.Unary_minus_applied_to_unsigned_expression (desc, try assert false with Assert_failure x -> x) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn;
      Prop.exp_normalize_prop prop exp, prop'
  | None, prop' -> Prop.exp_normalize_prop prop exp, prop'

(** Check if [cond] is testing for NULL a pointer already dereferenced *)
let check_already_dereferenced pname cond prop =
  let find_hpred lhs =
    try Some (list_find (function
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
  | Some (id, (n, pos)) ->
      let desc = Errdesc.explain_null_test_after_dereference (Sil.Var id) (State.get_node ()) n (State.get_loc ()) in
      let exn = (Exceptions.Null_test_after_dereference (desc, try assert false with Assert_failure x -> x)) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
      Reporting.log_warning pname ~pre: pre_opt exn
  | None -> ()

let run_with_abs_val_eq_zero f =
  let abs_val_old = !Config.abs_val in
  Config.abs_val := 0;
  let res = try f () with
    | exn ->
        Config.abs_val := abs_val_old;
        raise exn in
  Config.abs_val := abs_val_old;
  res

(** Check whether symbolic execution de-allocated a stack variable or a constant string, raising an exception in that case *)
let check_deallocate_static_memory prop_after =
  let check_deallocated_attribute = function
    | Sil.Lvar pv, Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra) when Sil.pvar_is_local pv || Sil.pvar_is_global pv ->
        let freed_desc = Errdesc.explain_deallocate_stack_var pv ra in
        raise (Exceptions.Deallocate_stack_variable freed_desc)
    | Sil.Const (Sil.Cstr s), Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra) ->
        let freed_desc = Errdesc.explain_deallocate_constant_string s ra in
        raise (Exceptions.Deallocate_static_memory freed_desc)
    | _ -> () in
  let exp_att_list = Prop.get_all_attributes prop_after in
  list_iter check_deallocated_attribute exp_att_list;
  prop_after

(** create a copy of a procdesc with a new proc name *)
let proc_desc_copy cfg pdesc pname pname' =
  if (Procname.equal pname pname') then pdesc
  else
    (match Cfg.Procdesc.find_from_name cfg pname' with
      | Some pdesc' -> pdesc'
      | None ->
          let open Cfg.Procdesc in
          create {
              cfg = cfg;
              name = pname';
              proc_attributes = Sil.copy_proc_attributes (get_attributes pdesc);
              is_defined = is_defined pdesc;
              ret_type = get_ret_type pdesc;
              formals = get_formals pdesc;
              locals = get_locals pdesc;
              captured = get_captured pdesc;
              loc = get_loc pdesc;
            })

let method_exists right_proc_name methods =
  if !Sil.curr_language = Sil.Java then
    list_exists (fun meth_name -> Procname.equal right_proc_name meth_name) methods
  else (* ObjC case *)
  Specs.summary_exists right_proc_name


let resolve_method tenv class_name proc_name =
  let found_class =
    let visited = ref Mangled.MangledSet.empty in
    let rec resolve class_name =
      visited := Mangled.MangledSet.add class_name !visited;
      let right_proc_name =
        if Procname.is_java proc_name then
          Procname.java_replace_class proc_name (Mangled.to_string class_name)
        else Procname.objc_replace_class proc_name (Mangled.to_string class_name) in
      let type_name = Sil.TN_csu (Sil.Class, class_name) in
      match Sil.tenv_lookup tenv type_name with
      | Some (Sil.Tstruct (_, _, Sil.Class, cls, super_classes, methods, iann)) ->
          if method_exists right_proc_name methods then
            Some right_proc_name
          else
            (match super_classes with
              | (Sil.Class, super_class):: interfaces ->
                  if not (Mangled.MangledSet.mem super_class !visited)
                  then resolve super_class
                  else None
              | _ -> None)
      | _ -> None in
    resolve class_name in
  match found_class with
  | None ->
      Logging.d_strln
        ("Couldn't find method in the hierarchy of type "^(Mangled.to_string class_name));
      proc_name
  | Some proc_name -> proc_name


let resolve_typename prop arg =
  let (arg_exp, _) = arg in
  let typexp_opt =
    let rec loop = function
      | [] -> None
      | Sil.Hpointsto(e, _, typexp) :: _ when Sil.exp_equal e arg_exp -> Some typexp
      | _ :: hpreds -> loop hpreds in
    loop (Prop.get_sigma prop) in
  match typexp_opt with
  | Some (Sil.Sizeof (Sil.Tstruct (_, _, Sil.Class, class_name_opt, _, _, _), _)) -> class_name_opt
  | _ -> None


(** If the dynamic type of the object calling a method is known, the method from the dynamic type
is called *)
let resolve_virtual_pname cfg tenv prop args pname : Procname.t =
  match args with
  | [] -> failwith "Expecting the first parameter to be the object expression"
  | obj_exp :: _ ->
      begin
        match resolve_typename prop obj_exp with
        | Some class_name -> resolve_method tenv class_name pname
        | _ -> pname
      end

(* let resolve_procname cfg tenv prop args pname : Procname.t = *)

(** recognize calls to shared_ptr procedures and re-direct them to infer procedures for modelling *)
let redirect_shared_ptr tenv cfg pname actual_params =
  let class_shared_ptr typ =
    try match Sil.expand_type tenv typ with
      | Sil.Tstruct (_, _, Sil.Class, Some cl_name, _, _, _) ->
          let name = Mangled.to_string cl_name in
          name = "shared_ptr" || name = "__shared_ptr"
      | t -> false
    with exn when exn_not_timeout exn -> false in
  (* We pattern match over some specific library function, *)
  (* so we make precise matching to distinghuis between *)
  (* references and pointers in C++ *)
  let ptr_to filter = function
    | Sil.Tptr (t, Sil.Pk_pointer)
    | Sil.Tptr (t, Sil.Pk_objc_weak)
    | Sil.Tptr (t, Sil.Pk_objc_unsafe_unretained)
    | Sil.Tptr (t, Sil.Pk_objc_autoreleasing) -> filter t
    | _ -> false in
  let ref_to filter = function
    | Sil.Tptr (t, Sil.Pk_reference) -> filter t
    | _ -> false in
  let ptr_to_shared_ptr typ = ptr_to class_shared_ptr typ in
  let ref_to_shared_ptr typ = ref_to class_shared_ptr typ in
  let ptr_to_something typ = ptr_to (fun _ -> true) typ in
  let pname' = match Procname.to_string pname, actual_params with
    | "shared_ptr", [(_, this_t); (_, t1)] when ptr_to_shared_ptr this_t && ptr_to_something t1 ->
        Procname.from_string "__infer_shared_ptr"
    | "shared_ptr", [(_, this_t); (_, t1)] when ptr_to_shared_ptr this_t && ref_to_shared_ptr t1 ->
        Procname.from_string "__infer_shared_ptr_ref"
    | "operator=", [(_, this_t); (_, t1)] when ptr_to_shared_ptr this_t && ref_to_shared_ptr t1 ->
        Procname.from_string "__infer_shared_ptr_eq"
    | "operator==", [(_, t1); (_, t2)] when ref_to_shared_ptr t1 && ref_to_shared_ptr t2 ->
        Procname.from_string "__infer_shared_ptr_eqeq"
    | ("operator->" | "operator*"),[(_, t1)] when ptr_to_shared_ptr t1 ->
        Procname.from_string "__infer_shared_ptr_arrow"
    | "~shared_ptr",[(_, t1)] ->
        Procname.from_string "__infer_shared_ptr_destructor"
    | _ -> pname in
  if Procname.equal pname pname' then pname
  else
    let found = Specs.summary_exists pname' in
    if found then
      match Cfg.Procdesc.find_from_name cfg pname with
      | None -> pname
      | Some pdesc ->
          let _pdesc = proc_desc_copy cfg pdesc pname pname' in
          pname'
    else pname

(** recognize calls to the constructor java.net.URL and splits the argument string to be only the protocol.  *)
let call_constructor_url_update_args tenv cfg pname actual_params =
  let url_pname = Procname.mangled_java ((Some "java.net"), "URL") None "<init>" [(Some "java.lang"), "String"] Procname.Non_Static in
  if (Procname.equal url_pname pname) then
    (match actual_params with
      | [this; (Sil.Const (Sil.Cstr s), atype)] ->
          let parts = Str.split (Str.regexp_string "://") s in
          (match parts with
            | frst:: parts ->
                if (frst = "http") || (frst = "ftp") || (frst = "https") || (frst = "mailto") || (frst = "jar") then
                  [this; (Sil.Const (Sil.Cstr frst), atype)]
                else actual_params
            | _ -> actual_params)
      | [this; _, atype] -> [this; (Sil.Const (Sil.Cstr "file"), atype)]
      | _ -> actual_params)
  else actual_params

(** Handles certain method calls in a special way *)
let handle_special_cases_call tenv cfg pname actual_params =
  if (!Sil.curr_language = Sil.Java) then
    pname, (call_constructor_url_update_args tenv cfg pname actual_params)
  else if (!Sil.curr_language = Sil.C_CPP) then
    (redirect_shared_ptr tenv cfg pname actual_params), actual_params
  else pname, actual_params

let handle_objc_method_call actual_pars actual_params pre tenv cfg ret_ids pdesc callee_pname loc path =
  let receiver_self receiver prop =
    list_exists (fun hpred ->
            match hpred with
            | Sil.Hpointsto (Sil.Lvar pv, Sil.Eexp (e, _), _) ->
                Sil.exp_equal e receiver && Sil.pvar_is_seed pv &&
                Sil.pvar_get_name pv = Mangled.from_string "self"
            | _ -> false) (Prop.get_sigma prop) in
  let path_description = "Message "^(Procname.to_simplified_string callee_pname)^" with receiver nil returns nil." in
  let receiver = (match actual_pars with
      | (e, _):: _ -> e
      | _ -> raise (Exceptions.Internal_error
              (Localise.verbatim_desc "In Objective-C instance method call there should be a receiver."))) in
  let is_receiver_null =
    match actual_pars with
    | (e, _):: _ when Sil.exp_equal e Sil.exp_zero || Option.is_some (Prop.get_objc_null_attribute pre e) -> true
    | _ -> false in
  let prop_null =
    match ret_ids with
    | [ret_id] -> Prop.conjoin_eq (Sil.Var ret_id) Sil.exp_zero pre
    | _ -> pre in
  let add_objc_null_attribute_or_nullify_result prop =
    match ret_ids with
    | [ret_id] ->
        (match Prop.find_equal_formal_path receiver prop with
          | Some info ->
              Prop.add_or_replace_exp_attribute (fun a1 a2 -> ()) prop (Sil.Var ret_id)
                (Sil.Aobjc_null info)
          | None -> Prop.conjoin_eq (Sil.Var ret_id) Sil.exp_zero prop)
    | _ -> prop in
  if is_receiver_null then  (* objective-c instance method with a null receiver just return objc_null(res) *)
  let path = Paths.Path.add_description path path_description in
  L.d_strln ("Object-C method " ^ Procname.to_string callee_pname^ " called with nil receiver. Returning 0/nil");
  (* We wish to nullify the result. However, in some cases, we want to add the attribute OBJC_NULL to it so that we *)
  (* can keep track of how this object became null, so that in a NPE we can separate it into a different error type *)
  [(add_objc_null_attribute_or_nullify_result pre, path)]
  else
    let res = Tabulation.exe_function_call tenv cfg ret_ids pdesc callee_pname loc actual_params pre path in
    let is_undef = match Prop.get_resource_undef_attribute pre receiver with
      | Some (Sil.Aundef _) -> true
      | _ -> false in
    if !Config.footprint && not is_undef then
      let res_null = (* returns: (objc_null(res) /\ receiver=0) or an empty list of results *)
        let is_receiver_self = receiver_self receiver pre in
        let pre_with_attr_or_null =
          if is_receiver_self then prop_null
          else add_objc_null_attribute_or_nullify_result pre in
        let propset = prune_ne tenv false receiver Sil.exp_zero pre_with_attr_or_null in
        if Propset.is_empty propset then []
        else
          let prop = list_hd (Propset.to_proplist propset) in
          let path =
            if is_receiver_self then path
            else Paths.Path.add_description path path_description in
          [(prop, path)] in
      res_null @ res
    else res (* Not known if receiver = 0 and not footprint. Standard tabulation *)

let normalize_params pdesc prop actual_params =
  let norm_arg (p, args) (e, t) =
    let e', p' = exp_norm_check_arith pdesc p e in
    (p', (e', t) :: args) in
  let prop, args = list_fold_left norm_arg (prop, []) actual_params in
  (prop, list_rev args)

(** Execute [instr] with a symbolic heap [prop].*)
let rec sym_exec cfg tenv pdesc _instr (_prop: Prop.normal Prop.t) path
: (Prop.normal Prop.t * Paths.Path.t) list =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  State.set_instr _instr; (* mark instruction last seen *)
  State.set_prop_tenv_pdesc _prop tenv pdesc; (* mark prop,tenv,pdesc last seen *)
  SymOp.pay(); (* pay one symop *)
  let ret_old_path pl = (* return the old path unchanged *)
    list_map (fun p -> (p, path)) pl in
  let instr = match _instr with
    | Sil.Call (ret, exp, par, loc, call_flags) ->
        let exp' = Prop.exp_normalize_prop _prop exp in
        let instr' = match exp' with
          | Sil.Const (Sil.Ctuple (e1 :: el)) -> (* closure: combine arguments to call *)
              let e1' = Prop.exp_normalize_prop _prop e1 in
              let par' = list_map (fun e -> (e, Sil.Tvoid)) el in
              Sil.Call (ret, e1', par' @ par, loc, call_flags)
          | _ ->
              Sil.Call (ret, exp', par, loc, call_flags) in
        instr'
    | _ -> _instr in
  match instr with
  | Sil.Letderef (id, rhs_exp, typ, loc) ->
      begin
        try
          let n_rhs_exp, prop = exp_norm_check_arith pdesc _prop rhs_exp in
          let n_rhs_exp' = Prop.exp_collapse_consecutive_indices_prop prop typ n_rhs_exp in
          match check_constant_string_dereference n_rhs_exp' with
          | Some value ->
              ret_old_path [Prop.conjoin_eq (Sil.Var id) value prop]
          | None ->
              let iter_list = Rearrange.rearrange pdesc tenv n_rhs_exp' typ prop loc in
              let prop_list =
                list_fold_left (execute_letderef pdesc tenv id n_rhs_exp') [] iter_list in
              ret_old_path (list_rev prop_list)
        with
        | Rearrange.ARRAY_ACCESS ->
            if (!Config.array_level = 0) then assert false
            else
              let undef = Sil.exp_get_undefined false in
              ret_old_path [Prop.conjoin_eq (Sil.Var id) undef _prop]
      end
  | Sil.Set (lhs_exp, typ, rhs_exp, loc) ->
      begin
        try
          let n_lhs_exp, _prop' = exp_norm_check_arith pdesc _prop lhs_exp in
          let n_rhs_exp, prop = exp_norm_check_arith pdesc _prop' rhs_exp in
          let prop = Prop.replace_objc_null prop n_lhs_exp n_rhs_exp in
          let n_lhs_exp' = Prop.exp_collapse_consecutive_indices_prop prop typ n_lhs_exp in
          let iter_list = Rearrange.rearrange pdesc tenv n_lhs_exp' typ prop loc in
          let prop_list = list_fold_left (execute_set pdesc tenv n_rhs_exp) [] iter_list in
          ret_old_path (list_rev prop_list)
        with
        | Rearrange.ARRAY_ACCESS ->
            if (!Config.array_level = 0) then assert false
            else ret_old_path [_prop]
      end
  | Sil.Prune (cond, loc, true_branch, ik) ->
      let check_condition_always_true_false () =
        let report_condition_always_true_false i =
          let skip_loop = match ik with
            | Sil.Ik_while | Sil.Ik_for -> not (Sil.Int.iszero i) (* skip wile(1) and for (;1;) *)
            | Sil.Ik_dowhile -> true (* skip do..while *)
            | Sil.Ik_land_lor -> true (* skip subpart of a condition obtained from compilation of && and || *)
            | _ -> false in
          true_branch && not skip_loop in
        match Prop.exp_normalize_prop Prop.prop_emp cond with
        | Sil.Const (Sil.Cint i) when report_condition_always_true_false i ->
            let node = State.get_node () in
            let desc = Errdesc.explain_condition_always_true_false i cond node loc in
            let exn = Exceptions.Condition_always_true_false (desc, not (Sil.Int.iszero i), try assert false with Assert_failure x -> x) in
            let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
            Reporting.log_warning pname ~pre: pre_opt exn
        | _ -> () in
      check_already_dereferenced pname cond _prop;
      check_condition_always_true_false ();
      let n_cond, prop = exp_norm_check_arith pdesc _prop cond in
      ret_old_path (Propset.to_proplist (prune_prop tenv n_cond prop))
  | Sil.Call (ret_ids, Sil.Const (Sil.Cfun fn), args, loc, call_flags) when function_is_builtin fn ->
      let sym_exe_builtin = Builtin.get_sym_exe_builtin fn in
      sym_exe_builtin cfg pdesc instr tenv _prop path ret_ids args fn loc
  | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname), actual_param, loc, call_flags) ->
  (** Generic fun call with known name *)
      let (prop_r, _n_actual_params) = normalize_params pdesc _prop actual_param in
      let fn, n_actual_params = handle_special_cases_call tenv cfg callee_pname _n_actual_params in
      let resolved_pname =
        if call_flags.Sil.cf_virtual then
          resolve_virtual_pname cfg tenv prop_r n_actual_params fn
        else fn in
      let ret_typ_opt =
        match Cfg.Procdesc.find_from_name cfg resolved_pname with
        | None -> None
        | Some pd -> Some (Cfg.Procdesc.get_ret_type pd) in
      let skip_call () =
        let exn = Exceptions.Skip_function (Localise.desc_skip_function callee_pname) in
        Reporting.log_info pname exn;
        L.d_strln
          ("Undefined function " ^ Procname.to_string callee_pname
            ^ ", returning undefined value.");
        (match Specs.get_summary pname with
          | None -> ()
          | Some summary ->
              Specs.CallStats.trace
                summary.Specs.stats.Specs.call_stats callee_pname loc
                (Specs.CallStats.CR_skip) !Config.footprint);
        call_unknown_or_scan
          false cfg pdesc tenv prop_r path
          ret_ids ret_typ_opt n_actual_params resolved_pname loc in
      begin
        match Specs.get_summary resolved_pname with
        | None -> skip_call ()
        | Some summary when call_should_be_skipped resolved_pname summary ->
            skip_call ()
        | Some summary ->
            sym_exec_call
              cfg pdesc tenv prop_r path ret_ids n_actual_params summary loc
      end
  | Sil.Call (ret_ids, fun_exp, actual_params, loc, call_flags) -> (** Call via function pointer *)
      let (prop_r, n_actual_params) = normalize_params pdesc _prop actual_params in
      if call_flags.Sil.cf_is_objc_block then
        Rearrange.check_call_to_objc_block_error pdesc prop_r fun_exp loc;
      Rearrange.check_dereference_error pdesc prop_r fun_exp loc;
      if call_flags.Sil.cf_noreturn then begin
        L.d_str "Unknown function pointer with noreturn attribute "; Sil.d_exp fun_exp; L.d_strln ", diverging.";
        execute_diverge prop_r path
      end else begin
        L.d_str "Unknown function pointer "; Sil.d_exp fun_exp; L.d_strln ", returning undefined value.";
        let callee_pname = Procname.from_string "__function_pointer__" in
        call_unknown_or_scan
          false cfg pdesc tenv prop_r path ret_ids None n_actual_params callee_pname loc
      end
  | Sil.Nullify (pvar, loc, deallocate) ->
      begin
        let eprop = Prop.expose _prop in
        match list_partition
          (function
            | Sil.Hpointsto (Sil.Lvar pvar', _, _) -> Sil.pvar_equal pvar pvar'
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
  | Sil.Abstract loc ->
      let node = State.get_node () in
      let blocks_nullified = get_nullified_block node in
      list_iter (check_block_retain_cycle cfg tenv pname _prop) blocks_nullified;
      if Prover.check_inconsistency _prop
      then
        ret_old_path []
      else
        ret_old_path [Abs.remove_redundant_array_elements pname tenv
            (Abs.abstract pname tenv _prop)]
  | Sil.Remove_temps (temps, loc) ->
      ret_old_path [Prop.exist_quantify (Sil.fav_from_list temps) _prop]
  | Sil.Declare_locals (ptl, loc) ->
      let sigma_locals =
        let add_None (x, y) = (x, Sil.Sizeof (y, Sil.Subtype.exact), None) in
        let fp_mode = !Config.footprint in
        Config.footprint := false; (* no footprint vars for locals *)
        let sigma_locals =
          list_map
            (Prop.mk_ptsto_lvar (Some tenv) Prop.Fld_init Sil.inst_initial)
            (list_map add_None ptl) in
        Config.footprint := fp_mode;
        sigma_locals in
      let sigma' = Prop.get_sigma _prop @ sigma_locals in
      let prop' = Prop.normalize (Prop.replace_sigma sigma' _prop) in
      ret_old_path [prop']
  | Sil.Stackop _ -> (* this should be handled at the propset level *)
      assert false
  | Sil.Goto_node (node_e, loc) ->
      let n_node_e, prop = exp_norm_check_arith pdesc _prop node_e in
      begin
        match n_node_e with
        | Sil.Const (Sil.Cint i) ->
            let node_id = Sil.Int.to_int i in
            State.set_goto_node node_id;
            [(prop, path)]
        | _ -> (* target not known, do nothing as the next nodes are set to the possible targets by the front-end *)
            [(prop, path)]
      end
and execute_diverge prop path =
  State.add_diverging_states (Paths.PathSet.from_renamed_list [(prop, path)]); (* diverge *)
  []

(** Like sym_exec but for generated instructions.
If errors occur and [mask_errors] is false, just treat as skip.*)
and sym_exec_generated mask_errors cfg tenv pdesc instrs ppl =
  let exe_instr instr (p, path) =
    L.d_str "Executing Generated Instruction "; Sil.d_instr instr; L.d_ln ();
    try sym_exec cfg tenv pdesc instr p path
    with exn when exn_not_timeout exn && mask_errors ->
        let err_name, _, ml_source, _ , _, _, _ = Exceptions.recognize_exception exn in
        let loc = (match ml_source with
            | Some (src, l, c) -> "at "^(src^" "^(string_of_int l))
            | None -> "") in
        L.d_warning ("Generated Instruction Failed with: " ^ (Localise.to_string err_name)^loc ); L.d_ln();
        [(p, path)] in
  let f plist instr = list_flatten (list_map (exe_instr instr) plist) in
  list_fold_left f ppl instrs

and add_to_footprint abducted_pv typ prop =
  let abducted_lvar = Sil.Lvar abducted_pv in
  let fresh_fp_var = Sil.Var (Ident.create_fresh Ident.kfootprint) in
  let lvar_pt_fpvar =
    let sizeof_exp = Sil.Sizeof (typ, Sil.Subtype.subtypes) in
    Prop.mk_ptsto abducted_lvar (Sil.Eexp (fresh_fp_var, Sil.Inone)) sizeof_exp in
  let sigma_fp = Prop.get_sigma_footprint prop in
  (Prop.normalize (Prop.replace_sigma_footprint (lvar_pt_fpvar :: sigma_fp) prop), fresh_fp_var)

and add_constraints_on_retval pdesc prop ret_ids ret_type_option callee_pname =
  if Procname.is_infer_undefined callee_pname then prop
  else
    match ret_ids, ret_type_option with
    | [ret_id], Some ret_typ ->
    (* To avoid obvious false positives, assume skip functions do not return null pointers *)
        let add_ret_non_null ret_id ret_typ prop =
          match ret_typ with
          | Sil.Tptr _ -> Prop.conjoin_neq (Sil.Var ret_id) Sil.exp_zero prop
          | _ -> prop in
        let is_rec_call pname = (* TODO: (t7147096) extend this to detect mutual recursion *)
          Procname.equal pname (Cfg.Procdesc.get_proc_name pdesc) in
        if !Config.angelic_execution && not (is_rec_call callee_pname) then
          (* introduce a fresh program variable to allow abduction on the return value *)
          let abducted_ret_pv = Sil.mk_pvar_abducted_ret callee_pname (State.get_loc ()) in
          let already_has_abducted_retval p =
            list_exists
              (fun hpred -> match hpred with
                    | Sil.Hpointsto (Sil.Lvar pv, _, _) -> Sil.pvar_equal pv abducted_ret_pv
                    | _ -> false)
              (Prop.get_sigma_footprint p) in
          (* prevent introducing multiple abducted retvals for a single call site in a loop *)
          if already_has_abducted_retval prop then prop
          else
          if !Config.footprint then
            let (prop', fresh_fp_var) = add_to_footprint abducted_ret_pv ret_typ prop in
            let prop'' = Prop.conjoin_eq ~footprint: true (Sil.Var ret_id) fresh_fp_var prop' in
            add_ret_non_null ret_id ret_typ prop''
          else
            (* find an hpred [abducted_pvar] |-> A in [prop] and add [exp] = A to prop *)
            let bind_exp_to_abducted_val exp_to_bind abducted_pvar prop =
              let bind_exp prop = function
                | Sil.Hpointsto (Sil.Lvar pv, Sil.Eexp (rhs, _), _)
                when Sil.pvar_equal pv abducted_pvar ->
                    Prop.conjoin_eq exp_to_bind rhs prop
                | _ -> prop in
              list_fold_left bind_exp prop (Prop.get_sigma prop) in
            (* bind return id to the abducted value pointed to by the pvar we introduced *)
            bind_exp_to_abducted_val (Sil.Var ret_id) abducted_ret_pv prop
        else add_ret_non_null ret_id ret_typ prop
    | _ -> prop

and add_constraints_on_actuals_by_ref prop actuals_by_ref callee_pname =
  (* replace an hpred of the form actual_var |-> _ with new_hpred in prop *)
  let replace_actual_hpred actual_var new_hpred prop =
    let sigma' =
      list_map
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
            Sil.mk_pvar_abducted_ref_param callee_pname actual_pv (State.get_loc ()) in
          let already_has_abducted_retval p =
            list_exists
              (fun hpred -> match hpred with
                    | Sil.Hpointsto (Sil.Lvar pv, _, _) -> Sil.pvar_equal pv abducted_ref_pv
                    | _ -> false)
              (Prop.get_sigma_footprint p) in
          (* prevent introducing multiple abducted retvals for a single call site in a loop *)
          if already_has_abducted_retval prop then prop
          else
          if !Config.footprint then
            let (prop', fresh_fp_var) =
              add_to_footprint abducted_ref_pv (Sil.typ_strip_ptr actual_typ) prop in
            (* replace [actual] |-> _ with [actual] |-> [fresh_fp_var] *)
            let filtered_sigma =
              list_map
                (function
                  | Sil.Hpointsto (lhs, _, typ_exp) when Sil.exp_equal lhs actual ->
                      Sil.Hpointsto (lhs, Sil.Eexp (fresh_fp_var, Sil.Inone), typ_exp)
                  | hpred -> hpred)
                (Prop.get_sigma prop') in
            Prop.normalize (Prop.replace_sigma filtered_sigma prop')
          else
            (* bind actual passed by ref to the abducted value pointed to by the synthetic pvar *)
            let prop' =
              let filtered_sigma =
                list_filter
                  (function
                    | Sil.Hpointsto (lhs, _, typ_exp) when Sil.exp_equal lhs actual ->
                        false
                    | _ -> true)
                  (Prop.get_sigma prop) in
              Prop.normalize (Prop.replace_sigma filtered_sigma prop) in
            list_fold_left
              (fun p hpred ->
                    match hpred with
                    | Sil.Hpointsto (Sil.Lvar pv, rhs, texp) when Sil.pvar_equal pv abducted_ref_pv ->
                        let new_hpred = Sil.Hpointsto (actual, rhs, texp) in
                        Prop.normalize (Prop.replace_sigma (new_hpred :: (Prop.get_sigma prop')) p)
                    | _ -> p)
              prop'
              (Prop.get_sigma prop')
      | _ -> assert false in
    list_fold_left add_actual_by_ref_to_footprint prop actuals_by_ref
  else
    (* non-angelic mode; havoc each var passed by reference by assigning it to a fresh id *)
    let havoc_actual_by_ref (actual, actual_typ) prop =
      let actual_pt_havocd_var =
        let havocd_var = Sil.Var (Ident.create_fresh Ident.kprimed) in
        let sizeof_exp = Sil.Sizeof (Sil.typ_strip_ptr actual_typ, Sil.Subtype.subtypes) in
        Prop.mk_ptsto actual (Sil.Eexp (havocd_var, Sil.Inone)) sizeof_exp in
      replace_actual_hpred actual actual_pt_havocd_var prop in
    list_fold_left (fun p var -> havoc_actual_by_ref var p) prop actuals_by_ref

(** execute a call for an unknown or scan function *)
and call_unknown_or_scan is_scan cfg pdesc tenv pre path
    ret_ids ret_type_option actual_pars callee_pname loc =
  let remove_file_attribute prop =
    let do_exp p (e, t) =
      let do_attribute q = function
        | Sil.Aresource res_action as res
        when res_action.Sil.ra_res = Sil.Rfile ->
            Prop.remove_attribute res q
        | _ -> q in
      list_fold_left do_attribute p (Prop.get_exp_attributes p e) in
    list_fold_left do_exp prop actual_pars in
  let actuals_by_ref =
    list_filter
      (function
        | Sil.Lvar _, _ -> true
        | _ -> false)
      actual_pars in
  (* in Java, assume that skip functions close resources passed as params *)
  let pre' = if !Sil.curr_language = Sil.Java then remove_file_attribute pre else pre in
  let pre'' = add_constraints_on_retval pdesc pre' ret_ids ret_type_option callee_pname in
  let pre''' = add_constraints_on_actuals_by_ref pre'' actuals_by_ref callee_pname in
  if is_scan (* if scan function, don't mark anything with undef attributes *)
  then [(Tabulation.remove_constant_string_class pre''', path)]
  else
    (* otherwise, add undefined attribute to retvals and actuals passed by ref *)
    let exps_to_mark =
      let ret_exps = list_map (fun ret_id -> Sil.Var ret_id) ret_ids in
      list_fold_left
        (fun exps_to_mark (exp, _) -> exp :: exps_to_mark) ret_exps actuals_by_ref in
    let path_pos = State.get_path_pos () in
    [(Prop.mark_vars_as_undefined pre''' exps_to_mark callee_pname loc path_pos, path)]

and sym_exe_check_variadic_sentinel cfg pdesc tenv prop path actual_params fails_on_nil (sentinel, null_pos) pname loc =
  let proc_attributes = Cfg.Procdesc.get_attributes pdesc in
  let args = if proc_attributes.Sil.is_objc_instance_method then
      (* we are calling an objc method so the first actual is the instance *)
      list_tl actual_params
    else
      actual_params in
  let nargs = list_length args in
  (* sentinels start counting from the last argument to the function *)
  let n = nargs - sentinel - 1 in
  let build_argsi (acc, i) a =
    if i = n then (acc, i +1)
    else ((a, i):: acc, i +1) in
  (* list_fold_left reverses the arguments *)
  let non_terminal_actuals_i = fst (list_fold_left build_argsi ([], 0) args) in
  let check_allocated result ((lexp, typ), i) =
    (* simulate a Letderef for [lexp] *)
    let tmp_id_deref = Ident.create_fresh Ident.kprimed in
    let letderef = Sil.Letderef (tmp_id_deref, lexp, typ, loc) in
    try
      sym_exec_generated false cfg tenv pdesc [letderef] result
    with e when exn_not_timeout e ->
        if not fails_on_nil then
          let deref_str = Localise.deref_str_nil_argument_in_variadic_method pname nargs i in
          let err_desc =
            Errdesc.explain_dereference ~use_buckets: true ~is_premature_nil: true
              deref_str prop loc in
          raise (Exceptions.Premature_nil_termination
              (err_desc, try assert false with Assert_failure x -> x))
        else
          raise e in
  (* list_fold_left reverses the arguments back so that we report an *)
  (* error on the first premature nil argument *)
  list_fold_left check_allocated [(prop, path)] non_terminal_actuals_i

(** Perform symbolic execution for a function call *)
and sym_exec_call cfg pdesc tenv pre path ret_ids actual_pars summary loc =
  let caller_pname = Cfg.Procdesc.get_proc_name pdesc in
  let callee_pname = Specs.get_proc_name summary in
  let ret_typ = Specs.get_ret_type summary in
  let check_return_value_ignored () = (* check if the return value of the call is ignored, and issue a warning *)
    let is_ignored = match ret_typ, ret_ids with
      | Sil.Tvoid, _ -> false
      | Sil.Tint _, _ when not (is_defined cfg callee_pname) ->
      (* if the proc returns Tint and is not defined, *)
      (* don't report ignored return value *)
          false
      | _, [] -> true
      | _, [id] -> Errdesc.id_is_assigned_then_dead (State.get_node ()) id
      | _ -> false in
    if is_ignored
    && Specs.get_flag callee_pname proc_flag_ignore_return = None then
      let err_desc = Localise.desc_return_value_ignored callee_pname loc in
      let exn = (Exceptions.Return_value_ignored (err_desc, try assert false with Assert_failure x -> x)) in
      let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop caller_pname) in
      Reporting.log_warning caller_pname ~pre: pre_opt exn in
  check_inherently_dangerous_function caller_pname callee_pname;
  begin
    let formal_types = list_map (fun (_, typ) -> typ) (Specs.get_formals summary) in
    let rec comb actual_pars formal_types =
      match actual_pars, formal_types with
      | [], [] -> actual_pars
      | (e, t_e):: etl', t:: tl' ->
          (e, if (!Config.Experiment.activate_subtyping_in_cpp || !Sil.curr_language = Sil.Java) then t_e else t) :: comb etl' tl'
      | _,[] ->
          if !Config.developer_mode then Errdesc.warning_err (State.get_loc ()) "likely use of variable-arguments function, or function prototype missing@.";
          L.d_warning "likely use of variable-arguments function, or function prototype missing"; L.d_ln();
          L.d_str "actual parameters: "; Sil.d_exp_list (list_map fst actual_pars); L.d_ln ();
          L.d_str "formal parameters: "; Sil.d_typ_list formal_types; L.d_ln ();
          actual_pars
      | [], _ ->
          L.d_str ("**** ERROR: Procedure " ^ Procname.to_string callee_pname);
          L.d_strln (" mismatch in the number of parameters ****");
          L.d_str "actual parameters: "; Sil.d_exp_list (list_map fst actual_pars); L.d_ln ();
          L.d_str "formal parameters: "; Sil.d_typ_list formal_types; L.d_ln ();
          raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x)) in
    let actual_params = comb actual_pars formal_types in
    (* Actual parameters are associated to their formal
    parameter type if there are enough formal parameters, and
    to their actual type otherwise. The latter case happens
    with variable - arguments functions *)
    check_return_value_ignored ();
    (* In case we call an objc instance method we add and extra spec *)
    (* were the receiver is null and the semantics of the call is nop*)
    if (!Sil.curr_language <> Sil.Java) && !Config.objc_method_call_semantics &&
    (Specs.get_attributes summary).Sil.is_objc_instance_method then
      handle_objc_method_call actual_pars actual_params pre tenv cfg ret_ids pdesc callee_pname loc path
    else  (* non-objective-c method call. Standard tabulation *)
    Tabulation.exe_function_call tenv cfg ret_ids pdesc callee_pname loc actual_params pre path
  end

(** perform symbolic execution for a single prop, and check for junk *)
and sym_exec_wrapper handle_exn cfg tenv pdesc instr ((prop: Prop.normal Prop.t), path)
: Paths.PathSet.t =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let prop_primed_to_normal p = (** Rename primed vars with fresh normal vars, and return them *)
    let fav = Prop.prop_fav p in
    Sil.fav_filter_ident fav Ident.is_primed;
    let ids_primed = Sil.fav_to_list fav in
    let ids_primed_normal =
      list_map (fun id -> (id, Ident.create_fresh Ident.knormal)) ids_primed in
    let ren_sub = Sil.sub_of_list (list_map (fun (id1, id2) -> (id1, Sil.Var id2)) ids_primed_normal) in
    let p' = Prop.normalize (Prop.prop_sub ren_sub p) in
    let fav_normal = Sil.fav_from_list (list_map snd ids_primed_normal) in
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
        list_exists instr_is_abstraction (Cfg.Node.get_instrs node) in
      let curr_node = State.get_node () in
      match Cfg.Node.get_kind curr_node with
      | Cfg.Node.Prune_node _ when not (node_has_abstraction curr_node) ->
      (* don't check for leaks in prune nodes, unless there is abstraction anyway, but force them into either branch *)
          p'
      | _ ->
          check_deallocate_static_memory (Abs.abstract_junk ~original_prop: p pname tenv p') in
    L.d_str "Instruction "; Sil.d_instr instr; L.d_ln ();
    let prop', fav_normal = pre_process_prop prop in
    let res_list = run_with_abs_val_eq_zero (* no exp abstraction during sym exe *)
        (fun () ->
              sym_exec cfg tenv pdesc instr prop' path) in
    let res_list_nojunk = list_map (fun (p, path) -> (post_process_result fav_normal p path, path)) res_list in
    let results = list_map (fun (p, path) -> (Prop.prop_rename_primed_footprint_vars p, path)) res_list_nojunk in
    L.d_strln "Instruction Returns";
    Propgraph.d_proplist prop (list_map fst results); L.d_ln ();
    State.mark_instr_ok ();
    Paths.PathSet.from_renamed_list results
  with exn when Exceptions.handle_exception exn && !Config.footprint ->
      handle_exn exn; (* calls State.mark_instr_fail *)
      if !Config.nonstop
      then (Paths.PathSet.from_renamed_list [(prop, path)]) (* in nonstop mode treat the instruction as skip *)
      else Paths.PathSet.empty

(** {2 Lifted Abstract Transfer Functions} *)

let lifted_sym_exec
    handle_exn cfg tenv pdesc (pset : Paths.PathSet.t) node (instrs : Sil.instr list)
: Paths.PathSet.t =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let exe_instr_prop instr p tr (pset1: Paths.PathSet.t) =
    let pset2 =
      if Tabulation.prop_is_exn pname p && not (Sil.instr_is_auxiliary instr)
      && Cfg.Node.get_kind node <> Cfg.Node.exn_handler_kind
      (* skip normal instructions if an exception was thrown, unless this is an exception handler node *)
      then
        begin
          L.d_str "Skipping instr "; Sil.d_instr instr; L.d_strln " due to exception";
          Paths.PathSet.from_renamed_list [(p, tr)]
        end
      else sym_exec_wrapper handle_exn cfg tenv pdesc instr (p, tr) in
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
  let pset', stack' = list_fold_left exe_instr_pset (pset, stack) instrs in
  if stack' != [] then assert false; (* final stack must be empty *)
  pset'

(* ============== START of ModelBuiltins ============== *)
module ModelBuiltins = struct
  (** This module contains models for the builtin functions supported *)

  let execute___no_op prop path: Builtin.ret_typ =
    [(prop, path)]

  (** model va_arg as always returning 0 *)
  let execute___builtin_va_arg cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp1, typ1); (lexp2, typ2); (lexp3, typ3)], _ ->
        let instr' = Sil.Set (lexp3, typ3, Sil.exp_zero, loc) in
        sym_exec_generated true cfg tenv pdesc [instr'] [(prop, path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let mk_empty_array size =
    Sil.Earray (size, [], Sil.inst_none)

  let extract_array_type typ =
    if (!Sil.curr_language = Sil.Java) then
      match typ with
      | Sil.Tptr ( Sil.Tarray (typ', _), _) -> Some typ'
      | _ -> None
    else
      match typ with
      | Sil.Tptr (typ', _) | Sil.Tarray (typ', _) ->
          Some typ'
      | _ -> None

  (** Return a result from a procedure call. *)
  let return_result e prop ret_ids =
    match ret_ids with
    | [ret_id] -> Prop.conjoin_eq e (Sil.Var ret_id) prop
    | _ -> prop

  let execute___get_array_size cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] when list_length ret_ids <= 1 ->
        let return_result_for_array_size e prop ret_ids = return_result e prop ret_ids in
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        begin
          try
            let hpred = list_find (function
                  | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
                  | _ -> false) (Prop.get_sigma prop) in
            match hpred with
            | Sil.Hpointsto(e, Sil.Earray(size, _, _), _) ->
                [(return_result_for_array_size size prop ret_ids, path)]
            | _ -> []
          with Not_found ->
              let otyp' = (extract_array_type typ) in
              match otyp' with
              | Some typ' ->
                  let size = Sil.Var(Ident.create_fresh Ident.kfootprint) in
                  let s = mk_empty_array size in
                  let hpred = Prop.mk_ptsto n_lexp s (Sil.Sizeof(Sil.Tarray(typ', size), Sil.Subtype.exact)) in
                  let sigma = Prop.get_sigma prop in
                  let sigma_fp = Prop.get_sigma_footprint prop in
                  let prop'= Prop.replace_sigma (hpred:: sigma) prop in
                  let prop''= Prop.replace_sigma_footprint (hpred:: sigma_fp) prop' in
                  let prop''= Prop.normalize prop'' in
                  [(return_result_for_array_size size prop'' ret_ids, path)]
              | _ -> []
        end
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___set_array_size cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ); (size, _)], [] ->
        let n_lexp, _prop' = exp_norm_check_arith pdesc _prop lexp in
        let n_size, prop = exp_norm_check_arith pdesc _prop' size in
        begin
          try
            let hpred, sigma' = list_partition (function
                  | Sil.Hpointsto(e, _, t) -> Sil.exp_equal e n_lexp
                  | _ -> false) (Prop.get_sigma prop) in
            match hpred with
            | [Sil.Hpointsto(e, Sil.Earray(_, esel, inst), t)] ->
                let hpred' = Sil.Hpointsto (e, Sil.Earray (n_size, esel, inst), t) in
                let prop' = Prop.replace_sigma (hpred':: sigma') prop in
                [(Prop.normalize prop', path)]
            | _ -> raise Not_found
          with Not_found ->
              match typ with
              | Sil.Tptr (typ', _) ->
                  let size_fp = Sil.Var(Ident.create_fresh Ident.kfootprint) in
                  let se = mk_empty_array n_size in
                  let se_fp = mk_empty_array size_fp in
                  let hpred = Prop.mk_ptsto n_lexp se (Sil.Sizeof(Sil.Tarray(typ', size), Sil.Subtype.exact)) in
                  let hpred_fp = Prop.mk_ptsto n_lexp se_fp (Sil.Sizeof(Sil.Tarray(typ', size_fp), Sil.Subtype.exact)) in
                  let sigma = Prop.get_sigma prop in
                  let sigma_fp = Prop.get_sigma_footprint prop in
                  let prop'= Prop.replace_sigma (hpred:: sigma) prop in
                  let prop''= Prop.replace_sigma_footprint (hpred_fp:: sigma_fp) prop' in
                  let prop''= Prop.normalize prop'' in
                  [(prop'', path)]
              | _ -> []
        end
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___print_value cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    L.err "__print_value: ";
    let do_arg (lexp, typ) =
      let n_lexp, _ = exp_norm_check_arith pdesc prop lexp in
      L.err "%a " (Sil.pp_exp pe_text) n_lexp in
    list_iter do_arg args;
    L.err "@.";
    [(prop, path)]

  let is_undefined_opt prop n_lexp =
    match Prop.get_resource_undef_attribute prop n_lexp with
    | Some (Sil.Aundef _) -> !Config.angelic_execution || !Config.optimistic_cast
    | _ -> false

  (** Creates an object in the heap with a given type, when the object is not known to be null or when it doesn't
  appear already in the heap. *)
  let create_type tenv n_lexp typ prop =
    let prop_type =
      try
        let _ = list_find (function
              | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
              | _ -> false) (Prop.get_sigma prop) in
        prop
      with Not_found ->
          let mhpred =
            match typ with
            | Sil.Tptr (typ', _) ->
                let sexp = Sil.Estruct ([], Sil.inst_none) in
                let typ'' = Sil.expand_type tenv typ' in
                let texp = Sil.Sizeof (typ'', Sil.Subtype.subtypes) in
                let hpred = Prop.mk_ptsto n_lexp sexp texp in
                Some hpred
            | Sil.Tarray (typ', _) ->
                let size = Sil.Var(Ident.create_fresh Ident.kfootprint) in
                let sexp = mk_empty_array size in
                let texp = Sil.Sizeof (typ, Sil.Subtype.subtypes) in
                let hpred = Prop.mk_ptsto n_lexp sexp texp in
                Some hpred
            | _ -> None in
          match mhpred with
          | Some hpred ->
              let sigma = Prop.get_sigma prop in
              let sigma_fp = Prop.get_sigma_footprint prop in
              let prop'= Prop.replace_sigma (hpred:: sigma) prop in
              let prop''=
                let has_normal_variables =
                  Sil.fav_exists (Sil.exp_fav n_lexp) Ident.is_normal in
                if (is_undefined_opt prop n_lexp) || has_normal_variables
                then prop'
                else Prop.replace_sigma_footprint (hpred:: sigma_fp) prop' in
              let prop''= Prop.normalize prop'' in
              prop''
          | None -> prop in
    let sil_is_null = Sil.BinOp (Sil.Eq, n_lexp, (Sil.exp_zero)) in
    let sil_is_nonnull = Sil.UnOp(Sil.LNot, sil_is_null, None) in
    let null_case = Propset.to_proplist (prune_prop tenv sil_is_null prop) in
    let non_null_case = Propset.to_proplist (prune_prop tenv sil_is_nonnull prop_type) in
    if ((list_length non_null_case) > 0) && (!Config.footprint) then
      non_null_case
    else if (is_undefined_opt prop n_lexp) then non_null_case
    else null_case@non_null_case

  let execute___get_type_of cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] when list_length ret_ids <= 1 ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        let props = create_type tenv n_lexp typ prop in
        let aux prop =
          begin
            try
              let hpred = list_find (function
                    | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
                    | _ -> false) (Prop.get_sigma prop) in
              match hpred with
              | Sil.Hpointsto(e, _, texp) ->
                  (return_result texp prop ret_ids), path
              | _ -> assert false
            with Not_found -> (return_result Sil.exp_zero prop ret_ids), path
          end in
        (list_map aux props)
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** replace the type of the ptsto rooted at [root_e] with [texp] in [prop] *)
  let replace_ptsto_texp prop root_e texp =
    let process_sigma sigma =
      let sigma1, sigma2 =
        list_partition (function
            | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e root_e
            | _ -> false) sigma in
      match sigma1 with
      | [Sil.Hpointsto(e, se, _)] -> (Sil.Hpointsto (e, se, texp)) :: sigma2
      | _ -> sigma in
    let sigma = Prop.get_sigma prop in
    let sigma_fp = Prop.get_sigma_footprint prop in
    let prop'= Prop.replace_sigma (process_sigma sigma) prop in
    let prop''= Prop.replace_sigma_footprint (process_sigma sigma_fp) prop' in
    Prop.normalize prop''

  let execute___instanceof_cast
      cfg pdesc instr tenv _prop path ret_ids args callee_pname loc instof
  : Builtin.ret_typ =
    match args with
    | [(_val1, typ1); (_texp2, typ2)] when list_length ret_ids <= 1 ->
        let val1, __prop = exp_norm_check_arith pdesc _prop _val1 in
        let texp2, prop = exp_norm_check_arith pdesc __prop _texp2 in
        let exe_one_prop prop =
          if Sil.exp_equal texp2 Sil.exp_zero then
            [(return_result Sil.exp_zero prop ret_ids, path)]
          else
            begin
              try
                let hpred = list_find (function
                      | Sil.Hpointsto(e1, _, _) -> Sil.exp_equal e1 val1
                      | _ -> false) (Prop.get_sigma prop) in
                match hpred with
                | Sil.Hpointsto(_, _, texp1) ->
                    let pos_type_opt, neg_type_opt = Prover.subtype_case_analysis tenv texp1 texp2 in
                    let mk_res type_opt res_e = match type_opt with
                      | None -> []
                      | Some texp1' ->
                          let prop' =
                            if Sil.exp_equal texp1 texp1' then prop
                            else replace_ptsto_texp prop val1 texp1' in
                          [(return_result res_e prop' ret_ids, path)] in
                    if (instof) then (* instanceof *)
                    begin
                      let pos_res = mk_res pos_type_opt Sil.exp_one in
                      let neg_res = mk_res neg_type_opt Sil.exp_zero in
                      pos_res @ neg_res
                    end
                    else (* cast *)
                    begin
                      if (!Config.footprint = true) then
                        begin
                          match pos_type_opt with
                          | None ->
                              Tabulation.raise_cast_exception
                                (try assert false with Assert_failure ml_loc -> ml_loc)
                                None texp1 texp2 val1
                          | Some texp1' -> (mk_res pos_type_opt val1)
                        end
                      else (* !Config.footprint = false *)
                      begin
                        match neg_type_opt with
                        | Some _ ->
                            if (is_undefined_opt prop val1) then (mk_res pos_type_opt val1)
                            else
                              Tabulation.raise_cast_exception
                                (try assert false with Assert_failure ml_loc -> ml_loc)
                                None texp1 texp2 val1
                        | None -> (mk_res pos_type_opt val1)
                      end
                    end
                | _ -> []
              with Not_found ->
                  [(return_result val1 prop ret_ids, path)]
            end in
        let props = create_type tenv val1 typ1 prop in
        list_flatten (list_map exe_one_prop props)
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___instanceof cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    (execute___instanceof_cast cfg pdesc instr tenv _prop path ret_ids args callee_pname loc true)

  let execute___cast cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    (execute___instanceof_cast cfg pdesc instr tenv _prop path ret_ids args callee_pname loc false)

  let set_resource_attribute prop path n_lexp loc ra_res =
    let prop' = match Prop.get_resource_undef_attribute prop n_lexp with
      | Some (Sil.Aresource (_ as ra)) ->
          let check_attr_change att_old att_new = () in
          Prop.add_or_replace_exp_attribute check_attr_change prop n_lexp (Sil.Aresource { ra with Sil.ra_res = ra_res })
      | _ ->
          ( let pname = Sil.mem_alloc_pname Sil.Mnew in
            let ra = { Sil.ra_kind = Sil.Racquire; Sil.ra_res = ra_res; Sil.ra_pname = pname; Sil.ra_loc = loc; Sil.ra_vpath = None } in
            let check_attr_change att_old att_new = () in
            Prop.add_or_replace_exp_attribute check_attr_change prop n_lexp (Sil.Aresource ra)) in
    [(prop', path)]

  (** Set the attibute of the value as file *)
  let execute___set_file_attribute cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        set_resource_attribute prop path n_lexp loc Sil.Rfile
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Set the attibute of the value as lock *)
  let execute___set_lock_attribute cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        set_resource_attribute prop path n_lexp loc Sil.Rlock
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Set the resource attribute of the first real argument of method as ignore, the first argument is assumed to be "this" *)
  let execute___method_set_ignore_attribute
      cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [_ ; (lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        set_resource_attribute prop path n_lexp loc Sil.Rignore
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Set the attibute of the value as memory *)
  let execute___set_mem_attribute cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        set_resource_attribute prop path n_lexp loc (Sil.Rmemory Sil.Mnew)
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Set the attibute of the value as tainted *)
  let execute___set_taint_attribute cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        let prop' = match Prop.get_taint_attribute prop n_lexp with
          | _ ->
              let check_attr_change att_old att_new = () in
              let p'= Prop.add_or_replace_exp_attribute check_attr_change prop n_lexp (Sil.Ataint) in
              p' in
        [(prop', path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Set the attibute of the value as untainted *)
  let execute___set_untaint_attribute cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        let prop' = match Prop.get_taint_attribute prop n_lexp with
          | _ ->
              let check_attr_change att_old att_new = () in
              let p'= Prop.add_or_replace_exp_attribute check_attr_change prop n_lexp (Sil.Auntaint) in
              p' in
        [(prop', path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** take a pointer to a struct, and return the value of a hidden field in the struct *)
  let execute___get_hidden_field cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] ->
        let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
        let ret_val = ref None in
        let return_val p = match !ret_val with
          | Some e -> return_result e p ret_ids
          | None -> p in
        let foot_var = lazy (Sil.Var (Ident.create_fresh Ident.kfootprint)) in
        let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
        let has_fld_hidden fsel = list_exists filter_fld_hidden fsel in
        let do_hpred in_foot hpred = match hpred with
          | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp) when Sil.exp_equal e n_lexp && (not (has_fld_hidden fsel)) ->
              let foot_e = Lazy.force foot_var in
              ret_val := Some foot_e;
              let se = Sil.Eexp(foot_e, Sil.inst_none) in
              let fsel' = (Ident.fieldname_hidden, se) :: fsel in
              Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
          | Sil.Hpointsto(e, Sil.Estruct (fsel, _), texp) when Sil.exp_equal e n_lexp && not in_foot && has_fld_hidden fsel ->
              let set_ret_val () =
                match list_find filter_fld_hidden fsel with
                | _, Sil.Eexp(e, _) -> ret_val := Some e
                | _ -> () in
              set_ret_val();
              hpred
          | _ -> hpred in
        let sigma' = list_map (do_hpred false) (Prop.get_sigma prop) in
        let sigma_fp' = list_map (do_hpred true) (Prop.get_sigma_footprint prop) in
        let prop' = Prop.replace_sigma_footprint sigma_fp' (Prop.replace_sigma sigma' prop) in
        let prop'' = return_val (Prop.normalize prop') in
        [(prop'', path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** take a pointer to a struct and a value, and set a hidden field in the struct to the given value *)
  let execute___set_hidden_field cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args with
    | [(lexp1, typ1); (lexp2, typ2)] ->
        let n_lexp1, _prop1 = exp_norm_check_arith pdesc _prop lexp1 in
        let n_lexp2, prop = exp_norm_check_arith pdesc _prop1 lexp2 in
        let foot_var = lazy (Sil.Var (Ident.create_fresh Ident.kfootprint)) in
        let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
        let has_fld_hidden fsel = list_exists filter_fld_hidden fsel in
        let do_hpred in_foot hpred = match hpred with
          | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp) when Sil.exp_equal e n_lexp1 && not in_foot ->
              let se = Sil.Eexp(n_lexp2, Sil.inst_none) in
              let fsel' = (Ident.fieldname_hidden, se) :: (list_filter (fun x -> not (filter_fld_hidden x)) fsel) in
              Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
          | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp) when Sil.exp_equal e n_lexp1 && in_foot && not (has_fld_hidden fsel) ->
              let foot_e = Lazy.force foot_var in
              let se = Sil.Eexp(foot_e, Sil.inst_none) in
              let fsel' = (Ident.fieldname_hidden, se) :: fsel in
              Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
          | _ -> hpred in
        let sigma' = list_map (do_hpred false) (Prop.get_sigma prop) in
        let sigma_fp' = list_map (do_hpred true) (Prop.get_sigma_footprint prop) in
        let prop' = Prop.replace_sigma_footprint sigma_fp' (Prop.replace_sigma sigma' prop) in
        let prop'' = Prop.normalize prop' in
        [(prop'', path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___state_untainted cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] when list_length ret_ids <= 1 ->
        (match ret_ids with
          | [ret_id] ->
              let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
              [(return_result (Sil.BinOp(Sil.Ne, n_lexp, (Sil.Const(Sil.Cattribute((Sil.Auntaint)))))) prop ret_ids, path)]
          | _ -> [(_prop, path)])
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (* Update the objective-c hidden counter by applying the operation op and the operand delta.*)
  (* Eg. op=+/- delta is an integer *)
  let execute___objc_counter_update
      suppress_npe_report op delta cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] ->
        let typ' = (match Sil.expand_type tenv typ with
            | Sil.Tstruct _ as s -> s
            | Sil.Tptr(t, _) -> Sil.expand_type tenv t
            | s' ->
                L.d_str ("Trying to update hidden field of not a struc. Type: "^(Sil.typ_to_string s'));
                assert false) in
        (* Assumes that lexp is a temp n$1 that has the value of the object. *)
        (* This is the case as a call f(o) it's translates as n$1=*&o; f(n$1) *)
        (* n$2 = *n$1.hidden *)
        let tmp = Ident.create_fresh Ident.knormal in
        let hidden_field = Sil.Lfield(lexp, Ident.fieldname_hidden, typ') in
        let counter_to_tmp = Sil.Letderef(tmp, hidden_field, typ', loc) in
        (* *n$1.hidden = (n$2 +/- delta) *)
        let update_counter = Sil.Set(hidden_field, typ', Sil.BinOp(op, Sil.Var tmp, Sil.Const (Sil.Cint delta)), loc) in
        let update_counter_instrs = [counter_to_tmp; update_counter; Sil.Remove_temps([tmp], loc)] in
        sym_exec_generated suppress_npe_report cfg tenv pdesc update_counter_instrs [(_prop, path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (* Given a list of args checks if the first is the flag indicating whether is a call to retain/release for which*)
  (* we have to suppress NPE report or not. If the flag is present it is removed from the list of args. *)
  let get_suppress_npe_flag args =
    match args with
    | (Sil.Const (Sil.Cint i), Sil.Tint Sil.IBool):: args' when Sil.Int.isone i ->
        false, args' (* this is a CFRelease/CFRetain *)
    | _ -> true, args

  let execute___objc_retain_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    let suppress_npe_report, args' = get_suppress_npe_flag args in
    match args' with
    | [(lexp, typ)] ->
        let prop = return_result lexp _prop ret_ids in
        execute___objc_counter_update suppress_npe_report (Sil.PlusA) (Sil.Int.one)
          cfg pdesc instr tenv prop path ret_ids args' callee_name loc
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___objc_retain cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    if !Config.objc_memory_model_on then
      execute___objc_retain_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc
    else execute___no_op _prop path

  let execute___objc_retain_cf cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    execute___objc_retain_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc

  let execute___objc_release_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    let suppress_npe_flag, args' = get_suppress_npe_flag args in
    execute___objc_counter_update suppress_npe_flag (Sil.MinusA) (Sil.Int.one)
      cfg pdesc instr tenv _prop path ret_ids args' callee_name loc

  let execute___objc_release cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    if !Config.objc_memory_model_on then
      execute___objc_release_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc
    else execute___no_op _prop path

  let execute___objc_release_cf cfg pdesc instr tenv _prop path ret_ids args callee_name loc
  : Builtin.ret_typ =
    execute___objc_release_impl cfg pdesc instr tenv _prop path ret_ids args callee_name loc

  (** Set the attibute of the value as objc autoreleased *)
  let execute___set_autorelease_attribute
      cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args, ret_ids with
    | [(lexp, typ)], _ ->
        let prop = return_result lexp _prop ret_ids in
        if !Config.objc_memory_model_on then
          let n_lexp, prop = exp_norm_check_arith pdesc prop lexp in
          let check_attr_change att_old att_new = () in
          let prop' = Prop.add_or_replace_exp_attribute check_attr_change prop n_lexp Sil.Aautorelease in
          [(prop', path)]
        else execute___no_op prop path
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (** Release all the objects in the pool *)
  let execute___release_autorelease_pool
      cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    if !Config.objc_memory_model_on then
      let autoreleased_objects = Prop.get_atoms_with_attribute Sil.Aautorelease _prop in
      let prop = Prop.remove_attribute Sil.Aautorelease _prop in
      let call_release res exp =
        match res with
        | (prop, path):: _ ->
            (try
              let hpred = list_find (function
                    | Sil.Hpointsto(e1, _, _) -> Sil.exp_equal e1 exp
                    | _ -> false) (Prop.get_sigma _prop) in
              match hpred with
              | Sil.Hpointsto(_, _, Sil.Sizeof (typ, st)) ->
                  let res1 =
                    execute___objc_release cfg pdesc instr tenv prop path ret_ids
                      [(exp, typ)] callee_pname loc in
                  res1
              | _ -> res
            with Not_found -> res)
        | [] -> res in
      list_fold_left call_release [(prop, path)] autoreleased_objects
    else execute___no_op _prop path

  let execute___objc_cast cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(_val1, typ1); (_texp2, typ2)] when list_length ret_ids <= 1 ->
        let val1, __prop = exp_norm_check_arith pdesc _prop _val1 in
        let texp2, prop = exp_norm_check_arith pdesc __prop _texp2 in
        (try
          let hpred = list_find (function
                | Sil.Hpointsto(e1, _, _) -> Sil.exp_equal e1 val1
                | _ -> false) (Prop.get_sigma prop) in
          match hpred, texp2 with
          | Sil.Hpointsto(val1, _, texp1), Sil.Sizeof (typ, st) ->
              let prop' = replace_ptsto_texp prop val1 texp2 in
              [(return_result val1 prop' ret_ids, path)]
          | _ -> [(return_result val1 prop ret_ids, path)]
        with Not_found -> [(return_result val1 prop ret_ids, path)])
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute_abort cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    raise (Exceptions.Precondition_not_found (Localise.verbatim_desc (Procname.to_string callee_pname), try assert false with Assert_failure x -> x))

  let execute_exit cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    execute_diverge prop path

  let _execute_free tenv mk loc acc iter =
    match Prop.prop_iter_current iter with
    | (Sil.Hpointsto(lexp, se, _), []) ->
        let prop = Prop.prop_iter_remove_curr_then_to_prop iter in
        let pname = Sil.mem_dealloc_pname mk in
        let ra = { Sil.ra_kind = Sil.Rrelease; Sil.ra_res = Sil.Rmemory mk; Sil.ra_pname = pname; Sil.ra_loc = loc; Sil.ra_vpath = None } in
        let p_res =
          Prop.add_or_replace_exp_attribute Tabulation.check_attr_dealloc_mismatch prop lexp (Sil.Aresource ra) in (* mark value as freed *)
        p_res :: acc
    | (Sil.Hpointsto _, o :: os) -> assert false (* alignment error *)
    | _ -> assert false (* should not happen *)

  let _execute_free_nonzero mk pdesc tenv instr prop path lexp typ loc =
    try
      begin
        match Prover.is_root prop lexp lexp with
        | None ->
            L.d_strln ".... Alignment Error: Freed a non root ....";
            assert false
        | Some _ ->
            let prop_list =
              list_fold_left (_execute_free tenv mk loc) []
                (Rearrange.rearrange pdesc tenv lexp typ prop loc) in
            list_rev prop_list
      end
    with Rearrange.ARRAY_ACCESS ->
        if (!Config.array_level = 0) then assert false
        else begin
          L.d_strln ".... Array containing allocated heap cells ....";
          L.d_str "  Instr: "; Sil.d_instr instr; L.d_ln ();
          L.d_str "  PROP: "; Prop.d_prop prop; L.d_ln ();
          raise (Exceptions.Array_of_pointsto (try assert false with Assert_failure x -> x))
        end

  let execute_free mk cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(lexp, typ)] ->
        begin
          let n_lexp, prop = exp_norm_check_arith pdesc _prop lexp in
          let prop_nonzero = (* case n_lexp!=0 *)
            Propset.to_proplist (prune_polarity tenv true n_lexp prop) in
          let prop_zero = (* case n_lexp==0 *)
            Propset.to_proplist (prune_polarity tenv false n_lexp prop) in
          let plist =
            prop_zero @ (* model: if 0 then skip else _execute_free_nonzero *)
            list_flatten (list_map (fun p ->
                        _execute_free_nonzero mk pdesc tenv instr p path
                          (Prop.exp_normalize_prop p lexp) typ loc) prop_nonzero) in
          list_map (fun p -> (p, path)) plist
        end
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute_alloc mk can_return_null cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    let rec evaluate_char_sizeof e = match e with
      | Sil.Var _ -> e
      | Sil.UnOp (uop, e', typ) ->
          Sil.UnOp (uop, evaluate_char_sizeof e', typ)
      | Sil.BinOp (bop, e1', e2') ->
          Sil.BinOp (bop, evaluate_char_sizeof e1', evaluate_char_sizeof e2')
      | Sil.Const _ | Sil.Cast _ | Sil.Lvar _ | Sil.Lfield _ | Sil.Lindex _ -> e
      | Sil.Sizeof (Sil.Tarray(Sil.Tint ik, size), _) when Sil.ikind_is_char ik ->
          evaluate_char_sizeof size
      | Sil.Sizeof _ -> e in
    let handle_sizeof_exp size_exp =
      Sil.Sizeof (Sil.Tarray (Sil.Tint Sil.IChar, size_exp), Sil.Subtype.exact) in
    let size_exp = match args with
      | [(size_exp, _)] -> (* for malloc and __new *)
          size_exp
      | [(num_obj, _); (base_exp, _)] -> (* for __new_array *)
          Sil.BinOp (Sil.Mult, num_obj, base_exp)
      | _ ->
          raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x)) in
    let ret_id = match ret_ids with
      | [ret_id] -> ret_id
      | _ -> Ident.create_fresh Ident.kprimed in
    let size_exp', prop =
      let n_size_exp, prop = exp_norm_check_arith pdesc _prop size_exp in
      let n_size_exp' = evaluate_char_sizeof n_size_exp in
      Prop.exp_normalize_prop prop n_size_exp', prop in
    let cnt_te = handle_sizeof_exp size_exp' in
    let id_new = Ident.create_fresh Ident.kprimed in
    let exp_new = Sil.Var id_new in
    let ptsto_new =
      Prop.mk_ptsto_exp (Some tenv) Prop.Fld_init (exp_new, cnt_te, None) Sil.Ialloc in
    let prop_plus_ptsto =
      let pname = Sil.mem_alloc_pname mk in
      let prop' = Prop.normalize (Prop.prop_sigma_star prop [ptsto_new]) in
      let ra = { Sil.ra_kind = Sil.Racquire; Sil.ra_res = Sil.Rmemory mk; Sil.ra_pname = pname; Sil.ra_loc = loc; Sil.ra_vpath = None } in
      let check_attr_change att_old att_new = () in
      Prop.add_or_replace_exp_attribute check_attr_change prop' exp_new (Sil.Aresource ra) in (* mark value as allocated *)
    let prop_alloc = Prop.conjoin_eq (Sil.Var ret_id) exp_new prop_plus_ptsto in
    if can_return_null then
      let prop_null = Prop.conjoin_eq (Sil.Var ret_id) Sil.exp_zero prop in
      [(prop_alloc, path); (prop_null, path)]
    else [(prop_alloc, path)]

  let execute_pthread_create cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [thread; attr; start_routine; arg] ->
        let routine_name = Prop.exp_normalize_prop prop (fst start_routine) in
        let routine_arg = Prop.exp_normalize_prop prop (fst arg) in
        (match routine_name, (snd start_routine) with
          | Sil.Lvar pvar, _ ->
              let fun_name = Sil.pvar_get_name pvar in
              let fun_string = Mangled.to_string fun_name in
              L.d_strln ("pthread_create: calling function " ^ fun_string);
              begin
                match Specs.get_summary (Procname.from_string fun_string) with
                | None -> assert false
                | Some callee_summary ->
                    sym_exec_call
                      cfg pdesc tenv prop path ret_ids [(routine_arg, snd arg)] callee_summary loc
              end
          | _ ->
              L.d_str "pthread_create: unknown function "; Sil.d_exp routine_name; L.d_strln ", skipping call.";
              [(prop, path)])
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute_skip cfg pdesc instr tenv prop path ret_ids args callee_pname loc : Builtin.ret_typ =
    [(prop, path)]

  let execute_scan_function
      skip_n_arguments cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | _ when list_length args >= skip_n_arguments ->
        let varargs = ref args in
        for i = 1 to skip_n_arguments do varargs := list_tl !varargs done;
        call_unknown_or_scan true cfg pdesc tenv prop path ret_ids None !varargs callee_pname loc
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute__unwrap_exception cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(ret_exn, _)] ->
        begin
          let n_ret_exn, prop = exp_norm_check_arith pdesc _prop ret_exn in
          match n_ret_exn with
          | Sil.Const (Sil.Cexn exp) ->
              let prop_with_exn = return_result exp prop ret_ids in
              [(prop_with_exn, path)]
          | _ -> assert false
        end
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute_return_first_argument cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | (_arg1, _):: _ ->
        let arg1, prop = exp_norm_check_arith pdesc _prop _arg1 in
        let prop' = return_result arg1 prop ret_ids in
        [(prop', path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___split_get_nth cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(lexp1, _); (lexp2, _); (lexp3, _)] ->
        let n_lexp1, prop = exp_norm_check_arith pdesc _prop lexp1 in
        let n_lexp2, prop = exp_norm_check_arith pdesc _prop lexp2 in
        let n_lexp3, prop = exp_norm_check_arith pdesc _prop lexp3 in
        (match n_lexp1, n_lexp2, n_lexp3 with
          | Sil.Const (Sil.Cstr str1), Sil.Const (Sil.Cstr str2), Sil.Const (Sil.Cint n_sil) ->
              (let n = Sil.Int.to_int n_sil in
                try
                  let parts = Str.split (Str.regexp_string str2) str1 in
                  let n_part = list_nth parts n in
                  let res = Sil.Const (Sil.Cstr n_part) in
                  [(return_result res prop ret_ids, path)]
                with Not_found -> assert false)
          | _ -> [(prop, path)])
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  let execute___create_tuple cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    let el = list_map fst args in
    let res = Sil.Const (Sil.Ctuple el) in
    [(return_result res prop ret_ids, path)]

  let execute___tuple_get_nth cfg pdesc instr tenv _prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    match args with
    | [(lexp1, _); (lexp2, _)] ->
        let n_lexp1, _prop' = exp_norm_check_arith pdesc _prop lexp1 in
        let n_lexp2, prop = exp_norm_check_arith pdesc _prop' lexp2 in
        (match n_lexp1, n_lexp2 with
          | Sil.Const (Sil.Ctuple el), Sil.Const (Sil.Cint i) ->
              let n = Sil.Int.to_int i in
              let en = list_nth el n in
              [(return_result en prop ret_ids, path)]
          | _ -> [(prop, path)])
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (* forces the expression passed as parameter to be assumed true at the point where this
  builtin is called, blocks if this causes an inconsistency *)
  let execute___infer_assume
      cfg pdesc instr tenv prop path ret_ids args callee_pname loc: Builtin.ret_typ =
    match args with
    | [(lexp, typ)] ->
        let prop_assume = Prop.conjoin_eq lexp (Sil.exp_bool true) prop in
        if Prover.check_inconsistency prop_assume then execute_diverge prop_assume path
        else [(prop_assume, path)]
    | _ -> raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x))

  (* creates a named error state *)
  let execute___infer_fail cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    let error_str =
      match args with
      | [(lexp_msg, _)] ->
          begin
            match Prop.exp_normalize_prop prop lexp_msg with
            | Sil.Const (Sil.Cstr str) -> str
            | _ -> assert false
          end
      | _ ->
          raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x)) in
    let set_instr =
      Sil.Set (Sil.Lvar Sil.global_error, Sil.Tvoid, Sil.Const (Sil.Cstr error_str), loc) in
    sym_exec_generated true cfg tenv pdesc [set_instr] [(prop, path)]

  (* translate builtin assertion failure *)
  let execute___assert_fail cfg pdesc instr tenv prop path ret_ids args callee_pname loc
  : Builtin.ret_typ =
    let error_str =
      match args with
      | l when list_length l = 4 ->
          Config.default_failure_name
      | _ ->
          raise (Exceptions.Wrong_argument_number (try assert false with Assert_failure x -> x)) in
    let set_instr =
      Sil.Set (Sil.Lvar Sil.global_error, Sil.Tvoid, Sil.Const (Sil.Cstr error_str), loc) in
    sym_exec_generated true cfg tenv pdesc [set_instr] [(prop, path)]

  let _ = Builtin.register "__method_set_ignore_attribute" execute___method_set_ignore_attribute
  let _ = Builtin.register "__builtin_va_arg" execute___builtin_va_arg (* model va_arg *)
  let _ = Builtin.register "__builtin_va_copy" execute_skip (** NOTE: __builtin_va_copy should have been handled in the translation already (see frontend.ml) *)
  let _ = Builtin.register "__builtin_va_end" execute_skip (* model va_end as skip *)
  let _ = Builtin.register "__builtin_va_start" execute_skip (** NOTE: __builtin_va_start should have been handled in the translation already (see frontend.ml) *)
  let __create_tuple = Builtin.register "__create_tuple" execute___create_tuple (* create a tuple value from the arguments *)
  let __delete = Builtin.register "__delete" (execute_free Sil.Mnew) (* like free *)
  let __delete_array = Builtin.register "__delete_array" (execute_free Sil.Mnew_array) (* like free *)
  let __exit = Builtin.register "_exit" execute_exit (* _exit from C library *)
  let __get_array_size = Builtin.register "__get_array_size" execute___get_array_size (* return the size of the array passed as a parameter *)
  let _ = Builtin.register "__get_hidden_field" execute___get_hidden_field   (* return the value of a hidden field in the struct *)
  let __get_type_of = Builtin.register "__get_type_of" execute___get_type_of (* return the get the type of the allocated object passed as a parameter *)
  let _ = Builtin.register "__infer_set_flag" execute_skip (** NOTE: __infer_set_flag should have been handled in the translation already (see frontend.ml) *)
  let __instanceof = Builtin.register "__instanceof" execute___instanceof (** [__instanceof(val,typ)] implements java's [val instanceof typ] *)
  let __cast = Builtin.register "__cast" execute___cast (** [__cast(val,typ)] implements java's [typ(val)] *)
  let __new = Builtin.register "__new" (execute_alloc Sil.Mnew false) (* like malloc, but always succeeds *)
  let __new_array = Builtin.register "__new_array" (execute_alloc Sil.Mnew_array false) (* like malloc, but always succeeds *)
  let __objc_alloc = Builtin.register "__objc_alloc" (execute_alloc Sil.Mobjc true) (* Objective C alloc *)
  let __objc_alloc_no_fail = Builtin.register "__objc_alloc_no_fail" (execute_alloc Sil.Mobjc false) (* like __objc_alloc, but does not return nil *)
  let __placement_delete = Builtin.register "__placement_delete" execute_skip (* placement delete is skip *)
  let __placement_new = Builtin.register "__placement_new" execute_return_first_argument (* placement new returns the first argument *)
  let _ = Builtin.register "__print_value" execute___print_value (* print a value as seen by the engine *)
  let __set_array_size = Builtin.register "__set_array_size" execute___set_array_size (* set the size of the array passed as a parameter *)
  let __set_file_attribute = Builtin.register "__set_file_attribute" execute___set_file_attribute (* set the attribute of the parameter as file *)
  let __set_lock_attribute = Builtin.register "__set_lock_attribute" execute___set_lock_attribute (* set the attribute of the parameter as file *)
  let __set_mem_attribute = Builtin.register "__set_mem_attribute" execute___set_mem_attribute (* set the attribute of the parameter as memory *)
  let __set_autorelease_attribute = Builtin.register "__set_autorelease_attribute" execute___set_autorelease_attribute (* set the attribute of the parameter as autorelease *)
  let __objc_release_autorelease_pool = Builtin.register "__objc_release_autorelease_pool" execute___release_autorelease_pool (* set the attribute of the parameter as autorelease *)
  let __split_get_nth = Builtin.register "__split_get_nth" execute___split_get_nth (* splits a string given a separator and returns the nth string *)

  (* builtin function to externally create new errors *)
  let __infer_fail = Builtin.register "__infer_fail" execute___infer_fail
  let __assert_fail = Builtin.register "__assert_fail" execute___assert_fail

  let _ = Builtin.register "__set_hidden_field" execute___set_hidden_field  (* set a hidden field in the struct to the given value *)
  let _ = Builtin.register "__set_taint_attribute" execute___set_taint_attribute (* set the attribute of the parameter as tainted *)
  let _ = Builtin.register "__set_untaint_attribute" execute___set_untaint_attribute (* set the attribute of the parameter as tainted *)
  let _ = Builtin.register "__state_untainted" execute___state_untainted (* check if the parameter is tainted *)
  let __objc_retain = Builtin.register "__objc_retain" execute___objc_retain (* objective-c "retain" *)
  let __objc_release = Builtin.register "__objc_release" execute___objc_release (* objective-c "release" *)
  let __objc_retain_cf = Builtin.register "__objc_retain_cf" execute___objc_retain_cf (* objective-c "retain" *)
  let __objc_release_cf = Builtin.register "__objc_release_cf" execute___objc_release_cf (* objective-c "release" *)
  let __objc_cast = Builtin.register "__objc_cast" execute___objc_cast (* objective-c "cast" *)
  let _ = Builtin.register "__throw" execute_skip (** NOTE: __throw should have been handled in the translation already (see frontend.ml) *)
  let __tuple_get_nth = Builtin.register "__tuple_get_nth" execute___tuple_get_nth (* return the nth element of a tuple *)
  let __unwrap_exception = Builtin.register "__unwrap_exception" execute__unwrap_exception (* the correct function to unwrapp execption remains to be written *)
  let __infer_assume = Builtin.register "__infer_assume" execute___infer_assume
  let _ = Builtin.register "abort" execute_abort (* abort from C library *)
  let _ = Builtin.register "exit" execute_exit (* exit from C library *)
  let _ = Builtin.register "free" (execute_free Sil.Mmalloc) (* free from C library, requires allocated memory *)
  let _ = Builtin.register "fscanf" (execute_scan_function 2) (* fscanf from C library *)
  let _ = Builtin.register "fwscanf" (execute_scan_function 2) (* vsscanf from C library *)
  let _ = Builtin.register "malloc" (execute_alloc Sil.Mmalloc true) (* malloc from C library *)
  let malloc_no_fail = Builtin.register "malloc_no_fail" (execute_alloc Sil.Mmalloc false) (* malloc from ObjC library *)
  let _ = Builtin.register "pthread_create" execute_pthread_create (* register execution handler for pthread_create *)
  let _ = Builtin.register "scanf" (execute_scan_function 1) (* scanf from C library *)
  let _ = Builtin.register "sscanf" (execute_scan_function 2) (* sscanf from C library *)
  let _ = Builtin.register "swscanf" (execute_scan_function 2) (* vsscanf from C library *)
  let _ = Builtin.register "vfscanf" (execute_scan_function 2) (* vfwscanf from C library *)
  let _ = Builtin.register "vfwscanf" (execute_scan_function 2) (* vsscanf from C library *)
  let _ = Builtin.register "vscanf" (execute_scan_function 1) (* vscanf from C library *)
  let _ = Builtin.register "vsscanf" (execute_scan_function 2) (* vsscanf from C library *)
  let _ = Builtin.register "vswscanf" (execute_scan_function 2) (* vsscanf from C library *)
  let _ = Builtin.register "vwscanf" (execute_scan_function 1) (* vsscanf from C library *)
  let _ = Builtin.register "wscanf" (execute_scan_function 1) (* vsscanf from C library *)

  let execute_objc_alloc_no_fail cfg pdesc tenv symb_state ret_ids typ loc =
    let alloc_fun = Sil.Const (Sil.Cfun __objc_alloc_no_fail) in
    let ptr_typ = Sil.Tptr (typ, Sil.Pk_pointer) in
    let sizeof_typ = Sil.Sizeof (typ, Sil.Subtype.exact) in
    let alloc_instr = Sil.Call (ret_ids, alloc_fun, [sizeof_typ, ptr_typ], loc, Sil.cf_default) in
    sym_exec_generated false cfg tenv pdesc [alloc_instr] symb_state

  let execute_objc_NSArray_alloc_no_fail cfg pdesc tenv symb_state ret_ids loc =
    let nsarray_typ = Sil.Tvar (Sil.TN_csu (Sil.Class, Mangled.from_string "NSArray")) in
    let nsarray_typ = Sil.expand_type tenv nsarray_typ in
    execute_objc_alloc_no_fail cfg pdesc tenv symb_state ret_ids nsarray_typ loc

  let execute_NSArray_arrayWithObjects_count cfg pdesc instr tenv prop path ret_ids args callee_pname loc =
    let res' = sym_exe_check_variadic_sentinel cfg pdesc tenv prop path args true (0,1) callee_pname loc in
    execute_objc_NSArray_alloc_no_fail cfg pdesc tenv res' ret_ids loc

  let execute_NSArray_arrayWithObjects cfg pdesc instr tenv prop path ret_ids args callee_pname loc =
    let res' = sym_exe_check_variadic_sentinel cfg pdesc tenv prop path args false (0,1) callee_pname loc in
    execute_objc_NSArray_alloc_no_fail cfg pdesc tenv res' ret_ids loc

  let _ =
    Builtin.register_procname
      (Procname.mangled_objc "NSArray" "arrayWithObjects:count:")
      execute_NSArray_arrayWithObjects_count
  let _ =
    Builtin.register_procname
      (Procname.mangled_objc "NSArray" "arrayWithObjects:")
      execute_NSArray_arrayWithObjects
end
(* ============== END of ModelBuiltins ============== *)
