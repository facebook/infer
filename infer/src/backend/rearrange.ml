(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Re-arrangement and extension of structures with fresh variables *)

module L = Logging
module F = Format

let list_product l1 l2 =
  let l1' = IList.rev l1 in
  let l2' = IList.rev l2 in
  IList.fold_left
    (fun acc x -> IList.fold_left (fun acc' y -> (x, y):: acc') acc l2')
    [] l1'

let rec list_rev_and_concat l1 l2 =
  match l1 with
  | [] -> l2
  | x1:: l1' -> list_rev_and_concat l1' (x1:: l2)

(** Check whether the index is out of bounds.
    If the size is - 1, no check is performed.
    If the index is provably out of bound, a bound error is given.
    If the size is a constant and the index is not provably in bound, a warning is given.
*)
let check_bad_index pname p size index loc =
  let size_is_constant = match size with
    | Sil.Const _ -> true
    | _ -> false in
  let index_provably_out_of_bound () =
    let index_too_large = Prop.mk_inequality (Sil.BinOp(Sil.Le, size, index)) in
    let index_negative = Prop.mk_inequality (Sil.BinOp(Sil.Le, index, Sil.exp_minus_one)) in
    (Prover.check_atom p index_too_large) || (Prover.check_atom p index_negative) in
  let index_provably_in_bound () =
    let size_minus_one = Sil.BinOp(Sil.PlusA, size, Sil.exp_minus_one) in
    let index_not_too_large = Prop.mk_inequality (Sil.BinOp(Sil.Le, index, size_minus_one)) in
    let index_nonnegative = Prop.mk_inequality (Sil.BinOp(Sil.Le, Sil.exp_zero, index)) in
    Prover.check_zero index || (* index 0 always in bound, even when we know nothing about size *)
    ((Prover.check_atom p index_not_too_large) && (Prover.check_atom p index_nonnegative)) in
  let index_has_bounds () =
    match Prover.get_bounds p index with
    | Some _, Some _ -> true
    | _ -> false in
  let get_const_opt = function
    | Sil.Const (Sil.Cint n) -> Some n
    | _ -> None in
  if not (index_provably_in_bound ()) then
    begin
      let size_const_opt = get_const_opt size in
      let index_const_opt = get_const_opt index in
      if index_provably_out_of_bound () then
        let deref_str = Localise.deref_str_array_bound size_const_opt index_const_opt in
        let exn =
          Exceptions.Array_out_of_bounds_l1
            (Errdesc.explain_array_access deref_str p loc, __POS__) in
        let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
        Reporting.log_warning pname ~pre: pre_opt exn
      else if size_is_constant then
        let deref_str = Localise.deref_str_array_bound size_const_opt index_const_opt in
        let desc = Errdesc.explain_array_access deref_str p loc in
        let exn = if index_has_bounds ()
          then Exceptions.Array_out_of_bounds_l2 (desc, __POS__)
          else Exceptions.Array_out_of_bounds_l3 (desc, __POS__) in
        let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
        Reporting.log_warning pname ~pre: pre_opt exn
    end

(** Perform bounds checking *)
let bounds_check pname prop size e =
  if !Config.trace_rearrange then
    begin
      L.d_str "Bounds check index:"; Sil.d_exp e;
      L.d_str " size: "; Sil.d_exp size;
      L.d_ln()
    end;
  check_bad_index pname prop size e

let rec create_struct_values pname tenv orig_prop footprint_part kind max_stamp t
    (off: Sil.offset list) inst : Sil.atom list * Sil.strexp * Sil.typ =
  if !Config.trace_rearrange then
    begin
      L.d_increase_indent 1;
      L.d_strln "entering create_struct_values";
      L.d_str "typ: "; Sil.d_typ_full t; L.d_ln ();
      L.d_str "off: "; Sil.d_offset_list off; L.d_ln (); L.d_ln ()
    end;
  let new_id () =
    incr max_stamp;
    Ident.create kind !max_stamp in
  let res =
    match t, off with
    | Sil.Tstruct _, [] ->
        ([], Sil.Estruct ([], inst), t)
    | Sil.Tstruct ({ Sil.instance_fields; static_fields } as struct_typ ),
      (Sil.Off_fld (f, _)):: off' ->
        let _, t', _ =
          try
            IList.find (fun (f', _, _) -> Ident.fieldname_equal f f')
              (instance_fields @ static_fields)
          with Not_found ->
            raise (Exceptions.Bad_footprint __POS__) in
        let atoms', se', res_t' =
          create_struct_values
            pname tenv orig_prop footprint_part kind max_stamp t' off' inst in
        let se = Sil.Estruct ([(f, se')], inst) in
        let replace_typ_of_f (f', t', a') = if Ident.fieldname_equal f f' then (f, res_t', a') else (f', t', a') in
        let instance_fields' =
          IList.sort Sil.fld_typ_ann_compare (IList.map replace_typ_of_f instance_fields) in
        (atoms', se, Sil.Tstruct { struct_typ with Sil.instance_fields = instance_fields'})
    | Sil.Tstruct _, (Sil.Off_index e):: off' ->
        let atoms', se', res_t' =
          create_struct_values
            pname tenv orig_prop footprint_part kind max_stamp t off' inst in
        let e' = Sil.array_clean_new_index footprint_part e in
        let size = Sil.exp_get_undefined false in
        let se = Sil.Earray (size, [(e', se')], inst) in
        let res_t = Sil.Tarray (res_t', size) in
        (Sil.Aeq(e, e'):: atoms', se, res_t)
    | Sil.Tarray(_, size),[] ->
        ([], Sil.Earray(size, [], inst), t)
    | Sil.Tarray(t', size'), (Sil.Off_index e) :: off' ->
        bounds_check pname orig_prop size' e (State.get_loc ());

        let atoms', se', res_t' =
          create_struct_values
            pname tenv orig_prop footprint_part kind max_stamp t' off' inst in
        let e' = Sil.array_clean_new_index footprint_part e in
        let se = Sil.Earray(size', [(e', se')], inst) in
        let res_t = Sil.Tarray(res_t', size') in
        (Sil.Aeq(e, e'):: atoms', se, res_t)
    | Sil.Tarray _, (Sil.Off_fld _) :: _ ->
        assert false

    | Sil.Tint _, [] | Sil.Tfloat _, [] | Sil.Tvoid, [] | Sil.Tfun _, [] | Sil.Tptr _, [] ->
        let id = new_id () in
        ([], Sil.Eexp (Sil.Var id, inst), t)
    | Sil.Tint _, [Sil.Off_index e] | Sil.Tfloat _, [Sil.Off_index e]
    | Sil.Tvoid, [Sil.Off_index e]
    | Sil.Tfun _, [Sil.Off_index e] | Sil.Tptr _, [Sil.Off_index e] ->
        (* In this case, we lift t to the t array. *)
        let t' = match t with
          | Sil.Tptr(t', _) -> t'
          | _ -> t in
        let size = Sil.Var (new_id ()) in

        let atoms', se', res_t' =
          create_struct_values
            pname tenv orig_prop footprint_part kind max_stamp t' [] inst in
        let e' = Sil.array_clean_new_index footprint_part e in
        let se = Sil.Earray(size, [(e', se')], inst) in
        let res_t = Sil.Tarray(res_t', size) in
        (Sil.Aeq(e, e'):: atoms', se, res_t)
    | Sil.Tint _, _ | Sil.Tfloat _, _ | Sil.Tvoid, _ | Sil.Tfun _, _ | Sil.Tptr _, _ ->
        L.d_str "create_struct_values type:"; Sil.d_typ_full t; L.d_str " off: "; Sil.d_offset_list off; L.d_ln();
        raise (Exceptions.Bad_footprint __POS__)

    | Sil.Tvar _, _ ->
        L.d_str "create_struct_values type:"; Sil.d_typ_full t; L.d_str " off: "; Sil.d_offset_list off; L.d_ln();
        assert false in

  if !Config.trace_rearrange then
    begin
      let _, se, _ = res in
      L.d_strln "exiting create_struct_values, returning";
      Sil.d_sexp se;
      L.d_decrease_indent 1;
      L.d_ln (); L.d_ln ()
    end;
  res

(** Extend the strexp by populating the path indicated by [off].
    This means that it will add missing flds and do the case - analysis
    for array accesses. This does not catch the array - bounds errors.
    If we want to implement the checks for array bounds errors,
    we need to change this function. *)
let rec _strexp_extend_values
    pname tenv orig_prop footprint_part kind max_stamp
    se typ (off : Sil.offset list) inst =
  match off, se, typ with
  | [], Sil.Eexp _, _
  | [], Sil.Estruct _, _ ->
      [([], se, typ)]
  | [], Sil.Earray _, _ ->
      let off_new = Sil.Off_index(Sil.exp_zero):: off in
      _strexp_extend_values
        pname tenv orig_prop footprint_part kind max_stamp se typ off_new inst
  | (Sil.Off_fld _):: _, Sil.Earray _, Sil.Tarray _ ->
      let off_new = Sil.Off_index(Sil.exp_zero):: off in
      _strexp_extend_values
        pname tenv orig_prop footprint_part kind max_stamp se typ off_new inst
  | (Sil.Off_fld (f, _)):: off', Sil.Estruct (fsel, inst'),
    Sil.Tstruct ({ Sil.instance_fields; static_fields } as struct_typ) ->
      let replace_fv new_v fv = if Ident.fieldname_equal (fst fv) f then (f, new_v) else fv in
      let _, typ', _ =
        try
          IList.find (fun (f', _, _) -> Ident.fieldname_equal f f')
            (instance_fields @ static_fields)
        with Not_found ->
          raise (Exceptions.Missing_fld (f, __POS__)) in
      begin
        try
          let _, se' = IList.find (fun (f', _) -> Ident.fieldname_equal f f') fsel in
          let atoms_se_typ_list' =
            _strexp_extend_values
              pname tenv orig_prop footprint_part kind max_stamp se' typ' off' inst in
          let replace acc (res_atoms', res_se', res_typ') =
            let replace_fse = replace_fv res_se' in
            let res_fsel' = IList.sort Sil.fld_strexp_compare (IList.map replace_fse fsel) in
            let replace_fta (f, t, a) = let f', t' = replace_fv res_typ' (f, t) in (f', t', a) in
            let instance_fields' =
              IList.sort Sil.fld_typ_ann_compare (IList.map replace_fta instance_fields) in
            let struct_typ =
              Sil.Tstruct { struct_typ with Sil.instance_fields = instance_fields' } in
            (res_atoms', Sil.Estruct (res_fsel', inst'), struct_typ) :: acc in
          IList.fold_left replace [] atoms_se_typ_list'
        with Not_found ->
          let atoms', se', res_typ' =
            create_struct_values
              pname tenv orig_prop footprint_part kind max_stamp typ' off' inst in
          let res_fsel' = IList.sort Sil.fld_strexp_compare ((f, se'):: fsel) in
          let replace_fta (f', t', a') = if Ident.fieldname_equal f' f then (f, res_typ', a') else (f', t', a') in
          let instance_fields' =
            IList.sort Sil.fld_typ_ann_compare (IList.map replace_fta instance_fields) in
          let struct_typ = Sil.Tstruct { struct_typ with Sil.instance_fields = instance_fields' } in
          [(atoms', Sil.Estruct (res_fsel', inst'), struct_typ)]
      end
  | (Sil.Off_fld (_, _)):: _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)

  | (Sil.Off_index _):: _, Sil.Eexp _, Sil.Tint _
  | (Sil.Off_index _):: _, Sil.Eexp _, Sil.Tfloat _
  | (Sil.Off_index _):: _, Sil.Eexp _, Sil.Tvoid
  | (Sil.Off_index _):: _, Sil.Eexp _, Sil.Tfun _
  | (Sil.Off_index _):: _, Sil.Eexp _, Sil.Tptr _
  | (Sil.Off_index _):: _, Sil.Estruct _, Sil.Tstruct _ ->
      (* L.d_strln_color Orange "turn into an array"; *)
      let size = match se with
        | Sil.Eexp (_, Sil.Ialloc) -> Sil.exp_one (* if allocated explicitly, we know size is 1 *)
        | _ ->
            if !Config.type_size then Sil.exp_one (* Sil.Sizeof (typ, Sil.Subtype.exact) *)
            else Sil.exp_get_undefined false in

      let se_new = Sil.Earray(size, [(Sil.exp_zero, se)], inst) in
      let typ_new = Sil.Tarray(typ, size) in
      _strexp_extend_values
        pname tenv orig_prop footprint_part kind max_stamp se_new typ_new off inst
  | (Sil.Off_index e):: off', Sil.Earray(size, esel, inst_arr), Sil.Tarray(typ', size_for_typ') ->
      bounds_check pname orig_prop size e (State.get_loc ());
      begin
        try
          let _, se' = IList.find (fun (e', _) -> Sil.exp_equal e e') esel in
          let atoms_se_typ_list' =
            _strexp_extend_values
              pname tenv orig_prop footprint_part kind max_stamp se' typ' off' inst in
          let replace acc (res_atoms', res_se', res_typ') =
            let replace_ise ise = if Sil.exp_equal e (fst ise) then (e, res_se') else ise in
            let res_esel' = IList.map replace_ise esel in
            if (Sil.typ_equal res_typ' typ') || (IList.length res_esel' = 1)
            then (res_atoms', Sil.Earray(size, res_esel', inst_arr), Sil.Tarray(res_typ', size_for_typ')) :: acc
            else raise (Exceptions.Bad_footprint __POS__) in
          IList.fold_left replace [] atoms_se_typ_list'
        with Not_found ->
          array_case_analysis_index pname tenv orig_prop
            footprint_part kind max_stamp
            size esel
            size_for_typ' typ'
            e off' inst_arr inst
      end
  | _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)

and array_case_analysis_index pname tenv orig_prop
    footprint_part kind max_stamp
    array_size array_cont
    typ_array_size typ_cont
    index off inst_arr inst
  =
  let check_sound t' =
    if not (Sil.typ_equal typ_cont t' || array_cont == [])
    then raise (Exceptions.Bad_footprint __POS__) in
  let index_in_array =
    IList.exists (fun (i, _) -> Prover.check_equal Prop.prop_emp index i) array_cont in
  let array_is_full =
    match array_size with
    | Sil.Const (Sil.Cint n') -> Sil.Int.geq (Sil.Int.of_int (IList.length array_cont)) n'
    | _ -> false in

  if index_in_array then
    let array_default = Sil.Earray(array_size, array_cont, inst_arr) in
    let typ_default = Sil.Tarray(typ_cont, typ_array_size) in
    [([], array_default, typ_default)]
  else if !Config.footprint then begin
    let atoms, elem_se, elem_typ =
      create_struct_values
        pname tenv orig_prop footprint_part kind max_stamp typ_cont off inst in
    check_sound elem_typ;
    let cont_new = IList.sort Sil.exp_strexp_compare ((index, elem_se):: array_cont) in
    let array_new = Sil.Earray(array_size, cont_new, inst_arr) in
    let typ_new = Sil.Tarray(elem_typ, typ_array_size) in
    [(atoms, array_new, typ_new)]
  end
  else begin
    let res_new =
      if array_is_full then []
      else begin
        let atoms, elem_se, elem_typ =
          create_struct_values
            pname tenv orig_prop footprint_part kind max_stamp typ_cont off inst in
        check_sound elem_typ;
        let cont_new = IList.sort Sil.exp_strexp_compare ((index, elem_se):: array_cont) in
        let array_new = Sil.Earray(array_size, cont_new, inst_arr) in
        let typ_new = Sil.Tarray(elem_typ, typ_array_size) in
        [(atoms, array_new, typ_new)]
      end in
    let rec handle_case acc isel_seen_rev = function
      | [] -> IList.flatten (IList.rev (res_new:: acc))
      | (i, se) as ise :: isel_unseen ->
          let atoms_se_typ_list =
            _strexp_extend_values
              pname tenv orig_prop footprint_part kind max_stamp se typ_cont off inst in
          let atoms_se_typ_list' =
            IList.fold_left (fun acc' (atoms', se', typ') ->
                check_sound typ';
                let atoms_new = Sil.Aeq(index, i) :: atoms' in
                let isel_new = list_rev_and_concat isel_seen_rev ((i, se'):: isel_unseen) in
                let array_new = Sil.Earray(array_size, isel_new, inst_arr) in
                let typ_new = Sil.Tarray(typ', typ_array_size) in
                (atoms_new, array_new, typ_new):: acc'
              ) [] atoms_se_typ_list in
          let acc_new = atoms_se_typ_list' :: acc in
          let isel_seen_rev_new = ise :: isel_seen_rev in
          handle_case acc_new isel_seen_rev_new isel_unseen in
    handle_case [] [] array_cont
  end

let exp_has_only_footprint_ids e =
  let fav = Sil.exp_fav e in
  Sil.fav_filter_ident fav (fun id -> not (Ident.is_footprint id));
  Sil.fav_is_empty fav

let laundry_offset_for_footprint max_stamp offs_in =
  let rec laundry offs_seen eqs offs =
    match offs with
    | [] ->
        (IList.rev offs_seen, IList.rev eqs)
    | (Sil.Off_fld _ as off):: offs' ->
        let offs_seen' = off:: offs_seen in
        laundry offs_seen' eqs offs'
    | (Sil.Off_index(idx) as off):: offs' ->
        if exp_has_only_footprint_ids idx then
          let offs_seen' = off:: offs_seen in
          laundry offs_seen' eqs offs'
        else
          let () = incr max_stamp in
          let fid_new = Ident.create Ident.kfootprint !max_stamp in
          let exp_new = Sil.Var fid_new in
          let off_new = Sil.Off_index exp_new in
          let offs_seen' = off_new:: offs_seen in
          let eqs' = (fid_new, idx):: eqs in
          laundry offs_seen' eqs' offs' in
  laundry [] [] offs_in

let strexp_extend_values
    pname tenv orig_prop footprint_part kind max_stamp
    se te (off : Sil.offset list) inst =
  let typ = Sil.texp_to_typ None te in
  let off', laundry_atoms =
    let off', eqs = laundry_offset_for_footprint max_stamp off in
    (* do laundry_offset whether footprint_part is true or not, so max_stamp is modified anyway *)
    if footprint_part then
      off', IList.map (fun (id, e) -> Prop.mk_eq (Sil.Var id) e) eqs
    else off, [] in
  if !Config.trace_rearrange then (L.d_str "entering strexp_extend_values se: "; Sil.d_sexp se; L.d_str " typ: ";
                                   Sil.d_typ_full typ; L.d_str " off': "; Sil.d_offset_list off'; L.d_strln (if footprint_part then " FP" else " RE"));
  let atoms_se_typ_list =
    _strexp_extend_values
      pname tenv orig_prop footprint_part kind max_stamp se typ off' inst in
  let atoms_se_typ_list_filtered =
    let neg_atom = function Sil.Aeq(e1, e2) -> Sil.Aneq(e1, e2) | Sil.Aneq(e1, e2) -> Sil.Aeq(e1, e2) in
    let check_neg_atom atom = Prover.check_atom Prop.prop_emp (neg_atom atom) in
    let check_not_inconsistent (atoms, _, _) = not (IList.exists check_neg_atom atoms) in
    IList.filter check_not_inconsistent atoms_se_typ_list in
  if !Config.trace_rearrange then L.d_strln "exiting strexp_extend_values";
  let st = match te with
    | Sil.Sizeof(_, st) -> st
    | _ -> Sil.Subtype.exact in
  IList.map (fun (atoms', se', typ') -> (laundry_atoms @ atoms', se', Sil.Sizeof (typ', st))) atoms_se_typ_list_filtered

let collect_root_offset exp =
  let root = Sil.root_of_lexp exp in
  let offsets = Sil.exp_get_offsets exp in
  (root, offsets)

(** Sil.Construct a points-to predicate for an expression, to add to a footprint. *)
let mk_ptsto_exp_footprint
    pname tenv orig_prop (lexp, typ) max_stamp inst : Sil.hpred * Sil.hpred * Sil.atom list =
  let root, off = collect_root_offset lexp in
  if not (exp_has_only_footprint_ids root)
  then begin
    (* in angelic mode, purposely ignore dangling pointer warnings during the footprint phase -- we
     * will fix them during the re - execution phase *)
    if not (!Config.angelic_execution && !Config.footprint) then
      begin
        if !Config.developer_mode then
          L.err "!!!! Footprint Error, Bad Root : %a !!!! @\n" (Sil.pp_exp pe_text) lexp;
        let deref_str = Localise.deref_str_dangling None in
        let err_desc =
          Errdesc.explain_dereference deref_str orig_prop (State.get_loc ()) in
        raise
          (Exceptions.Dangling_pointer_dereference
             (None, err_desc, __POS__))
      end
  end;
  let off_foot, eqs = laundry_offset_for_footprint max_stamp off in
  let st = match !Config.curr_language with
    | Config.C_CPP -> Sil.Subtype.exact
    | Config.Java -> Sil.Subtype.subtypes in
  let create_ptsto footprint_part off0 = match root, off0, typ with
    | Sil.Lvar pvar, [], Sil.Tfun _ ->
        let fun_name = Procname.from_string_c_fun (Mangled.to_string (Pvar.get_name pvar)) in
        let fun_exp = Sil.Const (Sil.Cfun fun_name) in
        ([], Prop.mk_ptsto root (Sil.Eexp (fun_exp, inst)) (Sil.Sizeof (typ, st)))
    | _, [], Sil.Tfun _ ->
        let atoms, se, t =
          create_struct_values
            pname tenv orig_prop footprint_part Ident.kfootprint max_stamp typ off0 inst in
        (atoms, Prop.mk_ptsto root se (Sil.Sizeof (t, st)))
    | _ ->
        let atoms, se, t =
          create_struct_values
            pname tenv orig_prop footprint_part Ident.kfootprint max_stamp typ off0 inst in
        (atoms, Prop.mk_ptsto root se (Sil.Sizeof (t, st))) in
  let atoms, ptsto_foot = create_ptsto true off_foot in
  let sub = Sil.sub_of_list eqs in
  let ptsto = Sil.hpred_sub sub ptsto_foot in
  let atoms' = IList.map (fun (id, e) -> Prop.mk_eq (Sil.Var id) e) eqs in
  (ptsto, ptsto_foot, atoms @ atoms')

(** Check if the path in exp exists already in the current ptsto predicate.
    If it exists, return None. Otherwise, return [Some fld] with [fld] the missing field. *)
let prop_iter_check_fields_ptsto_shallow iter lexp =
  let offset = Sil.exp_get_offsets lexp in
  let (_, se, _) =
    match Prop.prop_iter_current iter with
    | Sil.Hpointsto (e, se, t), _ -> (e, se, t)
    | _ -> assert false in
  let rec check_offset se = function
    | [] -> None
    | (Sil.Off_fld (fld, _)):: off' ->
        (match se with
         | Sil.Estruct (fsel, _) ->
             (try
                let _, se' = IList.find (fun (fld', _) -> Sil.fld_equal fld fld') fsel in
                check_offset se' off'
              with Not_found -> Some fld)
         | _ -> Some fld)
    | (Sil.Off_index _):: _ -> None in
  check_offset se offset

let fav_max_stamp fav =
  let max_stamp = ref 0 in
  let f id = max_stamp := max !max_stamp (Ident.get_stamp id) in
  IList.iter f (Sil.fav_to_list fav);
  max_stamp

(** [prop_iter_extend_ptsto iter lexp] extends the current psto
    predicate in [iter] with enough fields to follow the path in
    [lexp] -- field splitting model. It also materializes all
    indices accessed in lexp. *)
let prop_iter_extend_ptsto pname tenv orig_prop iter lexp inst =
  if !Config.trace_rearrange then (L.d_str "entering prop_iter_extend_ptsto lexp: "; Sil.d_exp lexp; L.d_ln ());
  let offset = Sil.exp_get_offsets lexp in
  let max_stamp = fav_max_stamp (Prop.prop_iter_fav iter) in
  let max_stamp_val = !max_stamp in
  let extend_footprint_pred = function
    | Sil.Hpointsto(e, se, te) ->
        let atoms_se_te_list =
          strexp_extend_values
            pname tenv orig_prop true Ident.kfootprint (ref max_stamp_val) se te offset inst in
        IList.map (fun (atoms', se', te') -> (atoms', Sil.Hpointsto (e, se', te'))) atoms_se_te_list
    | Sil.Hlseg (k, hpara, e1, e2, el) ->
        begin
          match hpara.Sil.body with
          | Sil.Hpointsto(e', se', te'):: body_rest ->
              let atoms_se_te_list =
                strexp_extend_values
                  pname tenv orig_prop true Ident.kfootprint
                  (ref max_stamp_val) se' te' offset inst in
              let atoms_body_list =
                IList.map (fun (atoms0, se0, te0) -> (atoms0, Sil.Hpointsto(e', se0, te0):: body_rest)) atoms_se_te_list in
              let atoms_hpara_list =
                IList.map (fun (atoms, body') -> (atoms, { hpara with Sil.body = body'})) atoms_body_list in
              IList.map (fun (atoms, hpara') -> (atoms, Sil.Hlseg(k, hpara', e1, e2, el))) atoms_hpara_list
          | _ -> assert false
        end
    | _ -> assert false in
  let atoms_se_te_to_iter e (atoms, se, te) =
    let iter' = IList.fold_left (Prop.prop_iter_add_atom !Config.footprint) iter atoms in
    Prop.prop_iter_update_current iter' (Sil.Hpointsto (e, se, te)) in
  let do_extend e se te =
    if !Config.trace_rearrange then begin
      L.d_strln "entering do_extend";
      L.d_str "e: "; Sil.d_exp e; L.d_str " se : "; Sil.d_sexp se; L.d_str " te: "; Sil.d_texp_full te;
      L.d_ln (); L.d_ln ()
    end;
    let extend_kind = match e with (* Determine whether to extend the footprint part or just the normal part *)
      | Sil.Var id when not (Ident.is_footprint id) -> Ident.kprimed
      | Sil.Lvar pvar when Pvar.is_local pvar -> Ident.kprimed
      | _ -> Ident.kfootprint in
    let iter_list =
      let atoms_se_te_list =
        strexp_extend_values
          pname tenv orig_prop false extend_kind max_stamp se te offset inst in
      IList.map (atoms_se_te_to_iter e) atoms_se_te_list in
    let res_iter_list =
      if Ident.kind_equal extend_kind Ident.kprimed
      then iter_list (* normal part already extended: nothing to do *)
      else (* extend footprint part *)
        let atoms_fp_sigma_list =
          let footprint_sigma = Prop.prop_iter_get_footprint_sigma iter in
          let sigma_pto, sigma_rest =
            IList.partition (function
                | Sil.Hpointsto(e', _, _) -> Sil.exp_equal e e'
                | Sil.Hlseg (_, _, e1, _, _) -> Sil.exp_equal e e1
                | Sil.Hdllseg (_, _, e_iF, _, _, e_iB, _) ->
                    Sil.exp_equal e e_iF || Sil.exp_equal e e_iB
              ) footprint_sigma in
          let atoms_sigma_list =
            match sigma_pto with
            | [hpred] ->
                let atoms_hpred_list = extend_footprint_pred hpred in
                IList.map (fun (atoms, hpred') -> (atoms, hpred' :: sigma_rest)) atoms_hpred_list
            | _ ->
                L.d_warning "Cannot extend "; Sil.d_exp lexp; L.d_strln " in"; Prop.d_prop (Prop.prop_iter_to_prop iter); L.d_ln();
                [([], footprint_sigma)] in
          IList.map (fun (atoms, sigma') -> (atoms, IList.stable_sort Sil.hpred_compare sigma')) atoms_sigma_list in
        let iter_atoms_fp_sigma_list =
          list_product iter_list atoms_fp_sigma_list in
        IList.map (fun (iter, (atoms, fp_sigma)) ->
            let iter' = IList.fold_left (Prop.prop_iter_add_atom !Config.footprint) iter atoms in
            Prop.prop_iter_replace_footprint_sigma iter' fp_sigma
          ) iter_atoms_fp_sigma_list in
    let res_prop_list =
      IList.map Prop.prop_iter_to_prop res_iter_list in
    begin
      L.d_str "in prop_iter_extend_ptsto lexp: "; Sil.d_exp lexp; L.d_ln ();
      L.d_strln "prop before:";
      let prop_before = Prop.prop_iter_to_prop iter in
      Prop.d_prop prop_before; L.d_ln ();
      L.d_ln (); L.d_ln ();
      L.d_strln "prop list after:";
      Propgraph.d_proplist prop_before res_prop_list; L.d_ln ();
      L.d_ln (); L.d_ln ();
      res_iter_list
    end in
  begin
    match Prop.prop_iter_current iter with
    | Sil.Hpointsto (e, se, te), _ -> do_extend e se te
    | _ -> assert false
  end

(** Add a pointsto for [root(lexp): typ] to the sigma and footprint of a
    prop, if it's compatible with the allowed footprint
    variables. Then, change it into a iterator. This function ensures
    that [root(lexp): typ] is the current hpred of the iterator. typ
    is the type of the root of lexp. *)
let prop_iter_add_hpred_footprint_to_prop pname tenv prop (lexp, typ) inst =
  let max_stamp = fav_max_stamp (Prop.prop_footprint_fav prop) in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint pname tenv prop (lexp, typ) max_stamp inst in
  L.d_strln "++++ Adding footprint frame";
  Prop.d_prop (Prop.prop_hpred_star Prop.prop_emp ptsto);
  L.d_ln (); L.d_ln ();
  let eprop = Prop.expose prop in
  let foot_sigma = ptsto_foot :: Prop.get_sigma_footprint eprop in
  let nfoot_sigma = Prop.sigma_normalize_prop Prop.prop_emp foot_sigma in
  let prop' = Prop.normalize (Prop.replace_sigma_footprint nfoot_sigma eprop) in
  let prop_new = IList.fold_left (Prop.prop_atom_and ~footprint:!Config.footprint) prop' atoms in
  let iter = match (Prop.prop_iter_create prop_new) with
    | None ->
        let prop_new' = Prop.normalize (Prop.prop_hpred_star prop_new ptsto) in
        begin
          match (Prop.prop_iter_create prop_new') with
          | None -> assert false
          | Some iter -> iter
        end
    | Some iter -> Prop.prop_iter_prev_then_insert iter ptsto in
  let offsets_default = Sil.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter offsets_default

(** Add a pointsto for [root(lexp): typ] to the iterator and to the
    footprint, if it's compatible with the allowed footprint
    variables. This function ensures that [root(lexp): typ] is the
    current hpred of the iterator. typ is the type of the root of lexp. *)
let prop_iter_add_hpred_footprint pname tenv orig_prop iter (lexp, typ) inst =
  let max_stamp = fav_max_stamp (Prop.prop_iter_footprint_fav iter) in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint pname tenv orig_prop (lexp, typ) max_stamp inst in
  L.d_strln "++++ Adding footprint frame";
  Prop.d_prop (Prop.prop_hpred_star Prop.prop_emp ptsto);
  L.d_ln (); L.d_ln ();
  let foot_sigma = ptsto_foot :: (Prop.prop_iter_get_footprint_sigma iter) in
  let iter_foot = Prop.prop_iter_prev_then_insert iter ptsto in
  let iter_foot_atoms = IList.fold_left (Prop.prop_iter_add_atom (!Config.footprint)) iter_foot atoms in
  let iter' = Prop.prop_iter_replace_footprint_sigma iter_foot_atoms foot_sigma in
  let offsets_default = Sil.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter' offsets_default

exception ARRAY_ACCESS

let rearrange_arith lexp prop =
  if !Config.trace_rearrange then begin
    L.d_strln "entering rearrange_arith";
    L.d_str "lexp: "; Sil.d_exp lexp; L.d_ln ();
    L.d_str "prop: "; L.d_ln (); Prop.d_prop prop; L.d_ln (); L.d_ln ()
  end;
  if (!Config.array_level >= 2) then raise ARRAY_ACCESS
  else
    let root = Sil.root_of_lexp lexp in
    if Prover.check_allocatedness prop root then
      raise ARRAY_ACCESS
    else
      raise (Exceptions.Symexec_memory_error __POS__)

let pp_rearrangement_error message prop lexp =
  L.d_strln (".... Rearrangement Error .... " ^ message);
  L.d_str "Exp:"; Sil.d_exp lexp; L.d_ln ();
  L.d_str "Prop:"; L.d_ln (); Prop.d_prop prop; L.d_ln (); L.d_ln ()

(** do re-arrangment for an iter whose current element is a pointsto *)
let iter_rearrange_ptsto pname tenv orig_prop iter lexp inst =
  if !Config.trace_rearrange then begin
    L.d_increase_indent 1;
    L.d_strln "entering iter_rearrange_ptsto";
    L.d_str "lexp: "; Sil.d_exp lexp; L.d_ln ();
    L.d_strln "prop:"; Prop.d_prop orig_prop; L.d_ln ();
    L.d_strln "iter:"; Prop.d_prop (Prop.prop_iter_to_prop iter);
    L.d_ln (); L.d_ln ()
  end;
  let check_field_splitting () =
    match prop_iter_check_fields_ptsto_shallow iter lexp with
    | None -> ()
    | Some fld ->
        begin
          pp_rearrangement_error "field splitting check failed" orig_prop lexp;
          raise (Exceptions.Missing_fld (fld, __POS__))
        end in
  let res =
    if !Config.footprint
    then
      prop_iter_extend_ptsto pname tenv orig_prop iter lexp inst
    else
      begin
        check_field_splitting ();
        match Prop.prop_iter_current iter with
        | Sil.Hpointsto (e, se, te), offset ->
            let max_stamp = fav_max_stamp (Prop.prop_iter_fav iter) in
            let atoms_se_te_list =
              strexp_extend_values
                pname tenv orig_prop false Ident.kprimed max_stamp se te offset inst in
            let handle_case (atoms', se', te') =
              let iter' = IList.fold_left (Prop.prop_iter_add_atom !Config.footprint) iter atoms' in
              Prop.prop_iter_update_current iter' (Sil.Hpointsto (e, se', te')) in
            let filter it =
              let p = Prop.prop_iter_to_prop it in
              not (Prover.check_inconsistency p) in
            IList.filter filter (IList.map handle_case atoms_se_te_list)
        | _ -> [iter]
      end in
  begin
    if !Config.trace_rearrange then begin
      L.d_strln "exiting iter_rearrange_ptsto, returning results";
      Prop.d_proplist_with_typ (IList.map Prop.prop_iter_to_prop res);
      L.d_decrease_indent 1;
      L.d_ln (); L.d_ln ()
    end;
    res
  end

(** do re-arrangment for an iter whose current element is a nonempty listseg *)
let iter_rearrange_ne_lseg recurse_on_iters iter para e1 e2 elist =
  if (!Config.nelseg) then
    let iter_inductive_case =
      let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
      let (_, para_inst1) = Sil.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1@[Prop.mk_lseg Sil.Lseg_NE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1 in
    let iter_base_case =
      let (_, para_inst) = Sil.hpara_instantiate para e1 e2 elist in
      Prop.prop_iter_update_current_by_list iter para_inst in
    recurse_on_iters [iter_inductive_case; iter_base_case]
  else
    let iter_inductive_case =
      let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
      let (_, para_inst1) = Sil.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1@[Prop.mk_lseg Sil.Lseg_PE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1 in
    recurse_on_iters [iter_inductive_case]

(** do re-arrangment for an iter whose current element is a nonempty dllseg to be unrolled from lhs *)
let iter_rearrange_ne_dllseg_first recurse_on_iters iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let (_, para_dll_inst1) = Sil.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 = para_dll_inst1@[Prop.mk_dllseg Sil.Lseg_NE para_dll n' e1 e3 e4 elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1 in
  let iter_base_case =
    let (_, para_dll_inst) = Sil.hpara_dll_instantiate para_dll e1 e2 e3 elist in
    let iter' = Prop.prop_iter_update_current_by_list iter para_dll_inst in
    let prop' = Prop.prop_iter_to_prop iter' in
    let prop'' = Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e4 prop' in
    match (Prop.prop_iter_create prop'') with
    | None -> assert false
    | Some iter' -> iter' in
  recurse_on_iters [iter_inductive_case; iter_base_case]

(** do re-arrangment for an iter whose current element is a nonempty dllseg to be unrolled from rhs *)
let iter_rearrange_ne_dllseg_last recurse_on_iters iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let (_, para_dll_inst1) = Sil.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 = para_dll_inst1@[Prop.mk_dllseg Sil.Lseg_NE para_dll e1 e2 e4 n' elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1 in
  let iter_base_case =
    let (_, para_dll_inst) = Sil.hpara_dll_instantiate para_dll e4 e2 e3 elist in
    let iter' = Prop.prop_iter_update_current_by_list iter para_dll_inst in
    let prop' = Prop.prop_iter_to_prop iter' in
    let prop'' = Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e4 prop' in
    match (Prop.prop_iter_create prop'') with
    | None -> assert false
    | Some iter' -> iter' in
  recurse_on_iters [iter_inductive_case; iter_base_case]

(** do re-arrangment for an iter whose current element is a possibly empty listseg *)
let iter_rearrange_pe_lseg recurse_on_iters default_case_iter iter para e1 e2 elist =
  let iter_nonemp_case =
    let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let (_, para_inst1) = Sil.hpara_instantiate para e1 n' elist in
    let hpred_list1 = para_inst1@[Prop.mk_lseg Sil.Lseg_PE para n' e2 elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1 in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop iter in
    let prop' = Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e2 removed_prop in
    match (Prop.prop_iter_create prop') with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' -> [iter_nonemp_case; iter'] in
  recurse_on_iters iter_subcases

(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled from lhs *)
let iter_rearrange_pe_dllseg_first recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let (_, para_dll_inst1) = Sil.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 = para_dll_inst1@[Prop.mk_dllseg Sil.Lseg_PE para_dll n' e1 e3 e4 elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1 in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop iter in
    let prop' = Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e3 removed_prop in
    let prop'' = Prop.conjoin_eq ~footprint: (!Config.footprint) e2 e4 prop' in
    match (Prop.prop_iter_create prop'') with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' -> [iter_inductive_case; iter'] in
  recurse_on_iters iter_subcases

(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled from rhs *)
let iter_rearrange_pe_dllseg_last recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Sil.Var (Ident.create_fresh Ident.kprimed) in
    let (_, para_dll_inst1) = Sil.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 = para_dll_inst1@[Prop.mk_dllseg Sil.Lseg_PE para_dll e1 e2 e4 n' elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1 in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop iter in
    let prop' = Prop.conjoin_eq ~footprint: (!Config.footprint) e1 e3 removed_prop in
    let prop'' = Prop.conjoin_eq ~footprint: (!Config.footprint) e2 e4 prop' in
    match (Prop.prop_iter_create prop'') with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' -> [iter_inductive_case; iter'] in
  recurse_on_iters iter_subcases

(** find the type at the offset from the given type expression, if any *)
let type_at_offset texp off =
  let rec strip_offset off typ = match off, typ with
    | [], _ -> Some typ
    | (Sil.Off_fld (f, _)):: off', Sil.Tstruct { Sil.instance_fields } ->
        (try
           let typ' =
             (fun (_, y, _) -> y)
               (IList.find (fun (f', _, _) -> Ident.fieldname_equal f f') instance_fields) in
           strip_offset off' typ'
         with Not_found -> None)
    | (Sil.Off_index _):: off', Sil.Tarray (typ', _) ->
        strip_offset off' typ'
    | _ -> None in
  match texp with
  | Sil.Sizeof(typ, _) ->
      strip_offset off typ
  | _ -> None

(** Check that the size of a type coming from an instruction does not exceed the size of the type from the pointsto predicate
    For example, that a pointer to int is not used to assign to a char *)
let check_type_size pname prop texp off typ_from_instr =
  L.d_strln_color Orange "check_type_size";
  L.d_str "off: "; Sil.d_offset_list off; L.d_ln ();
  L.d_str "typ_from_instr: "; Sil.d_typ_full typ_from_instr; L.d_ln ();
  match type_at_offset texp off with
  | Some typ_of_object ->
      L.d_str "typ_o: "; Sil.d_typ_full typ_of_object; L.d_ln ();
      if Prover.type_size_comparable typ_from_instr typ_of_object && Prover.check_type_size_leq typ_from_instr typ_of_object = false
      then begin
        let deref_str = Localise.deref_str_pointer_size_mismatch typ_from_instr typ_of_object in
        let loc = State.get_loc () in
        let exn =
          Exceptions.Pointer_size_mismatch (
            Errdesc.explain_dereference deref_str prop loc, __POS__) in
        let pre_opt = State.get_normalized_pre (Abs.abstract_no_symop pname) in
        Reporting.log_warning pname ~pre: pre_opt exn
      end
  | None ->
      L.d_str "texp: "; Sil.d_texp_full texp; L.d_ln ()

(** Exposes lexp |->- from iter. In case that it is not possible to
 * expose lexp |->-, this function prints an error message and
 * faults. There are four things to note. First, typ is the type of the
 * root of lexp. Second, prop should mean the same as iter. Third, the
 * result [] means that the given input iter is inconsistent. This
 * happens when the theorem prover can prove the inconsistency of prop,
 * only after unrolling some predicates in prop. This function ensures
 * that the theorem prover cannot prove the inconsistency of any of the
 * new iters in the result. *)
let rec iter_rearrange
    pname tenv lexp typ_from_instr prop iter
    inst: (Sil.offset list) Prop.prop_iter list =
  let typ = match Sil.exp_get_offsets lexp with
    | Sil.Off_fld (f, ((Sil.Tstruct _) as struct_typ)) :: _ -> (* access through field: get the struct type from the field *)
        if !Config.trace_rearrange then begin
          L.d_increase_indent 1;
          L.d_str "iter_rearrange: root of lexp accesses field "; L.d_strln (Ident.fieldname_to_string f);
          L.d_str "  type from instruction: "; Sil.d_typ_full typ_from_instr; L.d_ln();
          L.d_str "  struct type from field: "; Sil.d_typ_full struct_typ; L.d_ln();
          L.d_decrease_indent 1;
          L.d_ln();
        end;
        struct_typ
    | _ ->
        typ_from_instr in
  if !Config.trace_rearrange then begin
    L.d_increase_indent 1;
    L.d_strln "entering iter_rearrange";
    L.d_str "lexp: "; Sil.d_exp lexp; L.d_ln ();
    L.d_str "typ: "; Sil.d_typ_full typ; L.d_ln ();
    L.d_strln "prop:"; Prop.d_prop prop; L.d_ln ();
    L.d_strln "iter:"; Prop.d_prop (Prop.prop_iter_to_prop iter);
    L.d_ln (); L.d_ln ()
  end;
  let default_case_iter (iter': unit Prop.prop_iter) =
    if !Config.trace_rearrange then L.d_strln "entering default_case_iter";
    if !Config.footprint then
      prop_iter_add_hpred_footprint pname tenv prop iter' (lexp, typ) inst
    else
    if (!Config.array_level >= 1 && not !Config.footprint && Sil.exp_pointer_arith lexp)
    then rearrange_arith lexp prop
    else begin
      pp_rearrangement_error "cannot find predicate with root" prop lexp;
      if not !Config.footprint then Printer.force_delayed_prints ();
      raise (Exceptions.Symexec_memory_error __POS__)
    end in
  let recurse_on_iters iters =
    let f_one_iter iter' =
      let prop' = Prop.prop_iter_to_prop iter' in
      if Prover.check_inconsistency prop' then []
      else iter_rearrange pname tenv (Prop.lexp_normalize_prop prop' lexp) typ prop' iter' inst in
    let rec f_many_iters iters_lst = function
      | [] -> IList.flatten (IList.rev iters_lst)
      | iter':: iters' ->
          let iters_res' = f_one_iter iter' in
          f_many_iters (iters_res':: iters_lst) iters' in
    f_many_iters [] iters in
  let filter = function
    | Sil.Hpointsto (base, _, _) | Sil.Hlseg (_, _, base, _, _) ->
        Prover.is_root prop base lexp
    | Sil.Hdllseg (_, _, first, _, _, last, _) ->
        let result_first = Prover.is_root prop first lexp in
        match result_first with
        | None -> Prover.is_root prop last lexp
        | Some _ -> result_first in
  let res =
    match Prop.prop_iter_find iter filter with
    | None ->
        [default_case_iter iter]
    | Some iter ->
        match Prop.prop_iter_current iter with
        | (Sil.Hpointsto (_, _, texp), off) ->
            if !Config.type_size then check_type_size pname prop texp off typ_from_instr;
            iter_rearrange_ptsto pname tenv prop iter lexp inst
        | (Sil.Hlseg (Sil.Lseg_NE, para, e1, e2, elist), _) ->
            iter_rearrange_ne_lseg recurse_on_iters iter para e1 e2 elist
        | (Sil.Hlseg (Sil.Lseg_PE, para, e1, e2, elist), _) ->
            iter_rearrange_pe_lseg recurse_on_iters default_case_iter iter para e1 e2 elist
        | (Sil.Hdllseg (Sil.Lseg_NE, para_dll, e1, e2, e3, e4, elist), _) ->
            begin
              match Prover.is_root prop e1 lexp, Prover.is_root prop e4 lexp with
              | None, None -> assert false
              | Some _, _ -> iter_rearrange_ne_dllseg_first recurse_on_iters iter para_dll e1 e2 e3 e4 elist
              | _, Some _ -> iter_rearrange_ne_dllseg_last recurse_on_iters iter para_dll e1 e2 e3 e4 elist
            end
        | (Sil.Hdllseg (Sil.Lseg_PE, para_dll, e1, e2, e3, e4, elist), _) ->
            begin
              match Prover.is_root prop e1 lexp, Prover.is_root prop e4 lexp with
              | None, None -> assert false
              | Some _, _ -> iter_rearrange_pe_dllseg_first recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4 elist
              | _, Some _ -> iter_rearrange_pe_dllseg_last recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4 elist
            end in
  if !Config.trace_rearrange then begin
    L.d_strln "exiting iter_rearrange, returning results";
    Prop.d_proplist_with_typ (IList.map Prop.prop_iter_to_prop res);
    L.d_decrease_indent 1;
    L.d_ln (); L.d_ln ()
  end;
  res

(** Check for dereference errors: dereferencing 0, a freed value, or an undefined value *)
let check_dereference_error pdesc (prop : Prop.normal Prop.t) lexp loc =
  let nullable_obj_str = ref None in
  (* return true if deref_exp is only pointed to by fields/params with @Nullable annotations *)
  let is_only_pt_by_nullable_fld_or_param deref_exp =
    let ann_sig = Models.get_modelled_annotated_signature (Specs.pdesc_resolve_attributes pdesc) in
    IList.for_all
      (fun hpred ->
         match hpred with
         | Sil.Hpointsto (Sil.Lvar pvar, Sil.Eexp (Sil.Var _ as exp, _), _)
           when Sil.exp_equal exp deref_exp ->
             let is_nullable = Annotations.param_is_nullable pvar ann_sig in
             if is_nullable then
               nullable_obj_str := Some (Pvar.to_string pvar);
             (* it's ok for a non-nullable local to point to deref_exp *)
             is_nullable || Pvar.is_local pvar
         | Sil.Hpointsto (_, Sil.Estruct (flds, _), Sil.Sizeof (typ, _)) ->
             let fld_is_nullable fld =
               match Annotations.get_field_type_and_annotation fld typ with
               | Some (_, annot) -> Annotations.ia_is_nullable annot
               | _ -> false in
             let is_strexp_pt_by_nullable_fld (fld, strexp) =
               match strexp with
               | Sil.Eexp (Sil.Var _ as exp, _) when Sil.exp_equal exp deref_exp ->
                   let is_nullable = fld_is_nullable fld in
                   if is_nullable then
                     nullable_obj_str := Some (Ident.fieldname_to_simplified_string fld);
                   is_nullable
               | _ -> true in
             IList.for_all is_strexp_pt_by_nullable_fld flds
         | _ -> true)
      (Prop.get_sigma prop) &&
    !nullable_obj_str <> None in
  let root = Sil.root_of_lexp lexp in
  let is_deref_of_nullable =
    let is_definitely_non_null exp prop =
      Prover.check_disequal prop exp Sil.exp_zero in
    !Config.report_nullable_inconsistency && not (is_definitely_non_null root prop)
    && is_only_pt_by_nullable_fld_or_param root in
  let relevant_attributes_getters = [
    Prop.get_resource_attribute;
    Prop.get_undef_attribute;
  ] in
  let get_relevant_attributes exp =
    let rec fold_getters = function
      | [] -> None
      | getter:: tl -> match getter prop exp with
        | Some _ as some_attr -> some_attr
        | None -> fold_getters tl in
    fold_getters relevant_attributes_getters in
  let attribute_opt = match get_relevant_attributes root with
    | Some att -> Some att
    | None -> (* try to remove an offset if any, and find the attribute there *)
        let root_no_offset = match root with
          | Sil.BinOp((Sil.PlusPI | Sil.PlusA | Sil.MinusPI | Sil.MinusA), base, _) -> base
          | _ -> root in
        get_relevant_attributes root_no_offset in
  if Prover.check_zero (Sil.root_of_lexp root) || is_deref_of_nullable then
    begin
      let deref_str =
        if is_deref_of_nullable then
          match !nullable_obj_str with
          | Some str -> Localise.deref_str_nullable None str
          | None -> Localise.deref_str_nullable None ""
        else Localise.deref_str_null None in
      let err_desc =
        Errdesc.explain_dereference ~use_buckets: true ~is_nullable: is_deref_of_nullable
          deref_str prop loc in
      if Localise.is_parameter_not_null_checked_desc err_desc then
        raise (Exceptions.Parameter_not_null_checked (err_desc, __POS__))
      else if Localise.is_field_not_null_checked_desc err_desc then
        raise (Exceptions.Field_not_null_checked (err_desc, __POS__))
      else raise (Exceptions.Null_dereference (err_desc, __POS__))
    end;
  match attribute_opt with
  | Some (Sil.Adangling dk) ->
      let deref_str = Localise.deref_str_dangling (Some dk) in
      let err_desc = Errdesc.explain_dereference deref_str prop (State.get_loc ()) in
      raise (Exceptions.Dangling_pointer_dereference (Some dk, err_desc, __POS__))
  | Some (Sil.Aundef (s, undef_loc, _)) ->
      if !Config.angelic_execution then ()
      else
        let deref_str = Localise.deref_str_undef (s, undef_loc) in
        let err_desc = Errdesc.explain_dereference deref_str prop loc in
        raise (Exceptions.Skip_pointer_dereference (err_desc, __POS__))
  | Some (Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra)) ->
      let deref_str = Localise.deref_str_freed ra in
      let err_desc = Errdesc.explain_dereference ~use_buckets: true deref_str prop loc in
      raise (Exceptions.Use_after_free (err_desc, __POS__))
  | _ ->
      if Prover.check_equal Prop.prop_emp (Sil.root_of_lexp root) Sil.exp_minus_one then
        let deref_str = Localise.deref_str_dangling None in
        let err_desc = Errdesc.explain_dereference deref_str prop loc in
        raise (Exceptions.Dangling_pointer_dereference (None, err_desc, __POS__))

(* Check that an expression representin an objc block can be null and raise a [B1] null exception.*)
(* It's used to check that we don't call possibly null blocks *)
let check_call_to_objc_block_error pdesc prop fun_exp loc =
  let fun_exp_may_be_null () = (* may be null if we don't know if it is definitely not null *)
    not (Prover.check_disequal prop (Sil.root_of_lexp fun_exp) Sil.exp_zero) in
  let try_explaining_exp e = (* when e is a temp var, try to find the pvar defining e*)
    match e with
    | Sil.Var id ->
        (match (Errdesc.find_ident_assignment (State.get_node ()) id) with
         | Some (_, e') -> e'
         | None -> e)
    | _ -> e in
  let get_exp_called () = (* Exp called in the block's function call*)
    match State.get_instr () with
    | Some Sil.Call(_, Sil.Var id, _, _, _) ->
        Errdesc.find_ident_assignment (State.get_node ()) id
    | _ -> None in
  let is_fun_exp_captured_var () = (* Called expression is a captured variable of the block *)
    match get_exp_called () with
    | Some (_, Sil.Lvar pvar) -> (* pvar is the block *)
        let name = Pvar.get_name pvar in
        IList.exists (fun (cn, _) -> (Mangled.to_string name) = (Mangled.to_string cn)) (Cfg.Procdesc.get_captured pdesc)
    | _ -> false in
  let is_field_deref () = (*Called expression is a field *)
    match get_exp_called () with
    | Some (_, (Sil.Lfield(e', fn, t))) ->
        let e'' = try_explaining_exp e' in
        Some (Sil.Lfield(e'', fn, t)), true (* the block dereferences is a field of an object*)
    | Some (_, e) -> Some e, false
    | _ -> None, false in
  if (!Config.curr_language = Config.C_CPP) &&
     fun_exp_may_be_null () &&
     not (is_fun_exp_captured_var ()) then
    begin
      let deref_str = Localise.deref_str_null None in
      let err_desc_nobuckets = Errdesc.explain_dereference ~is_nullable: true deref_str prop loc in
      match fun_exp with
      | Sil.Var id when Ident.is_footprint id ->
          let e_opt, is_field_deref = is_field_deref () in
          let err_desc_nobuckets' = (match e_opt with
              | Some e -> Localise.parameter_field_not_null_checked_desc err_desc_nobuckets e
              | _ -> err_desc_nobuckets) in
          let err_desc =
            Localise.error_desc_set_bucket
              err_desc_nobuckets' Localise.BucketLevel.b1 !Config.show_buckets in
          if is_field_deref then
            raise
              (Exceptions.Field_not_null_checked
                 (err_desc, __POS__))
          else
            raise
              (Exceptions.Parameter_not_null_checked
                 (err_desc, __POS__))
      | _ ->
          (* HP: fun_exp is not a footprint therefore,
             either is a local or it's a modified param *)
          let err_desc =
            Localise.error_desc_set_bucket
              err_desc_nobuckets Localise.BucketLevel.b1 !Config.show_buckets in
          raise (Exceptions.Null_dereference
                   (err_desc, __POS__))
    end

(** [rearrange lexp prop] rearranges [prop] into the form [prop' * lexp|->strexp:typ].
    It returns an iterator with [lexp |-> strexp: typ] as current predicate
    and the path (an [offsetlist]) which leads to [lexp] as the iterator state. *)
let rearrange ?(report_deref_errors=true) pdesc tenv lexp typ prop loc
  : (Sil.offset list) Prop.prop_iter list =

  let nlexp = match Prop.exp_normalize_prop prop lexp with
    | Sil.BinOp(Sil.PlusPI, ep, e) -> (* array access with pointer arithmetic *)
        Sil.Lindex(ep, e)
    | e -> e in
  let ptr_tested_for_zero =
    Prover.check_disequal prop (Sil.root_of_lexp nlexp) Sil.exp_zero in
  let inst = Sil.inst_rearrange (not ptr_tested_for_zero) loc (State.get_path_pos ()) in
  L.d_strln ".... Rearrangement Start ....";
  L.d_str "Exp: "; Sil.d_exp nlexp; L.d_ln ();
  L.d_str "Prop: "; L.d_ln(); Prop.d_prop prop; L.d_ln (); L.d_ln ();
  if report_deref_errors then check_dereference_error pdesc prop nlexp (State.get_loc ());
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  match Prop.prop_iter_create prop with
  | None ->
      if !Config.footprint then
        [prop_iter_add_hpred_footprint_to_prop pname tenv prop (nlexp, typ) inst]
      else
        begin
          pp_rearrangement_error "sigma is empty" prop nlexp;
          raise (Exceptions.Symexec_memory_error __POS__)
        end
  | Some iter -> iter_rearrange pname tenv nlexp typ prop iter inst

(*
let pp_off fmt off =
  IList.iter (fun n -> match n with
      | Sil.Off_fld (f, t) -> F.fprintf fmt "%a " Ident.pp_fieldname f
      | Sil.Off_index e -> F.fprintf fmt "%a " (Sil.pp_exp pe_text) e) off

let sort_ftl ftl =
  let compare (f1, _) (f2, _) = Sil.fld_compare f1 f2 in
  IList.sort compare ftl
*)
