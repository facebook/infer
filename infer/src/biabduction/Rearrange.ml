(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Re-arrangement and extension of structures with fresh variables *)

module L = Logging

let list_product l1 l2 =
  let l1' = List.rev l1 in
  let l2' = List.rev l2 in
  List.fold
    ~f:(fun acc x -> List.fold ~f:(fun acc' y -> (x, y) :: acc') ~init:acc l2')
    ~init:[] l1'


let rec list_rev_and_concat l1 l2 =
  match l1 with [] -> l2 | x1 :: l1' -> list_rev_and_concat l1' (x1 :: l2)


(** Check whether the index is out of bounds.
    If the length is - 1, no check is performed.
    If the index is provably out of bound, a bound error is given.
    If the length is a constant and the index is not provably in bound, a warning is given.
*)
let check_bad_index tenv pname p len index loc =
  let len_is_constant = match len with Exp.Const _ -> true | _ -> false in
  let index_provably_out_of_bound () =
    let index_too_large = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, len, index)) in
    let index_negative = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, index, Exp.minus_one)) in
    Prover.check_atom tenv p index_too_large || Prover.check_atom tenv p index_negative
  in
  let index_provably_in_bound () =
    let len_minus_one = Exp.BinOp (Binop.PlusA None, len, Exp.minus_one) in
    let index_not_too_large =
      Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, index, len_minus_one))
    in
    let index_nonnegative = Prop.mk_inequality tenv (Exp.BinOp (Binop.Le, Exp.zero, index)) in
    Prover.check_zero tenv index
    || (* index 0 always in bound, even when we know nothing about len *)
       (Prover.check_atom tenv p index_not_too_large && Prover.check_atom tenv p index_nonnegative)
  in
  let index_has_bounds () =
    match Prover.get_bounds tenv p index with Some _, Some _ -> true | _ -> false
  in
  let get_const_opt = function Exp.Const (Const.Cint n) -> Some n | _ -> None in
  if not (index_provably_in_bound ()) then
    let len_const_opt = get_const_opt len in
    let index_const_opt = get_const_opt index in
    if index_provably_out_of_bound () then
      let deref_str = Localise.deref_str_array_bound len_const_opt index_const_opt in
      let exn =
        Exceptions.Array_out_of_bounds_l1
          (Errdesc.explain_array_access pname tenv deref_str p loc, __POS__)
      in
      Reporting.log_issue_deprecated_using_state Exceptions.Warning pname exn
    else if len_is_constant then
      let deref_str = Localise.deref_str_array_bound len_const_opt index_const_opt in
      let desc = Errdesc.explain_array_access pname tenv deref_str p loc in
      let exn =
        if index_has_bounds () then Exceptions.Array_out_of_bounds_l2 (desc, __POS__)
        else Exceptions.Array_out_of_bounds_l3 (desc, __POS__)
      in
      Reporting.log_issue_deprecated_using_state Exceptions.Warning pname exn


(** Perform bounds checking *)
let bounds_check tenv pname prop len e =
  if Config.trace_rearrange then (
    L.d_str "Bounds check index:" ; Sil.d_exp e ; L.d_str " len: " ; Sil.d_exp len ; L.d_ln () ) ;
  check_bad_index tenv pname prop len e


let rec create_struct_values pname tenv orig_prop footprint_part kind max_stamp (t : Typ.t)
    (off : Sil.offset list) inst : Sil.atom list * Sil.strexp * Typ.t =
  if Config.trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering create_struct_values" ;
    L.d_str "typ: " ;
    Typ.d_full t ;
    L.d_ln () ;
    L.d_str "off: " ;
    Sil.d_offset_list off ;
    L.d_ln () ;
    L.d_ln () ) ;
  let new_id () = incr max_stamp ; Ident.create kind !max_stamp in
  let res =
    let fail t off pos =
      L.d_str "create_struct_values type:" ;
      Typ.d_full t ;
      L.d_str " off: " ;
      Sil.d_offset_list off ;
      L.d_ln () ;
      raise (Exceptions.Bad_footprint pos)
    in
    match (t.desc, off) with
    | Tstruct _, [] ->
        ([], Sil.Estruct ([], inst), t)
    | Tstruct name, Off_fld (f, _) :: off' -> (
      match Tenv.lookup tenv name with
      | Some ({fields; statics} as struct_typ) -> (
        match List.find ~f:(fun (f', _, _) -> Typ.Fieldname.equal f f') (fields @ statics) with
        | Some (_, t', _) ->
            let atoms', se', res_t' =
              create_struct_values pname tenv orig_prop footprint_part kind max_stamp t' off' inst
            in
            let se = Sil.Estruct ([(f, se')], inst) in
            let replace_typ_of_f (f', t', a') =
              if Typ.Fieldname.equal f f' then (f, res_t', a') else (f', t', a')
            in
            let fields' =
              List.sort ~compare:Typ.Struct.compare_field (List.map ~f:replace_typ_of_f fields)
            in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            (atoms', se, t)
        | None ->
            fail t off __POS__ )
      | None ->
          fail t off __POS__ )
    | Tstruct _, Off_index e :: off' ->
        let atoms', se', res_t' =
          create_struct_values pname tenv orig_prop footprint_part kind max_stamp t off' inst
        in
        let e' = Absarray.array_clean_new_index footprint_part e in
        let len = Exp.Var (new_id ()) in
        let se = Sil.Earray (len, [(e', se')], inst) in
        let res_t = Typ.mk_array res_t' in
        (Sil.Aeq (e, e') :: atoms', se, res_t)
    | Tarray {elt= t'; length; stride}, off -> (
        let len =
          match length with None -> Exp.Var (new_id ()) | Some len -> Exp.Const (Const.Cint len)
        in
        match off with
        | [] ->
            ([], Sil.Earray (len, [], inst), t)
        | Sil.Off_index e :: off' ->
            bounds_check tenv pname orig_prop len e (State.get_loc_exn ()) ;
            let atoms', se', res_t' =
              create_struct_values pname tenv orig_prop footprint_part kind max_stamp t' off' inst
            in
            let e' = Absarray.array_clean_new_index footprint_part e in
            let se = Sil.Earray (len, [(e', se')], inst) in
            let res_t = Typ.mk_array ~default:t res_t' ?length ?stride in
            (Sil.Aeq (e, e') :: atoms', se, res_t)
        | Sil.Off_fld _ :: _ ->
            assert false )
    | Tint _, [] | Tfloat _, [] | Tvoid, [] | Tfun _, [] | Tptr _, [] | TVar _, [] ->
        let id = new_id () in
        ([], Sil.Eexp (Exp.Var id, inst), t)
    | (Tint _ | Tfloat _ | Tvoid | Tfun _ | Tptr _ | TVar _), Off_index e :: off' ->
        (* In this case, we lift t to the t array. *)
        let t', mk_typ_f =
          match t.Typ.desc with
          | Typ.Tptr (t', _) -> (
              (t', function desc -> Typ.mk ~default:t desc) )
          | _ ->
              (t, fun desc -> Typ.mk desc)
        in
        let len = Exp.Var (new_id ()) in
        let atoms', se', res_t' =
          create_struct_values pname tenv orig_prop footprint_part kind max_stamp t' off' inst
        in
        let e' = Absarray.array_clean_new_index footprint_part e in
        let se = Sil.Earray (len, [(e', se')], inst) in
        let res_t = mk_typ_f (Tarray {elt= res_t'; length= None; stride= None}) in
        (Sil.Aeq (e, e') :: atoms', se, res_t)
    | Tint _, _ | Tfloat _, _ | Tvoid, _ | Tfun _, _ | Tptr _, _ | TVar _, _ ->
        fail t off __POS__
  in
  if Config.trace_rearrange then (
    let _, se, _ = res in
    L.d_strln "exiting create_struct_values, returning" ;
    Sil.d_sexp se ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


(** Extend the strexp by populating the path indicated by [off].
    This means that it will add missing flds and do the case - analysis
    for array accesses. This does not catch the array - bounds errors.
    If we want to implement the checks for array bounds errors,
    we need to change this function. *)
let rec strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se (typ : Typ.t)
    (off : Sil.offset list) inst =
  let new_id () = incr max_stamp ; Ident.create kind !max_stamp in
  match (off, se, typ.desc) with
  | [], Sil.Eexp _, _ | [], Sil.Estruct _, _ ->
      [([], se, typ)]
  | [], Sil.Earray _, _ ->
      let off_new = Sil.Off_index Exp.zero :: off in
      strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se typ off_new inst
  | Off_fld _ :: _, Sil.Earray _, Tarray _ ->
      let off_new = Sil.Off_index Exp.zero :: off in
      strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se typ off_new inst
  | Off_fld (f, _) :: off', Sil.Estruct (fsel, inst'), Tstruct name -> (
    match Tenv.lookup tenv name with
    | Some ({fields; statics} as struct_typ) -> (
      match List.find ~f:(fun (f', _, _) -> Typ.Fieldname.equal f f') (fields @ statics) with
      | Some (_, typ', _) -> (
        match List.find ~f:(fun (f', _) -> Typ.Fieldname.equal f f') fsel with
        | Some (_, se') ->
            let atoms_se_typ_list' =
              strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se' typ'
                off' inst
            in
            let replace acc (res_atoms', res_se', res_typ') =
              let replace_fse ((f1, _) as ft1) =
                if Typ.Fieldname.equal f1 f then (f1, res_se') else ft1
              in
              let res_fsel' =
                List.sort ~compare:[%compare: Typ.Fieldname.t * Sil.strexp]
                  (List.map ~f:replace_fse fsel)
              in
              let replace_fta ((f1, _, a1) as fta1) =
                if Typ.Fieldname.equal f f1 then (f1, res_typ', a1) else fta1
              in
              let fields' =
                List.sort ~compare:Typ.Struct.compare_field (List.map ~f:replace_fta fields)
              in
              ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
              (res_atoms', Sil.Estruct (res_fsel', inst'), typ) :: acc
            in
            List.fold ~f:replace ~init:[] atoms_se_typ_list'
        | None ->
            let atoms', se', res_typ' =
              create_struct_values pname tenv orig_prop footprint_part kind max_stamp typ' off'
                inst
            in
            let res_fsel' =
              List.sort ~compare:[%compare: Typ.Fieldname.t * Sil.strexp] ((f, se') :: fsel)
            in
            let replace_fta (f', t', a') =
              if Typ.Fieldname.equal f' f then (f, res_typ', a') else (f', t', a')
            in
            let fields' =
              List.sort ~compare:Typ.Struct.compare_field (List.map ~f:replace_fta fields)
            in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            [(atoms', Sil.Estruct (res_fsel', inst'), typ)] )
      | None ->
          raise (Exceptions.Missing_fld (f, __POS__)) )
    | None ->
        raise (Exceptions.Missing_fld (f, __POS__)) )
  | Off_fld _ :: _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)
  | Off_index _ :: _, Sil.Eexp _, (Tint _ | Tfloat _ | Tvoid | Tfun _ | Tptr _)
  | Off_index _ :: _, Sil.Estruct _, Tstruct _ ->
      (* L.d_strln ~color:Orange "turn into an array"; *)
      let len =
        match se with
        | Sil.Eexp (_, Sil.Ialloc) ->
            Exp.one (* if allocated explicitly, we know len is 1 *)
        | _ ->
            if Config.type_size then Exp.one (* Exp.Sizeof (typ, Subtype.exact) *)
            else Exp.Var (new_id ())
      in
      let se_new = Sil.Earray (len, [(Exp.zero, se)], inst) in
      let typ_new = Typ.mk_array typ in
      strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se_new typ_new off
        inst
  | ( Off_index e :: off'
    , Sil.Earray (len, esel, inst_arr)
    , Tarray {elt= typ'; length= len_for_typ'; stride} ) -> (
      bounds_check tenv pname orig_prop len e (State.get_loc_exn ()) ;
      match List.find ~f:(fun (e', _) -> Exp.equal e e') esel with
      | Some (_, se') ->
          let atoms_se_typ_list' =
            strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se' typ' off'
              inst
          in
          let replace acc (res_atoms', res_se', res_typ') =
            let replace_ise ise = if Exp.equal e (fst ise) then (e, res_se') else ise in
            let res_esel' = List.map ~f:replace_ise esel in
            if Typ.equal res_typ' typ' || Int.equal (List.length res_esel') 1 then
              ( res_atoms'
              , Sil.Earray (len, res_esel', inst_arr)
              , Typ.mk_array ~default:typ res_typ' ?length:len_for_typ' ?stride )
              :: acc
            else raise (Exceptions.Bad_footprint __POS__)
          in
          List.fold ~f:replace ~init:[] atoms_se_typ_list'
      | None ->
          array_case_analysis_index pname tenv orig_prop footprint_part kind max_stamp len esel
            len_for_typ' typ' typ e off' inst_arr inst )
  | _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)


and array_case_analysis_index pname tenv orig_prop footprint_part kind max_stamp array_len
    array_cont typ_array_len typ_cont typ_array index off inst_arr inst =
  let check_sound t' =
    if not (Typ.equal typ_cont t' || List.is_empty array_cont) then
      raise (Exceptions.Bad_footprint __POS__)
  in
  let index_in_array =
    List.exists ~f:(fun (i, _) -> Prover.check_equal tenv Prop.prop_emp index i) array_cont
  in
  let array_is_full =
    match array_len with
    | Exp.Const (Const.Cint n') ->
        IntLit.geq (IntLit.of_int (List.length array_cont)) n'
    | _ ->
        false
  in
  if index_in_array then
    let array_default = Sil.Earray (array_len, array_cont, inst_arr) in
    let typ_default = Typ.mk_array ~default:typ_array typ_cont ?length:typ_array_len in
    [([], array_default, typ_default)]
  else if !BiabductionConfig.footprint then (
    let atoms, elem_se, elem_typ =
      create_struct_values pname tenv orig_prop footprint_part kind max_stamp typ_cont off inst
    in
    check_sound elem_typ ;
    let cont_new =
      List.sort ~compare:[%compare: Exp.t * Sil.strexp] ((index, elem_se) :: array_cont)
    in
    let array_new = Sil.Earray (array_len, cont_new, inst_arr) in
    let typ_new = Typ.mk_array ~default:typ_array elem_typ ?length:typ_array_len in
    [(atoms, array_new, typ_new)] )
  else
    let res_new =
      if array_is_full then []
      else
        let atoms, elem_se, elem_typ =
          create_struct_values pname tenv orig_prop footprint_part kind max_stamp typ_cont off inst
        in
        check_sound elem_typ ;
        let cont_new =
          List.sort ~compare:[%compare: Exp.t * Sil.strexp] ((index, elem_se) :: array_cont)
        in
        let array_new = Sil.Earray (array_len, cont_new, inst_arr) in
        let typ_new = Typ.mk_array ~default:typ_array elem_typ ?length:typ_array_len in
        [(atoms, array_new, typ_new)]
    in
    let rec handle_case acc isel_seen_rev = function
      | [] ->
          List.concat (List.rev (res_new :: acc))
      | ((i, se) as ise) :: isel_unseen ->
          let atoms_se_typ_list =
            strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se typ_cont
              off inst
          in
          let atoms_se_typ_list' =
            List.fold
              ~f:(fun acc' (atoms', se', typ') ->
                check_sound typ' ;
                let atoms_new = Sil.Aeq (index, i) :: atoms' in
                let isel_new = list_rev_and_concat isel_seen_rev ((i, se') :: isel_unseen) in
                let array_new = Sil.Earray (array_len, isel_new, inst_arr) in
                let typ_new = Typ.mk_array ~default:typ_array typ' ?length:typ_array_len in
                (atoms_new, array_new, typ_new) :: acc' )
              ~init:[] atoms_se_typ_list
          in
          let acc_new = atoms_se_typ_list' :: acc in
          let isel_seen_rev_new = ise :: isel_seen_rev in
          handle_case acc_new isel_seen_rev_new isel_unseen
    in
    handle_case [] [] array_cont


let exp_has_only_footprint_ids e = Exp.free_vars e |> Sequence.for_all ~f:Ident.is_footprint

let laundry_offset_for_footprint max_stamp offs_in =
  let rec laundry offs_seen eqs offs =
    match offs with
    | [] ->
        (List.rev offs_seen, List.rev eqs)
    | (Sil.Off_fld _ as off) :: offs' ->
        let offs_seen' = off :: offs_seen in
        laundry offs_seen' eqs offs'
    | (Sil.Off_index idx as off) :: offs' ->
        if exp_has_only_footprint_ids idx then
          let offs_seen' = off :: offs_seen in
          laundry offs_seen' eqs offs'
        else
          let () = incr max_stamp in
          let fid_new = Ident.create Ident.kfootprint !max_stamp in
          let exp_new = Exp.Var fid_new in
          let off_new = Sil.Off_index exp_new in
          let offs_seen' = off_new :: offs_seen in
          let eqs' = (fid_new, idx) :: eqs in
          laundry offs_seen' eqs' offs'
  in
  laundry [] [] offs_in


let strexp_extend_values pname tenv orig_prop footprint_part kind max_stamp se te
    (off : Sil.offset list) inst =
  let typ = Exp.texp_to_typ None te in
  let off', laundry_atoms =
    let off', eqs = laundry_offset_for_footprint max_stamp off in
    (* do laundry_offset whether footprint_part is true or not, so max_stamp is modified anyway *)
    if footprint_part then (off', List.map ~f:(fun (id, e) -> Prop.mk_eq tenv (Exp.Var id) e) eqs)
    else (off, [])
  in
  if Config.trace_rearrange then (
    L.d_str "entering strexp_extend_values se: " ;
    Sil.d_sexp se ;
    L.d_str " typ: " ;
    Typ.d_full typ ;
    L.d_str " off': " ;
    Sil.d_offset_list off' ;
    L.d_strln (if footprint_part then " FP" else " RE") ) ;
  let atoms_se_typ_list =
    strexp_extend_values_ pname tenv orig_prop footprint_part kind max_stamp se typ off' inst
  in
  let atoms_se_typ_list_filtered =
    let check_neg_atom atom =
      Prover.check_atom tenv Prop.prop_emp (Prover.atom_negate tenv atom)
    in
    let check_not_inconsistent (atoms, _, _) = not (List.exists ~f:check_neg_atom atoms) in
    List.filter ~f:check_not_inconsistent atoms_se_typ_list
  in
  if Config.trace_rearrange then L.d_strln "exiting strexp_extend_values" ;
  let sizeof_data =
    match te with
    | Exp.Sizeof sizeof_data ->
        sizeof_data
    | _ ->
        {Exp.typ= Typ.mk Typ.Tvoid; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
  in
  List.map
    ~f:(fun (atoms', se', typ') ->
      (laundry_atoms @ atoms', se', Exp.Sizeof {sizeof_data with typ= typ'}) )
    atoms_se_typ_list_filtered


let collect_root_offset exp =
  let root = Exp.root_of_lexp exp in
  let offsets = Sil.exp_get_offsets exp in
  (root, offsets)


(** Exp.Construct a points-to predicate for an expression, to add to a footprint. *)
let mk_ptsto_exp_footprint pname tenv orig_prop (lexp, typ) max_stamp inst :
    Sil.hpred * Sil.hpred * Sil.atom list =
  let root, off = collect_root_offset lexp in
  if not (exp_has_only_footprint_ids root) then
    if
      (* in angelic mode, purposely ignore dangling pointer warnings during the footprint phase -- we
     * will fix them during the re - execution phase *)
      not !BiabductionConfig.footprint
    then (
      L.internal_error "!!!! Footprint Error, Bad Root : %a !!!! @\n" Exp.pp lexp ;
      let deref_str = Localise.deref_str_dangling None in
      let err_desc =
        Errdesc.explain_dereference pname tenv deref_str orig_prop (State.get_loc_exn ())
      in
      raise (Exceptions.Dangling_pointer_dereference (None, err_desc, __POS__)) ) ;
  let off_foot, eqs = laundry_offset_for_footprint max_stamp off in
  let subtype =
    match !Language.curr_language with Clang -> Subtype.exact | Java -> Subtype.subtypes
  in
  let create_ptsto footprint_part off0 =
    match (root, off0, typ.Typ.desc) with
    | Exp.Lvar pvar, [], Typ.Tfun _ ->
        let fun_name = Typ.Procname.from_string_c_fun (Mangled.to_string (Pvar.get_name pvar)) in
        let fun_exp = Exp.Const (Const.Cfun fun_name) in
        ( []
        , Prop.mk_ptsto tenv root
            (Sil.Eexp (fun_exp, inst))
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype}) )
    | _, [], Typ.Tfun _ ->
        let atoms, se, typ =
          create_struct_values pname tenv orig_prop footprint_part Ident.kfootprint max_stamp typ
            off0 inst
        in
        ( atoms
        , Prop.mk_ptsto tenv root se
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype}) )
    | _ ->
        let atoms, se, typ =
          create_struct_values pname tenv orig_prop footprint_part Ident.kfootprint max_stamp typ
            off0 inst
        in
        ( atoms
        , Prop.mk_ptsto tenv root se
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype}) )
  in
  let atoms, ptsto_foot = create_ptsto true off_foot in
  let sub = Sil.subst_of_list eqs in
  let ptsto = Sil.hpred_sub sub ptsto_foot in
  let atoms' = List.map ~f:(fun (id, e) -> Prop.mk_eq tenv (Exp.Var id) e) eqs in
  (ptsto, ptsto_foot, atoms @ atoms')


(** Check if the path in exp exists already in the current ptsto predicate.
    If it exists, return None. Otherwise, return [Some fld] with [fld] the missing field. *)
let prop_iter_check_fields_ptsto_shallow tenv iter lexp =
  let offset = Sil.exp_get_offsets lexp in
  let _, se, _ =
    match Prop.prop_iter_current tenv iter with
    | Sil.Hpointsto (e, se, t), _ ->
        (e, se, t)
    | _ ->
        assert false
  in
  let rec check_offset se = function
    | [] ->
        None
    | Sil.Off_fld (fld, _) :: off' -> (
      match se with
      | Sil.Estruct (fsel, _) -> (
        match List.find ~f:(fun (fld', _) -> Typ.Fieldname.equal fld fld') fsel with
        | Some (_, se') ->
            check_offset se' off'
        | None ->
            Some fld )
      | _ ->
          Some fld )
    | Sil.Off_index _ :: _ ->
        None
  in
  check_offset se offset


let id_max_stamp curr_max id = max curr_max (Ident.get_stamp id)

(** [prop_iter_extend_ptsto iter lexp] extends the current psto
    predicate in [iter] with enough fields to follow the path in
    [lexp] -- field splitting model. It also materializes all
    indices accessed in lexp. *)
let prop_iter_extend_ptsto pname tenv orig_prop iter lexp inst =
  if Config.trace_rearrange then (
    L.d_str "entering prop_iter_extend_ptsto lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ) ;
  let offset = Sil.exp_get_offsets lexp in
  let max_stamp = Prop.prop_iter_free_vars iter |> Sequence.fold ~init:0 ~f:id_max_stamp in
  let extend_footprint_pred = function
    | Sil.Hpointsto (e, se, te) ->
        let atoms_se_te_list =
          strexp_extend_values pname tenv orig_prop true Ident.kfootprint (ref max_stamp) se te
            offset inst
        in
        List.map
          ~f:(fun (atoms', se', te') -> (atoms', Sil.Hpointsto (e, se', te')))
          atoms_se_te_list
    | Sil.Hlseg (k, hpara, e1, e2, el) -> (
      match hpara.Sil.body with
      | Sil.Hpointsto (e', se', te') :: body_rest ->
          let atoms_se_te_list =
            strexp_extend_values pname tenv orig_prop true Ident.kfootprint (ref max_stamp) se' te'
              offset inst
          in
          let atoms_body_list =
            List.map
              ~f:(fun (atoms0, se0, te0) -> (atoms0, Sil.Hpointsto (e', se0, te0) :: body_rest))
              atoms_se_te_list
          in
          let atoms_hpara_list =
            List.map
              ~f:(fun (atoms, body') -> (atoms, {hpara with Sil.body= body'}))
              atoms_body_list
          in
          List.map
            ~f:(fun (atoms, hpara') -> (atoms, Sil.Hlseg (k, hpara', e1, e2, el)))
            atoms_hpara_list
      | _ ->
          assert false )
    | _ ->
        assert false
  in
  let atoms_se_te_to_iter e (atoms, se, te) =
    let iter' =
      List.fold ~f:(Prop.prop_iter_add_atom !BiabductionConfig.footprint) ~init:iter atoms
    in
    Prop.prop_iter_update_current iter' (Sil.Hpointsto (e, se, te))
  in
  let do_extend e se te =
    if Config.trace_rearrange then (
      L.d_strln "entering do_extend" ;
      L.d_str "e: " ;
      Sil.d_exp e ;
      L.d_str " se : " ;
      Sil.d_sexp se ;
      L.d_str " te: " ;
      Sil.d_texp_full te ;
      L.d_ln () ;
      L.d_ln () ) ;
    let extend_kind =
      match e with
      (* Determine whether to extend the footprint part or just the normal part *)
      | Exp.Var id when not (Ident.is_footprint id) ->
          Ident.kprimed
      | Exp.Lvar pvar when Pvar.is_local pvar ->
          Ident.kprimed
      | _ ->
          Ident.kfootprint
    in
    let iter_list =
      let atoms_se_te_list =
        strexp_extend_values pname tenv orig_prop false extend_kind (ref max_stamp) se te offset
          inst
      in
      List.map ~f:(atoms_se_te_to_iter e) atoms_se_te_list
    in
    let res_iter_list =
      if Ident.equal_kind extend_kind Ident.kprimed then iter_list
        (* normal part already extended: nothing to do *)
      else
        (* extend footprint part *)
        let atoms_fp_sigma_list =
          let footprint_sigma = Prop.prop_iter_get_footprint_sigma iter in
          let sigma_pto, sigma_rest =
            List.partition_tf
              ~f:(function
                | Sil.Hpointsto (e', _, _) ->
                    Exp.equal e e'
                | Sil.Hlseg (_, _, e1, _, _) ->
                    Exp.equal e e1
                | Sil.Hdllseg (_, _, e_iF, _, _, e_iB, _) ->
                    Exp.equal e e_iF || Exp.equal e e_iB)
              footprint_sigma
          in
          let atoms_sigma_list =
            match sigma_pto with
            | [hpred] ->
                let atoms_hpred_list = extend_footprint_pred hpred in
                List.map ~f:(fun (atoms, hpred') -> (atoms, hpred' :: sigma_rest)) atoms_hpred_list
            | _ ->
                L.d_warning "Cannot extend " ;
                Sil.d_exp lexp ;
                L.d_strln " in" ;
                Prop.d_prop (Prop.prop_iter_to_prop tenv iter) ;
                L.d_ln () ;
                [([], footprint_sigma)]
          in
          List.map
            ~f:(fun (atoms, sigma') -> (atoms, List.stable_sort ~compare:Sil.compare_hpred sigma'))
            atoms_sigma_list
        in
        let iter_atoms_fp_sigma_list = list_product iter_list atoms_fp_sigma_list in
        List.map
          ~f:(fun (iter, (atoms, fp_sigma)) ->
            let iter' =
              List.fold ~f:(Prop.prop_iter_add_atom !BiabductionConfig.footprint) ~init:iter atoms
            in
            Prop.prop_iter_replace_footprint_sigma iter' fp_sigma )
          iter_atoms_fp_sigma_list
    in
    let res_prop_list = List.map ~f:(Prop.prop_iter_to_prop tenv) res_iter_list in
    L.d_str "in prop_iter_extend_ptsto lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ;
    L.d_strln "prop before:" ;
    let prop_before = Prop.prop_iter_to_prop tenv iter in
    Prop.d_prop prop_before ;
    L.d_ln () ;
    L.d_ln () ;
    L.d_ln () ;
    L.d_strln "prop list after:" ;
    Propgraph.d_proplist prop_before res_prop_list ;
    L.d_ln () ;
    L.d_ln () ;
    L.d_ln () ;
    res_iter_list
  in
  match Prop.prop_iter_current tenv iter with
  | Sil.Hpointsto (e, se, te), _ ->
      do_extend e se te
  | _ ->
      assert false


(** Add a pointsto for [root(lexp): typ] to the sigma and footprint of a
    prop, if it's compatible with the allowed footprint
    variables. Then, change it into a iterator. This function ensures
    that [root(lexp): typ] is the current hpred of the iterator. typ
    is the type of the root of lexp. *)
let prop_iter_add_hpred_footprint_to_prop pname tenv prop (lexp, typ) inst =
  let max_stamp = Prop.footprint_free_vars prop |> Sequence.fold ~init:0 ~f:id_max_stamp in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint pname tenv prop (lexp, typ) (ref max_stamp) inst
  in
  L.d_strln "++++ Adding footprint frame" ;
  Prop.d_prop (Prop.prop_hpred_star Prop.prop_emp ptsto) ;
  L.d_ln () ;
  L.d_ln () ;
  let eprop = Prop.expose prop in
  let sigma_fp = ptsto_foot :: eprop.Prop.sigma_fp in
  let nsigma_fp = Prop.sigma_normalize_prop tenv Prop.prop_emp sigma_fp in
  let prop' = Prop.normalize tenv (Prop.set eprop ~sigma_fp:nsigma_fp) in
  let prop_new =
    List.fold
      ~f:(Prop.prop_atom_and tenv ~footprint:!BiabductionConfig.footprint)
      ~init:prop' atoms
  in
  let iter =
    match Prop.prop_iter_create prop_new with
    | None -> (
        let prop_new' = Prop.normalize tenv (Prop.prop_hpred_star prop_new ptsto) in
        match Prop.prop_iter_create prop_new' with None -> assert false | Some iter -> iter )
    | Some iter ->
        Prop.prop_iter_prev_then_insert iter ptsto
  in
  let offsets_default = Sil.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter offsets_default


(** If [lexp] is an access to a field that is annotated with @GuardedBy, add constraints to [prop]
    expressing the safety conditions for the access. Complain if these conditions cannot be met. *)
let add_guarded_by_constraints tenv prop lexp pdesc =
  let lookup = Tenv.lookup tenv in
  let pname = Procdesc.get_proc_name pdesc in
  let excluded_guardedby_string str =
    (* nothing with a space in it can be a valid Java expression, shouldn't warn *)
    let is_invalid_exp_str str = String.contains str ' ' in
    (* don't warn on @GuardedBy("ui_thread") in any form *)
    let is_ui_thread str =
      let lowercase_str = String.lowercase str in
      String.equal lowercase_str "ui_thread"
      || String.equal lowercase_str "ui-thread"
      || String.equal lowercase_str "uithread"
    in
    is_invalid_exp_str str || is_ui_thread str
  in
  let guarded_by_str_is_this guarded_by_str = String.is_suffix ~suffix:"this" guarded_by_str in
  let guarded_by_str_is_class guarded_by_str class_str =
    let dollar_normalize s = String.map s ~f:(function '$' -> '.' | c -> c) in
    String.is_suffix ~suffix:(dollar_normalize guarded_by_str)
      (dollar_normalize (class_str ^ ".class"))
  in
  let guarded_by_str_is_current_class guarded_by_str = function
    | Typ.Procname.Java java_pname ->
        (* programmers write @GuardedBy("MyClass.class") when the field is guarded by the class *)
        guarded_by_str_is_class guarded_by_str (Typ.Procname.Java.get_class_name java_pname)
    | _ ->
        false
  in
  let guarded_by_str_is_class_this class_name guarded_by_str =
    let fully_qualified_this = Printf.sprintf "%s.this" class_name in
    String.is_suffix ~suffix:guarded_by_str fully_qualified_this
  in
  (* return true if [guarded_by_str] is a suffix of "<name_of_super_class>.this" *)
  let guarded_by_str_is_super_class_this guarded_by_str pname =
    match pname with
    | Typ.Procname.Java java_pname ->
        let current_class_type_name = Typ.Procname.Java.get_class_type_name java_pname in
        let comparison class_type_name _ =
          guarded_by_str_is_class_this (Typ.Name.to_string class_type_name) guarded_by_str
        in
        PatternMatch.supertype_exists tenv comparison current_class_type_name
    | _ ->
        false
  in
  (* return true if [guarded_by_str] is as suffix of "<name_of_current_class>.this" *)
  let guarded_by_str_is_current_class_this guarded_by_str = function
    | Typ.Procname.Java java_pname ->
        guarded_by_str_is_class_this (Typ.Procname.Java.get_class_name java_pname) guarded_by_str
    | _ ->
        false
  in
  let extract_guarded_by_str item_annot =
    let annot_extract_guarded_by_str ((annot : Annot.t), _) =
      if Annotations.annot_ends_with annot Annotations.guarded_by then
        match annot.parameters with
        | [guarded_by_str] when not (excluded_guardedby_string guarded_by_str) ->
            Some guarded_by_str
        | _ ->
            None
      else None
    in
    List.find_map ~f:annot_extract_guarded_by_str item_annot
  in
  let extract_suppress_warnings_str item_annot =
    let annot_suppress_warnings_str ((annot : Annot.t), _) =
      if Annotations.annot_ends_with annot Annotations.suppress_lint then
        match annot.parameters with [suppr_str] -> Some suppr_str | _ -> None
      else None
    in
    List.find_map ~f:annot_suppress_warnings_str item_annot
  in
  (* if [fld] is annotated with @GuardedBy("mLock"), return mLock *)
  let get_guarded_by_fld_str fld typ =
    match Typ.Struct.get_field_type_and_annotation ~lookup fld typ with
    | Some (_, item_annot) -> (
      match extract_guarded_by_str item_annot with
      | Some "this" ->
          (* expand "this" into <classname>.this *)
          Some (Printf.sprintf "%s.this" (Typ.Fieldname.Java.get_class fld))
      | guarded_by_str_opt ->
          guarded_by_str_opt )
    | _ ->
        None
  in
  (* find A.guarded_by_fld_str |-> B and return Some B, or None if there is no such hpred *)
  let find_guarded_by_exp guarded_by_str0 sigma =
    let is_guarded_by_fld guarded_by_str fld _ =
      (* this comparison needs to be somewhat fuzzy, since programmers are free to write
         @GuardedBy("mLock"), @GuardedBy("MyClass.mLock"), or use other conventions *)
      String.equal (Typ.Fieldname.to_flat_string fld) guarded_by_str
      || String.equal (Typ.Fieldname.to_string fld) guarded_by_str
    in
    let get_fld_strexp_and_typ typ f flds =
      let match_one (fld, strexp) =
        match Typ.Struct.get_field_type_and_annotation ~lookup fld typ with
        | Some (fld_typ, _) when f fld fld_typ ->
            Some (strexp, fld_typ)
        | _ ->
            None
      in
      List.find_map ~f:match_one flds
    in
    (* sometimes, programmers will write @GuardedBy("T.f") with the meaning "guarded by the field f
       of the object of type T in the current state." note that this is ambiguous when there are
       multiple objects of type T, but let's try to respect the intention *)
    let match_on_field_type typ flds =
      match String.rsplit2 guarded_by_str0 ~on:'.' with
      | Some (class_part, field_part) -> (
          let typ_matches_guarded_by _ {Typ.desc} =
            match desc with
            | Typ.Tptr (ptr_typ, _) ->
                String.is_suffix ~suffix:class_part (Typ.to_string ptr_typ)
            | _ ->
                false
          in
          match get_fld_strexp_and_typ typ typ_matches_guarded_by flds with
          | Some (Sil.Eexp (matching_exp, _), _) ->
              List.find_map
                ~f:(function
                  | Sil.Hpointsto (lhs_exp, Estruct (matching_flds, _), Sizeof {typ= fld_typ})
                    when Exp.equal lhs_exp matching_exp ->
                      get_fld_strexp_and_typ fld_typ (is_guarded_by_fld field_part) matching_flds
                  | _ ->
                      None)
                sigma
          | _ ->
              None )
      | _ ->
          None
    in
    List.find_map
      ~f:(fun hpred ->
        (* FIXME: silenced warning may be legit *)
        match[@warning "-57"] hpred with
        | Sil.Hpointsto ((Const (Cclass clazz) as lhs_exp), _, Exp.Sizeof {typ})
        | Sil.Hpointsto (_, Sil.Eexp ((Const (Cclass clazz) as lhs_exp), _), Exp.Sizeof {typ})
          when guarded_by_str_is_class guarded_by_str0 (Ident.name_to_string clazz) ->
            Some (Sil.Eexp (lhs_exp, Sil.inst_none), typ)
        | Sil.Hpointsto (_, Estruct (flds, _), Exp.Sizeof {typ}) -> (
          (* first, try to find a field that exactly matches the guarded-by string *)
          match get_fld_strexp_and_typ typ (is_guarded_by_fld guarded_by_str0) flds with
          | None when guarded_by_str_is_this guarded_by_str0 ->
              (* if the guarded-by string is "OuterClass.this", look for "this$n" for some n.
                     note that this is a bit sketchy when there are mutliple this$n's, but there's
                     nothing we can do to disambiguate them. *)
              get_fld_strexp_and_typ typ (fun f _ -> Typ.Fieldname.Java.is_outer_instance f) flds
          | None ->
              (* can't find an exact match. try a different convention. *)
              match_on_field_type typ flds
          | Some _ as res_opt ->
              res_opt )
        | Sil.Hpointsto (Lvar pvar, rhs_exp, Exp.Sizeof {typ})
          when ( guarded_by_str_is_current_class_this guarded_by_str0 pname
               || guarded_by_str_is_super_class_this guarded_by_str0 pname )
               && Pvar.is_this pvar ->
            Some (rhs_exp, typ)
        | _ ->
            None )
      sigma
  in
  (* warn if the access to [lexp] is not protected by the [guarded_by_fld_str] lock *)
  let enforce_guarded_access_ accessed_fld guarded_by_str prop =
    (* return true if [pdesc] has an annotation that matches [guarded_by_str] *)
    let proc_has_matching_annot pdesc guarded_by_str =
      match extract_guarded_by_str (Annotations.pdesc_get_return_annot pdesc) with
      | Some proc_guarded_by_str ->
          (* the lock is not held, but the procedure is annotated with @GuardedBy *)
          String.equal proc_guarded_by_str guarded_by_str
      | None ->
          false
    in
    let is_synchronized_on_class guarded_by_str =
      guarded_by_str_is_current_class guarded_by_str pname
      && Procdesc.is_java_synchronized pdesc
      &&
      match pname with
      | Typ.Procname.Java java_pname ->
          Typ.Procname.Java.is_static java_pname
      | _ ->
          false
    in
    let warn accessed_fld guarded_by_str =
      let loc = State.get_loc_exn () in
      let err_desc = Localise.desc_unsafe_guarded_by_access accessed_fld guarded_by_str loc in
      let exn = Exceptions.Unsafe_guarded_by_access (err_desc, __POS__) in
      Reporting.log_issue_deprecated_using_state Exceptions.Error pname exn
    in
    let rec is_read_write_lock typ =
      let str_is_read_write_lock str =
        String.is_suffix ~suffix:"ReadWriteUpdateLock" str
        || String.is_suffix ~suffix:"ReadWriteLock" str
      in
      match typ.Typ.desc with
      | Typ.Tstruct name ->
          str_is_read_write_lock (Typ.Name.name name)
      | Typ.Tptr (typ, _) ->
          is_read_write_lock typ
      | _ ->
          false
    in
    let has_lock guarded_by_exp =
      ( guarded_by_str_is_current_class_this guarded_by_str pname
      || guarded_by_str_is_super_class_this guarded_by_str pname )
      && Procdesc.is_java_synchronized pdesc
      || ( guarded_by_str_is_current_class guarded_by_str pname
         && Procdesc.is_java_synchronized pdesc
         &&
         match pname with
         | Typ.Procname.Java java_pname ->
             Typ.Procname.Java.is_static java_pname
         | _ ->
             false )
      || (* or the prop says we already have the lock *)
         List.exists
           ~f:(function Sil.Apred (Alocked, _) -> true | _ -> false)
           (Attribute.get_for_exp tenv prop guarded_by_exp)
    in
    let guardedby_is_self_referential =
      String.equal "itself" (String.lowercase guarded_by_str)
      || String.is_suffix ~suffix:guarded_by_str (Typ.Fieldname.to_string accessed_fld)
    in
    let proc_has_suppress_guarded_by_annot pdesc =
      match extract_suppress_warnings_str (Annotations.pdesc_get_return_annot pdesc) with
      | Some suppression_str ->
          String.equal suppression_str "InvalidAccessToGuardedField"
      | None ->
          false
    in
    let should_warn pdesc =
      (* adding this check implements "by reference" semantics for guarded-by rather than "by value"
         semantics. if this access is through a local L or field V.f
         (where f is not the @GuardedBy field!), we will not warn.
      *)
      let is_accessible_through_local_ref exp =
        List.exists
          ~f:(function
            | Sil.Hpointsto (Lvar _, Eexp (rhs_exp, _), _) ->
                Exp.equal exp rhs_exp
            | Sil.Hpointsto (_, Estruct (flds, _), _) ->
                List.exists
                  ~f:(fun (fld, strexp) ->
                    match strexp with
                    | Sil.Eexp (rhs_exp, _) ->
                        Exp.equal exp rhs_exp && not (Typ.Fieldname.equal fld accessed_fld)
                    | _ ->
                        false )
                  flds
            | _ ->
                false)
          prop.Prop.sigma
      in
      Procdesc.get_access pdesc <> PredSymb.Private
      && (not (Annotations.pdesc_return_annot_ends_with pdesc Annotations.visibleForTesting))
      && (not
            ( match Procdesc.get_proc_name pdesc with
            | Typ.Procname.Java java_pname ->
                Typ.Procname.Java.is_access_method java_pname
            | _ ->
                false ))
      && (not (is_accessible_through_local_ref lexp))
      && (not guardedby_is_self_referential)
      && not (proc_has_suppress_guarded_by_annot pdesc)
    in
    match find_guarded_by_exp guarded_by_str prop.Prop.sigma with
    | Some (Sil.Eexp (guarded_by_exp, _), typ) ->
        if is_read_write_lock typ then
          (* TODO: model/understand read-write locks rather than ignoring them *)
          prop
        else if has_lock guarded_by_exp then
          (* we have the lock; no need to add a proof obligation *)
          (* TODO: materialize [fld], but don't add [fld] to the footprint. *)
          prop
        else if (* we don't know if we have the lock or not. *)
                should_warn pdesc then (
          (* non-private method; can't ensure that the lock is held. warn. *)
          warn accessed_fld guarded_by_str ;
          prop )
        else
          (* private method. add locked proof obligation to [pdesc] *)
          Attribute.add tenv ~footprint:true prop Alocked [guarded_by_exp]
    | _ ->
        if
          (not
             ( proc_has_matching_annot pdesc guarded_by_str
             || is_synchronized_on_class guarded_by_str ))
          && should_warn pdesc
        then
          (* can't find the object the annotation refers to, and procedure is not annotated. warn *)
          warn accessed_fld guarded_by_str
        else
          (* procedure has same GuardedBy annotation as the field. we would like to add a proof
             obligation, but we can't (because we can't find an expression corresponding to the
             lock in the current prop). so just be silent. *)
          () ;
        prop
  in
  let enforce_guarded_access fld typ prop =
    match get_guarded_by_fld_str fld typ with
    | Some guarded_by_fld_str ->
        enforce_guarded_access_ fld guarded_by_fld_str prop
    | None ->
        prop
  in
  let check_fld_locks typ prop_acc (fld, strexp) =
    match strexp with
    | Sil.Eexp (exp, _) when Exp.equal exp lexp ->
        enforce_guarded_access fld typ prop_acc
    | _ ->
        prop_acc
  in
  let hpred_check_flds prop_acc = function
    | Sil.Hpointsto (_, Estruct (flds, _), Sizeof {typ}) ->
        List.fold ~f:(check_fld_locks typ) ~init:prop_acc flds
    | _ ->
        prop_acc
  in
  match lexp with
  | Exp.Lfield (_, fld, typ) ->
      (* check for direct access to field annotated with @GuardedBy *)
      enforce_guarded_access fld typ prop
  | _ ->
      (* check for access via alias *)
      List.fold ~f:hpred_check_flds ~init:prop prop.Prop.sigma


(** Add a pointsto for [root(lexp): typ] to the iterator and to the
    footprint, if it's compatible with the allowed footprint
    variables. This function ensures that [root(lexp): typ] is the
    current hpred of the iterator. typ is the type of the root of lexp. *)
let prop_iter_add_hpred_footprint pname tenv orig_prop iter (lexp, typ) inst =
  if Config.trace_rearrange then (
    L.d_strln "entering prop_iter_add_hpred_footprint" ;
    L.d_str "lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ;
    L.d_str "typ:" ;
    Typ.d_full typ ;
    L.d_ln () ) ;
  let max_stamp =
    Prop.prop_iter_footprint_free_vars iter |> Sequence.fold ~init:0 ~f:id_max_stamp
  in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint pname tenv orig_prop (lexp, typ) (ref max_stamp) inst
  in
  L.d_strln "++++ Adding footprint frame" ;
  Prop.d_prop (Prop.prop_hpred_star Prop.prop_emp ptsto) ;
  L.d_ln () ;
  L.d_ln () ;
  let sigma_fp = ptsto_foot :: Prop.prop_iter_get_footprint_sigma iter in
  let iter_foot = Prop.prop_iter_prev_then_insert iter ptsto in
  let iter_foot_atoms =
    List.fold ~f:(Prop.prop_iter_add_atom !BiabductionConfig.footprint) ~init:iter_foot atoms
  in
  let iter' = Prop.prop_iter_replace_footprint_sigma iter_foot_atoms sigma_fp in
  let offsets_default = Sil.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter' offsets_default


exception ARRAY_ACCESS

let rearrange_arith tenv lexp prop =
  if Config.trace_rearrange then (
    L.d_strln "entering rearrange_arith" ;
    L.d_str "lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ;
    L.d_str "prop: " ;
    L.d_ln () ;
    Prop.d_prop prop ;
    L.d_ln () ;
    L.d_ln () ) ;
  if Config.array_level >= 2 then raise ARRAY_ACCESS
  else
    let root = Exp.root_of_lexp lexp in
    if Prover.check_allocatedness tenv prop root then raise ARRAY_ACCESS
    else raise (Exceptions.Symexec_memory_error __POS__)


let pp_rearrangement_error message prop lexp =
  L.d_printfln ".... Rearrangement Error .... %s" message ;
  L.d_str "Exp:" ;
  Sil.d_exp lexp ;
  L.d_ln () ;
  L.d_str "Prop:" ;
  L.d_ln () ;
  Prop.d_prop prop ;
  L.d_ln () ;
  L.d_ln ()


(** do re-arrangement for an iter whose current element is a pointsto *)
let iter_rearrange_ptsto pname tenv orig_prop iter lexp inst =
  if Config.trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering iter_rearrange_ptsto" ;
    L.d_str "lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ;
    L.d_strln "prop:" ;
    Prop.d_prop orig_prop ;
    L.d_ln () ;
    L.d_strln "iter:" ;
    Prop.d_prop (Prop.prop_iter_to_prop tenv iter) ;
    L.d_ln () ;
    L.d_ln () ) ;
  let check_field_splitting () =
    match prop_iter_check_fields_ptsto_shallow tenv iter lexp with
    | None ->
        ()
    | Some fld ->
        pp_rearrangement_error "field splitting check failed" orig_prop lexp ;
        raise (Exceptions.Missing_fld (fld, __POS__))
  in
  let res =
    if !BiabductionConfig.footprint then prop_iter_extend_ptsto pname tenv orig_prop iter lexp inst
    else (
      check_field_splitting () ;
      match Prop.prop_iter_current tenv iter with
      | Sil.Hpointsto (e, se, te), offset ->
          let max_stamp = Prop.prop_iter_free_vars iter |> Sequence.fold ~init:0 ~f:id_max_stamp in
          let atoms_se_te_list =
            strexp_extend_values pname tenv orig_prop false Ident.kprimed (ref max_stamp) se te
              offset inst
          in
          let handle_case (atoms', se', te') =
            let iter' =
              List.fold ~f:(Prop.prop_iter_add_atom !BiabductionConfig.footprint) ~init:iter atoms'
            in
            Prop.prop_iter_update_current iter' (Sil.Hpointsto (e, se', te'))
          in
          let filter it =
            let p = Prop.prop_iter_to_prop tenv it in
            not (Prover.check_inconsistency tenv p)
          in
          List.filter ~f:filter (List.map ~f:handle_case atoms_se_te_list)
      | _ ->
          [iter] )
  in
  if Config.trace_rearrange then (
    L.d_strln "exiting iter_rearrange_ptsto, returning results" ;
    Prop.d_proplist_with_typ (List.map ~f:(Prop.prop_iter_to_prop tenv) res) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


(** do re-arrangment for an iter whose current element is a nonempty listseg *)
let iter_rearrange_ne_lseg tenv recurse_on_iters iter para e1 e2 elist =
  if Config.nelseg then
    let iter_inductive_case =
      let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let _, para_inst1 = Sil.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Sil.Lseg_NE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1
    in
    let iter_base_case =
      let _, para_inst = Sil.hpara_instantiate para e1 e2 elist in
      Prop.prop_iter_update_current_by_list iter para_inst
    in
    recurse_on_iters [iter_inductive_case; iter_base_case]
  else
    let iter_inductive_case =
      let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let _, para_inst1 = Sil.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Sil.Lseg_PE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1
    in
    recurse_on_iters [iter_inductive_case]


(** do re-arrangment for an iter whose current element is a nonempty dllseg to be unrolled from lhs *)
let iter_rearrange_ne_dllseg_first tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Sil.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 =
      para_dll_inst1 @ [Prop.mk_dllseg tenv Sil.Lseg_NE para_dll n' e1 e3 e4 elist]
    in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_base_case =
    let _, para_dll_inst = Sil.hpara_dll_instantiate para_dll e1 e2 e3 elist in
    let iter' = Prop.prop_iter_update_current_by_list iter para_dll_inst in
    let prop' = Prop.prop_iter_to_prop tenv iter' in
    let prop'' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e1 e4 prop' in
    match Prop.prop_iter_create prop'' with None -> assert false | Some iter' -> iter'
  in
  recurse_on_iters [iter_inductive_case; iter_base_case]


(** do re-arrangment for an iter whose current element is a nonempty dllseg to be unrolled from rhs *)
let iter_rearrange_ne_dllseg_last tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Sil.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 =
      para_dll_inst1 @ [Prop.mk_dllseg tenv Sil.Lseg_NE para_dll e1 e2 e4 n' elist]
    in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_base_case =
    let _, para_dll_inst = Sil.hpara_dll_instantiate para_dll e4 e2 e3 elist in
    let iter' = Prop.prop_iter_update_current_by_list iter para_dll_inst in
    let prop' = Prop.prop_iter_to_prop tenv iter' in
    let prop'' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e1 e4 prop' in
    match Prop.prop_iter_create prop'' with None -> assert false | Some iter' -> iter'
  in
  recurse_on_iters [iter_inductive_case; iter_base_case]


(** do re-arrangment for an iter whose current element is a possibly empty listseg *)
let iter_rearrange_pe_lseg tenv recurse_on_iters default_case_iter iter para e1 e2 elist =
  let iter_nonemp_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_inst1 = Sil.hpara_instantiate para e1 n' elist in
    let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Sil.Lseg_PE para n' e2 elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop tenv iter in
    let prop' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e1 e2 removed_prop in
    match Prop.prop_iter_create prop' with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' ->
        [iter_nonemp_case; iter']
  in
  recurse_on_iters iter_subcases


(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled from lhs *)
let iter_rearrange_pe_dllseg_first tenv recurse_on_iters default_case_iter iter para_dll e1 e2 e3
    e4 elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Sil.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 =
      para_dll_inst1 @ [Prop.mk_dllseg tenv Sil.Lseg_PE para_dll n' e1 e3 e4 elist]
    in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop tenv iter in
    let prop' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e1 e3 removed_prop in
    let prop'' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e2 e4 prop' in
    match Prop.prop_iter_create prop'' with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' ->
        [iter_inductive_case; iter']
  in
  recurse_on_iters iter_subcases


(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled from rhs *)
let iter_rearrange_pe_dllseg_last tenv recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4
    elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Sil.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 =
      para_dll_inst1 @ [Prop.mk_dllseg tenv Sil.Lseg_PE para_dll e1 e2 e4 n' elist]
    in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_subcases =
    let removed_prop = Prop.prop_iter_remove_curr_then_to_prop tenv iter in
    let prop' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e1 e3 removed_prop in
    let prop'' = Prop.conjoin_eq tenv ~footprint:!BiabductionConfig.footprint e2 e4 prop' in
    match Prop.prop_iter_create prop'' with
    | None ->
        let iter' = default_case_iter (Prop.prop_iter_set_state iter ()) in
        [Prop.prop_iter_set_state iter' ()]
    | Some iter' ->
        [iter_inductive_case; iter']
  in
  recurse_on_iters iter_subcases


(** find the type at the offset from the given type expression, if any *)
let type_at_offset tenv texp off =
  let rec strip_offset (off : Sil.offset list) (typ : Typ.t) =
    match (off, typ.desc) with
    | [], _ ->
        Some typ
    | Off_fld (f, _) :: off', Tstruct name -> (
      match Tenv.lookup tenv name with
      | Some {fields} -> (
        match List.find ~f:(fun (f', _, _) -> Typ.Fieldname.equal f f') fields with
        | Some (_, typ', _) ->
            strip_offset off' typ'
        | None ->
            None )
      | None ->
          None )
    | Off_index _ :: off', Tarray {elt= typ'} ->
        strip_offset off' typ'
    | _ ->
        None
  in
  match texp with Exp.Sizeof {typ} -> strip_offset off typ | _ -> None


(** Check that the size of a type coming from an instruction does not exceed the size of the type from the pointsto predicate
    For example, that a pointer to int is not used to assign to a char *)
let check_type_size tenv pname prop texp off typ_from_instr =
  L.d_strln ~color:Orange "check_type_size" ;
  L.d_str "off: " ;
  Sil.d_offset_list off ;
  L.d_ln () ;
  L.d_str "typ_from_instr: " ;
  Typ.d_full typ_from_instr ;
  L.d_ln () ;
  match type_at_offset tenv texp off with
  | Some typ_of_object ->
      L.d_str "typ_o: " ;
      Typ.d_full typ_of_object ;
      L.d_ln () ;
      if
        Prover.type_size_comparable typ_from_instr typ_of_object
        && not (Prover.check_type_size_leq typ_from_instr typ_of_object)
      then
        let deref_str = Localise.deref_str_pointer_size_mismatch typ_from_instr typ_of_object in
        let loc = State.get_loc_exn () in
        let exn =
          Exceptions.Pointer_size_mismatch
            (Errdesc.explain_dereference pname tenv deref_str prop loc, __POS__)
        in
        Reporting.log_issue_deprecated_using_state Exceptions.Warning pname exn
  | None ->
      L.d_str "texp: " ; Sil.d_texp_full texp ; L.d_ln ()


(** Exposes lexp |->- from iter. In case that it is not possible to
 * expose lexp |->-, this function prints an error message and
 * faults. There are four things to note. First, typ is the type of the
 * root of lexp. Second, prop should mean the same as iter. Third, the
 * result [] means that the given input iter is inconsistent. This
 * happens when the theorem prover can prove the inconsistency of prop,
 * only after unrolling some predicates in prop. This function ensures
 * that the theorem prover cannot prove the inconsistency of any of the
 * new iters in the result. *)
let rec iter_rearrange pname tenv lexp typ_from_instr prop iter inst :
    Sil.offset list Prop.prop_iter list =
  let rec root_typ_of_offsets = function
    | Sil.Off_fld (f, fld_typ) :: _ -> (
      match fld_typ.desc with
      | Tstruct _ ->
          (* access through field: get the struct type from the field *)
          if Config.trace_rearrange then (
            L.d_increase_indent () ;
            L.d_printfln "iter_rearrange: root of lexp accesses field %a" Typ.Fieldname.pp f ;
            L.d_str "  struct type from field: " ;
            Typ.d_full fld_typ ;
            L.d_ln () ;
            L.d_decrease_indent () ;
            L.d_ln () ) ;
          fld_typ
      | _ ->
          typ_from_instr )
    | Sil.Off_index _ :: off ->
        Typ.mk_array (root_typ_of_offsets off)
    | _ ->
        typ_from_instr
  in
  let typ = root_typ_of_offsets (Sil.exp_get_offsets lexp) in
  if Config.trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering iter_rearrange" ;
    L.d_str "lexp: " ;
    Sil.d_exp lexp ;
    L.d_ln () ;
    L.d_str "typ: " ;
    Typ.d_full typ ;
    L.d_ln () ;
    L.d_str "type from instruction: " ;
    Typ.d_full typ_from_instr ;
    L.d_ln () ;
    L.d_strln "prop:" ;
    Prop.d_prop prop ;
    L.d_ln () ;
    L.d_strln "iter:" ;
    Prop.d_prop (Prop.prop_iter_to_prop tenv iter) ;
    L.d_ln () ;
    L.d_ln () ) ;
  let default_case_iter (iter' : unit Prop.prop_iter) =
    if Config.trace_rearrange then L.d_strln "entering default_case_iter" ;
    if !BiabductionConfig.footprint then
      prop_iter_add_hpred_footprint pname tenv prop iter' (lexp, typ) inst
    else if Config.array_level >= 1 && (not !BiabductionConfig.footprint) && Exp.pointer_arith lexp
    then rearrange_arith tenv lexp prop
    else (
      pp_rearrangement_error "cannot find predicate with root" prop lexp ;
      if not !BiabductionConfig.footprint then Printer.force_delayed_prints () ;
      raise (Exceptions.Symexec_memory_error __POS__) )
  in
  let recurse_on_iters iters =
    let f_one_iter iter' =
      let prop' = Prop.prop_iter_to_prop tenv iter' in
      if Prover.check_inconsistency tenv prop' then []
      else
        iter_rearrange pname tenv (Prop.lexp_normalize_prop tenv prop' lexp) typ prop' iter' inst
    in
    let rec f_many_iters iters_lst = function
      | [] ->
          List.concat (List.rev iters_lst)
      | iter' :: iters' ->
          let iters_res' = f_one_iter iter' in
          f_many_iters (iters_res' :: iters_lst) iters'
    in
    f_many_iters [] iters
  in
  let filter = function
    | Sil.Hpointsto (base, _, _) | Sil.Hlseg (_, _, base, _, _) ->
        Prover.is_root tenv prop base lexp
    | Sil.Hdllseg (_, _, first, _, _, last, _) -> (
        let result_first = Prover.is_root tenv prop first lexp in
        match result_first with
        | None ->
            Prover.is_root tenv prop last lexp
        | Some _ ->
            result_first )
  in
  let res =
    match Prop.prop_iter_find iter filter with
    | None ->
        [default_case_iter iter]
    | Some iter -> (
      match Prop.prop_iter_current tenv iter with
      | Sil.Hpointsto (_, _, texp), off ->
          if Config.type_size then check_type_size tenv pname prop texp off typ_from_instr ;
          iter_rearrange_ptsto pname tenv prop iter lexp inst
      | Sil.Hlseg (Sil.Lseg_NE, para, e1, e2, elist), _ ->
          iter_rearrange_ne_lseg tenv recurse_on_iters iter para e1 e2 elist
      | Sil.Hlseg (Sil.Lseg_PE, para, e1, e2, elist), _ ->
          iter_rearrange_pe_lseg tenv recurse_on_iters default_case_iter iter para e1 e2 elist
      | Sil.Hdllseg (Sil.Lseg_NE, para_dll, e1, e2, e3, e4, elist), _ -> (
        match (Prover.is_root tenv prop e1 lexp, Prover.is_root tenv prop e4 lexp) with
        | None, None ->
            assert false
        | Some _, _ ->
            iter_rearrange_ne_dllseg_first tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist
        | _, Some _ ->
            iter_rearrange_ne_dllseg_last tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist )
      | Sil.Hdllseg (Sil.Lseg_PE, para_dll, e1, e2, e3, e4, elist), _ -> (
        match (Prover.is_root tenv prop e1 lexp, Prover.is_root tenv prop e4 lexp) with
        | None, None ->
            assert false
        | Some _, _ ->
            iter_rearrange_pe_dllseg_first tenv recurse_on_iters default_case_iter iter para_dll e1
              e2 e3 e4 elist
        | _, Some _ ->
            iter_rearrange_pe_dllseg_last tenv recurse_on_iters default_case_iter iter para_dll e1
              e2 e3 e4 elist ) )
  in
  if Config.trace_rearrange then (
    L.d_strln "exiting iter_rearrange, returning results" ;
    Prop.d_proplist_with_typ (List.map ~f:(Prop.prop_iter_to_prop tenv) res) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


let is_weak_captured_var pdesc var_name =
  let pname = Procdesc.get_proc_name pdesc in
  match pname with
  | Block _ ->
      let is_weak_captured (var, typ) =
        match typ.Typ.desc with
        | Typ.Tptr (_, Pk_objc_weak) ->
            String.equal var_name (Mangled.to_string var)
        | _ ->
            false
      in
      List.exists ~f:is_weak_captured (Procdesc.get_captured pdesc)
  | _ ->
      false


let var_has_annotation ?(check_weak_captured_var = false) pdesc is_annotation pvar =
  let is_weak_captured_var = is_weak_captured_var pdesc (Pvar.to_string pvar) in
  let ann_sig = Models.get_modelled_annotated_signature (Procdesc.get_attributes pdesc) in
  AnnotatedSignature.param_has_annot is_annotation pvar ann_sig
  || (check_weak_captured_var && is_weak_captured_var)


let attr_has_annot is_annotation tenv prop exp =
  let attr_has_annot = function
    | Sil.Apred ((Aretval (pname, ret_attr) | Aundef (pname, ret_attr, _, _)), _)
      when is_annotation ret_attr ->
        Some (Typ.Procname.to_string pname)
    | _ ->
        None
  in
  try List.find_map ~f:attr_has_annot (Attribute.get_for_exp tenv prop exp) with
  | Not_found_s _ | Caml.Not_found ->
      None


let is_strexp_pt_fld_with_annot tenv obj_str is_annotation typ deref_exp (fld, strexp) =
  let lookup = Tenv.lookup tenv in
  let fld_has_annot fld =
    match Typ.Struct.get_field_type_and_annotation ~lookup fld typ with
    | Some (_, annot) ->
        is_annotation annot
    | _ ->
        false
  in
  match strexp with
  | Sil.Eexp ((Exp.Var _ as exp), _) when Exp.equal exp deref_exp ->
      let has_annot = fld_has_annot fld in
      if has_annot then obj_str := Some (Typ.Fieldname.to_simplified_string fld) ;
      has_annot
  | _ ->
      true


(* This returns true if the exp is pointed to only by fields or parameters with a given
   annotation. In that case it also returns a string representation of the annotation
   recipient. *)
let is_only_pt_by_fld_or_param_with_annot ?(check_weak_captured_var = false) pdesc tenv prop
    deref_exp is_annotation =
  let obj_str = ref None in
  let is_pt_by_fld_or_param_with_annot hpred =
    match hpred with
    | Sil.Hpointsto (Exp.Lvar pvar, Sil.Eexp ((Exp.Var _ as exp), _), _)
      when Exp.equal exp deref_exp ->
        let var_has_annotation =
          Pvar.is_seed pvar && var_has_annotation ~check_weak_captured_var pdesc is_annotation pvar
        in
        if var_has_annotation then obj_str := Some (Pvar.to_string pvar) ;
        let procname_str_opt = attr_has_annot is_annotation tenv prop exp in
        if Option.is_some procname_str_opt then obj_str := procname_str_opt ;
        (* it's ok for a local with no annotation to point to deref_exp *)
        var_has_annotation || Option.is_some procname_str_opt || Pvar.is_local pvar
    | Sil.Hpointsto (_, Sil.Estruct (flds, _), Exp.Sizeof {typ}) ->
        List.for_all ~f:(is_strexp_pt_fld_with_annot tenv obj_str is_annotation typ deref_exp) flds
    | _ ->
        true
  in
  if List.for_all ~f:is_pt_by_fld_or_param_with_annot prop.Prop.sigma && !obj_str <> None then
    !obj_str
  else None


let is_only_pt_by_fld_or_param_nullable pdesc tenv prop deref_exp =
  is_only_pt_by_fld_or_param_with_annot ~check_weak_captured_var:true pdesc tenv prop deref_exp
    Annotations.ia_is_nullable


let is_only_pt_by_fld_or_param_nonnull pdesc tenv prop deref_exp =
  Option.is_some
    (is_only_pt_by_fld_or_param_with_annot pdesc tenv prop deref_exp Annotations.ia_is_nonnull)


(** Check for dereference errors: dereferencing 0, a freed value, or an undefined value *)
let check_dereference_error tenv pdesc (prop : Prop.normal Prop.t) lexp loc =
  let pname = Procdesc.get_proc_name pdesc in
  let root = Exp.root_of_lexp lexp in
  let nullable_var_opt = is_only_pt_by_fld_or_param_nullable pdesc tenv prop root in
  let is_deref_of_nullable =
    let is_definitely_non_null exp prop = Prover.check_disequal tenv prop exp Exp.zero in
    Config.report_nullable_inconsistency && Option.is_some nullable_var_opt
    && not (is_definitely_non_null root prop)
  in
  let relevant_attributes_getters = [Attribute.get_resource tenv; Attribute.get_undef tenv] in
  let get_relevant_attributes exp =
    let rec fold_getters = function
      | [] ->
          None
      | getter :: tl -> (
        match getter prop exp with Some _ as some_attr -> some_attr | None -> fold_getters tl )
    in
    fold_getters relevant_attributes_getters
  in
  let attribute_opt =
    match get_relevant_attributes root with
    | Some att ->
        Some att
    | None ->
        (* try to remove an offset if any, and find the attribute there *)
        let root_no_offset =
          match root with
          | Exp.BinOp ((Binop.PlusPI | Binop.PlusA _ | Binop.MinusPI | Binop.MinusA _), base, _) ->
              base
          | _ ->
              root
        in
        get_relevant_attributes root_no_offset
  in
  ( if Prover.check_zero tenv (Exp.root_of_lexp root) || is_deref_of_nullable then
    let deref_str =
      if is_deref_of_nullable then
        match nullable_var_opt with
        | Some str ->
            if is_weak_captured_var pdesc str then
              Localise.deref_str_weak_variable_in_block None str
            else Localise.deref_str_nullable None str
        | None ->
            Localise.deref_str_nullable None ""
      else Localise.deref_str_null None
    in
    let err_desc =
      Errdesc.explain_dereference pname tenv ~use_buckets:true ~is_nullable:is_deref_of_nullable
        deref_str prop loc
    in
    if Localise.is_parameter_not_null_checked_desc err_desc then
      raise (Exceptions.Parameter_not_null_checked (err_desc, __POS__))
    else if Localise.is_field_not_null_checked_desc err_desc then
      raise (Exceptions.Field_not_null_checked (err_desc, __POS__))
    else if Localise.is_empty_vector_access_desc err_desc then
      raise (Exceptions.Empty_vector_access (err_desc, __POS__))
    else raise (Exceptions.Null_dereference (err_desc, __POS__)) ) ;
  match attribute_opt with
  | Some (Apred (Adangling dk, _)) ->
      let deref_str = Localise.deref_str_dangling (Some dk) in
      let err_desc =
        Errdesc.explain_dereference pname tenv deref_str prop (State.get_loc_exn ())
      in
      raise (Exceptions.Dangling_pointer_dereference (Some dk, err_desc, __POS__))
  | Some (Apred (Aundef _, _)) ->
      ()
  | Some (Apred (Aresource ({ra_kind= Rrelease} as ra), _)) ->
      let deref_str = Localise.deref_str_freed ra in
      let err_desc = Errdesc.explain_dereference pname tenv ~use_buckets:true deref_str prop loc in
      raise (Exceptions.Use_after_free (err_desc, __POS__))
  | _ ->
      if Prover.check_equal tenv Prop.prop_emp (Exp.root_of_lexp root) Exp.minus_one then
        let deref_str = Localise.deref_str_dangling None in
        let err_desc = Errdesc.explain_dereference pname tenv deref_str prop loc in
        raise (Exceptions.Dangling_pointer_dereference (None, err_desc, __POS__))


(* Check that an expression representin an objc block can be null and raise a [B1] null exception.*)
(* It's used to check that we don't call possibly null blocks *)
let check_call_to_objc_block_error tenv pdesc prop fun_exp loc =
  let pname = Procdesc.get_proc_name pdesc in
  let is_this = function
    | Exp.Lvar pvar -> (
        let {ProcAttributes.clang_method_kind} = Procdesc.get_attributes pdesc in
        match clang_method_kind with
        | ClangMethodKind.OBJC_INSTANCE ->
            Pvar.is_self pvar
        | ClangMethodKind.CPP_INSTANCE ->
            Pvar.is_this pvar
        | _ ->
            false )
    | _ ->
        false
  in
  let fun_exp_may_be_null () =
    (* may be null if we don't know if it is definitely not null *)
    not (Prover.check_disequal tenv prop (Exp.root_of_lexp fun_exp) Exp.zero)
  in
  let try_explaining_exp e =
    (* when e is a temp var, try to find the pvar defining e*)
    match e with
    | Exp.Var id -> (
      match Errdesc.find_ident_assignment (State.get_node_exn ()) id with
      | Some (_, e') ->
          e'
      | None ->
          e )
    | _ ->
        e
  in
  let get_exp_called () =
    (* Exp called in the block's function call*)
    match State.get_instr () with
    | Some (Sil.Call (_, Exp.Var id, _, _, _)) ->
        Errdesc.find_ident_assignment (State.get_node_exn ()) id
    | _ ->
        None
  in
  let is_fun_exp_captured_var () =
    (* Called expression is a captured variable of the block *)
    match get_exp_called () with
    | Some (_, Exp.Lvar pvar) ->
        (* pvar is the block *)
        let name = Pvar.get_name pvar in
        List.exists ~f:(fun (cn, _) -> Mangled.equal name cn) (Procdesc.get_captured pdesc)
    | _ ->
        false
  in
  let is_field_deref () =
    (*Called expression is a field *)
    match get_exp_called () with
    | Some (_, Exp.Lfield (e', fn, t)) ->
        let e'' = try_explaining_exp e' in
        (Some (Exp.Lfield (e'', fn, t)), true)
        (* the block dereferences is a field of an object*)
    | Some (_, e) ->
        (Some e, false)
    | _ ->
        (None, false)
  in
  if Language.curr_language_is Clang && fun_exp_may_be_null () && not (is_fun_exp_captured_var ())
  then
    let deref_str = Localise.deref_str_null None in
    let err_desc_nobuckets =
      Errdesc.explain_dereference pname tenv ~is_nullable:true deref_str prop loc
    in
    match fun_exp with
    | Exp.Var id when Ident.is_footprint id -> (
        let e_opt, is_field_deref = is_field_deref () in
        let warn err_desc =
          let err_desc = Localise.error_desc_set_bucket err_desc Localise.BucketLevel.b1 in
          if is_field_deref then raise (Exceptions.Field_not_null_checked (err_desc, __POS__))
          else raise (Exceptions.Parameter_not_null_checked (err_desc, __POS__))
        in
        match e_opt with
        | Some e when is_this e ->
            (* don't warn that this/self can be null *)
            ()
        | Some e ->
            warn (Localise.parameter_field_not_null_checked_desc err_desc_nobuckets e)
        | _ ->
            warn err_desc_nobuckets )
    | _ ->
        (* HP: fun_exp is not a footprint therefore,
             either is a local or it's a modified param *)
        let err_desc = Localise.error_desc_set_bucket err_desc_nobuckets Localise.BucketLevel.b1 in
        raise (Exceptions.Null_dereference (err_desc, __POS__))


(** [rearrange lexp prop] rearranges [prop] into the form [prop' * lexp|->strexp:typ].
    It returns an iterator with [lexp |-> strexp: typ] as current predicate
    and the path (an [offsetlist]) which leads to [lexp] as the iterator state. *)
let rearrange ?(report_deref_errors = true) pdesc tenv lexp typ prop loc :
    Sil.offset list Prop.prop_iter list =
  let nlexp =
    match Prop.exp_normalize_prop tenv prop lexp with
    | Exp.BinOp (Binop.PlusPI, ep, e) ->
        (* array access with pointer arithmetic *)
        Exp.Lindex (ep, e)
    | e ->
        e
  in
  let ptr_tested_for_zero = Prover.check_disequal tenv prop (Exp.root_of_lexp nlexp) Exp.zero in
  let inst = Sil.inst_rearrange (not ptr_tested_for_zero) loc (State.get_path_pos ()) in
  L.d_strln ".... Rearrangement Start ...." ;
  L.d_str "Exp: " ;
  Sil.d_exp nlexp ;
  L.d_ln () ;
  L.d_strln "Prop:" ;
  Prop.d_prop prop ;
  L.d_ln () ;
  L.d_ln () ;
  if report_deref_errors then check_dereference_error tenv pdesc prop nlexp (State.get_loc_exn ()) ;
  let pname = Procdesc.get_proc_name pdesc in
  let prop' =
    match pname with
    | Typ.Procname.Java java_pname
      when Config.csl_analysis && !BiabductionConfig.footprint
           && not
                ( Typ.Procname.is_constructor pname
                || Typ.Procname.Java.is_class_initializer java_pname ) ->
        add_guarded_by_constraints tenv prop lexp pdesc
    | _ ->
        prop
  in
  match Prop.prop_iter_create prop' with
  | None ->
      if !BiabductionConfig.footprint then
        [prop_iter_add_hpred_footprint_to_prop pname tenv prop' (nlexp, typ) inst]
      else (
        pp_rearrangement_error "sigma is empty" prop nlexp ;
        raise (Exceptions.Symexec_memory_error __POS__) )
  | Some iter ->
      iter_rearrange pname tenv nlexp typ prop' iter inst
