(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  List.fold ~f:(fun acc x -> List.fold ~f:(fun acc' y -> (x, y) :: acc') ~init:acc l2') ~init:[] l1'


let rec list_rev_and_concat l1 l2 =
  match l1 with [] -> l2 | x1 :: l1' -> list_rev_and_concat l1' (x1 :: l2)


(** Check whether the index is out of bounds. If the length is - 1, no check is performed. If the
    index is provably out of bound, a bound error is given. If the length is a constant and the
    index is not provably in bound, a warning is given. *)
let check_bad_index {InterproceduralAnalysis.proc_desc; err_log; tenv} pname p len index loc =
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
      BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn
    else if len_is_constant then
      let deref_str = Localise.deref_str_array_bound len_const_opt index_const_opt in
      let desc = Errdesc.explain_array_access pname tenv deref_str p loc in
      let exn =
        if index_has_bounds () then Exceptions.Array_out_of_bounds_l2 (desc, __POS__)
        else Exceptions.Array_out_of_bounds_l3 (desc, __POS__)
      in
      BiabductionReporting.log_issue_deprecated_using_state proc_desc err_log exn


(** Perform bounds checking *)
let bounds_check analysis_data pname prop len e =
  if Config.biabduction_trace_rearrange then (
    L.d_str "Bounds check index:" ;
    Exp.d_exp e ;
    L.d_str " len: " ;
    Exp.d_exp len ;
    L.d_ln () ) ;
  check_bad_index analysis_data pname prop len e


let rec create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp
    (t : Typ.t) (off : Predicates.offset list) inst :
    Predicates.atom list * Predicates.strexp * Typ.t =
  if Config.biabduction_trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering create_struct_values" ;
    L.d_str "typ: " ;
    Typ.d_full t ;
    L.d_ln () ;
    L.d_str "off: " ;
    Predicates.d_offset_list off ;
    L.d_ln () ;
    L.d_ln () ) ;
  let new_id () =
    incr max_stamp ;
    Ident.create kind !max_stamp
  in
  let res =
    let fail t off pos =
      L.d_str "create_struct_values type:" ;
      Typ.d_full t ;
      L.d_str " off: " ;
      Predicates.d_offset_list off ;
      L.d_ln () ;
      raise (Exceptions.Bad_footprint pos)
    in
    match (t.desc, off) with
    | Tstruct _, [] ->
        ([], Predicates.Estruct ([], inst), t)
    | Tstruct name, Off_fld (f, _) :: off' -> (
      match Tenv.lookup tenv name with
      | Some ({fields; statics} as struct_typ) -> (
        match List.find ~f:(fun {Struct.name= f'} -> Fieldname.equal f f') (fields @ statics) with
        | Some {Struct.typ= t'} ->
            let atoms', se', res_t' =
              create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp
                t' off' inst
            in
            let se = Predicates.Estruct ([(f, se')], inst) in
            let replace_typ_of_f {Struct.name= f'; typ= t'; annot= a'} =
              if Fieldname.equal f f' then Struct.mk_field f res_t' ~annot:a'
              else Struct.mk_field f' t' ~annot:a'
            in
            let fields' =
              List.sort ~compare:Struct.compare_field (List.map ~f:replace_typ_of_f fields)
            in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            (atoms', se, t)
        | None ->
            fail t off __POS__ )
      | None ->
          fail t off __POS__ )
    | Tstruct _, Off_index e :: off' ->
        let atoms', se', res_t' =
          create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp t
            off' inst
        in
        let e' = Absarray.array_clean_new_index footprint_part e in
        let len = Exp.Var (new_id ()) in
        let se = Predicates.Earray (len, [(e', se')], inst) in
        let res_t = Typ.mk_array res_t' in
        (Predicates.Aeq (e, e') :: atoms', se, res_t)
    | Tarray {elt= t'; length; stride}, off -> (
        let len =
          match length with None -> Exp.Var (new_id ()) | Some len -> Exp.Const (Const.Cint len)
        in
        match off with
        | [] ->
            ([], Predicates.Earray (len, [], inst), t)
        | Predicates.Off_index e :: off' ->
            bounds_check analysis_data pname orig_prop len e (AnalysisState.get_loc_exn ()) ;
            let atoms', se', res_t' =
              create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp
                t' off' inst
            in
            let e' = Absarray.array_clean_new_index footprint_part e in
            let se = Predicates.Earray (len, [(e', se')], inst) in
            let res_t = Typ.mk_array ~default:t res_t' ?length ?stride in
            (Predicates.Aeq (e, e') :: atoms', se, res_t)
        | Predicates.Off_fld _ :: _ ->
            assert false )
    | Tint _, [] | Tfloat _, [] | Tvoid, [] | Tfun, [] | Tptr _, [] | TVar _, [] ->
        let id = new_id () in
        ([], Predicates.Eexp (Exp.Var id, inst), t)
    | (Tint _ | Tfloat _ | Tvoid | Tfun | Tptr _ | TVar _), Off_index e :: off' ->
        (* In this case, we lift t to the t array. *)
        let t', mk_typ_f =
          match t.Typ.desc with
          | Typ.Tptr (t', _) ->
              (t', function desc -> Typ.mk ~default:t desc)
          | _ ->
              (t, fun desc -> Typ.mk desc)
        in
        let len = Exp.Var (new_id ()) in
        let atoms', se', res_t' =
          create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp t'
            off' inst
        in
        let e' = Absarray.array_clean_new_index footprint_part e in
        let se = Predicates.Earray (len, [(e', se')], inst) in
        let res_t = mk_typ_f (Tarray {elt= res_t'; length= None; stride= None}) in
        (Predicates.Aeq (e, e') :: atoms', se, res_t)
    | Tint _, _ | Tfloat _, _ | Tvoid, _ | Tfun, _ | Tptr _, _ | TVar _, _ ->
        fail t off __POS__
  in
  if Config.biabduction_trace_rearrange then (
    let _, se, _ = res in
    L.d_strln "exiting create_struct_values, returning" ;
    Predicates.d_sexp se ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


(** Extend the strexp by populating the path indicated by [off]. This means that it will add missing
    flds and do the case - analysis for array accesses. This does not catch the array - bounds
    errors. If we want to implement the checks for array bounds errors, we need to change this
    function. *)
let rec strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp se
    (typ : Typ.t) (off : Predicates.offset list) inst =
  let new_id () =
    incr max_stamp ;
    Ident.create kind !max_stamp
  in
  match (off, se, typ.desc) with
  | [], Predicates.Eexp _, _ | [], Predicates.Estruct _, _ ->
      [([], se, typ)]
  | [], Predicates.Earray _, _ ->
      let off_new = Predicates.Off_index Exp.zero :: off in
      strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp se typ
        off_new inst
  | Off_fld _ :: _, Predicates.Earray _, Tarray _ ->
      let off_new = Predicates.Off_index Exp.zero :: off in
      strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp se typ
        off_new inst
  | Off_fld (f, _) :: off', Predicates.Estruct (fsel, inst'), Tstruct name -> (
    match Tenv.lookup tenv name with
    | Some ({fields; statics} as struct_typ) -> (
      match List.find ~f:(fun {Struct.name= f'} -> Fieldname.equal f f') (fields @ statics) with
      | Some {Struct.typ= typ'} -> (
        match List.find ~f:(fun (f', _) -> Fieldname.equal f f') fsel with
        | Some (_, se') ->
            let atoms_se_typ_list' =
              strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp
                se' typ' off' inst
            in
            let replace acc (res_atoms', res_se', res_typ') =
              let replace_fse ((f1, _) as ft1) =
                if Fieldname.equal f1 f then (f1, res_se') else ft1
              in
              let res_fsel' =
                List.sort ~compare:[%compare: Fieldname.t * Predicates.strexp]
                  (List.map ~f:replace_fse fsel)
              in
              let replace_fta ({Struct.name= f1; annot= a1} as fta1) =
                if Fieldname.equal f f1 then Struct.mk_field f1 res_typ' ~annot:a1 else fta1
              in
              let fields' =
                List.sort ~compare:Struct.compare_field (List.map ~f:replace_fta fields)
              in
              ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
              (res_atoms', Predicates.Estruct (res_fsel', inst'), typ) :: acc
            in
            List.fold ~f:replace ~init:[] atoms_se_typ_list'
        | None ->
            let atoms', se', res_typ' =
              create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp
                typ' off' inst
            in
            let res_fsel' =
              List.sort ~compare:[%compare: Fieldname.t * Predicates.strexp] ((f, se') :: fsel)
            in
            let replace_fta {Struct.name= f'; typ= t'; annot= a'} =
              if Fieldname.equal f' f then Struct.mk_field f res_typ' ~annot:a'
              else Struct.mk_field f' t' ~annot:a'
            in
            let fields' =
              List.sort ~compare:Struct.compare_field (List.map ~f:replace_fta fields)
            in
            ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:fields' name) ;
            [(atoms', Predicates.Estruct (res_fsel', inst'), typ)] )
      | None ->
          raise (Exceptions.Missing_fld (f, __POS__)) )
    | None ->
        raise (Exceptions.Missing_fld (f, __POS__)) )
  | Off_fld _ :: _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)
  | Off_index _ :: _, Predicates.Eexp _, (Tint _ | Tfloat _ | Tvoid | Tfun | Tptr _)
  | Off_index _ :: _, Predicates.Estruct _, Tstruct _ ->
      (* L.d_strln ~color:Orange "turn into an array"; *)
      let len =
        match se with
        | Predicates.Eexp (_, Predicates.Ialloc) ->
            Exp.one (* if allocated explicitly, we know len is 1 *)
        | _ ->
            if Config.biabduction_type_size then Exp.one (* Exp.Sizeof (typ, Subtype.exact) *)
            else Exp.Var (new_id ())
      in
      let se_new = Predicates.Earray (len, [(Exp.zero, se)], inst) in
      let typ_new = Typ.mk_array typ in
      strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp se_new
        typ_new off inst
  | ( Off_index e :: off'
    , Predicates.Earray (len, esel, inst_arr)
    , Tarray {elt= typ'; length= len_for_typ'; stride} ) -> (
      bounds_check analysis_data pname orig_prop len e (AnalysisState.get_loc_exn ()) ;
      match List.find ~f:(fun (e', _) -> Exp.equal e e') esel with
      | Some (_, se') ->
          let atoms_se_typ_list' =
            strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp
              se' typ' off' inst
          in
          let replace acc (res_atoms', res_se', res_typ') =
            let replace_ise ise = if Exp.equal e (fst ise) then (e, res_se') else ise in
            let res_esel' = List.map ~f:replace_ise esel in
            if Typ.equal res_typ' typ' || Int.equal (List.length res_esel') 1 then
              ( res_atoms'
              , Predicates.Earray (len, res_esel', inst_arr)
              , Typ.mk_array ~default:typ res_typ' ?length:len_for_typ' ?stride )
              :: acc
            else raise (Exceptions.Bad_footprint __POS__)
          in
          List.fold ~f:replace ~init:[] atoms_se_typ_list'
      | None ->
          array_case_analysis_index analysis_data pname tenv orig_prop footprint_part kind max_stamp
            len esel len_for_typ' typ' typ e off' inst_arr inst )
  | _, _, _ ->
      raise (Exceptions.Bad_footprint __POS__)


and array_case_analysis_index analysis_data pname tenv orig_prop footprint_part kind max_stamp
    array_len array_cont typ_array_len typ_cont typ_array index off inst_arr inst =
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
    let array_default = Predicates.Earray (array_len, array_cont, inst_arr) in
    let typ_default = Typ.mk_array ~default:typ_array typ_cont ?length:typ_array_len in
    [([], array_default, typ_default)]
  else if !BiabductionConfig.footprint then (
    let atoms, elem_se, elem_typ =
      create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp typ_cont
        off inst
    in
    check_sound elem_typ ;
    let cont_new =
      List.sort ~compare:[%compare: Exp.t * Predicates.strexp] ((index, elem_se) :: array_cont)
    in
    let array_new = Predicates.Earray (array_len, cont_new, inst_arr) in
    let typ_new = Typ.mk_array ~default:typ_array elem_typ ?length:typ_array_len in
    [(atoms, array_new, typ_new)] )
  else
    let res_new =
      if array_is_full then []
      else
        let atoms, elem_se, elem_typ =
          create_struct_values analysis_data pname tenv orig_prop footprint_part kind max_stamp
            typ_cont off inst
        in
        check_sound elem_typ ;
        let cont_new =
          List.sort ~compare:[%compare: Exp.t * Predicates.strexp] ((index, elem_se) :: array_cont)
        in
        let array_new = Predicates.Earray (array_len, cont_new, inst_arr) in
        let typ_new = Typ.mk_array ~default:typ_array elem_typ ?length:typ_array_len in
        [(atoms, array_new, typ_new)]
    in
    let rec handle_case acc isel_seen_rev = function
      | [] ->
          List.concat (List.rev (res_new :: acc))
      | ((i, se) as ise) :: isel_unseen ->
          let atoms_se_typ_list =
            strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp
              se typ_cont off inst
          in
          let atoms_se_typ_list' =
            List.fold
              ~f:(fun acc' (atoms', se', typ') ->
                check_sound typ' ;
                let atoms_new = Predicates.Aeq (index, i) :: atoms' in
                let isel_new = list_rev_and_concat isel_seen_rev ((i, se') :: isel_unseen) in
                let array_new = Predicates.Earray (array_len, isel_new, inst_arr) in
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
    | (Predicates.Off_fld _ as off) :: offs' ->
        let offs_seen' = off :: offs_seen in
        laundry offs_seen' eqs offs'
    | (Predicates.Off_index idx as off) :: offs' ->
        if exp_has_only_footprint_ids idx then
          let offs_seen' = off :: offs_seen in
          laundry offs_seen' eqs offs'
        else
          let () = incr max_stamp in
          let fid_new = Ident.create Ident.kfootprint !max_stamp in
          let exp_new = Exp.Var fid_new in
          let off_new = Predicates.Off_index exp_new in
          let offs_seen' = off_new :: offs_seen in
          let eqs' = (fid_new, idx) :: eqs in
          laundry offs_seen' eqs' offs'
  in
  laundry [] [] offs_in


let strexp_extend_values analysis_data pname tenv orig_prop footprint_part kind max_stamp se te
    (off : Predicates.offset list) inst =
  let typ = Exp.texp_to_typ None te in
  let off', laundry_atoms =
    let off', eqs = laundry_offset_for_footprint max_stamp off in
    (* do laundry_offset whether footprint_part is true or not, so max_stamp is modified anyway *)
    if footprint_part then (off', List.map ~f:(fun (id, e) -> Prop.mk_eq tenv (Exp.Var id) e) eqs)
    else (off, [])
  in
  if Config.biabduction_trace_rearrange then (
    L.d_str "entering strexp_extend_values se: " ;
    Predicates.d_sexp se ;
    L.d_str " typ: " ;
    Typ.d_full typ ;
    L.d_str " off': " ;
    Predicates.d_offset_list off' ;
    L.d_strln (if footprint_part then " FP" else " RE") ) ;
  let atoms_se_typ_list =
    strexp_extend_values_ analysis_data pname tenv orig_prop footprint_part kind max_stamp se typ
      off' inst
  in
  let atoms_se_typ_list_filtered =
    let check_neg_atom atom = Prover.check_atom tenv Prop.prop_emp (Prover.atom_negate tenv atom) in
    let check_not_inconsistent (atoms, _, _) = not (List.exists ~f:check_neg_atom atoms) in
    List.filter ~f:check_not_inconsistent atoms_se_typ_list
  in
  if Config.biabduction_trace_rearrange then L.d_strln "exiting strexp_extend_values" ;
  let sizeof_data =
    match te with
    | Exp.Sizeof sizeof_data ->
        sizeof_data
    | _ ->
        { Exp.typ= StdTyp.void
        ; nbytes= None
        ; dynamic_length= None
        ; subtype= Subtype.exact
        ; nullable= false }
  in
  List.map
    ~f:(fun (atoms', se', typ') ->
      (laundry_atoms @ atoms', se', Exp.Sizeof {sizeof_data with typ= typ'}) )
    atoms_se_typ_list_filtered


let collect_root_offset exp =
  let root = Exp.root_of_lexp exp in
  let offsets = Predicates.exp_get_offsets exp in
  (root, offsets)


(** Exp.Construct a points-to predicate for an expression, to add to a footprint. *)
let mk_ptsto_exp_footprint analysis_data pname tenv orig_prop (lexp, typ) max_stamp inst :
    Predicates.hpred * Predicates.hpred * Predicates.atom list =
  let root, off = collect_root_offset lexp in
  if not (exp_has_only_footprint_ids root) then
    if
      (* in angelic mode, purposely ignore dangling pointer warnings during the footprint phase -- we
         will fix them during the re-execution phase *)
      not !BiabductionConfig.footprint
    then (
      L.internal_error "!!!! Footprint Error, Bad Root : %a !!!! @\n" Exp.pp lexp ;
      let deref_str = Localise.deref_str_dangling None in
      let err_desc =
        Errdesc.explain_dereference pname tenv deref_str orig_prop (AnalysisState.get_loc_exn ())
      in
      raise (Exceptions.Dangling_pointer_dereference (false, err_desc, __POS__)) ) ;
  let off_foot, eqs = laundry_offset_for_footprint max_stamp off in
  let subtype =
    match !Language.curr_language with
    | Clang ->
        Subtype.exact
    | Java ->
        Subtype.subtypes
    | CIL ->
        Subtype.subtypes
    | Erlang ->
        L.die InternalError "Erlang not supported"
    | Hack ->
        L.die InternalError "Hack not supported"
    | Python ->
        L.die InternalError "Python not supported"
  in
  let create_ptsto footprint_part off0 =
    match (root, off0, typ.Typ.desc) with
    | Exp.Lvar pvar, [], Typ.Tfun ->
        let fun_name = Procname.from_string_c_fun (Mangled.to_string (Pvar.get_name pvar)) in
        let fun_exp = Exp.Const (Const.Cfun fun_name) in
        ( []
        , Prop.mk_ptsto tenv root
            (Predicates.Eexp (fun_exp, inst))
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype; nullable= false}) )
    | _, [], Typ.Tfun ->
        let atoms, se, typ =
          create_struct_values analysis_data pname tenv orig_prop footprint_part Ident.kfootprint
            max_stamp typ off0 inst
        in
        ( atoms
        , Prop.mk_ptsto tenv root se
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype; nullable= false}) )
    | _ ->
        let atoms, se, typ =
          create_struct_values analysis_data pname tenv orig_prop footprint_part Ident.kfootprint
            max_stamp typ off0 inst
        in
        ( atoms
        , Prop.mk_ptsto tenv root se
            (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype; nullable= false}) )
  in
  let atoms, ptsto_foot = create_ptsto true off_foot in
  let sub = Predicates.subst_of_list eqs in
  let ptsto = Predicates.hpred_sub sub ptsto_foot in
  let atoms' = List.map ~f:(fun (id, e) -> Prop.mk_eq tenv (Exp.Var id) e) eqs in
  (ptsto, ptsto_foot, atoms @ atoms')


(** Check if the path in exp exists already in the current ptsto predicate. If it exists, return
    None. Otherwise, return [Some fld] with [fld] the missing field. *)
let prop_iter_check_fields_ptsto_shallow tenv iter lexp =
  let offset = Predicates.exp_get_offsets lexp in
  let _, se, _ =
    match Prop.prop_iter_current tenv iter with
    | Predicates.Hpointsto (e, se, t), _ ->
        (e, se, t)
    | _ ->
        assert false
  in
  let rec check_offset se = function
    | [] ->
        None
    | Predicates.Off_fld (fld, _) :: off' -> (
      match se with
      | Predicates.Estruct (fsel, _) -> (
        match List.find ~f:(fun (fld', _) -> Fieldname.equal fld fld') fsel with
        | Some (_, se') ->
            check_offset se' off'
        | None ->
            Some fld )
      | _ ->
          Some fld )
    | Predicates.Off_index _ :: _ ->
        None
  in
  check_offset se offset


(** [prop_iter_extend_ptsto iter lexp] extends the current psto predicate in [iter] with enough
    fields to follow the path in [lexp] -- field splitting model. It also materializes all indices
    accessed in lexp. *)
let prop_iter_extend_ptsto analysis_data pname tenv orig_prop iter lexp inst =
  if Config.biabduction_trace_rearrange then (
    L.d_str "entering prop_iter_extend_ptsto lexp: " ;
    Exp.d_exp lexp ;
    L.d_ln () ) ;
  let offset = Predicates.exp_get_offsets lexp in
  let max_stamp = Prop.prop_iter_max_stamp iter in
  let extend_footprint_pred = function
    | Predicates.Hpointsto (e, se, te) ->
        let atoms_se_te_list =
          strexp_extend_values analysis_data pname tenv orig_prop true Ident.kfootprint
            (ref max_stamp) se te offset inst
        in
        List.map
          ~f:(fun (atoms', se', te') -> (atoms', Predicates.Hpointsto (e, se', te')))
          atoms_se_te_list
    | Predicates.Hlseg (k, hpara, e1, e2, el) -> (
      match hpara.Predicates.body with
      | Predicates.Hpointsto (e', se', te') :: body_rest ->
          let atoms_se_te_list =
            strexp_extend_values analysis_data pname tenv orig_prop true Ident.kfootprint
              (ref max_stamp) se' te' offset inst
          in
          let atoms_body_list =
            List.map
              ~f:(fun (atoms0, se0, te0) ->
                (atoms0, Predicates.Hpointsto (e', se0, te0) :: body_rest) )
              atoms_se_te_list
          in
          let atoms_hpara_list =
            List.map
              ~f:(fun (atoms, body') -> (atoms, {hpara with Predicates.body= body'}))
              atoms_body_list
          in
          List.map
            ~f:(fun (atoms, hpara') -> (atoms, Predicates.Hlseg (k, hpara', e1, e2, el)))
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
    Prop.prop_iter_update_current iter' (Predicates.Hpointsto (e, se, te))
  in
  let do_extend e se te =
    if Config.biabduction_trace_rearrange then (
      L.d_strln "entering do_extend" ;
      L.d_str "e: " ;
      Exp.d_exp e ;
      L.d_str " se : " ;
      Predicates.d_sexp se ;
      L.d_str " te: " ;
      Exp.d_texp_full te ;
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
        strexp_extend_values analysis_data pname tenv orig_prop false extend_kind (ref max_stamp) se
          te offset inst
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
                | Predicates.Hpointsto (e', _, _) ->
                    Exp.equal e e'
                | Predicates.Hlseg (_, _, e1, _, _) ->
                    Exp.equal e e1
                | Predicates.Hdllseg (_, _, e_iF, _, _, e_iB, _) ->
                    Exp.equal e e_iF || Exp.equal e e_iB )
              footprint_sigma
          in
          let atoms_sigma_list =
            match sigma_pto with
            | [hpred] ->
                let atoms_hpred_list = extend_footprint_pred hpred in
                List.map ~f:(fun (atoms, hpred') -> (atoms, hpred' :: sigma_rest)) atoms_hpred_list
            | _ ->
                L.d_warning "Cannot extend " ;
                Exp.d_exp lexp ;
                L.d_strln " in" ;
                Prop.d_prop (Prop.prop_iter_to_prop tenv iter) ;
                L.d_ln () ;
                [([], footprint_sigma)]
          in
          List.map
            ~f:(fun (atoms, sigma') ->
              (atoms, List.stable_sort ~compare:Predicates.compare_hpred sigma') )
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
    Exp.d_exp lexp ;
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
  | Predicates.Hpointsto (e, se, te), _ ->
      do_extend e se te
  | _ ->
      assert false


(** Add a pointsto for [root(lexp): typ] to the sigma and footprint of a prop, if it's compatible
    with the allowed footprint variables. Then, change it into a iterator. This function ensures
    that [root(lexp): typ] is the current hpred of the iterator. typ is the type of the root of
    lexp. *)
let prop_iter_add_hpred_footprint_to_prop analysis_data pname tenv prop (lexp, typ) inst =
  let max_stamp = Prop.max_stamp ~f:Ident.is_footprint prop in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint analysis_data pname tenv prop (lexp, typ) (ref max_stamp) inst
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
    List.fold ~f:(Prop.prop_atom_and tenv ~footprint:!BiabductionConfig.footprint) ~init:prop' atoms
  in
  let iter =
    match Prop.prop_iter_create prop_new with
    | None -> (
        let prop_new' = Prop.normalize tenv (Prop.prop_hpred_star prop_new ptsto) in
        match Prop.prop_iter_create prop_new' with None -> assert false | Some iter -> iter )
    | Some iter ->
        Prop.prop_iter_prev_then_insert iter ptsto
  in
  let offsets_default = Predicates.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter offsets_default


(** Add a pointsto for [root(lexp): typ] to the iterator and to the footprint, if it's compatible
    with the allowed footprint variables. This function ensures that [root(lexp): typ] is the
    current hpred of the iterator. typ is the type of the root of lexp. *)
let prop_iter_add_hpred_footprint analysis_data pname tenv orig_prop iter (lexp, typ) inst =
  if Config.biabduction_trace_rearrange then (
    L.d_strln "entering prop_iter_add_hpred_footprint" ;
    L.d_str "lexp: " ;
    Exp.d_exp lexp ;
    L.d_ln () ;
    L.d_str "typ:" ;
    Typ.d_full typ ;
    L.d_ln () ) ;
  let max_stamp = Prop.prop_iter_max_stamp ~f:Ident.is_footprint iter in
  let ptsto, ptsto_foot, atoms =
    mk_ptsto_exp_footprint analysis_data pname tenv orig_prop (lexp, typ) (ref max_stamp) inst
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
  let offsets_default = Predicates.exp_get_offsets lexp in
  Prop.prop_iter_set_state iter' offsets_default


exception ARRAY_ACCESS

let rearrange_arith tenv lexp prop =
  if Config.biabduction_trace_rearrange then (
    L.d_strln "entering rearrange_arith" ;
    L.d_str "lexp: " ;
    Exp.d_exp lexp ;
    L.d_ln () ;
    L.d_str "prop: " ;
    L.d_ln () ;
    Prop.d_prop prop ;
    L.d_ln () ;
    L.d_ln () ) ;
  if Config.biabduction_array_level >= 2 then raise ARRAY_ACCESS
  else
    let root = Exp.root_of_lexp lexp in
    if Prover.check_allocatedness tenv prop root then raise ARRAY_ACCESS
    else raise (Exceptions.Symexec_memory_error __POS__)


let pp_rearrangement_error message prop lexp =
  L.d_printfln ".... Rearrangement Error .... %s" message ;
  L.d_str "Exp:" ;
  Exp.d_exp lexp ;
  L.d_ln () ;
  L.d_str "Prop:" ;
  L.d_ln () ;
  Prop.d_prop prop ;
  L.d_ln () ;
  L.d_ln ()


(** do re-arrangement for an iter whose current element is a pointsto *)
let iter_rearrange_ptsto analysis_data pname tenv orig_prop iter lexp inst =
  if Config.biabduction_trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering iter_rearrange_ptsto" ;
    L.d_str "lexp: " ;
    Exp.d_exp lexp ;
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
    if !BiabductionConfig.footprint then
      prop_iter_extend_ptsto analysis_data pname tenv orig_prop iter lexp inst
    else (
      check_field_splitting () ;
      match Prop.prop_iter_current tenv iter with
      | Predicates.Hpointsto (e, se, te), offset ->
          let max_stamp = Prop.prop_iter_max_stamp iter in
          let atoms_se_te_list =
            strexp_extend_values analysis_data pname tenv orig_prop false Ident.kprimed
              (ref max_stamp) se te offset inst
          in
          let handle_case (atoms', se', te') =
            let iter' =
              List.fold ~f:(Prop.prop_iter_add_atom !BiabductionConfig.footprint) ~init:iter atoms'
            in
            Prop.prop_iter_update_current iter' (Predicates.Hpointsto (e, se', te'))
          in
          let filter it =
            let p = Prop.prop_iter_to_prop tenv it in
            not (Prover.check_inconsistency tenv p)
          in
          List.filter ~f:filter (List.map ~f:handle_case atoms_se_te_list)
      | _ ->
          [iter] )
  in
  if Config.biabduction_trace_rearrange then (
    L.d_strln "exiting iter_rearrange_ptsto, returning results" ;
    Prop.d_proplist_with_typ (List.map ~f:(Prop.prop_iter_to_prop tenv) res) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


(** do re-arrangment for an iter whose current element is a nonempty listseg *)
let iter_rearrange_ne_lseg tenv recurse_on_iters iter para e1 e2 elist =
  if Config.biabduction_nelseg then
    let iter_inductive_case =
      let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let _, para_inst1 = Predicates.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Lseg_NE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1
    in
    let iter_base_case =
      let _, para_inst = Predicates.hpara_instantiate para e1 e2 elist in
      Prop.prop_iter_update_current_by_list iter para_inst
    in
    recurse_on_iters [iter_inductive_case; iter_base_case]
  else
    let iter_inductive_case =
      let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let _, para_inst1 = Predicates.hpara_instantiate para e1 n' elist in
      let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Lseg_PE para n' e2 elist] in
      Prop.prop_iter_update_current_by_list iter hpred_list1
    in
    recurse_on_iters [iter_inductive_case]


(** do re-arrangment for an iter whose current element is a nonempty dllseg to be unrolled from lhs *)
let iter_rearrange_ne_dllseg_first tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Predicates.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 = para_dll_inst1 @ [Prop.mk_dllseg tenv Lseg_NE para_dll n' e1 e3 e4 elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_base_case =
    let _, para_dll_inst = Predicates.hpara_dll_instantiate para_dll e1 e2 e3 elist in
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
    let _, para_dll_inst1 = Predicates.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 = para_dll_inst1 @ [Prop.mk_dllseg tenv Lseg_NE para_dll e1 e2 e4 n' elist] in
    Prop.prop_iter_update_current_by_list iter hpred_list1
  in
  let iter_base_case =
    let _, para_dll_inst = Predicates.hpara_dll_instantiate para_dll e4 e2 e3 elist in
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
    let _, para_inst1 = Predicates.hpara_instantiate para e1 n' elist in
    let hpred_list1 = para_inst1 @ [Prop.mk_lseg tenv Lseg_PE para n' e2 elist] in
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


(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled
    from lhs *)
let iter_rearrange_pe_dllseg_first tenv recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4
    elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Predicates.hpara_dll_instantiate para_dll e1 e2 n' elist in
    let hpred_list1 = para_dll_inst1 @ [Prop.mk_dllseg tenv Lseg_PE para_dll n' e1 e3 e4 elist] in
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


(** do re-arrangment for an iter whose current element is a possibly empty dllseg to be unrolled
    from rhs *)
let iter_rearrange_pe_dllseg_last tenv recurse_on_iters default_case_iter iter para_dll e1 e2 e3 e4
    elist =
  let iter_inductive_case =
    let n' = Exp.Var (Ident.create_fresh Ident.kprimed) in
    let _, para_dll_inst1 = Predicates.hpara_dll_instantiate para_dll e4 n' e3 elist in
    let hpred_list1 = para_dll_inst1 @ [Prop.mk_dllseg tenv Lseg_PE para_dll e1 e2 e4 n' elist] in
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


(** Exposes lexp |->- from iter. In case that it is not possible to * expose lexp |->-, this
    function prints an error message and * faults. There are four things to note. First, typ is the
    type of the * root of lexp. Second, prop should mean the same as iter. Third, the * result []
    means that the given input iter is inconsistent. This * happens when the theorem prover can
    prove the inconsistency of prop, * only after unrolling some predicates in prop. This function
    ensures * that the theorem prover cannot prove the inconsistency of any of the * new iters in
    the result. *)
let rec iter_rearrange analysis_data pname tenv lexp typ_from_instr prop iter inst :
    Predicates.offset list Prop.prop_iter list =
  let rec root_typ_of_offsets = function
    | Predicates.Off_fld (f, fld_typ) :: _ -> (
      match fld_typ.desc with
      | Tstruct _ ->
          (* access through field: get the struct type from the field *)
          if Config.biabduction_trace_rearrange then (
            L.d_increase_indent () ;
            L.d_printfln "iter_rearrange: root of lexp accesses field %a" Fieldname.pp f ;
            L.d_str "  struct type from field: " ;
            Typ.d_full fld_typ ;
            L.d_ln () ;
            L.d_decrease_indent () ;
            L.d_ln () ) ;
          fld_typ
      | _ ->
          typ_from_instr )
    | Predicates.Off_index _ :: off ->
        Typ.mk_array (root_typ_of_offsets off)
    | _ ->
        typ_from_instr
  in
  let typ = root_typ_of_offsets (Predicates.exp_get_offsets lexp) in
  if Config.biabduction_trace_rearrange then (
    L.d_increase_indent () ;
    L.d_strln "entering iter_rearrange" ;
    L.d_str "lexp: " ;
    Exp.d_exp lexp ;
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
    if Config.biabduction_trace_rearrange then L.d_strln "entering default_case_iter" ;
    if !BiabductionConfig.footprint then
      prop_iter_add_hpred_footprint analysis_data pname tenv prop iter' (lexp, typ) inst
    else if
      Config.biabduction_array_level >= 1
      && (not !BiabductionConfig.footprint)
      && Exp.pointer_arith lexp
    then rearrange_arith tenv lexp prop
    else (
      pp_rearrangement_error "cannot find predicate with root" prop lexp ;
      raise (Exceptions.Symexec_memory_error __POS__) )
  in
  let recurse_on_iters iters =
    let f_one_iter iter' =
      let prop' = Prop.prop_iter_to_prop tenv iter' in
      if Prover.check_inconsistency tenv prop' then []
      else
        iter_rearrange analysis_data pname tenv
          (Prop.lexp_normalize_prop tenv prop' lexp)
          typ prop' iter' inst
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
    | Predicates.Hpointsto (base, _, _) | Predicates.Hlseg (_, _, base, _, _) ->
        Prover.is_root tenv prop base lexp
    | Predicates.Hdllseg (_, _, first, _, _, last, _) -> (
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
      | Predicates.Hpointsto (_, _, _), _ ->
          iter_rearrange_ptsto analysis_data pname tenv prop iter lexp inst
      | Predicates.Hlseg (Lseg_NE, para, e1, e2, elist), _ ->
          iter_rearrange_ne_lseg tenv recurse_on_iters iter para e1 e2 elist
      | Predicates.Hlseg (Lseg_PE, para, e1, e2, elist), _ ->
          iter_rearrange_pe_lseg tenv recurse_on_iters default_case_iter iter para e1 e2 elist
      | Predicates.Hdllseg (Lseg_NE, para_dll, e1, e2, e3, e4, elist), _ -> (
        match (Prover.is_root tenv prop e1 lexp, Prover.is_root tenv prop e4 lexp) with
        | None, None ->
            assert false
        | Some _, _ ->
            iter_rearrange_ne_dllseg_first tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist
        | _, Some _ ->
            iter_rearrange_ne_dllseg_last tenv recurse_on_iters iter para_dll e1 e2 e3 e4 elist )
      | Predicates.Hdllseg (Lseg_PE, para_dll, e1, e2, e3, e4, elist), _ -> (
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
  if Config.biabduction_trace_rearrange then (
    L.d_strln "exiting iter_rearrange, returning results" ;
    Prop.d_proplist_with_typ (List.map ~f:(Prop.prop_iter_to_prop tenv) res) ;
    L.d_decrease_indent () ;
    L.d_ln () ;
    L.d_ln () ) ;
  res


let param_has_annot predicate pvar params_with_annotations =
  List.exists
    ~f:(fun (mangled, _, param_annotation_deprecated) ->
      Mangled.equal mangled (Pvar.get_name pvar) && predicate param_annotation_deprecated )
    params_with_annotations


let var_has_annotation pdesc is_annotation pvar =
  Procdesc.get_formals pdesc |> param_has_annot is_annotation pvar


let attr_has_annot is_annotation tenv prop exp =
  let attr_has_annot = function
    | Predicates.Apred ((Aretval (pname, ret_attr) | Aundef (pname, ret_attr, _, _)), _)
      when is_annotation ret_attr ->
        Some (Procname.to_string pname)
    | _ ->
        None
  in
  try List.find_map ~f:attr_has_annot (Attribute.get_for_exp tenv prop exp)
  with Not_found_s _ | Caml.Not_found -> None


let is_strexp_pt_fld_with_annot tenv obj_str is_annotation typ deref_exp (fld, strexp) =
  let lookup = Tenv.lookup tenv in
  let fld_has_annot fld =
    match Struct.get_field_type_and_annotation ~lookup fld typ with
    | Some (_, annot) ->
        is_annotation annot
    | _ ->
        false
  in
  match strexp with
  | Predicates.Eexp ((Exp.Var _ as exp), _) when Exp.equal exp deref_exp ->
      let has_annot = fld_has_annot fld in
      if has_annot then obj_str := Some (Fieldname.to_simplified_string fld) ;
      has_annot
  | _ ->
      true


(* This returns true if the exp is pointed to only by fields or parameters with a given
   annotation. In that case it also returns a string representation of the annotation
   recipient. *)
let is_only_pt_by_fld_or_param_with_annot pdesc tenv prop deref_exp is_annotation =
  let obj_str = ref None in
  let is_pt_by_fld_or_param_with_annot hpred =
    match hpred with
    | Predicates.Hpointsto (Exp.Lvar pvar, Eexp ((Exp.Var _ as exp), _), _)
      when Exp.equal exp deref_exp ->
        let var_has_annotation = Pvar.is_seed pvar && var_has_annotation pdesc is_annotation pvar in
        if var_has_annotation then obj_str := Some (Pvar.to_string pvar) ;
        let procname_str_opt = attr_has_annot is_annotation tenv prop exp in
        if Option.is_some procname_str_opt then obj_str := procname_str_opt ;
        (* it's ok for a local with no annotation to point to deref_exp *)
        var_has_annotation || Option.is_some procname_str_opt || Pvar.is_local pvar
    | Predicates.Hpointsto (_, Estruct (flds, _), Exp.Sizeof {typ}) ->
        List.for_all ~f:(is_strexp_pt_fld_with_annot tenv obj_str is_annotation typ deref_exp) flds
    | _ ->
        true
  in
  if List.for_all ~f:is_pt_by_fld_or_param_with_annot prop.Prop.sigma then !obj_str else None


let is_only_pt_by_fld_or_param_nullable pdesc tenv prop deref_exp =
  is_only_pt_by_fld_or_param_with_annot pdesc tenv prop deref_exp Annotations.ia_is_nullable


let is_only_pt_by_fld_or_param_nonnull pdesc tenv prop deref_exp =
  Option.is_some
    (is_only_pt_by_fld_or_param_with_annot pdesc tenv prop deref_exp Annotations.ia_is_nonnull)


(** Check for dereference errors: dereferencing 0, a freed value, or an undefined value *)
let check_dereference_error tenv pdesc (prop : Prop.normal Prop.t) lexp loc =
  let pname = Procdesc.get_proc_name pdesc in
  let root = Exp.root_of_lexp lexp in
  let nullable_var_opt =
    let is_definitely_non_null exp prop = Prover.check_disequal tenv prop exp Exp.zero in
    if Config.report_nullable_inconsistency then
      match is_only_pt_by_fld_or_param_nullable pdesc tenv prop root with
      | Some _ as nullable_var when not (is_definitely_non_null root prop) ->
          nullable_var
      | _ ->
          None
    else None
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
  ( if Prover.check_zero tenv (Exp.root_of_lexp root) || Option.is_some nullable_var_opt then
      let deref_str =
        match nullable_var_opt with
        | Some str ->
            Localise.deref_str_nullable None str
        | None ->
            Localise.deref_str_null None
      in
      let err_desc =
        Errdesc.explain_dereference pname tenv ~use_buckets:true
          ~is_nullable:(Option.is_some nullable_var_opt) deref_str prop loc
      in
      if Localise.is_empty_vector_access_desc err_desc then
        raise (Exceptions.Empty_vector_access (err_desc, __POS__))
      else raise (Exceptions.Null_dereference (err_desc, __POS__)) ) ;
  match attribute_opt with
  | Some (Apred (Adangling dk, _)) ->
      let deref_str = Localise.deref_str_dangling (Some dk) in
      let err_desc =
        Errdesc.explain_dereference pname tenv deref_str prop (AnalysisState.get_loc_exn ())
      in
      raise (Exceptions.Dangling_pointer_dereference (true, err_desc, __POS__))
  | Some (Apred (Aundef _, _)) ->
      ()
  | _ ->
      if Prover.check_equal tenv Prop.prop_emp (Exp.root_of_lexp root) Exp.minus_one then
        let deref_str = Localise.deref_str_dangling None in
        let err_desc = Errdesc.explain_dereference pname tenv deref_str prop loc in
        raise (Exceptions.Dangling_pointer_dereference (false, err_desc, __POS__))


(** [rearrange lexp prop] rearranges [prop] into the form [prop' * lexp|->strexp:typ]. It returns an
    iterator with [lexp |-> strexp: typ] as current predicate and the path (an [offsetlist]) which
    leads to [lexp] as the iterator state. *)
let rearrange ?(report_deref_errors = true)
    ({InterproceduralAnalysis.proc_desc= pdesc; tenv} as analysis_data) lexp typ prop loc :
    Predicates.offset list Prop.prop_iter list =
  let nlexp =
    match Prop.exp_normalize_prop tenv prop lexp with
    | Exp.BinOp (Binop.PlusPI, ep, e) ->
        (* array access with pointer arithmetic *)
        Exp.Lindex (ep, e)
    | e ->
        e
  in
  let ptr_tested_for_zero = Prover.check_disequal tenv prop (Exp.root_of_lexp nlexp) Exp.zero in
  let inst = Predicates.inst_rearrange (not ptr_tested_for_zero) loc (State.get_path_pos ()) in
  L.d_strln ".... Rearrangement Start ...." ;
  L.d_str "Exp: " ;
  Exp.d_exp nlexp ;
  L.d_ln () ;
  L.d_strln "Prop:" ;
  Prop.d_prop prop ;
  L.d_ln () ;
  L.d_ln () ;
  if report_deref_errors then
    check_dereference_error tenv pdesc prop nlexp (AnalysisState.get_loc_exn ()) ;
  let pname = Procdesc.get_proc_name pdesc in
  match Prop.prop_iter_create prop with
  | None ->
      if !BiabductionConfig.footprint then
        [prop_iter_add_hpred_footprint_to_prop analysis_data pname tenv prop (nlexp, typ) inst]
      else (
        pp_rearrangement_error "sigma is empty" prop nlexp ;
        raise (Exceptions.Symexec_memory_error __POS__) )
  | Some iter ->
      iter_rearrange analysis_data pname tenv nlexp typ prop iter inst
