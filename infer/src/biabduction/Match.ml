(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for "Smart" Pattern Matching *)

module L = Logging

let mem_idlist i l = List.exists ~f:(Ident.equal i) l

(** Type for a hpred pattern. flag=false means that the implication
    between hpreds is not considered, and flag = true means that it is
    considered during pattern matching *)
type hpred_pat = {hpred: Sil.hpred; flag: bool}

(** Checks [e1 = e2[sub ++ sub']] for some [sub'] with [dom(sub') subseteq vars].
    Returns [(sub ++ sub', vars - dom(sub'))]. *)
let rec exp_match e1 sub vars e2 : (Sil.subst * Ident.t list) option =
  let check_equal sub vars e1 e2 =
    let e2_inst = Sil.exp_sub sub e2 in
    if Exp.equal e1 e2_inst then Some (sub, vars) else None
  in
  match (e1, e2) with
  | _, Exp.Var id2 when Ident.is_primed id2 && mem_idlist id2 vars ->
      let vars_new = List.filter ~f:(fun id -> not (Ident.equal id id2)) vars in
      let sub_new =
        match Sil.extend_sub sub id2 e1 with
        | None ->
            assert false (* happens when vars contains the same variable twice. *)
        | Some sub_new ->
            sub_new
      in
      Some (sub_new, vars_new)
  | _, Exp.Var _ ->
      check_equal sub vars e1 e2
  | Exp.Var _, _ ->
      None
  | Exp.Const _, _ | _, Exp.Const _ ->
      check_equal sub vars e1 e2
  | Exp.Sizeof _, _ | _, Exp.Sizeof _ ->
      check_equal sub vars e1 e2
  | Exp.Cast (_, e1'), Exp.Cast (_, e2') ->
      (* we are currently ignoring cast *)
      exp_match e1' sub vars e2'
  | Exp.Cast _, _ | _, Exp.Cast _ ->
      None
  | Exp.UnOp (o1, e1', _), Exp.UnOp (o2, e2', _) when Unop.equal o1 o2 ->
      exp_match e1' sub vars e2'
  | Exp.UnOp _, _ | _, Exp.UnOp _ ->
      None (* Naive *)
  | Exp.BinOp (b1, e1', e1''), Exp.BinOp (b2, e2', e2'') when Binop.equal b1 b2 -> (
    match exp_match e1' sub vars e2' with
    | None ->
        None
    | Some (sub', vars') ->
        exp_match e1'' sub' vars' e2'' )
  | Exp.BinOp _, _ | _, Exp.BinOp _ ->
      None (* Naive *)
  | Exp.Exn _, _ | _, Exp.Exn _ ->
      check_equal sub vars e1 e2
  | Exp.Closure _, _ | _, Exp.Closure _ ->
      check_equal sub vars e1 e2
  | Exp.Lvar _, _ | _, Exp.Lvar _ ->
      check_equal sub vars e1 e2
  | Exp.Lfield (e1', fld1, _), Exp.Lfield (e2', fld2, _) when Typ.Fieldname.equal fld1 fld2 ->
      exp_match e1' sub vars e2'
  | Exp.Lfield _, _ | _, Exp.Lfield _ ->
      None
  | Exp.Lindex (base1, idx1), Exp.Lindex (base2, idx2) -> (
    match exp_match base1 sub vars base2 with
    | None ->
        None
    | Some (sub', vars') ->
        exp_match idx1 sub' vars' idx2 )


let exp_list_match es1 sub vars es2 =
  let f res_acc (e1, e2) =
    match res_acc with
    | None ->
        None
    | Some (sub_acc, vars_leftover) ->
        exp_match e1 sub_acc vars_leftover e2
  in
  Option.find_map
    ~f:(fun es_combined -> List.fold ~f ~init:(Some (sub, vars)) es_combined)
    (List.zip es1 es2)


(** Checks [sexp1 = sexp2[sub ++ sub']] for some [sub'] with
    [dom(sub') subseteq vars]. Returns [(sub ++ sub', vars - dom(sub'))].
    WARNING: This function does not consider the fact that the analyzer
    sometimes forgets fields of hpred. It can possibly cause a problem. *)
let rec strexp_match sexp1 sub vars sexp2 : (Sil.subst * Ident.t list) option =
  match (sexp1, sexp2) with
  | Sil.Eexp (exp1, _), Sil.Eexp (exp2, _) ->
      exp_match exp1 sub vars exp2
  | Sil.Eexp _, _ | _, Sil.Eexp _ ->
      None
  | Sil.Estruct (fsel1, _), Sil.Estruct (fsel2, _) ->
      fsel_match fsel1 sub vars fsel2
  | Sil.Estruct _, _ | _, Sil.Estruct _ ->
      None
  | Sil.Earray (len1, isel1, _), Sil.Earray (len2, isel2, _) -> (
    match exp_match len1 sub vars len2 with
    | Some (sub', vars') ->
        isel_match isel1 sub' vars' isel2
    | None ->
        None )


(** Checks [fsel1 = fsel2[sub ++ sub']] for some [sub'] with
    [dom(sub') subseteq vars]. Returns [(sub ++ sub', vars - dom(sub'))]. *)
and fsel_match fsel1 sub vars fsel2 =
  match (fsel1, fsel2) with
  | [], [] ->
      Some (sub, vars)
  | [], _ ->
      None
  | _, [] ->
      if Config.abs_struct <= 0 then None
      else Some (sub, vars) (* This can lead to great information loss *)
  | (fld1, se1') :: fsel1', (fld2, se2') :: fsel2' ->
      let n = Typ.Fieldname.compare fld1 fld2 in
      if Int.equal n 0 then
        match strexp_match se1' sub vars se2' with
        | None ->
            None
        | Some (sub', vars') ->
            fsel_match fsel1' sub' vars' fsel2'
      else if n < 0 && Config.abs_struct > 0 then fsel_match fsel1' sub vars fsel2
        (* This can lead to great information loss *)
      else None


(** Checks [isel1 = isel2[sub ++ sub']] for some [sub'] with
    [dom(sub') subseteq vars]. Returns [(sub ++ sub', vars - dom(sub'))]. *)
and isel_match isel1 sub vars isel2 =
  match (isel1, isel2) with
  | [], [] ->
      Some (sub, vars)
  | [], _ | _, [] ->
      None
  | (idx1, se1') :: isel1', (idx2, se2') :: isel2' ->
      let idx2 = Sil.exp_sub sub idx2 in
      let sanity_check = not (List.exists ~f:(fun id -> Exp.ident_mem idx2 id) vars) in
      if not sanity_check then (
        let pe = Pp.text in
        L.internal_error "@[.... Sanity Check Failure while Matching Index-Strexps ....@\n" ;
        L.internal_error "@[<4>    IDX1: %a, STREXP1: %a@\n" (Sil.pp_exp_printenv pe) idx1
          (Sil.pp_sexp pe) se1' ;
        L.internal_error "@[<4>    IDX2: %a, STREXP2: %a@\n@." (Sil.pp_exp_printenv pe) idx2
          (Sil.pp_sexp pe) se2' ;
        assert false )
      else if Exp.equal idx1 idx2 then
        match strexp_match se1' sub vars se2' with
        | None ->
            None
        | Some (sub', vars') ->
            isel_match isel1' sub' vars' isel2'
      else None


(* extends substitution sub by creating a new substitution for vars *)
let sub_extend_with_ren (sub : Sil.subst) vars =
  let f id = (id, Exp.Var (Ident.create_fresh Ident.kprimed)) in
  let renaming_for_vars = Sil.subst_of_list (List.map ~f vars) in
  Sil.sub_join sub renaming_for_vars


type sidecondition = Prop.normal Prop.t -> Sil.subst -> bool

let rec execute_with_backtracking = function
  | [] ->
      None
  | [f] ->
      f ()
  | f :: fs -> (
      let res_f = f () in
      match res_f with None -> execute_with_backtracking fs | Some _ -> res_f )


let rec instantiate_to_emp p condition (sub : Sil.subst) vars = function
  | [] ->
      if condition p sub then Some (sub, p) else None
  | hpat :: hpats -> (
      if not hpat.flag then None
      else
        match hpat.hpred with
        | Sil.Hpointsto _
        | Sil.Hlseg (Sil.Lseg_NE, _, _, _, _)
        | Sil.Hdllseg (Sil.Lseg_NE, _, _, _, _, _, _) ->
            None
        | Sil.Hlseg (_, _, e1, e2, _) -> (
            let fully_instantiated = not (List.exists ~f:(fun id -> Exp.ident_mem e1 id) vars) in
            if not fully_instantiated then None
            else
              let e1' = Sil.exp_sub sub e1 in
              match exp_match e1' sub vars e2 with
              | None ->
                  None
              | Some (sub_new, vars_leftover) ->
                  instantiate_to_emp p condition sub_new vars_leftover hpats )
        | Sil.Hdllseg (_, _, iF, oB, oF, iB, _) -> (
            let fully_instantiated =
              not (List.exists ~f:(fun id -> Exp.ident_mem iF id || Exp.ident_mem oB id) vars)
            in
            if not fully_instantiated then None
            else
              let iF' = Sil.exp_sub sub iF in
              let oB' = Sil.exp_sub sub oB in
              match exp_list_match [iF'; oB'] sub vars [oF; iB] with
              | None ->
                  None
              | Some (sub_new, vars_leftover) ->
                  instantiate_to_emp p condition sub_new vars_leftover hpats ) )


(* This function has to be changed in order to
 * implement the idea "All lsegs outside are NE, and all lsegs inside
 * are PE" *)
let rec iter_match_with_impl tenv iter condition sub vars hpat hpats =
  (*
  L.out "@[.... iter_match_with_impl ....@.";
  L.out "@[<4>  sub: %a@\n@." pp_sub sub;
  L.out "@[<4>  PROP: %a@\n@." pp_prop (Prop.prop_iter_to_prop iter);
  L.out "@[<4>  hpred: %a@\n@." pp_hpat hpat;
  L.out "@[<4>  hpred_rest: %a@\n@." pp_hpat_list hpats;
  *)
  let do_next iter_cur _ =
    match Prop.prop_iter_next iter_cur with
    | None ->
        None
    | Some iter_next ->
        iter_match_with_impl tenv iter_next condition sub vars hpat hpats
  in
  let do_empty_hpats iter_cur _ =
    let sub_new, vars_leftover =
      match Prop.prop_iter_current tenv iter_cur with _, (sub_new, vars_leftover) ->
        (sub_new, vars_leftover)
    in
    let sub_res = sub_extend_with_ren sub_new vars_leftover in
    let p_leftover = Prop.prop_iter_remove_curr_then_to_prop tenv iter_cur in
    (*
    L.out "@[.... iter_match_with_impl (final condtion check) ....@\n@.";
    L.out "@[<4>  sub_res : %a@\n@." pp_sub sub_res;
    L.out "@[<4>  p_leftover : %a@\n@." pp_prop p_leftover;
    *)
    if condition p_leftover sub_res then Some (sub_res, p_leftover) else None
  in
  let do_nonempty_hpats iter_cur _ =
    let sub_new, vars_leftover =
      match Prop.prop_iter_current tenv iter_cur with _, (sub_new, vars_leftover) ->
        (sub_new, vars_leftover)
    in
    let hpat_next, hpats_rest =
      match hpats with [] -> assert false | hpat_next :: hpats_rest -> (hpat_next, hpats_rest)
    in
    let p_rest = Prop.prop_iter_remove_curr_then_to_prop tenv iter_cur in
    prop_match_with_impl_sub tenv p_rest condition sub_new vars_leftover hpat_next hpats_rest
  in
  let gen_filter_pointsto lexp2 strexp2 te2 = function
    | Sil.Hpointsto (lexp1, strexp1, te1) when Exp.equal te1 te2 -> (
      match exp_match lexp1 sub vars lexp2 with
      | None ->
          None
      | Some (sub', vars_leftover) ->
          strexp_match strexp1 sub' vars_leftover strexp2 )
    | _ ->
        None
  in
  let gen_filter_lseg k2 para2 e_start2 e_end2 es_shared2 = function
    | Sil.Hpointsto _ ->
        None
    | Sil.Hlseg (k1, para1, e_start1, e_end1, es_shared1) ->
        let do_kinds_match =
          match (k1, k2) with
          | Sil.Lseg_NE, Sil.Lseg_NE | Sil.Lseg_NE, Sil.Lseg_PE | Sil.Lseg_PE, Sil.Lseg_PE ->
              true
          | Sil.Lseg_PE, Sil.Lseg_NE ->
              false
        in
        (* let do_paras_match = hpara_match_with_impl tenv hpat.flag para1 para2 *)
        let do_paras_match = hpara_match_with_impl tenv true para1 para2 in
        if not (do_kinds_match && do_paras_match) then None
        else
          let es1 = [e_start1; e_end1] @ es_shared1 in
          let es2 = [e_start2; e_end2] @ es_shared2 in
          exp_list_match es1 sub vars es2
    | Sil.Hdllseg _ ->
        None
  in
  let gen_filter_dllseg k2 para2 iF2 oB2 oF2 iB2 es_shared2 = function
    | Sil.Hpointsto _ | Sil.Hlseg _ ->
        None
    | Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, es_shared1) ->
        let do_kinds_match =
          match (k1, k2) with
          | Sil.Lseg_NE, Sil.Lseg_NE | Sil.Lseg_NE, Sil.Lseg_PE | Sil.Lseg_PE, Sil.Lseg_PE ->
              true
          | Sil.Lseg_PE, Sil.Lseg_NE ->
              false
        in
        (* let do_paras_match = hpara_dll_match_with_impl tenv hpat.flag para1 para2 *)
        let do_paras_match = hpara_dll_match_with_impl tenv true para1 para2 in
        if not (do_kinds_match && do_paras_match) then None
        else
          let es1 = [iF1; oB1; oF1; iB1] @ es_shared1 in
          let es2 = [iF2; oB2; oF2; iB2] @ es_shared2 in
          exp_list_match es1 sub vars es2
  in
  match hpat.hpred with
  | Sil.Hpointsto (lexp2, strexp2, te2) -> (
      let filter = gen_filter_pointsto lexp2 strexp2 te2 in
      match (Prop.prop_iter_find iter filter, hpats) with
      | None, _ ->
          None
      | Some iter_cur, [] ->
          do_empty_hpats iter_cur ()
      | Some iter_cur, _ ->
          execute_with_backtracking [do_nonempty_hpats iter_cur; do_next iter_cur] )
  | Sil.Hlseg (k2, para2, e_start2, e_end2, es_shared2) -> (
      let filter = gen_filter_lseg k2 para2 e_start2 e_end2 es_shared2 in
      let do_emp_lseg _ =
        let fully_instantiated_start2 =
          not (List.exists ~f:(fun id -> Exp.ident_mem e_start2 id) vars)
        in
        if not fully_instantiated_start2 then None
        else
          let e_start2' = Sil.exp_sub sub e_start2 in
          match (exp_match e_start2' sub vars e_end2, hpats) with
          | None, _ ->
              (*
          L.out "@.... iter_match_with_impl (empty_case, fail) ....@\n@.";
          L.out "@[<4>  sub: %a@\n@." pp_sub sub;
          L.out "@[<4>  e_start2': %a@\n@." pp_exp e_start2';
          L.out "@[<4>  e_end2: %a@\n@." pp_exp e_end2;
          *)
              None
          | Some (sub_new, vars_leftover), [] ->
              let sub_res = sub_extend_with_ren sub_new vars_leftover in
              let p_leftover = Prop.prop_iter_to_prop tenv iter in
              if condition p_leftover sub_res then Some (sub_res, p_leftover) else None
          | Some (sub_new, vars_leftover), hpat_next :: hpats_rest ->
              let p = Prop.prop_iter_to_prop tenv iter in
              prop_match_with_impl_sub tenv p condition sub_new vars_leftover hpat_next hpats_rest
      in
      let do_para_lseg _ =
        let para2_exist_vars, para2_inst =
          Sil.hpara_instantiate para2 e_start2 e_end2 es_shared2
        in
        (* let allow_impl hpred = {hpred=hpred; flag=hpat.flag} in *)
        let allow_impl hpred = {hpred; flag= true} in
        let para2_hpat, para2_hpats =
          match List.map ~f:allow_impl para2_inst with
          | [] ->
              assert false (* the body of a parameter should contain at least one * conjunct *)
          | para2_pat :: para2_pats ->
              (para2_pat, para2_pats)
        in
        let new_vars = para2_exist_vars @ vars in
        let new_hpats = para2_hpats @ hpats in
        match iter_match_with_impl tenv iter condition sub new_vars para2_hpat new_hpats with
        | None ->
            None
        | Some (sub_res, p_leftover) when condition p_leftover sub_res ->
            let not_in_para2_exist_vars id =
              not (List.exists ~f:(fun id' -> Ident.equal id id') para2_exist_vars)
            in
            let sub_res' = Sil.sub_filter not_in_para2_exist_vars sub_res in
            Some (sub_res', p_leftover)
        | Some _ ->
            None
      in
      match (Prop.prop_iter_find iter filter, hpats) with
      | None, _ when not hpat.flag ->
          (* L.out "@[.... iter_match_with_impl (lseg not-matched) ....@\n@."; *)
          None
      | None, _ when Sil.equal_lseg_kind k2 Sil.Lseg_NE ->
          (* L.out "@[.... iter_match_with_impl (lseg not-matched) ....@\n@."; *)
          do_para_lseg ()
      | None, _ ->
          (* L.out "@[.... iter_match_with_impl (lseg not-matched) ....@\n@."; *)
          execute_with_backtracking [do_emp_lseg; do_para_lseg]
      | Some iter_cur, [] ->
          (* L.out "@[.... iter_match_with_impl (lseg matched) ....@\n@."; *)
          do_empty_hpats iter_cur ()
      | Some iter_cur, _ ->
          (* L.out "@[.... iter_match_with_impl (lseg matched) ....@\n@."; *)
          execute_with_backtracking [do_nonempty_hpats iter_cur; do_next iter_cur] )
  | Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, es_shared2) -> (
      let filter = gen_filter_dllseg k2 para2 iF2 oB2 oF2 iB2 es_shared2 in
      let do_emp_dllseg _ =
        let fully_instantiated_iFoB2 =
          not (List.exists ~f:(fun id -> Exp.ident_mem iF2 id || Exp.ident_mem oB2 id) vars)
        in
        if not fully_instantiated_iFoB2 then None
        else
          let iF2' = Sil.exp_sub sub iF2 in
          let oB2' = Sil.exp_sub sub oB2 in
          match (exp_list_match [iF2'; oB2'] sub vars [oF2; iB2], hpats) with
          | None, _ ->
              None
          | Some (sub_new, vars_leftover), [] ->
              let sub_res = sub_extend_with_ren sub_new vars_leftover in
              let p_leftover = Prop.prop_iter_to_prop tenv iter in
              if condition p_leftover sub_res then Some (sub_res, p_leftover) else None
          | Some (sub_new, vars_leftover), hpat_next :: hpats_rest ->
              let p = Prop.prop_iter_to_prop tenv iter in
              prop_match_with_impl_sub tenv p condition sub_new vars_leftover hpat_next hpats_rest
      in
      let do_para_dllseg _ =
        let fully_instantiated_iF2 = not (List.exists ~f:(fun id -> Exp.ident_mem iF2 id) vars) in
        if not fully_instantiated_iF2 then None
        else
          let iF2' = Sil.exp_sub sub iF2 in
          match exp_match iF2' sub vars iB2 with
          | None ->
              None
          | Some (sub_new, vars_leftover) -> (
              let para2_exist_vars, para2_inst =
                Sil.hpara_dll_instantiate para2 iF2 oB2 oF2 es_shared2
              in
              (* let allow_impl hpred = {hpred=hpred; flag=hpat.flag} in *)
              let allow_impl hpred = {hpred; flag= true} in
              let para2_hpat, para2_hpats =
                match List.map ~f:allow_impl para2_inst with
                | [] ->
                    assert false
                    (* the body of a parameter should contain at least one * conjunct *)
                | para2_pat :: para2_pats ->
                    (para2_pat, para2_pats)
              in
              let new_vars = para2_exist_vars @ vars_leftover in
              let new_hpats = para2_hpats @ hpats in
              match
                iter_match_with_impl tenv iter condition sub_new new_vars para2_hpat new_hpats
              with
              | None ->
                  None
              | Some (sub_res, p_leftover) when condition p_leftover sub_res ->
                  let not_in_para2_exist_vars id =
                    not (List.exists ~f:(fun id' -> Ident.equal id id') para2_exist_vars)
                  in
                  let sub_res' = Sil.sub_filter not_in_para2_exist_vars sub_res in
                  Some (sub_res', p_leftover)
              | Some _ ->
                  None )
      in
      match (Prop.prop_iter_find iter filter, hpats) with
      | None, _ when not hpat.flag ->
          None
      | None, _ when Sil.equal_lseg_kind k2 Sil.Lseg_NE ->
          do_para_dllseg ()
      | None, _ ->
          execute_with_backtracking [do_emp_dllseg; do_para_dllseg]
      | Some iter_cur, [] ->
          do_empty_hpats iter_cur ()
      | Some iter_cur, _ ->
          execute_with_backtracking [do_nonempty_hpats iter_cur; do_next iter_cur] )


and prop_match_with_impl_sub tenv p condition sub vars hpat hpats =
  (*
  L.out "@[.... prop_match_with_impl_sub ....@.";
  L.out "@[<4>  sub: %a@\n@." pp_sub sub;
  L.out "@[<4>  PROP: %a@\n@." pp_prop p;
  L.out "@[<4>  hpat: %a@\n@." pp_hpat hpat;
  L.out "@[<4>  hpred_rest: %a@\n@." pp_hpat_list hpats;
  *)
  match Prop.prop_iter_create p with
  | None ->
      instantiate_to_emp p condition sub vars (hpat :: hpats)
  | Some iter ->
      iter_match_with_impl tenv iter condition sub vars hpat hpats


and hpara_common_match_with_impl tenv impl_ok ids1 sigma1 eids2 ids2 sigma2 =
  try
    let sub_ids =
      let ren_ids = List.zip_exn ids2 ids1 in
      let f (id2, id1) = (id2, Exp.Var id1) in
      List.map ~f ren_ids
    in
    let sub_eids, eids_fresh =
      let f id = (id, Ident.create_fresh Ident.kprimed) in
      let ren_eids = List.map ~f eids2 in
      let eids_fresh = List.map ~f:snd ren_eids in
      let sub_eids = List.map ~f:(fun (id2, id1) -> (id2, Exp.Var id1)) ren_eids in
      (sub_eids, eids_fresh)
    in
    let sub = Sil.subst_of_list (sub_ids @ sub_eids) in
    match sigma2 with
    | [] ->
        if List.is_empty sigma1 then true else false
    | hpred2 :: sigma2 -> (
        let hpat2, hpats2 =
          let hpred2_ren, sigma2_ren = (Sil.hpred_sub sub hpred2, Prop.sigma_sub sub sigma2) in
          let allow_impl hpred = {hpred; flag= impl_ok} in
          (allow_impl hpred2_ren, List.map ~f:allow_impl sigma2_ren)
        in
        let condition _ _ = true in
        let p1 = Prop.normalize tenv (Prop.from_sigma sigma1) in
        match prop_match_with_impl_sub tenv p1 condition Sil.sub_empty eids_fresh hpat2 hpats2 with
        | None ->
            false
        | Some (_, p1') when Prop.prop_is_emp p1' ->
            true
        | _ ->
            false )
  with Invalid_argument _ -> false


and hpara_match_with_impl tenv impl_ok para1 para2 : bool =
  (*
  L.out "@[.... hpara_match_with_impl_sub ....@.";
  L.out "@[<4>  HPARA1: %a@\n@." pp_hpara para1;
  L.out "@[<4>  HPARA2: %a@\n@." pp_hpara para2;
  *)
  let ids1 = para1.Sil.root :: para1.Sil.next :: para1.Sil.svars in
  let ids2 = para2.Sil.root :: para2.Sil.next :: para2.Sil.svars in
  let eids2 = para2.Sil.evars in
  hpara_common_match_with_impl tenv impl_ok ids1 para1.Sil.body eids2 ids2 para2.Sil.body


and hpara_dll_match_with_impl tenv impl_ok para1 para2 : bool =
  (*
  L.out "@[.... hpara_dll_match_with_impl_sub ....@.";
  L.out "@[<4>  HPARA1: %a@\n@." pp_hpara_dll para1;
  L.out "@[<4>  HPARA2: %a@\n@." pp_hpara_dll para2;
  *)
  let ids1 = para1.Sil.cell :: para1.Sil.blink :: para1.Sil.flink :: para1.Sil.svars_dll in
  let ids2 = para2.Sil.cell :: para2.Sil.blink :: para2.Sil.flink :: para2.Sil.svars_dll in
  let eids2 = para2.Sil.evars_dll in
  hpara_common_match_with_impl tenv impl_ok ids1 para1.Sil.body_dll eids2 ids2 para2.Sil.body_dll


(** [prop_match_with_impl p condition vars hpat hpats]
    returns [(subst, p_leftover)] such that
    + [dom(subst) = vars]
    + [p |- (hpat.hpred * hpats.hpred)[subst] * p_leftover].
    Using the flag [field], we can control the strength of |-. *)
let prop_match_with_impl tenv p condition vars hpat hpats =
  prop_match_with_impl_sub tenv p condition Sil.sub_empty vars hpat hpats


let sigma_remove_hpred eq sigma e =
  let filter = function
    | Sil.Hpointsto (root, _, _)
    | Sil.Hlseg (_, _, root, _, _)
    | Sil.Hdllseg (_, _, root, _, _, _, _) ->
        eq root e
  in
  let sigma_e, sigma_no_e = List.partition_tf ~f:filter sigma in
  match sigma_e with
  | [] ->
      (None, sigma)
  | [hpred_e] ->
      (Some hpred_e, sigma_no_e)
  | _ ->
      assert false


(** {2 Routines used when finding disjoint isomorphic sigmas from a single sigma} *)

type iso_mode = Exact | LFieldForget | RFieldForget [@@deriving compare]

let equal_iso_mode = [%compare.equal: iso_mode]

let rec generate_todos_from_strexp mode todos sexp1 sexp2 =
  match (sexp1, sexp2) with
  | Sil.Eexp (exp1, _), Sil.Eexp (exp2, _) ->
      let new_todos = (exp1, exp2) :: todos in
      Some new_todos
  | Sil.Eexp _, _ ->
      None
  | Sil.Estruct (fel1, _), Sil.Estruct (fel2, _) ->
      (* assume sorted w.r.t. fields *)
      if List.length fel1 <> List.length fel2 && equal_iso_mode mode Exact then None
      else generate_todos_from_fel mode todos fel1 fel2
  | Sil.Estruct _, _ ->
      None
  | Sil.Earray (len1, iel1, _), Sil.Earray (len2, iel2, _) ->
      if (not (Exp.equal len1 len2)) || List.length iel1 <> List.length iel2 then None
      else generate_todos_from_iel mode todos iel1 iel2
  | Sil.Earray _, _ ->
      None


and generate_todos_from_fel mode todos fel1 fel2 =
  match (fel1, fel2) with
  | [], [] ->
      Some todos
  | [], _ ->
      if equal_iso_mode mode RFieldForget then Some todos else None
  | _, [] ->
      if equal_iso_mode mode LFieldForget then Some todos else None
  | (fld1, strexp1) :: fel1', (fld2, strexp2) :: fel2' ->
      let n = Typ.Fieldname.compare fld1 fld2 in
      if Int.equal n 0 then
        match generate_todos_from_strexp mode todos strexp1 strexp2 with
        | None ->
            None
        | Some todos' ->
            generate_todos_from_fel mode todos' fel1' fel2'
      else if n < 0 && equal_iso_mode mode LFieldForget then
        generate_todos_from_fel mode todos fel1' fel2
      else if n > 0 && equal_iso_mode mode RFieldForget then
        generate_todos_from_fel mode todos fel1 fel2'
      else None


and generate_todos_from_iel mode todos iel1 iel2 =
  match (iel1, iel2) with
  | [], [] ->
      Some todos
  | (idx1, strexp1) :: iel1', (idx2, strexp2) :: iel2' -> (
    match generate_todos_from_strexp mode todos strexp1 strexp2 with
    | None ->
        None
    | Some todos' ->
        let new_todos = (idx1, idx2) :: todos' in
        generate_todos_from_iel mode new_todos iel1' iel2' )
  | _ ->
      None


(** add (e1,e2) at the front of corres, if necessary. *)
let corres_extend_front e1 e2 corres =
  let filter (e1', e2') = Exp.equal e1 e1' || Exp.equal e2 e2' in
  let checker e1' e2' = Exp.equal e1 e1' && Exp.equal e2 e2' in
  match List.filter ~f:filter corres with
  | [] ->
      Some ((e1, e2) :: corres)
  | [(e1', e2')] when checker e1' e2' ->
      Some corres
  | _ ->
      None


let corres_extensible corres e1 e2 =
  let predicate (e1', e2') = Exp.equal e1 e1' || Exp.equal e2 e2' in
  (not (List.exists ~f:predicate corres)) && not (Exp.equal e1 e2)


let corres_related corres e1 e2 =
  let filter (e1', e2') = Exp.equal e1 e1' || Exp.equal e2 e2' in
  let checker e1' e2' = Exp.equal e1 e1' && Exp.equal e2 e2' in
  match List.filter ~f:filter corres with
  | [] ->
      Exp.equal e1 e2
  | [(e1', e2')] when checker e1' e2' ->
      true
  | _ ->
      false


(* TO DO. Perhaps OK. Need to implemenet a better isomorphism check later.*)
let hpara_iso tenv para1 para2 =
  hpara_match_with_impl tenv false para1 para2 && hpara_match_with_impl tenv false para2 para1


let hpara_dll_iso tenv para1 para2 =
  hpara_dll_match_with_impl tenv false para1 para2
  && hpara_dll_match_with_impl tenv false para2 para1


(** [generic_find_partial_iso] finds isomorphic subsigmas of [sigma_todo].
    The function [update] is used to get rid of hpred pairs from [sigma_todo].
    [sigma_corres] records the isormophic copies discovered so far. The first
    parameter determines how much flexibility we will allow during this partial
    isomorphism finding. *)
let rec generic_find_partial_iso tenv mode update corres sigma_corres todos sigma_todo =
  match todos with
  | [] ->
      let sigma1, sigma2 = sigma_corres in
      Some (List.rev corres, List.rev sigma1, List.rev sigma2, sigma_todo)
  | (e1, e2) :: todos' when corres_related corres e1 e2 -> (
    match corres_extend_front e1 e2 corres with
    | None ->
        assert false
    | Some new_corres ->
        generic_find_partial_iso tenv mode update new_corres sigma_corres todos' sigma_todo )
  | (e1, e2) :: todos' when corres_extensible corres e1 e2 -> (
      let hpredo1, hpredo2, new_sigma_todo = update e1 e2 sigma_todo in
      match (hpredo1, hpredo2) with
      | None, None -> (
        match corres_extend_front e1 e2 corres with
        | None ->
            assert false
        | Some new_corres ->
            generic_find_partial_iso tenv mode update new_corres sigma_corres todos' sigma_todo )
      | None, _ | _, None ->
          None
      | Some (Sil.Hpointsto (_, _, te1)), Some (Sil.Hpointsto (_, _, te2))
        when not (Exp.equal te1 te2) ->
          None
      | Some (Sil.Hpointsto (_, se1, _) as hpred1), Some (Sil.Hpointsto (_, se2, _) as hpred2) -> (
        match generate_todos_from_strexp mode [] se1 se2 with
        | None ->
            None
        | Some todos'' ->
            let new_corres =
              match corres_extend_front e1 e2 corres with
              | None ->
                  assert false
              | Some new_corres ->
                  new_corres
            in
            let new_sigma_corres =
              let sigma1, sigma2 = sigma_corres in
              let new_sigma1 = hpred1 :: sigma1 in
              let new_sigma2 = hpred2 :: sigma2 in
              (new_sigma1, new_sigma2)
            in
            let new_todos = todos'' @ todos' in
            generic_find_partial_iso tenv mode update new_corres new_sigma_corres new_todos
              new_sigma_todo )
      | ( Some (Sil.Hlseg (k1, para1, root1, next1, shared1) as hpred1)
        , Some (Sil.Hlseg (k2, para2, root2, next2, shared2) as hpred2) ) -> (
          if k1 <> k2 || not (hpara_iso tenv para1 para2) then None
          else
            try
              let new_corres =
                match corres_extend_front e1 e2 corres with
                | None ->
                    assert false
                | Some new_corres ->
                    new_corres
              in
              let new_sigma_corres =
                let sigma1, sigma2 = sigma_corres in
                let new_sigma1 = hpred1 :: sigma1 in
                let new_sigma2 = hpred2 :: sigma2 in
                (new_sigma1, new_sigma2)
              in
              let new_todos =
                let shared12 = List.zip_exn shared1 shared2 in
                ((root1, root2) :: (next1, next2) :: shared12) @ todos'
              in
              generic_find_partial_iso tenv mode update new_corres new_sigma_corres new_todos
                new_sigma_todo
            with Invalid_argument _ -> None )
      | ( Some (Sil.Hdllseg (k1, para1, iF1, oB1, oF1, iB1, shared1) as hpred1)
        , Some (Sil.Hdllseg (k2, para2, iF2, oB2, oF2, iB2, shared2) as hpred2) ) -> (
          if k1 <> k2 || not (hpara_dll_iso tenv para1 para2) then None
          else
            try
              let new_corres =
                match corres_extend_front e1 e2 corres with
                | None ->
                    assert false
                | Some new_corres ->
                    new_corres
              in
              let new_sigma_corres =
                let sigma1, sigma2 = sigma_corres in
                let new_sigma1 = hpred1 :: sigma1 in
                let new_sigma2 = hpred2 :: sigma2 in
                (new_sigma1, new_sigma2)
              in
              let new_todos =
                let shared12 = List.zip_exn shared1 shared2 in
                ((iF1, iF2) :: (oB1, oB2) :: (oF1, oF2) :: (iB1, iB2) :: shared12) @ todos'
              in
              generic_find_partial_iso tenv mode update new_corres new_sigma_corres new_todos
                new_sigma_todo
            with Invalid_argument _ -> None )
      | _ ->
          None )
  | _ ->
      None


(** [find_partial_iso] finds disjoint isomorphic sub-sigmas inside a given sigma.
    The function returns a partial iso and three sigmas. The first sigma is the first
    copy of the two isomorphic sigmas, so it uses expressions in the domain of
    the returned isomorphism. The second is the second copy of the two isomorphic sigmas,
    and it uses expressions in the range of the isomorphism. The third is the unused
    part of the input sigma. *)
let find_partial_iso tenv eq corres todos sigma =
  let update e1 e2 sigma0 =
    let hpredo1, sigma0_no_e1 = sigma_remove_hpred eq sigma0 e1 in
    let hpredo2, sigma0_no_e12 = sigma_remove_hpred eq sigma0_no_e1 e2 in
    (hpredo1, hpredo2, sigma0_no_e12)
  in
  let init_sigma_corres = ([], []) in
  let init_sigma_todo = sigma in
  generic_find_partial_iso tenv Exact update corres init_sigma_corres todos init_sigma_todo


(** Lift the kind of list segment predicates to PE *)
let hpred_lift_to_pe hpred =
  match hpred with
  | Sil.Hpointsto _ ->
      hpred
  | Sil.Hlseg (_, para, root, next, shared) ->
      Sil.Hlseg (Sil.Lseg_PE, para, root, next, shared)
  | Sil.Hdllseg (_, para, iF, oB, oF, iB, shared) ->
      Sil.Hdllseg (Sil.Lseg_PE, para, iF, oB, oF, iB, shared)


(** Lift the kind of list segment predicates to PE in a given sigma *)
let sigma_lift_to_pe sigma = List.map ~f:hpred_lift_to_pe sigma

(** [generic_para_create] takes a correspondence, and a sigma
    and a list of expressions for the first part of this
    correspondence. Then, it creates a renaming of expressions
    in the domain of the given correspondence, and applies this
    renaming to the given sigma. The result is a tuple of the renaming,
    the renamed sigma, ids for existentially quantified expressions,
    ids for shared expressions, and shared expressions. *)
let generic_para_create tenv corres sigma1 elist1 =
  let corres_ids =
    let not_same_consts = function
      | Exp.Const c1, Exp.Const c2 ->
          not (Const.equal c1 c2)
      | _ ->
          true
    in
    let new_corres' = List.filter ~f:not_same_consts corres in
    let add_fresh_id pair = (pair, Ident.create_fresh Ident.kprimed) in
    List.map ~f:add_fresh_id new_corres'
  in
  let es_shared, ids_shared, ids_exists =
    let not_in_elist1 ((e1, _), _) = not (List.exists ~f:(Exp.equal e1) elist1) in
    let corres_ids_no_elist1 = List.filter ~f:not_in_elist1 corres_ids in
    let should_be_shared ((e1, e2), _) = Exp.equal e1 e2 in
    let shared, exists = List.partition_tf ~f:should_be_shared corres_ids_no_elist1 in
    let es_shared = List.map ~f:(fun ((e1, _), _) -> e1) shared in
    (es_shared, List.map ~f:snd shared, List.map ~f:snd exists)
  in
  let renaming = List.map ~f:(fun ((e1, _), id) -> (e1, id)) corres_ids in
  let body =
    let sigma1' = sigma_lift_to_pe sigma1 in
    let renaming_exp = List.map ~f:(fun (e1, id) -> (e1, Exp.Var id)) renaming in
    Prop.sigma_replace_exp tenv renaming_exp sigma1'
  in
  (renaming, body, ids_exists, ids_shared, es_shared)


(** [hpara_create] takes a correspondence, and a sigma, a root
    and a next for the first part of this correspondence. Then, it creates a
    hpara and discovers a list of shared expressions that are
    passed as arguments to hpara. Both of them are returned as a result. *)
let hpara_create tenv corres sigma1 root1 next1 =
  let renaming, body, ids_exists, ids_shared, es_shared =
    generic_para_create tenv corres sigma1 [root1; next1]
  in
  let get_id1 e1 =
    let is_equal_to_e1 (e1', _) = Exp.equal e1 e1' in
    match List.find ~f:is_equal_to_e1 renaming with Some (_, id) -> id | None -> assert false
  in
  let id_root = get_id1 root1 in
  let id_next = get_id1 next1 in
  let hpara =
    {Sil.root= id_root; Sil.next= id_next; Sil.svars= ids_shared; Sil.evars= ids_exists; Sil.body}
  in
  (hpara, es_shared)


(** [hpara_dll_create] takes a correspondence, and a sigma, a root,
    a blink and a flink for the first part of this correspondence. Then, it creates a
    hpara_dll and discovers a list of shared expressions that are
    passed as arguments to hpara. Both of them are returned as a result. *)
let hpara_dll_create tenv corres sigma1 root1 blink1 flink1 =
  let renaming, body, ids_exists, ids_shared, es_shared =
    generic_para_create tenv corres sigma1 [root1; blink1; flink1]
  in
  let get_id1 e1 =
    let is_equal_to_e1 (e1', _) = Exp.equal e1 e1' in
    match List.find ~f:is_equal_to_e1 renaming with Some (_, id) -> id | None -> assert false
  in
  let id_root = get_id1 root1 in
  let id_blink = get_id1 blink1 in
  let id_flink = get_id1 flink1 in
  let hpara_dll =
    { Sil.cell= id_root
    ; Sil.blink= id_blink
    ; Sil.flink= id_flink
    ; Sil.svars_dll= ids_shared
    ; Sil.evars_dll= ids_exists
    ; Sil.body_dll= body }
  in
  (hpara_dll, es_shared)
