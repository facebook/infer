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

(** Implementation of Abstraction Functions *)

module L = Logging
module F = Format

(** {2 Abstraction} *)

type rule =
  { r_vars: Ident.t list;
    r_root: Match.hpred_pat;
    r_sigma: Match.hpred_pat list; (* sigma should be in a specific order *)
    r_new_sigma: Sil.hpred list;
    r_new_pi: Prop.normal Prop.t -> Prop.normal Prop.t -> Sil.subst -> Sil.atom list;
    r_condition: Prop.normal Prop.t -> Sil.subst -> bool }

let sigma_rewrite tenv p r : Prop.normal Prop.t option =
  match (Match.prop_match_with_impl tenv p r.r_condition r.r_vars r.r_root r.r_sigma) with
  | None -> None
  | Some(sub, p_leftover) ->
      if not (r.r_condition p_leftover sub) then None
      else
        let res_pi = r.r_new_pi p p_leftover sub in
        let res_sigma = Prop.sigma_sub sub r.r_new_sigma in
        let p_with_res_pi = List.fold ~f:(Prop.prop_atom_and tenv) ~init:p_leftover res_pi in
        let p_new = Prop.prop_sigma_star p_with_res_pi res_sigma in
        Some (Prop.normalize tenv p_new)

let sigma_fav_list sigma =
  Sil.fav_to_list (Prop.sigma_fav sigma)

let sigma_fav_in_pvars =
  Sil.fav_imperative_to_functional Prop.sigma_fav_in_pvars_add

let sigma_fav_in_pvars_list sigma =
  Sil.fav_to_list (sigma_fav_in_pvars sigma)

(******************** Start of SLL abstraction rules  *****************)
let create_fresh_primeds_ls para =
  let id_base = Ident.create_fresh Ident.kprimed in
  let id_next = Ident.create_fresh Ident.kprimed in
  let id_end = Ident.create_fresh Ident.kprimed in
  let ids_shared =
    let svars = para.Sil.svars in
    let f _ = Ident.create_fresh Ident.kprimed in
    List.map ~f svars in
  let ids_tuple = (id_base, id_next, id_end, ids_shared) in
  let exp_base = Exp.Var id_base in
  let exp_next = Exp.Var id_next in
  let exp_end = Exp.Var id_end in
  let exps_shared = List.map ~f:(fun id -> Exp.Var id) ids_shared in
  let exps_tuple = (exp_base, exp_next, exp_end, exps_shared) in
  (ids_tuple, exps_tuple)

let create_condition_ls ids_private id_base p_leftover (inst: Sil.subst) =
  let (insts_of_private_ids, insts_of_public_ids, inst_of_base) =
    let f id' = List.exists ~f:(fun id'' -> Ident.equal id' id'') ids_private in
    let (inst_private, inst_public) = Sil.sub_domain_partition f inst in
    let insts_of_public_ids = Sil.sub_range inst_public in
    let inst_of_base = try Sil.sub_find (Ident.equal id_base) inst_public with Not_found -> assert false in
    let insts_of_private_ids = Sil.sub_range inst_private in
    (insts_of_private_ids, insts_of_public_ids, inst_of_base) in
  let fav_insts_of_public_ids = List.concat_map ~f:Sil.exp_fav_list insts_of_public_ids in
  let fav_insts_of_private_ids = List.concat_map ~f:Sil.exp_fav_list insts_of_private_ids in
  let (fav_p_leftover, _) =
    let sigma = p_leftover.Prop.sigma in
    (sigma_fav_list sigma, sigma_fav_in_pvars_list sigma) in
  let fpv_inst_of_base = Sil.exp_fpv inst_of_base in
  let fpv_insts_of_private_ids = List.concat_map ~f:Sil.exp_fpv insts_of_private_ids in
  (*
  let fav_inst_of_base = Sil.exp_fav_list inst_of_base in
  L.out "@[.... application of condition ....@\n@.";
  L.out "@[<4>  private ids : %a@\n@." pp_exp_list insts_of_private_ids;
  L.out "@[<4>  public ids : %a@\n@." pp_exp_list insts_of_public_ids;
  *)
  (* (not (IList.intersect compare fav_inst_of_base fav_in_pvars)) && *)
  (List.is_empty fpv_inst_of_base) &&
  (List.is_empty fpv_insts_of_private_ids) &&
  (not (List.exists ~f:Ident.is_normal fav_insts_of_private_ids)) &&
  (not (IList.intersect Ident.compare fav_insts_of_private_ids fav_p_leftover)) &&
  (not (IList.intersect Ident.compare fav_insts_of_private_ids fav_insts_of_public_ids))

let mk_rule_ptspts_ls tenv impl_ok1 impl_ok2 (para: Sil.hpara) =
  let (ids_tuple, exps_tuple) = create_fresh_primeds_ls para in
  let (id_base, id_next, id_end, ids_shared) = ids_tuple in
  let (exp_base, exp_next, exp_end, exps_shared) = exps_tuple in
  let (ids_exist_fst, para_fst) = Sil.hpara_instantiate para exp_base exp_next exps_shared in
  let (para_fst_start, para_fst_rest) =
    let mark_impl_flag hpred = { Match.hpred = hpred; Match.flag = impl_ok1 } in
    match para_fst with
    | [] -> L.out "@.@.ERROR (Empty Para): %a @.@." (Sil.pp_hpara Pp.text) para; assert false
    | hpred :: hpreds ->
        let hpat = mark_impl_flag hpred in
        let hpats = List.map ~f:mark_impl_flag hpreds in
        (hpat, hpats) in
  let (ids_exist_snd, para_snd) =
    let mark_impl_flag hpred = { Match.hpred = hpred; Match.flag = impl_ok2 } in
    let (ids, para_body) = Sil.hpara_instantiate para exp_next exp_end exps_shared in
    let para_body_hpats = List.map ~f:mark_impl_flag para_body in
    (ids, para_body_hpats) in
  let lseg_res = Prop.mk_lseg tenv Sil.Lseg_NE para exp_base exp_end exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = id_next :: (ids_exist_fst @ ids_exist_snd) in
    create_condition_ls ids_private id_base in
  { r_vars = id_base :: id_next :: id_end :: ids_shared @ ids_exist_fst @ ids_exist_snd;
    r_root = para_fst_start;
    r_sigma = para_fst_rest @ para_snd;
    r_new_sigma = [lseg_res];
    r_new_pi = gen_pi_res;
    r_condition = condition }

let mk_rule_ptsls_ls tenv k2 impl_ok1 impl_ok2 para =
  let (ids_tuple, exps_tuple) = create_fresh_primeds_ls para in
  let (id_base, id_next, id_end, ids_shared) = ids_tuple in
  let (exp_base, exp_next, exp_end, exps_shared) = exps_tuple in
  let (ids_exist, para_inst) = Sil.hpara_instantiate para exp_base exp_next exps_shared in
  let (para_inst_start, para_inst_rest) =
    match para_inst with
    | [] -> L.out "@.@.ERROR (Empty Para): %a @.@." (Sil.pp_hpara Pp.text) para; assert false
    | hpred :: hpreds ->
        let allow_impl hpred = { Match.hpred = hpred; Match.flag = impl_ok1 } in
        (allow_impl hpred, List.map ~f:allow_impl hpreds) in
  let lseg_pat = { Match.hpred = Prop.mk_lseg tenv k2 para exp_next exp_end exps_shared; Match.flag = impl_ok2 } in
  let lseg_res = Prop.mk_lseg tenv Sil.Lseg_NE para exp_base exp_end exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = id_next :: ids_exist in
    create_condition_ls ids_private id_base in
  { r_vars = id_base :: id_next :: id_end :: ids_shared @ ids_exist;
    r_root = para_inst_start;
    r_sigma = para_inst_rest @ [lseg_pat];
    r_new_pi = gen_pi_res;
    r_new_sigma = [lseg_res];
    r_condition = condition }

let mk_rule_lspts_ls tenv k1 impl_ok1 impl_ok2 para =
  let (ids_tuple, exps_tuple) = create_fresh_primeds_ls para in
  let (id_base, id_next, id_end, ids_shared) = ids_tuple in
  let (exp_base, exp_next, exp_end, exps_shared) = exps_tuple in
  let lseg_pat = { Match.hpred = Prop.mk_lseg tenv k1 para exp_base exp_next exps_shared; Match.flag = impl_ok1 } in
  let (ids_exist, para_inst_pat) =
    let (ids, para_body) = Sil.hpara_instantiate para exp_next exp_end exps_shared in
    let allow_impl hpred = { Match.hpred = hpred; Match.flag = impl_ok2 } in
    let para_body_pat = List.map ~f:allow_impl para_body in
    (ids, para_body_pat) in
  let lseg_res = Prop.mk_lseg tenv Sil.Lseg_NE para exp_base exp_end exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = id_next :: ids_exist in
    create_condition_ls ids_private id_base in
  { r_vars = id_base :: id_next :: id_end :: ids_shared @ ids_exist;
    r_root = lseg_pat;
    r_sigma = para_inst_pat;
    r_new_sigma = [lseg_res];
    r_new_pi = gen_pi_res;
    r_condition = condition }

let lseg_kind_add k1 k2 = match k1, k2 with
  | Sil.Lseg_NE, Sil.Lseg_NE | Sil.Lseg_NE, Sil.Lseg_PE | Sil.Lseg_PE, Sil.Lseg_NE -> Sil.Lseg_NE
  | Sil.Lseg_PE, Sil.Lseg_PE -> Sil.Lseg_PE

let mk_rule_lsls_ls tenv k1 k2 impl_ok1 impl_ok2 para =
  let (ids_tuple, exps_tuple) = create_fresh_primeds_ls para in
  let (id_base, id_next, id_end, ids_shared) = ids_tuple in
  let (exp_base, exp_next, exp_end, exps_shared) = exps_tuple in
  let lseg_fst_pat =
    { Match.hpred = Prop.mk_lseg tenv k1 para exp_base exp_next exps_shared; Match.flag = impl_ok1 } in
  let lseg_snd_pat =
    { Match.hpred = Prop.mk_lseg tenv k2 para exp_next exp_end exps_shared; Match.flag = impl_ok2 } in
  let k_res = lseg_kind_add k1 k2 in
  let lseg_res = Prop.mk_lseg tenv k_res para exp_base exp_end exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = []
  (*
  let inst_base, inst_next, inst_end =
  let find x = sub_find (equal x) inst in
  try
  (find id_base, find id_next, find id_end)
  with Not_found -> assert false in
  let spooky_case _ =
  (equal_lseg_kind Sil.Lseg_PE k_res)
  && (check_allocatedness p_leftover inst_end)
  && ((check_disequal p_start inst_base inst_next)
  || (check_disequal p_start inst_next inst_end)) in
  let obvious_case _ =
  check_disequal p_start inst_base inst_end &&
  not (check_disequal p_leftover inst_base inst_end) in
  if not (spooky_case () || obvious_case ()) then []
  else [Aneq(inst_base, inst_end)]
  *)
  in
  let condition =
    let ids_private = [id_next] in
    create_condition_ls ids_private id_base in
  { r_vars = id_base :: id_next :: id_end :: ids_shared ;
    r_root = lseg_fst_pat;
    r_sigma = [lseg_snd_pat];
    r_new_sigma = [lseg_res];
    r_new_pi = gen_pi_res;
    r_condition = condition }

let mk_rules_for_sll tenv (para : Sil.hpara) : rule list =
  if not Config.nelseg then
    begin
      let pts_pts = mk_rule_ptspts_ls tenv true true para in
      let pts_pels = mk_rule_ptsls_ls tenv Sil.Lseg_PE true false para in
      let pels_pts = mk_rule_lspts_ls tenv Sil.Lseg_PE false true para in
      let pels_nels = mk_rule_lsls_ls tenv Sil.Lseg_PE Sil.Lseg_NE false false para in
      let nels_pels = mk_rule_lsls_ls tenv Sil.Lseg_NE Sil.Lseg_PE false false para in
      let pels_pels = mk_rule_lsls_ls tenv Sil.Lseg_PE Sil.Lseg_PE false false para in
      [pts_pts; pts_pels; pels_pts; pels_nels; nels_pels; pels_pels]
    end
  else
    begin
      let pts_pts = mk_rule_ptspts_ls tenv true true para in
      let pts_nels = mk_rule_ptsls_ls tenv Sil.Lseg_NE true false para in
      let nels_pts = mk_rule_lspts_ls tenv Sil.Lseg_NE false true para in
      let nels_nels = mk_rule_lsls_ls tenv Sil.Lseg_NE Sil.Lseg_NE false false para in
      [pts_pts; pts_nels; nels_pts; nels_nels]
    end
(******************  End of SLL abstraction rules ******************)

(******************  Start of DLL abstraction rules  ******************)
let create_condition_dll = create_condition_ls

let mk_rule_ptspts_dll tenv impl_ok1 impl_ok2 para =
  let id_iF = Ident.create_fresh Ident.kprimed in
  let id_iF' = Ident.create_fresh Ident.kprimed in
  let id_oB = Ident.create_fresh Ident.kprimed in
  let id_oF = Ident.create_fresh Ident.kprimed in
  let ids_shared =
    let svars = para.Sil.svars_dll in
    let f _ = Ident.create_fresh Ident.kprimed in
    List.map ~f svars in
  let exp_iF = Exp.Var id_iF in
  let exp_iF' = Exp.Var id_iF' in
  let exp_oB = Exp.Var id_oB in
  let exp_oF = Exp.Var id_oF in
  let exps_shared = List.map ~f:(fun id -> Exp.Var id) ids_shared in
  let (ids_exist_fst, para_fst) = Sil.hpara_dll_instantiate para exp_iF exp_oB exp_iF' exps_shared in
  let (para_fst_start, para_fst_rest) =
    let mark_impl_flag hpred = { Match.hpred = hpred; Match.flag = impl_ok1 } in
    match para_fst with
    | [] -> L.out "@.@.ERROR (Empty DLL para): %a@.@." (Sil.pp_hpara_dll Pp.text) para; assert false
    | hpred :: hpreds ->
        let hpat = mark_impl_flag hpred in
        let hpats = List.map ~f:mark_impl_flag hpreds in
        (hpat, hpats) in
  let (ids_exist_snd, para_snd) =
    let mark_impl_flag hpred = { Match.hpred = hpred; Match.flag = impl_ok2 } in
    let (ids, para_body) = Sil.hpara_dll_instantiate para exp_iF' exp_iF exp_oF exps_shared in
    let para_body_hpats = List.map ~f:mark_impl_flag para_body in
    (ids, para_body_hpats) in
  let dllseg_res = Prop.mk_dllseg tenv Sil.Lseg_NE para exp_iF exp_oB exp_oF exp_iF' exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    (* for the case of ptspts since iF'=iB therefore iF' cannot be private*)
    let ids_private = ids_exist_fst @ ids_exist_snd in
    create_condition_dll ids_private id_iF in
  (*
  L.out "r_root/para_fst_start=%a @.@." pp_hpat para_fst_start;
  L.out "para_fst_rest=%a @.@." pp_hpat_list para_fst_rest;
  L.out "para_snd=%a @.@." pp_hpat_list para_snd;
  L.out "dllseg_res=%a @.@." pp_hpred dllseg_res;
  *)
  { r_vars = id_iF :: id_oB :: id_iF':: id_oF :: ids_shared @ ids_exist_fst @ ids_exist_snd;
    r_root = para_fst_start;
    r_sigma = para_fst_rest @ para_snd;
    r_new_sigma = [dllseg_res];
    r_new_pi = gen_pi_res;
    r_condition = condition }

let mk_rule_ptsdll_dll tenv k2 impl_ok1 impl_ok2 para =
  let id_iF = Ident.create_fresh Ident.kprimed in
  let id_iF' = Ident.create_fresh Ident.kprimed in
  let id_oB = Ident.create_fresh Ident.kprimed in
  let id_oF = Ident.create_fresh Ident.kprimed in
  let id_iB = Ident.create_fresh Ident.kprimed in
  let ids_shared =
    let svars = para.Sil.svars_dll in
    let f _ = Ident.create_fresh Ident.kprimed in
    List.map ~f svars in
  let exp_iF = Exp.Var id_iF in
  let exp_iF' = Exp.Var id_iF' in
  let exp_oB = Exp.Var id_oB in
  let exp_oF = Exp.Var id_oF in
  let exp_iB = Exp.Var id_iB in
  let exps_shared = List.map ~f:(fun id -> Exp.Var id) ids_shared in
  let (ids_exist, para_inst) = Sil.hpara_dll_instantiate para exp_iF exp_oB exp_iF' exps_shared in
  let (para_inst_start, para_inst_rest) =
    match para_inst with
    | [] -> assert false
    | hpred :: hpreds ->
        let allow_impl hpred = { Match.hpred = hpred; Match.flag = impl_ok1 } in
        (allow_impl hpred, List.map ~f:allow_impl hpreds) in
  let dllseg_pat = { Match.hpred = Prop.mk_dllseg tenv k2 para exp_iF' exp_iF exp_oF exp_iB exps_shared; Match.flag = impl_ok2 } in
  let dllseg_res = Prop.mk_dllseg tenv Sil.Lseg_NE para exp_iF exp_oB exp_oF exp_iB exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = id_iF':: ids_exist in
    create_condition_dll ids_private id_iF in
  { r_vars = id_iF :: id_oB :: id_iF':: id_oF:: id_iB:: ids_shared @ ids_exist;
    r_root = para_inst_start;
    r_sigma = para_inst_rest @ [dllseg_pat];
    r_new_pi = gen_pi_res;
    r_new_sigma = [dllseg_res];
    r_condition = condition }

let mk_rule_dllpts_dll tenv k1 impl_ok1 impl_ok2 para =
  let id_iF = Ident.create_fresh Ident.kprimed in
  let id_iF' = Ident.create_fresh Ident.kprimed in
  let id_oB = Ident.create_fresh Ident.kprimed in
  let id_oB' = Ident.create_fresh Ident.kprimed in
  let id_oF = Ident.create_fresh Ident.kprimed in
  let ids_shared =
    let svars = para.Sil.svars_dll in
    let f _ = Ident.create_fresh Ident.kprimed in
    List.map ~f svars in
  let exp_iF = Exp.Var id_iF in
  let exp_iF' = Exp.Var id_iF' in
  let exp_oB = Exp.Var id_oB in
  let exp_oB' = Exp.Var id_oB' in
  let exp_oF = Exp.Var id_oF in
  let exps_shared = List.map ~f:(fun id -> Exp.Var id) ids_shared in
  let (ids_exist, para_inst) = Sil.hpara_dll_instantiate para exp_iF' exp_oB' exp_oF exps_shared in
  let para_inst_pat =
    let allow_impl hpred = { Match.hpred = hpred; Match.flag = impl_ok2 } in
    List.map ~f:allow_impl para_inst in
  let dllseg_pat = { Match.hpred = Prop.mk_dllseg tenv k1 para exp_iF exp_oB exp_iF' exp_oB' exps_shared; Match.flag = impl_ok1 } in
  let dllseg_res = Prop.mk_dllseg tenv Sil.Lseg_NE para exp_iF exp_oB exp_oF exp_iF' exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = id_oB':: ids_exist in
    create_condition_dll ids_private id_iF in
  { r_vars = id_iF :: id_oB :: id_iF':: id_oB':: id_oF:: ids_shared @ ids_exist;
    r_root = dllseg_pat;
    r_sigma = para_inst_pat;
    r_new_pi = gen_pi_res;
    r_new_sigma = [dllseg_res];
    r_condition = condition }

let mk_rule_dlldll_dll tenv k1 k2 impl_ok1 impl_ok2 para =
  let id_iF = Ident.create_fresh Ident.kprimed in
  let id_iF' = Ident.create_fresh Ident.kprimed in
  let id_oB = Ident.create_fresh Ident.kprimed in
  let id_oB' = Ident.create_fresh Ident.kprimed in
  let id_oF = Ident.create_fresh Ident.kprimed in
  let id_iB = Ident.create_fresh Ident.kprimed in
  let ids_shared =
    let svars = para.Sil.svars_dll in
    let f _ = Ident.create_fresh Ident.kprimed in
    List.map ~f svars in
  let exp_iF = Exp.Var id_iF in
  let exp_iF' = Exp.Var id_iF' in
  let exp_oB = Exp.Var id_oB in
  let exp_oB' = Exp.Var id_oB' in
  let exp_oF = Exp.Var id_oF in
  let exp_iB = Exp.Var id_iB in
  let exps_shared = List.map ~f:(fun id -> Exp.Var id) ids_shared in
  let lseg_fst_pat = { Match.hpred = Prop.mk_dllseg tenv k1 para exp_iF exp_oB exp_iF' exp_oB' exps_shared; Match.flag = impl_ok1 } in
  let lseg_snd_pat = { Match.hpred = Prop.mk_dllseg tenv k2 para exp_iF' exp_oB' exp_oF exp_iB exps_shared; Match.flag = impl_ok2 } in
  let k_res = lseg_kind_add k1 k2 in
  let lseg_res = Prop.mk_dllseg tenv k_res para exp_iF exp_oB exp_oF exp_iB exps_shared in
  let gen_pi_res _ _ (_: Sil.subst) = [] in
  let condition =
    let ids_private = [id_iF'; id_oB'] in
    create_condition_dll ids_private id_iF in
  { r_vars = id_iF :: id_iF' :: id_oB:: id_oB' :: id_oF:: id_iB:: ids_shared ;
    r_root = lseg_fst_pat;
    r_sigma = [lseg_snd_pat];
    r_new_sigma = [lseg_res];
    r_new_pi = gen_pi_res;
    r_condition = condition }

let mk_rules_for_dll tenv (para : Sil.hpara_dll) : rule list =
  if not Config.nelseg then
    begin
      let pts_pts = mk_rule_ptspts_dll tenv true true para in
      let pts_pedll = mk_rule_ptsdll_dll tenv Sil.Lseg_PE true false para in
      let pedll_pts = mk_rule_dllpts_dll tenv Sil.Lseg_PE false true para in
      let pedll_nedll = mk_rule_dlldll_dll tenv Sil.Lseg_PE Sil.Lseg_NE false false para in
      let nedll_pedll = mk_rule_dlldll_dll tenv Sil.Lseg_NE Sil.Lseg_PE false false para in
      let pedll_pedll = mk_rule_dlldll_dll tenv Sil.Lseg_PE Sil.Lseg_PE false false para in
      [pts_pts; pts_pedll; pedll_pts; pedll_nedll; nedll_pedll; pedll_pedll]
    end
  else
    begin
      let ptspts_dll = mk_rule_ptspts_dll tenv true true para in
      let ptsdll_dll = mk_rule_ptsdll_dll tenv Sil.Lseg_NE true false para in
      let dllpts_dll = mk_rule_dllpts_dll tenv Sil.Lseg_NE false true para in
      let dlldll_dll = mk_rule_dlldll_dll tenv Sil.Lseg_NE Sil.Lseg_NE false false para in
      [ptspts_dll; ptsdll_dll; dllpts_dll; dlldll_dll]
    end
(******************  End of DLL abstraction rules  ******************)

(******************  Start of Predicate Discovery  ******************)
let typ_get_recursive_flds tenv typ_exp =
  let filter typ (_, (t: Typ.t), _) =
    match t.desc with
    | Tstruct _ | Tint _ | Tfloat _ | Tvoid | Tfun _ ->
        false
    | Tptr ({desc=Tstruct _} as typ', _) ->
        Typ.equal typ' typ
    | Tptr _ | Tarray _ ->
        false
  in
  match typ_exp with
  | Exp.Sizeof {typ} -> (
      match typ.desc with
      | Tstruct name -> (
          match Tenv.lookup tenv name with
          | Some { fields } -> List.map ~f:fst3 (List.filter ~f:(filter typ) fields)
          | None ->
              L.err "@.typ_get_recursive: unexpected type expr: %a@." Exp.pp typ_exp;
              [] (* ToDo: assert false *)
        )
      | Tint _ | Tvoid | Tfun _ | Tptr _ | Tfloat _ | Tarray _ -> []
    )
  | Exp.Var _ -> [] (* type of |-> not known yet *)
  | Exp.Const _ -> []
  | _ ->
      L.err "@.typ_get_recursive: unexpected type expr: %a@." Exp.pp typ_exp;
      assert false

let discover_para_roots tenv p root1 next1 root2 next2 : Sil.hpara option =
  let eq_arg1 = Exp.equal root1 next1 in
  let eq_arg2 = Exp.equal root2 next2 in
  let precondition_check = (not eq_arg1 && not eq_arg2) in
  if not precondition_check then None
  else
    let corres = [(next1, next2)] in
    let todos = [(root1, root2)] in
    let sigma = p.Prop.sigma in
    match Match.find_partial_iso tenv (Prover.check_equal tenv p) corres todos sigma with
    | None -> None
    | Some (new_corres, new_sigma1, _, _) ->
        let hpara, _ = Match.hpara_create tenv new_corres new_sigma1 root1 next1 in
        Some hpara

let discover_para_dll_roots tenv p root1 blink1 flink1 root2 blink2 flink2 : Sil.hpara_dll option =
  let eq_arg1 = Exp.equal root1 blink1 in
  let eq_arg1' = Exp.equal root1 flink1 in
  let eq_arg2 = Exp.equal root2 blink2 in
  let eq_arg2' = Exp.equal root2 flink2 in
  let precondition_check = not (eq_arg1 || eq_arg1' || eq_arg2 || eq_arg2') in
  if not precondition_check then None
  else
    let corres = [(blink1, blink2); (flink1, flink2)] in
    let todos = [(root1, root2)] in
    let sigma = p.Prop.sigma in
    match Match.find_partial_iso tenv (Prover.check_equal tenv p) corres todos sigma with
    | None -> None
    | Some (new_corres, new_sigma1, _, _) ->
        let hpara_dll, _ = Match.hpara_dll_create tenv new_corres new_sigma1 root1 blink1 flink1 in
        Some hpara_dll

let discover_para_candidates tenv p =
  let edges = ref [] in
  let add_edge edg = edges := edg :: !edges in
  let get_edges_strexp rec_flds root se =
    let is_rec_fld fld = List.exists ~f:(Fieldname.equal fld) rec_flds in
    match se with
    | Sil.Eexp _ | Sil.Earray _ -> ()
    | Sil.Estruct (fsel, _) ->
        let fsel' = List.filter ~f:(fun (fld, _) -> is_rec_fld fld) fsel in
        let process (_, nextse) =
          match nextse with
          | Sil.Eexp (next, _) -> add_edge (root, next)
          | _ -> assert false in
        List.iter ~f:process fsel' in
  let rec get_edges_sigma = function
    | [] -> ()
    | Sil.Hlseg _ :: sigma_rest | Sil.Hdllseg _ :: sigma_rest ->
        get_edges_sigma sigma_rest
    | Sil.Hpointsto (root, se, te) :: sigma_rest ->
        let rec_flds = typ_get_recursive_flds tenv te in
        get_edges_strexp rec_flds root se;
        get_edges_sigma sigma_rest in
  let rec find_all_consecutive_edges found edges_seen = function
    | [] -> List.rev found
    | (e1, e2) :: edges_notseen ->
        let edges_others = List.rev_append edges_seen edges_notseen in
        let edges_matched = List.filter ~f:(fun (e1', _) -> Exp.equal e2 e1') edges_others in
        let new_found =
          let f found_acc (_, e3) = (e1, e2, e3) :: found_acc in
          List.fold ~f ~init:found edges_matched in
        let new_edges_seen = (e1, e2) :: edges_seen in
        find_all_consecutive_edges new_found new_edges_seen edges_notseen in
  let sigma = p.Prop.sigma in
  get_edges_sigma sigma;
  find_all_consecutive_edges [] [] !edges

let discover_para_dll_candidates tenv p =
  let edges = ref [] in
  let add_edge edg = (edges := edg :: !edges) in
  let get_edges_strexp rec_flds root se =
    let is_rec_fld fld = List.exists ~f:(Fieldname.equal fld) rec_flds in
    match se with
    | Sil.Eexp _ | Sil.Earray _ -> ()
    | Sil.Estruct (fsel, _) ->
        let fsel' = List.rev_filter ~f:(fun (fld, _) -> is_rec_fld fld) fsel in
        let convert_to_exp acc (_, se) =
          match se with
          | Sil.Eexp (e, _) -> e:: acc
          | _ -> assert false in
        let links = List.fold ~f:convert_to_exp ~init:[] fsel' in
        let rec iter_pairs = function
          | [] -> ()
          | x:: l -> (List.iter ~f:(fun y -> add_edge (root, x, y)) l; iter_pairs l) in
        iter_pairs links in
  let rec get_edges_sigma = function
    | [] -> ()
    | Sil.Hlseg _ :: sigma_rest | Sil.Hdllseg _ :: sigma_rest ->
        get_edges_sigma sigma_rest
    | Sil.Hpointsto (root, se, te) :: sigma_rest ->
        let rec_flds = typ_get_recursive_flds tenv te in
        get_edges_strexp rec_flds root se;
        get_edges_sigma sigma_rest in
  let rec find_all_consecutive_edges found edges_seen = function
    | [] -> List.rev found
    | (iF, blink, flink) :: edges_notseen ->
        let edges_others = List.rev_append edges_seen edges_notseen in
        let edges_matched = List.filter ~f:(fun (e1', _, _) -> Exp.equal flink e1') edges_others in
        let new_found =
          let f found_acc (_, _, flink2) = (iF, blink, flink, flink2) :: found_acc in
          List.fold ~f ~init:found edges_matched in
        let new_edges_seen = (iF, blink, flink) :: edges_seen in
        find_all_consecutive_edges new_found new_edges_seen edges_notseen in
  let sigma = p.Prop.sigma in
  get_edges_sigma sigma;
  find_all_consecutive_edges [] [] !edges

let discover_para tenv p =
  let candidates = discover_para_candidates tenv p in
  let already_defined para paras =
    List.exists ~f:(fun para' -> Match.hpara_iso tenv para para') paras in
  let f paras (root, next, out) =
    match (discover_para_roots tenv p root next next out) with
    | None -> paras
    | Some para -> if already_defined para paras then paras else para :: paras in
  List.fold ~f ~init:[] candidates

let discover_para_dll tenv p =
  (*
  L.out "@[.... Called discover_dll para ...@.";
  L.out "@[<4>  PROP : %a@\n@." pp_prop p;
  *)
  let candidates = discover_para_dll_candidates tenv p in
  let already_defined para paras =
    List.exists ~f:(fun para' -> Match.hpara_dll_iso tenv para para') paras in
  let f paras (iF, oB, iF', oF) =
    match (discover_para_dll_roots tenv p iF oB iF' iF' iF oF) with
    | None -> paras
    | Some para -> if already_defined para paras then paras else para :: paras in
  List.fold ~f ~init:[] candidates
(******************  End of Predicate Discovery  ******************)

(****************** Start of the ADT abs_rules ******************)

(** Type of parameter for abstraction rules *)
type para_ty =
  | SLL of Sil.hpara
  | DLL of Sil.hpara_dll

(** Rule set: a list of rules of a given type *)
type rule_set = para_ty * rule list

type rules = rule_set list

module Global =
struct
  let current_rules : rules ref =
    ref []
end

let get_current_rules () =
  !Global.current_rules

let set_current_rules rules =
  Global.current_rules := rules

let reset_current_rules () =
  Global.current_rules := []

let eqs_sub subst eqs =
  List.map ~f:(fun (e1, e2) -> (Sil.exp_sub subst e1, Sil.exp_sub subst e2)) eqs

let eqs_solve ids_in eqs_in =
  let rec solve (sub: Sil.subst) (eqs: (Exp.t * Exp.t) list) : Sil.subst option =
    let do_default id e eqs_rest =
      if not (List.exists ~f:(fun id' -> Ident.equal id id') ids_in) then None
      else
        let sub' = match Sil.extend_sub sub id e with
          | None -> L.out "@.@.ERROR : Buggy Implementation.@.@."; assert false
          | Some sub' -> sub' in
        let eqs_rest' = eqs_sub sub' eqs_rest in
        solve sub' eqs_rest' in
    match eqs with
    | [] -> Some sub
    | (e1, e2) :: eqs_rest when Exp.equal e1 e2 ->
        solve sub eqs_rest
    | (Exp.Var id1, (Exp.Const _ as e2)) :: eqs_rest ->
        do_default id1 e2 eqs_rest
    | ((Exp.Const _ as e1), (Exp.Var _ as e2)) :: eqs_rest ->
        solve sub ((e2, e1):: eqs_rest)
    | ((Exp.Var id1 as e1), (Exp.Var id2 as e2)) :: eqs_rest ->
        let n = Ident.compare id1 id2 in
        begin
          if Int.equal n 0 then solve sub eqs_rest
          else if n > 0 then solve sub ((e2, e1):: eqs_rest)
          else do_default id1 e2 eqs_rest
        end
    | _ :: _ -> None in
  let compute_ids sub =
    let sub_list = Sil.sub_to_list sub in
    let sub_dom = List.map ~f:fst sub_list in
    let filter id =
      not (List.exists ~f:(fun id' -> Ident.equal id id') sub_dom) in
    List.filter ~f:filter ids_in in
  match solve Sil.sub_empty eqs_in with
  | None -> None
  | Some sub -> Some (compute_ids sub, sub)

let sigma_special_cases_eqs sigma =
  let rec f ids_acc eqs_acc sigma_acc = function
    | [] ->
        [(List.rev ids_acc, List.rev eqs_acc, List.rev sigma_acc)]
    | Sil.Hpointsto _ as hpred :: sigma_rest ->
        f ids_acc eqs_acc (hpred:: sigma_acc) sigma_rest
    | Sil.Hlseg(_, para, e1, e2, es) as hpred :: sigma_rest ->
        let empty_case =
          f ids_acc ((e1, e2):: eqs_acc) sigma_acc sigma_rest in
        let pointsto_case =
          let (eids, para_inst) = Sil.hpara_instantiate para e1 e2 es in
          f (eids@ids_acc) eqs_acc sigma_acc (para_inst@sigma_rest) in
        let general_case =
          f ids_acc eqs_acc (hpred:: sigma_acc) sigma_rest in
        empty_case @ pointsto_case @ general_case
    | Sil.Hdllseg(_, para, e1, e2, e3, e4, es) as hpred :: sigma_rest ->
        let empty_case =
          f ids_acc ((e1, e3):: (e2, e4):: eqs_acc) sigma_acc sigma_rest in
        let pointsto_case =
          let (eids, para_inst) = Sil.hpara_dll_instantiate para e1 e2 e3 es in
          f (eids@ids_acc) eqs_acc sigma_acc (para_inst@sigma_rest) in
        let general_case =
          f ids_acc eqs_acc (hpred:: sigma_acc) sigma_rest in
        empty_case @ pointsto_case @ general_case in
  f [] [] [] sigma

let sigma_special_cases ids sigma : (Ident.t list * Sil.hpred list) list =
  let special_cases_eqs = sigma_special_cases_eqs sigma in
  let special_cases_rev =
    let f acc (eids_cur, eqs_cur, sigma_cur) =
      let ids_all = ids @ eids_cur in
      match (eqs_solve ids_all eqs_cur) with
      | None -> acc
      | Some (ids_res, sub) ->
          (ids_res, List.map ~f:(Sil.hpred_sub sub) sigma_cur) :: acc in
    List.fold ~f ~init:[] special_cases_eqs in
  List.rev special_cases_rev

let hpara_special_cases hpara : Sil.hpara list =
  let update_para (evars', body') = { hpara with Sil.evars = evars'; Sil.body = body'} in
  let special_cases = sigma_special_cases hpara.Sil.evars hpara.Sil.body in
  List.map ~f:update_para special_cases

let hpara_special_cases_dll hpara : Sil.hpara_dll list =
  let update_para (evars', body') = { hpara with Sil.evars_dll = evars'; Sil.body_dll = body'} in
  let special_cases = sigma_special_cases hpara.Sil.evars_dll hpara.Sil.body_dll in
  List.map ~f:update_para special_cases

let abs_rules_apply_rsets tenv (rsets: rule_set list) (p_in: Prop.normal Prop.t) : Prop.normal Prop.t =
  let apply_rule (changed, p) r =
    match (sigma_rewrite tenv p r) with
    | None -> (changed, p)
    | Some p' ->
    (*
    L.out "@[.... abstraction (rewritten in abs_rules) ....@.";
    L.out "@[<4>  PROP:%a@\n@." pp_prop p';
    *)
        (true, p') in
  let rec apply_rule_set p rset =
    let (_, rules) = rset in
    let (changed, p') = List.fold ~f:apply_rule ~init:(false, p) rules in
    if changed then apply_rule_set p' rset else p' in
  List.fold ~f:apply_rule_set ~init:p_in rsets

let abs_rules_apply_lists tenv (p_in: Prop.normal Prop.t) : Prop.normal Prop.t =
  let new_rsets = ref [] in
  let old_rsets = get_current_rules () in
  let rec discover_then_abstract p =
    let (closed_paras_sll, closed_paras_dll) =
      let paras_sll = discover_para tenv p in
      let paras_dll = discover_para_dll tenv p in
      let closed_paras_sll = List.concat_map ~f:hpara_special_cases paras_sll in
      let closed_paras_dll = List.concat_map ~f:hpara_special_cases_dll paras_dll in
      begin
        (closed_paras_sll, closed_paras_dll)
      end in
    let (todo_paras_sll, todo_paras_dll) =
      let eq_sll para rset = match rset with
        | (SLL para', _) -> Match.hpara_iso tenv para para'
        | _ -> false in
      let eq_dll para rset = match rset with
        | (DLL para', _) -> Match.hpara_dll_iso tenv para para'
        | _ -> false in
      let filter_sll para =
        not (List.exists ~f:(eq_sll para) old_rsets) &&
        not (List.exists ~f:(eq_sll para) !new_rsets) in
      let filter_dll para =
        not (List.exists ~f:(eq_dll para) old_rsets) &&
        not (List.exists ~f:(eq_dll para) !new_rsets) in
      let todo_paras_sll = List.filter ~f:filter_sll closed_paras_sll in
      let todo_paras_dll = List.filter ~f:filter_dll closed_paras_dll in
      (todo_paras_sll, todo_paras_dll) in
    let f_recurse () =
      let todo_rsets_sll =
        List.map ~f:(fun para -> (SLL para, mk_rules_for_sll tenv para)) todo_paras_sll in
      let todo_rsets_dll =
        List.map ~f:(fun para -> (DLL para, mk_rules_for_dll tenv para)) todo_paras_dll in
      new_rsets := !new_rsets @ todo_rsets_sll @ todo_rsets_dll;
      let p' = abs_rules_apply_rsets tenv todo_rsets_sll p in
      let p'' = abs_rules_apply_rsets tenv todo_rsets_dll p' in
      discover_then_abstract p'' in
    match todo_paras_sll, todo_paras_dll with
    | [], [] -> p
    | _ -> f_recurse () in
  let p1 = abs_rules_apply_rsets tenv old_rsets p_in in
  let p2 = discover_then_abstract p1 in
  let new_rules = old_rsets @ !new_rsets in
  set_current_rules new_rules;
  p2

let abs_rules_apply tenv (p_in: Prop.normal Prop.t) : Prop.normal Prop.t =
  abs_rules_apply_lists tenv p_in
(****************** End of the ADT abs_rules ******************)

(****************** Start of Main Abstraction Functions ******************)
let abstract_pure_part tenv p ~(from_abstract_footprint: bool) =
  let do_pure pure =
    let pi_filtered =
      let sigma = p.Prop.sigma in
      let fav_sigma = Prop.sigma_fav sigma in
      let fav_nonpure = Prop.prop_fav_nonpure p in (* vars in current and footprint sigma *)
      let filter atom =
        let fav' = Sil.atom_fav atom in
        Sil.fav_for_all fav' (fun id ->
            if Ident.is_primed id then Sil.fav_mem fav_sigma id
            else if Ident.is_footprint id then Sil.fav_mem fav_nonpure id
            else true) in
      List.filter ~f:filter pure in
    let new_pure =
      List.fold
        ~f:(fun pi a ->
            match a with
            (* we only use Lt and Le because Gt and Ge are inserted in terms of Lt and Le. *)
            | Sil.Aeq (Exp.Const (Const.Cint i), Exp.BinOp (Binop.Lt, _, _))
            | Sil.Aeq (Exp.BinOp (Binop.Lt, _, _), Exp.Const (Const.Cint i))
            | Sil.Aeq (Exp.Const (Const.Cint i), Exp.BinOp (Binop.Le, _, _))
            | Sil.Aeq (Exp.BinOp (Binop.Le, _, _), Exp.Const (Const.Cint i)) when IntLit.isone i ->
                a :: pi
            | Sil.Aeq (Exp.Var name, e) when not (Ident.is_primed name) ->
                (match e with
                 | Exp.Var _
                 | Exp.Const _ -> a :: pi
                 | _ -> pi)
            | Sil.Aneq (Var _, _)
            | Sil.Apred (_, Var _ :: _) | Anpred (_, Var _ :: _) -> a :: pi
            | Sil.Aeq _ | Aneq _ | Apred _ | Anpred _ -> pi
          )
        ~init:[] pi_filtered in
    List.rev new_pure in

  let new_pure = do_pure (Prop.get_pure p) in
  let eprop' = Prop.set p ~pi:new_pure ~sub:Sil.sub_empty in
  let eprop'' =
    if !Config.footprint && not from_abstract_footprint then
      let new_pi_footprint = do_pure p.Prop.pi_fp in
      Prop.set eprop' ~pi_fp:new_pi_footprint
    else eprop' in
  Prop.normalize tenv eprop''

(** Collect symbolic garbage from pi and sigma *)
let abstract_gc tenv p =
  let pi = p.Prop.pi in
  let p_without_pi = Prop.normalize tenv (Prop.set p ~pi:[]) in
  let fav_p_without_pi = Prop.prop_fav p_without_pi in
  (* let weak_filter atom =
     let fav_atom = atom_fav atom in
     IList.intersect compare fav_p_without_pi fav_atom in *)
  let strong_filter = function
    | Sil.Aeq(e1, e2) | Sil.Aneq(e1, e2) ->
        let fav_e1 = Sil.exp_fav e1 in
        let fav_e2 = Sil.exp_fav e2 in
        let intersect_e1 _ = IList.intersect Ident.compare (Sil.fav_to_list fav_e1) (Sil.fav_to_list fav_p_without_pi) in
        let intersect_e2 _ = IList.intersect Ident.compare (Sil.fav_to_list fav_e2) (Sil.fav_to_list fav_p_without_pi) in
        let no_fav_e1 = Sil.fav_is_empty fav_e1 in
        let no_fav_e2 = Sil.fav_is_empty fav_e2 in
        (no_fav_e1 || intersect_e1 ()) && (no_fav_e2 || intersect_e2 ())
    | (Sil.Apred _ | Anpred _) as a ->
        let fav_a = Sil.atom_fav a in
        Sil.fav_is_empty fav_a
        ||
        IList.intersect Ident.compare (Sil.fav_to_list fav_a) (Sil.fav_to_list fav_p_without_pi) in
  let new_pi = List.filter ~f:strong_filter pi in
  let prop = Prop.normalize tenv (Prop.set p ~pi:new_pi) in
  match Prop.prop_iter_create prop with
  | None -> prop
  | Some iter -> Prop.prop_iter_to_prop tenv (Prop.prop_iter_gc_fields iter)

module IdMap = Caml.Map.Make (Ident) (** maps from identifiers *)

module HpredSet =
  Caml.Set.Make(struct
    type t = Sil.hpred
    let compare = Sil.compare_hpred ~inst:false
  end)

let hpred_entries hpred = match hpred with
  | Sil.Hpointsto (e, _, _) -> [e]
  | Sil.Hlseg (_, _, e, _, _) -> [e]
  | Sil.Hdllseg (_, _, e1, _, _, e2, _) -> [e1; e2]

(** find the id's in sigma reachable from the given roots *)
let sigma_reachable root_fav sigma =
  let fav_to_set fav = Ident.idlist_to_idset (Sil.fav_to_list fav) in
  let reach_set = ref (fav_to_set root_fav) in
  let edges = ref [] in
  let do_hpred hpred =
    let hp_fav_set = fav_to_set (Sil.hpred_fav hpred) in
    let add_entry e = edges := (e, hp_fav_set) :: !edges in
    List.iter ~f:add_entry (hpred_entries hpred) in
  List.iter ~f:do_hpred sigma;
  let edge_fires (e, _) = match e with
    | Exp.Var id ->
        if (Ident.is_primed id || Ident.is_footprint id) then Ident.IdentSet.mem id !reach_set
        else true
    | _ -> true in
  let rec apply_once edges_to_revisit edges_todo modified = match edges_todo with
    | [] -> (edges_to_revisit, modified)
    | edge:: edges_todo' ->
        if edge_fires edge then
          begin
            reach_set := Ident.IdentSet.union (snd edge) !reach_set;
            apply_once edges_to_revisit edges_todo' true
          end
        else apply_once (edge :: edges_to_revisit) edges_todo' modified in
  let rec find_fixpoint edges_todo =
    let edges_to_revisit, modified = apply_once [] edges_todo false in
    if modified then find_fixpoint edges_to_revisit in
  find_fixpoint !edges;
  (* L.d_str "reachable: ";
     Ident.IdentSet.iter (fun id -> Sil.d_exp (Exp.Var id); L.d_str " ") !reach_set;
     L.d_ln (); *)
  !reach_set

let get_cycle root prop =
  let sigma = prop.Prop.sigma in
  let get_points_to e =
    match e with
    | Sil.Eexp(e', _) ->
        List.find
          ~f:(fun hpred -> match hpred with
              | Sil.Hpointsto (e'', _, _) -> Exp.equal e'' e'
              | _ -> false)
          sigma
    | _ -> None in
  let print_cycle cyc =
    (L.d_str "Cycle= ";
     List.iter ~f:(fun ((e, t), f, e') ->
         match e, e' with
         | Sil.Eexp (e, _), Sil.Eexp (e', _) ->
             L.d_str ("("^(Exp.to_string e)^": "^(Typ.to_string t)^", "
                      ^(Fieldname.to_string f)^", "^(Exp.to_string e')^")")
         | _ -> ()) cyc;
     L.d_strln "") in
  (* Perform a dfs of a graph stopping when e_root is reached.
     Returns a pair (path, bool) where path is a list of edges ((e1,type_e1),f,e2)
     describing the path to e_root and bool is true if e_root is reached. *)
  let rec dfs e_root et_src path el visited =
    match el with
    | [] -> path, false
    | (f, e):: el' ->
        if Sil.equal_strexp e e_root then
          (et_src, f, e):: path, true
        else if List.mem ~equal:Sil.equal_strexp visited e then
          path, false
        else (
          let visited' = (fst et_src):: visited in
          let res = (match get_points_to e with
              | None -> path, false
              | Some (Sil.Hpointsto (_, Sil.Estruct (fl, _), Exp.Sizeof {typ=te})) ->
                  dfs e_root (e, te) ((et_src, f, e):: path) fl visited'
              | _ -> path, false (* check for lists *)) in
          if snd res then res
          else dfs e_root et_src path el' visited') in
  L.d_strln "Looking for cycle with root expression: "; Sil.d_hpred root; L.d_strln "";
  match root with
  | Sil.Hpointsto (e_root, Sil.Estruct (fl, _), Exp.Sizeof {typ=te}) ->
      let se_root = Sil.Eexp(e_root, Sil.Inone) in
      (* start dfs with empty path and expr pointing to root *)
      let (pot_cycle, res) = dfs se_root (se_root, te) [] fl [] in
      if res then (
        print_cycle pot_cycle;
        pot_cycle
      ) else (
        L.d_strln "NO cycle found from root";
        [])
  | _ -> L.d_strln "Root exp is not an allocated object. No cycle found"; []


(** Check whether the hidden counter field of a struct representing an objective-c object is
    positive, and whether the leak is part of the specified buckets. In the positive case, it
    returns the bucket *)
let should_raise_objc_leak hpred =
  match hpred with
  | Sil.Hpointsto(_, Sil.Estruct((fn, Sil.Eexp( (Exp.Const (Const.Cint i)), _)):: _, _),
                  Exp.Sizeof {typ})
    when Fieldname.is_hidden fn && IntLit.gt i IntLit.zero (* counter > 0 *) ->
      Mleak_buckets.should_raise_objc_leak typ
  | _ -> None

let get_retain_cycle_dotty _prop cycle =
  match _prop with
  | None -> None
  | Some (Some _prop) ->
      Dotty.dotty_prop_to_str _prop cycle
  | _ -> None

let get_var_retain_cycle prop_ =
  let sigma = prop_.Prop.sigma in
  let is_pvar v h =
    match h with
    | Sil.Hpointsto (Exp.Lvar _, v', _) when Sil.equal_strexp v v' -> true
    | _ -> false in
  let is_hpred_block v h =
    match h, v with
    | Sil.Hpointsto (e, _, Exp.Sizeof {typ}), Sil.Eexp (e', _)
      when Exp.equal e e' && Typ.is_block_type typ -> true
    | _, _ -> false in
  let find v =
    List.find ~f:(is_pvar v) sigma |>
    Option.map ~f:Sil.hpred_get_lhs in
  let find_block v =
    if (List.exists ~f:(is_hpred_block v) sigma) then
      Some (Exp.Lvar Sil.block_pvar)
    else None in
  let sexp e = Sil.Eexp (e, Sil.Inone) in
  let find_or_block ((e, t), f, e') =
    match find e with
    | Some pvar -> [((sexp pvar, t), f, e')]
    | _ -> (match find_block e with
        | Some blk -> [((sexp blk, t), f, e')]
        | _ ->
            let sizeof = {Exp.typ=t; nbytes=None; dynamic_length=None; subtype=Subtype.exact} in
            [((sexp (Exp.Sizeof sizeof), t), f, e')]) in
  (* returns the pvars of the first cycle we find in sigma.
     This is an heuristic that works if there is one cycle.
     In case there are more than one cycle we may return not necessarily
     the one we are looking for. *)
  let rec do_sigma sigma_todo =
    match sigma_todo with
    | [] -> []
    | hp:: sigma' ->
        let cycle = get_cycle hp prop_ in
        L.d_strln "Filtering pvar in cycle ";
        let cycle' = List.concat_map ~f:find_or_block cycle in
        if List.is_empty cycle' then do_sigma sigma'
        else cycle' in
  do_sigma sigma

let remove_opt _prop =
  match _prop with
  | Some (Some p) -> p
  | _ -> Prop.prop_emp

(** Checks if cycle has fields (derived from a property or directly defined as ivar) with attributes
    weak/unsafe_unretained/assing *)
let cycle_has_weak_or_unretained_or_assign_field tenv cycle =
  (* returns items annotation for field fn in struct t *)
  let get_item_annotation (t: Typ.t) fn =
    match t.desc with
    | Tstruct name -> (
        let equal_fn (fn', _, _) = Fieldname.equal fn fn' in
        match Tenv.lookup tenv name with
        | Some { fields; statics } -> (
            List.find ~f:equal_fn (fields @ statics) |>
            Option.value_map ~f:trd3 ~default:[]
          )
        | None -> []
      )
    | _ -> [] in
  let rec has_weak_or_unretained_or_assign params =
    match params with
    | [] -> false
    | att:: _ when String.equal Config.unsafe_unret att ||
                   String.equal Config.weak att ||
                   String.equal Config.assign att -> true
    | _:: params' -> has_weak_or_unretained_or_assign params' in
  let do_annotation ((a: Annot.t), _) =
    ((String.equal a.class_name Config.property_attributes) ||
     (String.equal a.class_name Config.ivar_attributes))
    && has_weak_or_unretained_or_assign a.parameters in
  let rec do_cycle c =
    match c with
    | [] -> false
    | ((_, t), fn, _):: c' ->
        let ia = get_item_annotation t fn in
        if (List.exists ~f:do_annotation ia) then true
        else do_cycle c' in
  do_cycle cycle

let check_observer_is_unsubscribed_deallocation tenv prop e =
  let pvar_opt = match Attribute.get_resource tenv prop e with
    | Some (Apred (Aresource ({ ra_vpath =  Some (Dpvar pvar) }), _)) -> Some pvar
    | _ -> None in
  let loc = State.get_loc () in
  match Attribute.get_observer tenv prop e with
  | Some (Apred (Aobserver, _)) ->
      (match pvar_opt with
       |  Some pvar when Config.nsnotification_center_checker_backend ->
           L.d_strln (" ERROR: Object " ^ (Pvar.to_string pvar) ^
                      " is being deallocated while still registered in a notification center");
           let desc = Localise.desc_registered_observer_being_deallocated pvar loc in
           raise (Exceptions.Registered_observer_being_deallocated (desc, __POS__))
       | _ -> ())
  | _ -> ()

let check_junk ?original_prop pname tenv prop =
  let fav_sub_sigmafp = Sil.fav_new () in
  Sil.sub_fav_add fav_sub_sigmafp prop.Prop.sub;
  Prop.sigma_fav_add fav_sub_sigmafp prop.Prop.sigma_fp;
  let leaks_reported = ref [] in

  let remove_junk_once fp_part fav_root sigma =
    let id_considered_reachable = (* reachability function *)
      let reach_set = sigma_reachable fav_root sigma in
      fun id -> Ident.IdentSet.mem id reach_set in
    let should_remove_hpred entries =
      let predicate = function
        | Exp.Var id ->
            (Ident.is_primed id || Ident.is_footprint id)
            && not (Sil.fav_mem fav_root id) && not (id_considered_reachable id)
        | _ -> false in
      List.for_all ~f:predicate entries in
    let hpred_in_cycle hpred = (* check if the predicate belongs to a cycle in the heap *)
      let id_in_cycle id =
        let set1 = sigma_reachable (Sil.fav_from_list [id]) sigma in
        let set2 = Ident.IdentSet.remove id set1 in
        let fav2 = Sil.fav_from_list (Ident.IdentSet.elements set2) in
        let set3 = sigma_reachable fav2 sigma in
        Ident.IdentSet.mem id set3 in
      let entries = hpred_entries hpred in
      let predicate = function
        | Exp.Var id -> id_in_cycle id
        | _ -> false in
      let hpred_is_loop = match hpred with (* true if hpred has a self loop, ie one field points to id *)
        | Sil.Hpointsto (Exp.Var id, se, _) ->
            let fav = Sil.fav_new () in
            Sil.strexp_fav_add fav se;
            Sil.fav_mem fav id
        | _ -> false in
      hpred_is_loop || List.exists ~f:predicate entries in
    let rec remove_junk_recursive sigma_done sigma_todo =
      match sigma_todo with
      | [] -> List.rev sigma_done
      | hpred :: sigma_todo' ->
          let entries = hpred_entries hpred in
          if should_remove_hpred entries then
            begin
              let part = if fp_part then "footprint" else "normal" in
              L.d_strln (".... Prop with garbage in " ^ part ^ " part ....");
              L.d_increase_indent 1;
              L.d_strln "PROP:";
              Prop.d_prop prop; L.d_ln ();
              L.d_strln "PREDICATE:";
              Prop.d_sigma [hpred];
              L.d_ln ();
              let alloc_attribute =
                (* find the alloc attribute of one of the roots of hpred, if it exists *)
                let res = ref None in
                let do_entry e =
                  check_observer_is_unsubscribed_deallocation tenv prop e;
                  match Attribute.get_resource tenv prop e with
                  | Some (Apred (Aresource ({ ra_kind = Racquire }) as a, _)) ->
                      L.d_str "ATTRIBUTE: "; PredSymb.d_attribute a; L.d_ln ();
                      res := Some a
                  | _ ->
                      (match Attribute.get_undef tenv prop e with
                       | Some (Apred (Aundef _ as a, _)) ->
                           res := Some a
                       | _ -> ()) in
                List.iter ~f:do_entry entries;
                !res in
              L.d_decrease_indent 1;
              let is_undefined =
                Option.value_map ~f:PredSymb.is_undef ~default:false alloc_attribute in
              let resource = match Errdesc.hpred_is_open_resource tenv prop hpred with
                | Some res -> res
                | None -> PredSymb.Rmemory PredSymb.Mmalloc in
              let ml_bucket_opt =
                match resource with
                | PredSymb.Rmemory PredSymb.Mobjc -> should_raise_objc_leak hpred
                | PredSymb.Rmemory PredSymb.Mnew | PredSymb.Rmemory PredSymb.Mnew_array
                  when Config.curr_language_is Config.Clang ->
                    Mleak_buckets.should_raise_cpp_leak
                | _ -> None in
              let exn_retain_cycle cycle =
                let cycle_dotty = get_retain_cycle_dotty original_prop cycle in
                let desc = Errdesc.explain_retain_cycle cycle (State.get_loc ()) cycle_dotty in
                Exceptions.Retain_cycle (hpred, desc, __POS__) in
              let exn_leak = Exceptions.Leak
                  (fp_part, hpred,
                   Errdesc.explain_leak tenv hpred prop alloc_attribute ml_bucket_opt,
                   !Absarray.array_abstraction_performed,
                   resource,
                   __POS__) in
              let ignore_resource, exn =
                (match alloc_attribute, resource with
                 | Some _, Rmemory Mobjc when (hpred_in_cycle hpred) ->
                     (* When there is a cycle in objc we ignore it
                        only if it's empty or it has weak or unsafe_unretained fields.
                        Otherwise we report a retain cycle. *)
                     let cycle = get_var_retain_cycle (remove_opt original_prop) in
                     let ignore_cycle =
                       (Int.equal (List.length cycle) 0) ||
                       (cycle_has_weak_or_unretained_or_assign_field tenv cycle) in
                     ignore_cycle, exn_retain_cycle cycle
                 | Some _, Rmemory Mobjc
                 | Some _, Rmemory Mnew
                 | Some _, Rmemory Mnew_array when Config.curr_language_is Config.Clang ->
                     is_none ml_bucket_opt, exn_leak
                 | Some _, Rmemory _ -> Config.curr_language_is Config.Java, exn_leak
                 | Some _, Rignore -> true, exn_leak
                 | Some _, Rfile -> false, exn_leak
                 | Some _, Rlock -> false, exn_leak
                 | _ when hpred_in_cycle hpred && Sil.has_objc_ref_counter tenv hpred ->
                     (* When it's a cycle and the object has a ref counter then
                        we have a retain cycle. Objc object may not have the
                        Mobjc qualifier when added in footprint doing abduction *)
                     let cycle = get_var_retain_cycle (remove_opt original_prop) in
                     Int.equal (List.length cycle) 0, exn_retain_cycle cycle
                 | _ -> Config.curr_language_is Config.Java, exn_leak) in
              let already_reported () =
                let attr_opt_equal ao1 ao2 = match ao1, ao2 with
                  | None, None -> true
                  | Some a1, Some a2 -> PredSymb.equal a1 a2
                  | Some _, None
                  | None, Some _ -> false in
                (is_none alloc_attribute && !leaks_reported <> []) ||
                (* None attribute only reported if it's the first one *)
                List.mem ~equal:attr_opt_equal !leaks_reported alloc_attribute in
              let ignore_leak =
                !Config.allow_leak || ignore_resource || is_undefined || already_reported () in
              let report_and_continue =
                Config.curr_language_is Config.Java || !Config.footprint in
              let report_leak () =
                if not report_and_continue then raise exn
                else
                  begin
                    Reporting.log_error pname exn;
                    leaks_reported := alloc_attribute :: !leaks_reported;
                  end in
              if not ignore_leak then report_leak ();
              remove_junk_recursive sigma_done sigma_todo'
            end
          else
            remove_junk_recursive (hpred :: sigma_done) sigma_todo' in
    remove_junk_recursive [] sigma in
  let rec remove_junk fp_part fav_root sigma = (* call remove_junk_once until sigma stops shrinking *)
    let sigma' = remove_junk_once fp_part fav_root sigma in
    if Int.equal (List.length sigma') (List.length sigma) then sigma'
    else remove_junk fp_part fav_root sigma' in
  let sigma_new = remove_junk false fav_sub_sigmafp prop.Prop.sigma in
  let sigma_fp_new = remove_junk true (Sil.fav_new ()) prop.Prop.sigma_fp in
  if
    Prop.equal_sigma prop.Prop.sigma sigma_new
    && Prop.equal_sigma prop.Prop.sigma_fp sigma_fp_new
  then prop
  else Prop.normalize tenv (Prop.set prop ~sigma:sigma_new ~sigma_fp:sigma_fp_new)

(** Check whether the prop contains junk.
    If it does, and [Config.allowleak] is true, remove the junk, otherwise raise a Leak exception. *)
let abstract_junk ?original_prop pname tenv prop =
  Absarray.array_abstraction_performed := false;
  check_junk ~original_prop pname tenv prop

(** Remove redundant elements in an array, and check for junk afterwards *)
let remove_redundant_array_elements pname tenv prop =
  Absarray.array_abstraction_performed := false;
  let prop' = Absarray.remove_redundant_elements tenv prop in
  check_junk ~original_prop: (Some(prop)) pname tenv prop'

let abstract_prop pname tenv ~(rename_primed: bool) ~(from_abstract_footprint: bool) p =
  Absarray.array_abstraction_performed := false;
  let pure_abs_p = abstract_pure_part tenv ~from_abstract_footprint: true p in
  let array_abs_p =
    if from_abstract_footprint
    then pure_abs_p
    else
      abstract_pure_part tenv ~from_abstract_footprint (Absarray.abstract_array_check tenv pure_abs_p) in
  let abs_p = abs_rules_apply tenv array_abs_p in
  let abs_p = abstract_gc tenv abs_p in (* abstraction might enable more gc *)
  let abs_p = check_junk ~original_prop: (Some(p)) pname tenv abs_p in
  let ren_abs_p =
    if rename_primed
    then Prop.prop_rename_primed_footprint_vars tenv abs_p
    else abs_p in
  ren_abs_p

let get_local_stack cur_sigma init_sigma =
  let filter_stack = function
    | Sil.Hpointsto (Exp.Lvar _, _, _) -> true
    | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> false in
  let get_stack_var = function
    | Sil.Hpointsto (Exp.Lvar pvar, _, _) -> pvar
    | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> assert false in
  let filter_local_stack olds = function
    | Sil.Hpointsto (Exp.Lvar pvar, _, _) -> not (List.exists ~f:(Pvar.equal pvar) olds)
    | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> false in
  let init_stack = List.filter ~f:filter_stack init_sigma in
  let init_stack_pvars = List.map ~f:get_stack_var init_stack in
  let cur_local_stack = List.filter ~f:(filter_local_stack init_stack_pvars) cur_sigma in
  let cur_local_stack_pvars = List.map ~f:get_stack_var cur_local_stack in
  (cur_local_stack, cur_local_stack_pvars)

(** Extract the footprint, add a local stack and return it as a prop *)
let extract_footprint_for_abs (p : 'a Prop.t) : Prop.exposed Prop.t * Pvar.t list =
  let sigma = p.Prop.sigma in
  let pi_fp = p.Prop.pi_fp in
  let sigma_fp = p.Prop.sigma_fp in
  let (local_stack, local_stack_pvars) = get_local_stack sigma sigma_fp in
  let p0 = Prop.from_sigma (local_stack @ sigma_fp) in
  let p1 = Prop.set p0 ~pi:pi_fp in
  (p1, local_stack_pvars)

let remove_local_stack sigma pvars =
  let filter_non_stack = function
    | Sil.Hpointsto (Exp.Lvar pvar, _, _) -> not (List.exists ~f:(Pvar.equal pvar) pvars)
    | Sil.Hpointsto _ | Sil.Hlseg _ | Sil.Hdllseg _ -> true in
  List.filter ~f:filter_non_stack sigma

(** [prop_set_fooprint p p_foot] removes a local stack from [p_foot],
    and sets proposition [p_foot] as footprint of [p]. *)
let set_footprint_for_abs (p : 'a Prop.t) (p_foot : 'a Prop.t) local_stack_pvars : Prop.exposed Prop.t =
  let p_foot_pure = Prop.get_pure p_foot in
  let p_sigma_fp = p_foot.Prop.sigma in
  let pi = p_foot_pure in
  let sigma = remove_local_stack p_sigma_fp local_stack_pvars in
  Prop.set p ~pi_fp:pi ~sigma_fp:sigma

(** Abstract the footprint of prop *)
let abstract_footprint pname (tenv : Tenv.t) (prop : Prop.normal Prop.t) : Prop.normal Prop.t =
  let (p, added_local_vars) = extract_footprint_for_abs prop in
  let p_abs =
    abstract_prop
      pname tenv ~rename_primed: false
      ~from_abstract_footprint: true (Prop.normalize tenv p) in
  let prop' = set_footprint_for_abs prop p_abs added_local_vars in
  Prop.normalize tenv prop'

let _abstract pname pay tenv p =
  if pay then SymOp.pay(); (* pay one symop *)
  let p' = if !Config.footprint then abstract_footprint pname tenv p else p in
  abstract_prop pname tenv ~rename_primed: true ~from_abstract_footprint: false p'

let abstract pname tenv p =
  _abstract pname true tenv p

let abstract_no_symop pname tenv p =
  _abstract pname false tenv p

let lifted_abstract pname tenv pset =
  let f p =
    if Prover.check_inconsistency tenv p then None
    else Some (abstract pname tenv p) in
  let abstracted_pset = Propset.map_option tenv f pset in
  abstracted_pset

(***************** End of Main Abstraction Functions *****************)
