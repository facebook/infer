(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Interprocedural footprint analysis *)

module L = Logging

type splitting =
  { sub: Predicates.subst
  ; frame: Predicates.hpred list
  ; missing_pi: Predicates.atom list
  ; missing_sigma: Predicates.hpred list
  ; frame_fld: Predicates.hpred list
  ; missing_fld: Predicates.hpred list
  ; frame_typ: (Exp.t * Exp.t) list
  ; missing_typ: (Exp.t * Exp.t) list }

type deref_error =
  | Deref_minusone  (** dereference -1 *)
  | Deref_null of PredSymb.path_pos  (** dereference null *)
  | Deref_undef of Procname.t * Location.t * PredSymb.path_pos
      (** dereference a value coming from the given undefined function *)
  | Deref_undef_exp  (** dereference an undefined expression *)

type invalid_res =
  | Dereference_error of deref_error * Localise.error_desc * Paths.Path.t option
      (** dereference error and description *)
  | Prover_checks of Prover.check list  (** the abduction prover failed some checks *)
  | Cannot_combine  (** cannot combine actual pre with splitting and post *)
  | Missing_fld_not_empty  (** missing_fld not empty in re-execution mode *)
  | Missing_sigma_not_empty  (** missing sigma not empty in re-execution mode *)

type valid_res =
  { incons_pre_missing: bool  (** whether the actual pre is consistent with the missing part *)
  ; vr_pi: Predicates.atom list  (** missing pi *)
  ; vr_sigma: Predicates.hpred list  (** missing sigma *)
  ; vr_cons_res: (Prop.normal Prop.t * Paths.Path.t) list  (** consistent result props *)
  ; vr_incons_res: (Prop.normal Prop.t * Paths.Path.t) list  (** inconsistent result props *) }

(** Result of (bi)-abduction on a single spec. A result is invalid if no splitting was found, or if
    combine failed, or if we are in re - execution mode and the sigma part of the splitting is not
    empty. A valid result contains the missing pi ans sigma, as well as the resulting props. *)
type abduction_res =
  | Valid_res of valid_res  (** valid result for a function cal *)
  | Invalid_res of invalid_res  (** reason for invalid result *)

let print_results tenv actual_pre results =
  L.d_strln "***** RESULTS FUNCTION CALL *******" ;
  Propset.d actual_pre (Propset.from_proplist tenv results) ;
  L.d_strln "***** END RESULTS FUNCTION CALL *******"


(** Rename the variables in the spec. *)
let spec_rename_vars pname spec =
  let prop_add_callee_suffix p =
    let f = function Exp.Lvar pv -> Exp.Lvar (Pvar.to_callee pname pv) | e -> e in
    Prop.prop_expmap f p
  in
  let jprop_add_callee_suffix = function
    | BiabductionSummary.Jprop.Prop (n, p) ->
        BiabductionSummary.Jprop.Prop (n, prop_add_callee_suffix p)
    | BiabductionSummary.Jprop.Joined (n, p, jp1, jp2) ->
        BiabductionSummary.Jprop.Joined (n, prop_add_callee_suffix p, jp1, jp2)
  in
  let fav =
    let fav =
      BiabductionSummary.Jprop.free_vars spec.BiabductionSummary.pre |> Ident.hashqueue_of_sequence
    in
    List.fold_left spec.BiabductionSummary.posts ~init:fav ~f:(fun fav (p, _) ->
        Prop.free_vars p |> Ident.hashqueue_of_sequence ~init:fav )
  in
  let ids = Ident.HashQueue.keys fav in
  let ids' = List.map ~f:(fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Predicates.subst_of_list (List.map ~f:(fun (i, i') -> (i, Exp.Var i')) ids') in
  let pre' = BiabductionSummary.Jprop.jprop_sub ren_sub spec.BiabductionSummary.pre in
  let posts' =
    List.map ~f:(fun (p, path) -> (Prop.prop_sub ren_sub p, path)) spec.BiabductionSummary.posts
  in
  let pre'' = jprop_add_callee_suffix pre' in
  let posts'' = List.map ~f:(fun (p, path) -> (prop_add_callee_suffix p, path)) posts' in
  BiabductionSummary.{pre= pre''; posts= posts''; visited= spec.BiabductionSummary.visited}


(** Find and number the specs for [proc_attrs], after renaming their vars, and also return the
    parameters *)
let spec_find_rename proc_attrs specs :
    (int * Prop.exposed BiabductionSummary.spec) list * Pvar.t list =
  let proc_name = proc_attrs.ProcAttributes.proc_name in
  try
    let count = ref 0 in
    let rename_vars spec =
      incr count ;
      (!count, spec_rename_vars proc_name spec)
    in
    if List.is_empty specs then
      raise
        (Exceptions.Precondition_not_found
           (Localise.verbatim_desc (Procname.to_string proc_name), __POS__) ) ;
    let formal_parameters =
      List.map ~f:(fun (x, _, _) -> Pvar.mk_callee x proc_name) proc_attrs.ProcAttributes.formals
    in
    (List.map ~f:rename_vars specs, formal_parameters)
  with Caml.Not_found ->
    L.d_printfln "ERROR: found no entry for procedure %a. Give up..." Procname.pp proc_name ;
    raise
      (Exceptions.Precondition_not_found
         (Localise.verbatim_desc (Procname.to_string proc_name), __POS__) )


(** Process a splitting coming straight from a call to the prover: change the instantiating
    substitution so that it returns primed vars, except for vars occurring in the missing part,
    where it returns footprint vars. *)
let process_splitting actual_pre sub1 sub2 frame missing_pi missing_sigma frame_fld missing_fld
    frame_typ missing_typ =
  let hpred_has_only_footprint_vars hpred =
    Predicates.hpred_free_vars hpred |> Sequence.for_all ~f:Ident.is_footprint
  in
  let sub = Predicates.sub_join sub1 sub2 in
  let sub1_inverse =
    let sub1_list = Predicates.sub_to_list sub1 in
    let sub1_list' = List.filter ~f:(function _, Exp.Var _ -> true | _ -> false) sub1_list in
    let sub1_inverse_list =
      List.map ~f:(function id, Exp.Var id' -> (id', Exp.Var id) | _ -> assert false) sub1_list'
    in
    Predicates.subst_of_list_duplicates sub1_inverse_list
  in
  let fav_actual_pre =
    let fav_pre = Prop.free_vars actual_pre |> Ident.hashqueue_of_sequence in
    let filter id = Int.equal (Ident.get_stamp id) (-1) in
    (* vars which represent expansions of fields *)
    Predicates.sub_range sub2
    |> List.fold_left ~init:fav_pre ~f:(fun res e ->
           Exp.free_vars e |> Sequence.filter ~f:filter |> Ident.hashqueue_of_sequence ~init:res )
  in
  let fav_missing_primed =
    let filter id = Ident.is_primed id && not (Ident.HashQueue.mem fav_actual_pre id) in
    let fav =
      Prop.sigma_sub sub missing_sigma |> Prop.sigma_free_vars |> Sequence.filter ~f:filter
      |> Ident.hashqueue_of_sequence
    in
    Prop.pi_sub sub missing_pi |> Prop.pi_free_vars |> Sequence.filter ~f:filter
    |> Ident.hashqueue_of_sequence ~init:fav
    |> Ident.HashQueue.keys
  in
  let fav_missing_fld =
    Prop.sigma_sub sub missing_fld |> Prop.sigma_free_vars |> Ident.hashqueue_of_sequence
  in
  let map_var_to_pre_var_or_fresh id =
    match Predicates.exp_sub sub1_inverse (Exp.Var id) with
    | Exp.Var id' ->
        if
          Ident.HashQueue.mem fav_actual_pre id' || Ident.is_path id'
          (* a path id represents a position in the pre *)
        then Exp.Var id'
        else Exp.Var (Ident.create_fresh Ident.kprimed)
    | _ ->
        assert false
  in
  let sub_list = Predicates.sub_to_list sub in
  let sub1 =
    let f id =
      if Ident.HashQueue.mem fav_actual_pre id then (id, Exp.Var id)
      else if Ident.is_normal id then (id, map_var_to_pre_var_or_fresh id)
      else if Ident.HashQueue.mem fav_missing_fld id then (id, Exp.Var id)
      else if Ident.is_footprint id then (id, Exp.Var id)
      else
        let dom1 = Predicates.sub_domain sub1 in
        let rng1 = Predicates.sub_range sub1 in
        let dom2 = Predicates.sub_domain sub2 in
        let rng2 = Predicates.sub_range sub2 in
        let vars_actual_pre =
          List.map ~f:(fun id -> Exp.Var id) (Ident.HashQueue.keys fav_actual_pre)
        in
        L.d_str "fav_actual_pre: " ;
        Exp.d_list vars_actual_pre ;
        L.d_ln () ;
        L.d_str "Dom(Sub1): " ;
        Exp.d_list (List.map ~f:(fun id -> Exp.Var id) dom1) ;
        L.d_ln () ;
        L.d_str "Ran(Sub1): " ;
        Exp.d_list rng1 ;
        L.d_ln () ;
        L.d_str "Dom(Sub2): " ;
        Exp.d_list (List.map ~f:(fun id -> Exp.Var id) dom2) ;
        L.d_ln () ;
        L.d_str "Ran(Sub2): " ;
        Exp.d_list rng2 ;
        L.d_ln () ;
        L.d_str "Don't know about id: " ;
        Exp.d_exp (Exp.Var id) ;
        L.d_ln () ;
        assert false
    in
    let fav_sub_list =
      List.fold_left sub_list ~init:(Ident.HashQueue.create ()) ~f:(fun fav (_, e) ->
          Exp.free_vars e |> Ident.hashqueue_of_sequence ~init:fav )
      |> Ident.HashQueue.keys
    in
    Predicates.subst_of_list (List.map ~f fav_sub_list)
  in
  let sub2_list =
    let f id = (id, Exp.Var (Ident.create_fresh Ident.kfootprint)) in
    List.map ~f fav_missing_primed
  in
  let sub_list' = List.map ~f:(fun (id, e) -> (id, Predicates.exp_sub sub1 e)) sub_list in
  let sub' = Predicates.subst_of_list (sub2_list @ sub_list') in
  (* normalize everything w.r.t sub' *)
  let norm_missing_pi = Prop.pi_sub sub' missing_pi in
  let norm_missing_sigma = Prop.sigma_sub sub' missing_sigma in
  let norm_frame_fld = Prop.sigma_sub sub' frame_fld in
  let norm_frame_typ =
    List.map ~f:(fun (e, te) -> (Predicates.exp_sub sub' e, Predicates.exp_sub sub' te)) frame_typ
  in
  let norm_missing_typ =
    List.map ~f:(fun (e, te) -> (Predicates.exp_sub sub' e, Predicates.exp_sub sub' te)) missing_typ
  in
  let norm_missing_fld =
    let sigma = Prop.sigma_sub sub' missing_fld in
    let filter hpred =
      if not (hpred_has_only_footprint_vars hpred) then (
        L.d_warning "Missing fields hpred has non-footprint vars: " ;
        Predicates.d_hpred hpred ;
        L.d_ln () ;
        false )
      else
        match hpred with
        | Predicates.Hpointsto (Exp.Var _, _, _) ->
            true
        | Predicates.Hpointsto (Exp.Lvar pvar, _, _) ->
            Pvar.is_global pvar
        | _ ->
            L.d_warning "Missing fields in complex pred: " ;
            Predicates.d_hpred hpred ;
            L.d_ln () ;
            false
    in
    List.filter ~f:filter sigma
  in
  let norm_frame = Prop.sigma_sub sub' frame in
  { sub= sub'
  ; frame= norm_frame
  ; missing_pi= norm_missing_pi
  ; missing_sigma= norm_missing_sigma
  ; frame_fld= norm_frame_fld
  ; missing_fld= norm_missing_fld
  ; frame_typ= norm_frame_typ
  ; missing_typ= norm_missing_typ }


(** Check whether an inst represents a dereference without null check, and return the line number
    and path position *)
let find_dereference_without_null_check_in_inst = function
  | Predicates.Iupdate (Some true, _, n, pos) | Predicates.Irearrange (Some true, _, n, pos) ->
      Some (n, pos)
  | _ ->
      None


(** Check whether a sexp contains a dereference without null check, and return the line number and
    path position *)
let rec find_dereference_without_null_check_in_sexp = function
  | Predicates.Eexp (_, inst) ->
      find_dereference_without_null_check_in_inst inst
  | Predicates.Estruct (fsel, inst) ->
      let res = find_dereference_without_null_check_in_inst inst in
      if is_none res then find_dereference_without_null_check_in_sexp_list (List.map ~f:snd fsel)
      else res
  | Predicates.Earray (_, esel, inst) ->
      let res = find_dereference_without_null_check_in_inst inst in
      if is_none res then find_dereference_without_null_check_in_sexp_list (List.map ~f:snd esel)
      else res


and find_dereference_without_null_check_in_sexp_list = function
  | [] ->
      None
  | se :: sel -> (
    match find_dereference_without_null_check_in_sexp se with
    | None ->
        find_dereference_without_null_check_in_sexp_list sel
    | Some x ->
        Some x )


(** Check dereferences implicit in the spec pre. In case of dereference error, return
    [Some(deref_error, description)], otherwise [None] *)
let check_dereferences caller_pname tenv callee_pname actual_pre sub spec_pre formal_params =
  let check_dereference e sexp =
    let e_sub = Predicates.exp_sub sub e in
    let desc use_buckets deref_str =
      let error_desc =
        Errdesc.explain_dereference_as_caller_expression caller_pname tenv ~use_buckets deref_str
          actual_pre spec_pre e (AnalysisState.get_node_exn ()) (AnalysisState.get_loc_exn ())
          formal_params
      in
      L.d_strln ~color:Red "found error in dereference" ;
      L.d_strln "spec_pre:" ;
      Prop.d_prop spec_pre ;
      L.d_ln () ;
      L.d_str "exp " ;
      Exp.d_exp e ;
      L.d_printfln " desc: %a" Localise.pp_error_desc error_desc ;
      error_desc
    in
    let deref_no_null_check_pos =
      if Exp.equal e_sub Exp.zero then
        match find_dereference_without_null_check_in_sexp sexp with
        | Some (_, pos) ->
            Some pos
        | None ->
            None
      else None
    in
    if Option.is_some deref_no_null_check_pos then
      (* only report a dereference null error if we know
         there was a dereference without null check *)
      match deref_no_null_check_pos with
      | Some pos ->
          Some (Deref_null pos, desc true (Localise.deref_str_null (Some callee_pname)))
      | None ->
          assert false
    else if
      (* Check if the dereferenced expr has the dangling uninitialized attribute. *)
      (* In that case it raise a dangling pointer dereference *)
      Attribute.has_dangling_uninit tenv spec_pre e
    then Some (Deref_undef_exp, desc false (Localise.deref_str_dangling (Some PredSymb.DAuninit)))
    else if Exp.equal e_sub Exp.minus_one then
      Some (Deref_minusone, desc true (Localise.deref_str_dangling None))
    else
      match Attribute.get_undef tenv actual_pre e_sub with
      | Some (Apred (Aundef (s, _, loc, pos), _)) ->
          Some (Deref_undef (s, loc, pos), desc false (Localise.deref_str_undef (s, loc)))
      | _ ->
          None
  in
  let check_hpred = function
    | Predicates.Hpointsto (lexp, se, _) ->
        check_dereference (Exp.root_of_lexp lexp) se
    | _ ->
        None
  in
  let deref_err_list =
    List.fold
      ~f:(fun deref_errs hpred ->
        match check_hpred hpred with Some reason -> reason :: deref_errs | None -> deref_errs )
      ~init:[] spec_pre.Prop.sigma
  in
  match deref_err_list with
  | [] ->
      None
  | deref_err :: _ -> (
    (* Prefer to report Deref_null over other kinds of deref errors. this
       * makes sure we report a NULL_DEREFERENCE instead of
       a less interesting PRECONDITION_NOT_MET
       * whenever possible *)
    (* TOOD (t4893533): use this trick outside of angelic mode and in other parts of the code *)
    match
      List.find ~f:(fun err -> match err with Deref_null _, _ -> true | _ -> false) deref_err_list
    with
    | Some x ->
        Some x
    | None ->
        Some deref_err )


let post_process_sigma tenv (sigma : Predicates.hpred list) loc : Predicates.hpred list =
  let map_inst inst = Predicates.inst_new_loc loc inst in
  let do_hpred (_, _, hpred) = Predicates.hpred_instmap map_inst hpred in
  (* update the location of instrumentations *)
  List.map ~f:(fun hpred -> do_hpred (Prover.expand_hpred_pointer tenv false hpred)) sigma


(** check for interprocedural path errors in the post *)
let check_path_errors_in_post {InterproceduralAnalysis.proc_desc= caller_pdesc; err_log; tenv} post
    post_path =
  let check_attr atom =
    match atom with
    | Predicates.Apred (Adiv0 path_pos, [e]) ->
        if Prover.check_zero tenv e then (
          let desc =
            Errdesc.explain_divide_by_zero tenv e (AnalysisState.get_node_exn ())
              (AnalysisState.get_loc_exn ())
          in
          let new_path, path_pos_opt =
            let current_path, _ = State.get_path () in
            if Paths.Path.contains_position post_path path_pos then (post_path, Some path_pos)
            else (current_path, None)
            (* position not found, only use the path up to the callee *)
          in
          State.set_path new_path path_pos_opt ;
          let exn = Exceptions.Divide_by_zero (desc, __POS__) in
          BiabductionReporting.log_issue_deprecated_using_state caller_pdesc err_log exn )
    | _ ->
        ()
  in
  List.iter ~f:check_attr (Attribute.get_all post)


(** Post process the instantiated post after the function call so that [x.f |-> se] becomes
    [x |-> { f: se }]. Also, update any Aresource attributes to refer to the caller *)
let post_process_post ({InterproceduralAnalysis.tenv; _} as analysis_data) callee_pname loc
    actual_pre ((post : Prop.exposed Prop.t), post_path) =
  let actual_pre_has_freed_attribute e =
    match Attribute.get_resource tenv actual_pre e with
    | Some (Apred (Aresource {ra_kind= Rrelease}, _)) ->
        true
    | _ ->
        false
  in
  let atom_update_alloc_attribute = function
    | Predicates.Apred (Aresource ra, [e])
      when not
             ( PredSymb.equal_res_act_kind ra.ra_kind PredSymb.Rrelease
             && actual_pre_has_freed_attribute e ) ->
        (* unless it was already freed before the call *)
        let vpath, _ = Errdesc.vpath_find tenv post e in
        let ra' = {ra with ra_pname= callee_pname; ra_loc= loc; ra_vpath= vpath} in
        Predicates.Apred (Aresource ra', [e])
    | a ->
        a
  in
  let prop' = Prop.set post ~sigma:(post_process_sigma tenv post.Prop.sigma loc) in
  let pi' = List.map ~f:atom_update_alloc_attribute prop'.Prop.pi in
  (* update alloc attributes to refer to the caller *)
  let post' = Prop.set prop' ~pi:pi' in
  check_path_errors_in_post analysis_data post' post_path ;
  (post', post_path)


let hpred_lhs_compare hpred1 hpred2 =
  match (hpred1, hpred2) with
  | Predicates.Hpointsto (e1, _, _), Predicates.Hpointsto (e2, _, _) ->
      Exp.compare e1 e2
  | Predicates.Hpointsto _, _ ->
      -1
  | _, Predicates.Hpointsto _ ->
      1
  | hpred1, hpred2 ->
      Predicates.compare_hpred hpred1 hpred2


(** set the inst everywhere in a sexp *)
let rec sexp_set_inst inst = function
  | Predicates.Eexp (e, _) ->
      Predicates.Eexp (e, inst)
  | Predicates.Estruct (fsel, _) ->
      Predicates.Estruct (List.map ~f:(fun (f, se) -> (f, sexp_set_inst inst se)) fsel, inst)
  | Predicates.Earray (len, esel, _) ->
      Predicates.Earray (len, List.map ~f:(fun (e, se) -> (e, sexp_set_inst inst se)) esel, inst)


let rec fsel_star_fld fsel1 fsel2 =
  match (fsel1, fsel2) with
  | [], fsel2 ->
      fsel2
  | fsel1, [] ->
      fsel1
  | (f1, se1) :: fsel1', (f2, se2) :: fsel2' -> (
    match Fieldname.compare f1 f2 with
    | 0 ->
        (f1, sexp_star_fld se1 se2) :: fsel_star_fld fsel1' fsel2'
    | n when n < 0 ->
        (f1, se1) :: fsel_star_fld fsel1' fsel2
    | _ ->
        (f2, se2) :: fsel_star_fld fsel1 fsel2' )


and array_content_star se1 se2 =
  try sexp_star_fld se1 se2 with exn when Exception.exn_not_failure exn -> se1


(* let postcondition override *)
and esel_star_fld esel1 esel2 =
  match (esel1, esel2) with
  | [], esel2 ->
      (* don't know whether element is read or written in fun call with array *)
      List.map ~f:(fun (e, se) -> (e, sexp_set_inst Predicates.Inone se)) esel2
  | esel1, [] ->
      esel1
  | (e1, se1) :: esel1', (e2, se2) :: esel2' -> (
    match Exp.compare e1 e2 with
    | 0 ->
        (e1, array_content_star se1 se2) :: esel_star_fld esel1' esel2'
    | n when n < 0 ->
        (e1, se1) :: esel_star_fld esel1' esel2
    | _ ->
        let se2' = sexp_set_inst Predicates.Inone se2 in
        (* don't know whether element is read or written in fun call with array *)
        (e2, se2') :: esel_star_fld esel1 esel2' )


and sexp_star_fld se1 se2 : Predicates.strexp =
  (* L.d_str "sexp_star_fld "; Predicates.d_sexp se1; L.d_str " "; Predicates.d_sexp se2; L.d_ln (); *)
  match (se1, se2) with
  | Predicates.Estruct (fsel1, _), Predicates.Estruct (fsel2, inst2) ->
      Predicates.Estruct (fsel_star_fld fsel1 fsel2, inst2)
  | Predicates.Earray (len1, esel1, _), Predicates.Earray (_, esel2, inst2) ->
      Predicates.Earray (len1, esel_star_fld esel1 esel2, inst2)
  | Predicates.Eexp (_, inst1), Predicates.Earray (len2, esel2, _) ->
      let esel1 = [(Exp.zero, se1)] in
      Predicates.Earray (len2, esel_star_fld esel1 esel2, inst1)
  | _ ->
      L.d_str "cannot star " ;
      Predicates.d_sexp se1 ;
      L.d_str " and " ;
      Predicates.d_sexp se2 ;
      L.d_ln () ;
      assert false


let texp_star tenv texp1 texp2 =
  let rec ftal_sub ftal1 ftal2 =
    match (ftal1, ftal2) with
    | [], _ ->
        true
    | _, [] ->
        false
    | {Struct.name= f1} :: ftal1', {Struct.name= f2} :: ftal2' -> (
      match Fieldname.compare f1 f2 with
      | n when n < 0 ->
          false
      | 0 ->
          ftal_sub ftal1' ftal2'
      | _ ->
          ftal_sub ftal1 ftal2' )
  in
  let typ_star (t1 : Typ.t) (t2 : Typ.t) =
    match (t1.desc, t2.desc) with
    | Tstruct name1, Tstruct name2 when Typ.Name.is_same_type name1 name2 -> (
      match (Tenv.lookup tenv name1, Tenv.lookup tenv name2) with
      | Some {fields= fields1}, Some {fields= fields2} when ftal_sub fields1 fields2 ->
          t2
      | _ ->
          t1 )
    | _ ->
        t1
  in
  match (texp1, texp2) with
  | Exp.Sizeof ({typ= t1; subtype= st1} as sizeof1), Exp.Sizeof {typ= t2; subtype= st2} ->
      Exp.Sizeof {sizeof1 with typ= typ_star t1 t2; subtype= Subtype.join st1 st2}
  | _ ->
      texp1


let hpred_star_fld tenv (hpred1 : Predicates.hpred) (hpred2 : Predicates.hpred) : Predicates.hpred =
  match (hpred1, hpred2) with
  | Hpointsto (e1, se1, t1), Hpointsto (_, se2, t2) ->
      (* L.d_str "hpred_star_fld t1: "; Sil.d_texp_full t1; L.d_str " t2: "; Sil.d_texp_full t2;
         L.d_str " se1: "; Sil.d_sexp se1; L.d_str " se2: "; Sil.d_sexp se2; L.d_ln (); *)
      Hpointsto (e1, sexp_star_fld se1 se2, texp_star tenv t1 t2)
  | _ ->
      assert false


(** Implementation of [*] for the field-splitting model *)
let sigma_star_fld tenv (sigma1 : Predicates.hpred list) (sigma2 : Predicates.hpred list) :
    Predicates.hpred list =
  let sigma1 = List.stable_sort ~compare:hpred_lhs_compare sigma1 in
  let sigma2 = List.stable_sort ~compare:hpred_lhs_compare sigma2 in
  (* L.out "@.@. computing %a@.STAR @.%a@.@." pp_sigma sigma1 pp_sigma sigma2; *)
  let rec star sg1 sg2 : Predicates.hpred list =
    match (sg1, sg2) with
    | [], _ ->
        []
    | sigma1, [] ->
        sigma1
    | hpred1 :: sigma1', hpred2 :: sigma2' -> (
      match hpred_lhs_compare hpred1 hpred2 with
      | 0 ->
          hpred_star_fld tenv hpred1 hpred2 :: star sigma1' sigma2'
      | n when n < 0 ->
          hpred1 :: star sigma1' sg2
      | _ ->
          star sg1 sigma2' )
  in
  try star sigma1 sigma2
  with exn when Exception.exn_not_failure exn ->
    L.d_str "cannot star " ;
    Prop.d_sigma sigma1 ;
    L.d_str " and " ;
    Prop.d_sigma sigma2 ;
    L.d_ln () ;
    raise (Exceptions.Cannot_star __POS__)


let hpred_typing_lhs_compare hpred1 (e2, _) =
  match hpred1 with Predicates.Hpointsto (e1, _, _) -> Exp.compare e1 e2 | _ -> -1


let hpred_star_typing (hpred1 : Predicates.hpred) (_, te2) : Predicates.hpred =
  match hpred1 with Hpointsto (e1, se1, _) -> Hpointsto (e1, se1, te2) | _ -> assert false


(** Implementation of [*] between predicates and typings *)
let sigma_star_typ (sigma1 : Predicates.hpred list) (typings2 : (Exp.t * Exp.t) list) :
    Predicates.hpred list =
  let typing_lhs_compare (e1, _) (e2, _) = Exp.compare e1 e2 in
  let sigma1 = List.stable_sort ~compare:hpred_lhs_compare sigma1 in
  let typings2 = List.stable_sort ~compare:typing_lhs_compare typings2 in
  let rec star sg1 typ2 : Predicates.hpred list =
    match (sg1, typ2) with
    | [], _ ->
        []
    | sigma1, [] ->
        sigma1
    | hpred1 :: sigma1', typing2 :: typings2' -> (
      match hpred_typing_lhs_compare hpred1 typing2 with
      | 0 ->
          hpred_star_typing hpred1 typing2 :: star sigma1' typings2'
      | n when n < 0 ->
          hpred1 :: star sigma1' typ2
      | _ ->
          star sg1 typings2' )
  in
  try star sigma1 typings2
  with exn when Exception.exn_not_failure exn ->
    L.d_str "cannot star " ;
    Prop.d_sigma sigma1 ;
    L.d_str " and " ;
    Prover.d_typings typings2 ;
    L.d_ln () ;
    raise (Exceptions.Cannot_star __POS__)


(** [prop_footprint_add_pi_sigma_starfld_sigma prop pi sigma missing_fld] extends the footprint of
    [prop] with [pi,sigma] and extends the fields of |-> with [missing_fld] *)
let prop_footprint_add_pi_sigma_starfld_sigma tenv (prop : 'a Prop.t) pi_new sigma_new missing_fld
    missing_typ : Prop.normal Prop.t option =
  let rec extend_sigma current_sigma new_sigma =
    match new_sigma with
    | [] ->
        Some current_sigma
    | hpred :: new_sigma' ->
        (* TODO (t4893479): make this check less angelic *)
        extend_sigma (hpred :: current_sigma) new_sigma'
  in
  let rec extend_pi current_pi new_pi =
    match new_pi with
    | [] ->
        current_pi
    | a :: new_pi' ->
        if Predicates.atom_free_vars a |> Sequence.exists ~f:(fun id -> not (Ident.is_footprint id))
        then (
          L.d_warning "dropping atom with non-footprint variable" ;
          L.d_ln () ;
          Predicates.d_atom a ;
          L.d_ln () ;
          extend_pi current_pi new_pi' )
        else extend_pi (a :: current_pi) new_pi'
  in
  let pi_fp' = extend_pi prop.Prop.pi_fp pi_new in
  match extend_sigma prop.Prop.sigma_fp sigma_new with
  | None ->
      None
  | Some sigma' ->
      let sigma_fp' = sigma_star_fld tenv sigma' missing_fld in
      let sigma_fp'' = sigma_star_typ sigma_fp' missing_typ in
      let pi' = pi_new @ prop.Prop.pi in
      Some (Prop.normalize tenv (Prop.set prop ~pi:pi' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp''))


(** [prop_copy_footprint p1 p2] copies the footprint and pure part of [p1] into [p2] *)
let prop_copy_footprint_pure tenv p1 p2 =
  let p2' = Prop.set p2 ~pi_fp:p1.Prop.pi_fp ~sigma_fp:p1.Prop.sigma_fp in
  let pi2 = p2'.Prop.pi in
  let pi2_attr, pi2_noattr = List.partition_tf ~f:Attribute.is_pred pi2 in
  let res_noattr = Prop.set p2' ~pi:(Prop.get_pure p1 @ pi2_noattr) in
  let replace_attr prop atom =
    (* call replace_atom_attribute which deals with existing attibutes *)
    (* if [atom] represents an attribute [att], add the attribure to [prop] *)
    if Attribute.is_pred atom then Attribute.add_or_replace_check_changed tenv prop atom else prop
  in
  List.fold ~f:replace_attr ~init:(Prop.normalize tenv res_noattr) pi2_attr


(** check if an expression is an exception *)
let exp_is_exn = function Exp.Exn _ -> true | _ -> false

(** check if a prop is an exception *)
let prop_is_exn pname prop =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let is_exn = function
    | Predicates.Hpointsto (e1, Predicates.Eexp (e2, _), _) when Exp.equal e1 ret_pvar ->
        exp_is_exn e2
    | _ ->
        false
  in
  List.exists ~f:is_exn prop.Prop.sigma


(** when prop is an exception, return the exception name *)
let prop_get_exn_name pname prop =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let rec search_exn e = function
    | [] ->
        None
    | Predicates.Hpointsto (e1, _, Sizeof {typ= {desc= Tstruct name}}) :: _ when Exp.equal e1 e ->
        Some name
    | _ :: tl ->
        search_exn e tl
  in
  let rec find_exn_name hpreds = function
    | [] ->
        None
    | Predicates.Hpointsto (e1, Eexp (Exp.Exn e2, _), _) :: _ when Exp.equal e1 ret_pvar ->
        search_exn e2 hpreds
    | _ :: tl ->
        find_exn_name hpreds tl
  in
  let hpreds = prop.Prop.sigma in
  find_exn_name hpreds hpreds


(** search in prop for some assignment of global errors *)
let lookup_custom_errors prop =
  let rec search_error = function
    | [] ->
        None
    | Predicates.Hpointsto (Exp.Lvar var, Eexp (Exp.Const (Const.Cstr error_str), _), _) :: _
      when Pvar.equal var Predicates.custom_error ->
        Some error_str
    | _ :: tl ->
        search_error tl
  in
  search_error prop.Prop.sigma


(** set a prop to an exception sexp *)
let prop_set_exn tenv pname prop se_exn =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let map_hpred = function
    | Predicates.Hpointsto (e, _, t) when Exp.equal e ret_pvar ->
        Predicates.Hpointsto (e, se_exn, t)
    | hpred ->
        hpred
  in
  let sigma' = List.map ~f:map_hpred prop.Prop.sigma in
  Prop.normalize tenv (Prop.set prop ~sigma:sigma')


(** Include a subtrace for a procedure call if the callee is not a model. *)
let include_subtrace callee_pname =
  match Attributes.load callee_pname with
  | Some attrs ->
      (not attrs.ProcAttributes.is_biabduction_model)
      && SourceFile.is_under_project_root attrs.ProcAttributes.loc.Location.file
  | None ->
      false


(** combine the spec's post with a splitting and actual precondition *)
let combine ({InterproceduralAnalysis.proc_desc= caller_pdesc; tenv; _} as analysis_data) ret_id
    (posts : ('a Prop.t * Paths.Path.t) list) actual_pre path_pre split callee_pname loc =
  let instantiated_post =
    let posts' =
      if !BiabductionConfig.footprint && List.is_empty posts then
        (* in case of divergence, produce a prop *)
        (* with updated footprint and inconsistent current *)
        [(Prop.set Prop.prop_emp ~pi:[Predicates.Aneq (Exp.zero, Exp.zero)], path_pre)]
      else
        List.map
          ~f:(fun (p, path_post) ->
            (p, Paths.Path.add_call (include_subtrace callee_pname) path_pre callee_pname path_post) )
          posts
    in
    List.map
      ~f:(fun (p, path) ->
        post_process_post analysis_data callee_pname loc actual_pre (Prop.prop_sub split.sub p, path) )
      posts'
  in
  L.d_increase_indent () ;
  L.d_strln "New footprint:" ;
  Prop.d_pi_sigma split.missing_pi split.missing_sigma ;
  L.d_ln () ;
  L.d_strln "Frame fld:" ;
  Prop.d_sigma split.frame_fld ;
  L.d_ln () ;
  if not (List.is_empty split.frame_typ) then (
    L.d_strln "Frame typ:" ;
    Prover.d_typings split.frame_typ ;
    L.d_ln () ) ;
  L.d_strln "Missing fld:" ;
  Prop.d_sigma split.missing_fld ;
  L.d_ln () ;
  if not (List.is_empty split.missing_typ) then (
    L.d_strln "Missing typ:" ;
    Prover.d_typings split.missing_typ ;
    L.d_ln () ) ;
  L.d_strln "Instantiated frame:" ;
  Prop.d_sigma split.frame ;
  L.d_ln () ;
  L.d_strln "Instantiated post:" ;
  Propgraph.d_proplist Prop.prop_emp (List.map ~f:fst instantiated_post) ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  let compute_result post_p =
    let post_p' =
      let post_sigma = sigma_star_fld tenv post_p.Prop.sigma split.frame_fld in
      let post_sigma' = sigma_star_typ post_sigma split.frame_typ in
      Prop.set post_p ~sigma:post_sigma'
    in
    let post_p1 =
      Prop.prop_sigma_star (prop_copy_footprint_pure tenv actual_pre post_p') split.frame
    in
    let handle_null_case_analysis sigma =
      let id_assigned_to_null id =
        let filter = function
          | Predicates.Aeq (Exp.Var id', Exp.Const (Const.Cint i)) ->
              Ident.equal id id' && IntLit.isnull i
          | _ ->
              false
        in
        List.exists ~f:filter split.missing_pi
      in
      let f (e, inst_opt) =
        match (e, inst_opt) with
        | Exp.Var id, Some inst when id_assigned_to_null id ->
            let inst' = Predicates.inst_set_null_case_flag inst in
            (e, Some inst')
        | _ ->
            (e, inst_opt)
      in
      Predicates.hpred_list_expmap f sigma
    in
    let post_p2 =
      let post_p1_sigma = post_p1.Prop.sigma in
      let post_p1_sigma' = handle_null_case_analysis post_p1_sigma in
      let post_p1' = Prop.set post_p1 ~sigma:post_p1_sigma' in
      Prop.normalize tenv (Prop.set post_p1' ~pi:(post_p1.Prop.pi @ split.missing_pi))
    in
    let post_p3 =
      (* replace [result|callee] with an aux variable dedicated to this proc *)
      let callee_ret_pvar =
        Exp.Lvar (Pvar.to_callee callee_pname (Pvar.get_ret_pvar callee_pname))
      in
      match Prop.prop_iter_create post_p2 with
      | None ->
          post_p2
      | Some iter -> (
          let filter = function
            | Predicates.Hpointsto (e, _, _) when Exp.equal e callee_ret_pvar ->
                Some ()
            | _ ->
                None
          in
          match Prop.prop_iter_find iter filter with
          | None ->
              post_p2
          | Some iter' -> (
            match fst (Prop.prop_iter_current tenv iter') with
            | Predicates.Hpointsto (_, Eexp (e', inst), _) when exp_is_exn e' ->
                (* resuls is an exception: set in caller *)
                let p = Prop.prop_iter_remove_curr_then_to_prop tenv iter' in
                let caller_pname = Procdesc.get_proc_name caller_pdesc in
                prop_set_exn tenv caller_pname p (Eexp (e', inst))
            | Predicates.Hpointsto (_, Eexp (e', _), _) ->
                let p = Prop.prop_iter_remove_curr_then_to_prop tenv iter' in
                Prop.conjoin_eq tenv e' (Exp.Var ret_id) p
            | Predicates.Hpointsto _ ->
                (* returning nothing or unexpected sexp, turning into nondet *)
                Prop.prop_iter_remove_curr_then_to_prop tenv iter'
            | _ ->
                assert false ) )
    in
    let post_p4 =
      if !BiabductionConfig.footprint then
        prop_footprint_add_pi_sigma_starfld_sigma tenv post_p3 split.missing_pi split.missing_sigma
          split.missing_fld split.missing_typ
      else Some post_p3
    in
    post_p4
  in
  let results_ = List.map ~f:(fun (p, path) -> (compute_result p, path)) instantiated_post in
  if List.exists ~f:(fun (x, _) -> is_none x) results_ then (* at least one combine failed *)
    None
  else
    let results =
      List.map ~f:(function Some x, path -> (x, path) | None, _ -> assert false) results_
    in
    print_results tenv actual_pre (List.map ~f:fst results) ;
    Some results


(** Construct the actual precondition: add to the current state a copy of the (callee's) formal
    parameters instantiated with the actual parameters. *)
let mk_actual_precondition tenv prop actual_params formal_params =
  let formals_actuals =
    let rec comb fpars apars =
      match (fpars, apars) with
      | f :: fpars', a :: apars' ->
          (f, a) :: comb fpars' apars'
      | [], _ ->
          if not (List.is_empty apars) then (
            let str =
              "more actual pars than formal pars in fun call ("
              ^ string_of_int (List.length actual_params)
              ^ " vs "
              ^ string_of_int (List.length formal_params)
              ^ ")"
            in
            L.d_warning str ;
            L.d_ln () ) ;
          []
      | _ :: _, [] ->
          raise (Exceptions.Wrong_argument_number __POS__)
    in
    comb formal_params actual_params
  in
  let mk_instantiation (formal_var, (actual_e, actual_t)) =
    Prop.mk_ptsto tenv (Exp.Lvar formal_var)
      (Eexp (actual_e, Predicates.inst_actual_precondition))
      (Exp.Sizeof
         {typ= actual_t; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
      )
  in
  let instantiated_formals = List.map ~f:mk_instantiation formals_actuals in
  let actual_pre = Prop.prop_sigma_star prop instantiated_formals in
  Prop.normalize tenv actual_pre


let mk_posts tenv prop callee_pname posts =
  let mk_getter_idempotent posts =
    (* if we have seen a previous call to the same function, only use specs whose return value
       is consistent with constraints on the return value of the previous call w.r.t to
       nullness. meant to eliminate false NPE warnings from the common
       "if (get() != null) get().something()" pattern *)
    let last_call_ret_non_null =
      List.exists
        ~f:(function
          | Predicates.Apred (Aretval (pname, _), [exp]) when Procname.equal callee_pname pname ->
              Prover.check_disequal tenv prop exp Exp.zero
          | _ ->
              false )
        (Attribute.get_all prop)
    in
    if last_call_ret_non_null then
      let returns_null prop =
        List.exists
          ~f:(function
            | Predicates.Hpointsto (Exp.Lvar pvar, Eexp (e, _), _) when Pvar.is_return pvar ->
                Prover.check_equal tenv (Prop.normalize tenv prop) e Exp.zero
            | _ ->
                false )
          prop.Prop.sigma
      in
      List.filter ~f:(fun (prop, _) -> not (returns_null prop)) posts
    else posts
  in
  if Config.idempotent_getters && Language.curr_language_is Java then mk_getter_idempotent posts
  else posts


(** Check if actual_pre * missing_footprint |- false *)
let inconsistent_actualpre_missing tenv actual_pre split_opt =
  match split_opt with
  | Some split ->
      let prop' = Prop.normalize tenv (Prop.prop_sigma_star actual_pre split.missing_sigma) in
      let prop'' = List.fold ~f:(Prop.prop_atom_and tenv) ~init:prop' split.missing_pi in
      Prover.check_inconsistency tenv prop''
  | None ->
      false


let class_cast_exn tenv pname_opt texp1 texp2 exp ml_loc =
  let desc =
    Errdesc.explain_class_cast_exception tenv pname_opt texp1 texp2 exp
      (AnalysisState.get_node_exn ()) (AnalysisState.get_loc_exn ())
  in
  Exceptions.Class_cast_exception (desc, ml_loc)


let create_cast_exception tenv ml_loc pname_opt texp1 texp2 exp =
  class_cast_exn tenv pname_opt texp1 texp2 exp ml_loc


let get_check_exn tenv check callee_pname loc ml_loc =
  match check with
  | Prover.Bounds_check ->
      let desc = Localise.desc_precondition_not_met (Some Localise.Pnm_bounds) callee_pname loc in
      Exceptions.Precondition_not_met (desc, ml_loc)
  | Prover.Class_cast_check (texp1, texp2, exp) ->
      class_cast_exn tenv (Some callee_pname) texp1 texp2 exp ml_loc


let check_uninitialize_dangling_deref caller_pname tenv callee_pname actual_pre sub formal_params
    props =
  List.iter
    ~f:(fun (p, _) ->
      match check_dereferences caller_pname tenv callee_pname actual_pre sub p formal_params with
      | Some (Deref_undef_exp, desc) ->
          raise (Exceptions.Dangling_pointer_dereference (true, desc, __POS__))
      | _ ->
          () )
    props


let missing_sigma_need_adding_to_tenv tenv hpreds =
  let field_is_missing struc (field, _) =
    not
      (List.exists struc.Struct.fields ~f:(fun {Struct.name= fname} -> Fieldname.equal fname field))
  in
  let missing_hpred_need_adding_to_tenv hpred =
    match hpred with
    | Predicates.Hpointsto
        (_, Estruct (missing_fields, _), Exp.Sizeof {typ= {desc= Typ.Tstruct name}}) -> (
      match Tenv.lookup tenv name with
      | Some struc ->
          List.exists ~f:(field_is_missing struc) missing_fields
      | _ ->
          false )
    | _ ->
        false
  in
  List.exists hpreds ~f:missing_hpred_need_adding_to_tenv


let add_missing_field_to_tenv ~missing_sigma exe_env caller_tenv callee_pname hpreds
    callee_attributes =
  (* if hpreds are missing_sigma, we may not need to add the fields to the tenv, so we check that first *)
  let add_fields =
    if missing_sigma then missing_sigma_need_adding_to_tenv caller_tenv hpreds else true
  in
  (* if the callee is a model, then we don't have a tenv for it *)
  if (not callee_attributes.ProcAttributes.is_biabduction_model) && add_fields then
    let callee_tenv_opt =
      try Some (Exe_env.get_proc_tenv exe_env callee_pname)
      with _ ->
        let source_file = callee_attributes.ProcAttributes.loc.Location.file in
        Exe_env.get_source_tenv exe_env source_file
    in
    match callee_tenv_opt with
    | None ->
        ()
    | Some callee_tenv ->
        let add_field_in_hpred hpred =
          match hpred with
          | Predicates.Hpointsto (_, Estruct (_, _), Exp.Sizeof {typ= {desc= Typ.Tstruct name}})
            -> (
            match Tenv.lookup callee_tenv name with
            | Some {fields} ->
                List.iter ~f:(fun field -> Tenv.add_field caller_tenv name field) fields
            | None ->
                () )
          | _ ->
              ()
        in
        List.iter ~f:add_field_in_hpred hpreds


(** Perform symbolic execution for a single spec *)
let exe_spec
    ({InterproceduralAnalysis.exe_env; proc_desc= caller_pdesc; err_log; tenv} as analysis_data)
    ret_id (n, nspecs) callee_pname loc prop path_pre (spec : Prop.exposed BiabductionSummary.spec)
    actual_params formal_params callee_summary : abduction_res =
  let caller_pname = Procdesc.get_proc_name caller_pdesc in
  let posts = mk_posts tenv prop callee_pname spec.BiabductionSummary.posts in
  let actual_pre = mk_actual_precondition tenv prop actual_params formal_params in
  let spec_pre = BiabductionSummary.Jprop.to_prop spec.BiabductionSummary.pre in
  L.d_printfln "EXECUTING SPEC %d/%d" n nspecs ;
  L.d_strln "ACTUAL PRECONDITION =" ;
  L.d_increase_indent () ;
  Prop.d_prop actual_pre ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  L.d_strln "SPEC =" ;
  L.d_increase_indent () ;
  L.d_pp BiabductionSummary.pp_spec spec ;
  L.d_decrease_indent () ;
  L.d_ln () ;
  SymOp.pay () ;
  (* pay one symop *)
  match Prover.check_implication_for_footprint analysis_data actual_pre spec_pre with
  | Prover.ImplFail checks ->
      Invalid_res (Prover_checks checks)
  | Prover.ImplOK
      ( checks
      , sub1
      , sub2
      , frame
      , missing_pi
      , missing_sigma
      , frame_fld
      , missing_fld
      , frame_typ
      , missing_typ ) -> (
      (* check if a missing_fld hpred is from a dyn language (ObjC) *)
      let hpred_missing_objc_class = function
        | Predicates.Hpointsto (_, Estruct (_, _), Exp.Sizeof {typ}) ->
            Typ.is_objc_class typ
        | _ ->
            false
      in
      let missing_fld_objc_class, missing_fld_not_objc_class =
        List.partition_tf ~f:(fun hp -> hpred_missing_objc_class hp) missing_fld
      in
      let missing_sigma_objc_class =
        List.filter ~f:(fun hp -> hpred_missing_objc_class hp) missing_sigma
      in
      if not (List.is_empty missing_fld_objc_class) then (
        L.d_strln "Objective-C missing_fld not empty: adding it to current tenv..." ;
        add_missing_field_to_tenv ~missing_sigma:false exe_env tenv callee_pname
          missing_fld_objc_class callee_summary ) ;
      if not (List.is_empty missing_sigma_objc_class) then (
        L.d_strln "Objective-C missing_sigma not empty: adding it to current tenv..." ;
        add_missing_field_to_tenv ~missing_sigma:true exe_env tenv callee_pname
          missing_sigma_objc_class callee_summary ) ;
      let log_check_exn check =
        let exn = get_check_exn tenv check callee_pname loc __POS__ in
        BiabductionReporting.log_issue_deprecated_using_state caller_pdesc err_log exn
      in
      let do_split () =
        process_splitting actual_pre sub1 sub2 frame missing_pi missing_sigma frame_fld missing_fld
          frame_typ missing_typ
      in
      let report_valid_res split =
        match combine analysis_data ret_id posts actual_pre path_pre split callee_pname loc with
        | None ->
            Invalid_res Cannot_combine
        | Some results ->
            (* After combining we check that we have not added
               a points-to of initialized variables.*)
            check_uninitialize_dangling_deref caller_pname tenv callee_pname actual_pre split.sub
              formal_params results ;
            let inconsistent_results, consistent_results =
              List.partition_tf ~f:(fun (p, _) -> Prover.check_inconsistency tenv p) results
            in
            let incons_pre_missing = inconsistent_actualpre_missing tenv actual_pre (Some split) in
            Valid_res
              { incons_pre_missing
              ; vr_pi= split.missing_pi
              ; vr_sigma= split.missing_sigma
              ; vr_cons_res= consistent_results
              ; vr_incons_res= inconsistent_results }
      in
      List.iter ~f:log_check_exn checks ;
      let subbed_pre = Prop.prop_sub sub1 actual_pre in
      match
        check_dereferences caller_pname tenv callee_pname subbed_pre sub2 spec_pre formal_params
      with
      | Some (Deref_undef _, _) ->
          let split = do_split () in
          report_valid_res split
      | Some (deref_error, desc) ->
          let rec join_paths = function
            | [] ->
                None
            | (_, p) :: l -> (
              match join_paths l with None -> Some p | Some p' -> Some (Paths.Path.join p p') )
          in
          let pjoin = join_paths posts in
          (* join the paths from the posts *)
          Invalid_res (Dereference_error (deref_error, desc, pjoin))
      | None ->
          let split = do_split () in
          if (not !BiabductionConfig.footprint) && not (List.is_empty split.missing_sigma) then (
            L.d_strln "Implication error: missing_sigma not empty in re-execution" ;
            Invalid_res Missing_sigma_not_empty )
          else if
            (not !BiabductionConfig.footprint) && not (List.is_empty missing_fld_not_objc_class)
          then (
            L.d_strln "Implication error: missing_fld not empty in re-execution" ;
            Invalid_res Missing_fld_not_empty )
          else report_valid_res split )


let remove_constant_string_class tenv prop =
  let filter = function
    | Predicates.Hpointsto (Const (Cstr _ | Cclass _), _, _) ->
        false
    | _ ->
        true
  in
  let sigma = List.filter ~f:filter prop.Prop.sigma in
  let sigmafp = List.filter ~f:filter prop.Prop.sigma_fp in
  let prop' = Prop.set prop ~sigma ~sigma_fp:sigmafp in
  Prop.normalize tenv prop'


(** existentially quantify the path identifier generated by the prover to keep track of expansions
    of lhs paths and remove pointsto's whose lhs is a constant string *)
let quantify_path_idents_remove_constant_strings tenv (prop : Prop.normal Prop.t) :
    Prop.normal Prop.t =
  let ids_queue =
    Prop.free_vars prop |> Sequence.filter ~f:Ident.is_path |> Ident.hashqueue_of_sequence
  in
  let ids_list = Ident.HashQueue.keys ids_queue in
  remove_constant_string_class tenv (Prop.exist_quantify tenv ~ids_queue ids_list prop)


(** Strengthen the footprint by adding pure facts from the current part *)
let prop_pure_to_footprint tenv (p : 'a Prop.t) : Prop.normal Prop.t =
  let is_footprint_atom_not_attribute a =
    (not (Attribute.is_pred a))
    && Predicates.atom_free_vars a |> Sequence.for_all ~f:Ident.is_footprint
  in
  let pure = Prop.get_pure p in
  let new_footprint_atoms = List.filter ~f:is_footprint_atom_not_attribute pure in
  if List.is_empty new_footprint_atoms then p
  else
    (* add pure fact to footprint *)
    let filtered_pi_fp =
      List.filter (p.Prop.pi_fp @ new_footprint_atoms) ~f:(fun a ->
          not (Predicates.atom_has_local_addr a) )
    in
    Prop.normalize tenv (Prop.set p ~pi_fp:filtered_pi_fp)


(** post-process the raw result of a function call *)
let exe_call_postprocess tenv ret_id callee_pname callee_attrs loc results =
  let filter_valid_res = function Invalid_res _ -> false | Valid_res _ -> true in
  let valid_res0, invalid_res0 = List.partition_tf ~f:filter_valid_res results in
  let valid_res =
    List.map ~f:(function Valid_res cr -> cr | Invalid_res _ -> assert false) valid_res0
  in
  let invalid_res =
    List.map ~f:(function Valid_res _ -> assert false | Invalid_res ir -> ir) invalid_res0
  in
  let valid_res_miss_pi, valid_res_no_miss_pi =
    List.partition_tf ~f:(fun vr -> not (List.is_empty vr.vr_pi)) valid_res
  in
  let _, valid_res_cons_pre_missing =
    List.partition_tf ~f:(fun vr -> vr.incons_pre_missing) valid_res
  in
  let deref_errors =
    List.filter ~f:(function Dereference_error _ -> true | _ -> false) invalid_res
  in
  let print_pi pi =
    L.d_str "pi: " ;
    Prop.d_pi pi ;
    L.d_ln ()
  in
  let call_desc kind_opt = Localise.desc_precondition_not_met kind_opt callee_pname loc in
  let res_with_path_idents =
    if !BiabductionConfig.footprint then
      if List.is_empty valid_res_cons_pre_missing then
        (* no valid results where actual pre and missing are consistent *)
        match deref_errors with
        | error :: _ -> (
            (* dereference error detected *)
            let extend_path path_opt path_pos_opt =
              match path_opt with
              | None ->
                  ()
              | Some path_post ->
                  let old_path, _ = State.get_path () in
                  let new_path =
                    Paths.Path.add_call (include_subtrace callee_pname) old_path callee_pname
                      path_post
                  in
                  State.set_path new_path path_pos_opt
            in
            match error with
            | Dereference_error (Deref_minusone, desc, path_opt) ->
                extend_path path_opt None ;
                raise (Exceptions.Dangling_pointer_dereference (true, desc, __POS__))
            | Dereference_error (Deref_undef_exp, desc, path_opt) ->
                extend_path path_opt None ;
                raise (Exceptions.Dangling_pointer_dereference (true, desc, __POS__))
            | Dereference_error (Deref_null pos, desc, path_opt) ->
                extend_path path_opt (Some pos) ;
                if Localise.is_empty_vector_access_desc desc then
                  raise (Exceptions.Empty_vector_access (desc, __POS__))
                else raise (Exceptions.Null_dereference (desc, __POS__))
            | Dereference_error (Deref_undef _, _, _)
            | Prover_checks _
            | Cannot_combine
            | Missing_sigma_not_empty
            | Missing_fld_not_empty ->
                assert false )
        | [] ->
            (* no dereference error detected *)
            let desc =
              if List.exists ~f:(function Cannot_combine -> true | _ -> false) invalid_res then
                call_desc (Some Localise.Pnm_dangling)
              else if
                List.exists
                  ~f:(function
                    | Prover_checks (check :: _) ->
                        let exn = get_check_exn tenv check callee_pname loc __POS__ in
                        raise exn
                    | _ ->
                        false )
                  invalid_res
              then call_desc (Some Localise.Pnm_bounds)
              else call_desc None
            in
            raise (Exceptions.Precondition_not_met (desc, __POS__))
      else
        (* combine the valid results, and store diverging states *)
        let process_valid_res vr =
          let save_diverging_states () =
            if (not vr.incons_pre_missing) && List.is_empty vr.vr_cons_res then
              (* no consistent results on one spec: divergence *)
              let incons_res =
                List.map
                  ~f:(fun (p, path) -> (prop_pure_to_footprint tenv p, path))
                  vr.vr_incons_res
              in
              State.add_diverging_states (Paths.PathSet.from_renamed_list incons_res)
          in
          save_diverging_states () ;
          vr.vr_cons_res
        in
        List.map
          ~f:(fun (p, path) -> (prop_pure_to_footprint tenv p, path))
          (List.concat_map ~f:process_valid_res valid_res)
    else if not (List.is_empty valid_res_no_miss_pi) then
      List.concat_map ~f:(fun vr -> vr.vr_cons_res) valid_res_no_miss_pi
    else if List.is_empty valid_res_miss_pi then
      raise (Exceptions.Precondition_not_met (call_desc None, __POS__))
    else (
      L.d_strln "Missing pure facts for the function call:" ;
      List.iter ~f:print_pi (List.map ~f:(fun vr -> vr.vr_pi) valid_res_miss_pi) ;
      match
        Prover.find_minimum_pure_cover tenv
          (List.map ~f:(fun vr -> (vr.vr_pi, vr.vr_cons_res)) valid_res_miss_pi)
      with
      | None ->
          raise (Exceptions.Precondition_not_met (call_desc None, __POS__))
      | Some cover ->
          L.d_strln "Found minimum cover" ;
          List.iter ~f:print_pi (List.map ~f:fst cover) ;
          List.concat_map ~f:snd cover )
  in
  let res =
    List.map
      ~f:(fun (p, path) -> (quantify_path_idents_remove_constant_strings tenv p, path))
      res_with_path_idents
  in
  let ret_annots = callee_attrs.ProcAttributes.ret_annots in
  let returns_nullable ret_annots = Annotations.ia_is_nullable ret_annots in
  let should_add_ret_attr _ =
    let is_likely_getter = function
      | Procname.Java pn_java ->
          List.is_empty (Procname.Java.get_parameters pn_java)
      | _ ->
          false
    in
    (Config.idempotent_getters && Language.curr_language_is Java && is_likely_getter callee_pname)
    || returns_nullable ret_annots
  in
  if should_add_ret_attr () then
    (* add attribute to remember what function call a return id came from *)
    let ret_var = Exp.Var ret_id in
    let mark_id_as_retval (p, path) =
      let att_retval = PredSymb.Aretval (callee_pname, ret_annots) in
      (Attribute.add tenv p att_retval [ret_var], path)
    in
    List.map ~f:mark_id_as_retval res
  else res


(** Execute the function call and return the list of results with return value *)
let exe_function_call ({InterproceduralAnalysis.tenv; _} as analysis_data) ~callee_attributes
    ~callee_pname ~callee_summary ~ret_id loc ~actuals prop path =
  let spec_list, formal_params =
    spec_find_rename callee_attributes (BiabductionSummary.get_specs callee_summary)
  in
  let nspecs = List.length spec_list in
  L.d_printfln "Found %d specs for function %s" nspecs (Procname.to_unique_id callee_pname) ;
  L.d_printfln "START EXECUTING SPECS FOR %s from state" (Procname.to_unique_id callee_pname) ;
  Prop.d_prop prop ;
  L.d_ln () ;
  let exe_one_spec (n, spec) =
    exe_spec analysis_data ret_id (n, nspecs) callee_pname loc prop path spec actuals formal_params
      callee_attributes
  in
  let results = List.map ~f:exe_one_spec spec_list in
  exe_call_postprocess tenv ret_id callee_pname callee_attributes loc results
