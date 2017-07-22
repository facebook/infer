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

(** Interprocedural footprint analysis *)

module L = Logging
module F = Format

type splitting =
  { sub: Sil.subst
  ; frame: Sil.hpred list
  ; missing_pi: Sil.atom list
  ; missing_sigma: Sil.hpred list
  ; frame_fld: Sil.hpred list
  ; missing_fld: Sil.hpred list
  ; frame_typ: (Exp.t * Exp.t) list
  ; missing_typ: (Exp.t * Exp.t) list }

type deref_error =
  | Deref_freed of PredSymb.res_action  (** dereference a freed pointer *)
  | Deref_minusone  (** dereference -1 *)
  | Deref_null of PredSymb.path_pos  (** dereference null *)
  | Deref_undef of Typ.Procname.t * Location.t * PredSymb.path_pos
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
  ; vr_pi: Sil.atom list  (** missing pi *)
  ; vr_sigma: Sil.hpred list  (** missing sigma *)
  ; vr_cons_res: (Prop.normal Prop.t * Paths.Path.t) list  (** consistent result props *)
  ; vr_incons_res: (Prop.normal Prop.t * Paths.Path.t) list  (** inconsistent result props *) }

(** Result of (bi)-abduction on a single spec.
    A result is invalid if no splitting was found,
    or if combine failed, or if we are in re - execution mode and the sigma
    part of the splitting is not empty.
    A valid result contains the missing pi ans sigma, as well as the resulting props. *)
type abduction_res =
  | Valid_res of valid_res  (** valid result for a function cal *)
  | Invalid_res of invalid_res  (** reason for invalid result *)

(**************** printing functions ****************)
let d_splitting split =
  L.d_strln "Actual splitting" ;
  L.d_increase_indent 1 ;
  L.d_strln "------------------------------------------------------------" ;
  L.d_strln "SUB = " ;
  Prop.d_sub split.sub ;
  L.d_ln () ;
  L.d_strln "FRAME =" ;
  Prop.d_sigma split.frame ;
  L.d_ln () ;
  L.d_strln "MISSING =" ;
  Prop.d_pi_sigma split.missing_pi split.missing_sigma ;
  L.d_ln () ;
  L.d_strln "FRAME FLD = " ;
  Prop.d_sigma split.frame_fld ;
  L.d_ln () ;
  L.d_strln "MISSING FLD = " ;
  Prop.d_sigma split.missing_fld ;
  L.d_ln () ;
  if split.frame_typ <> [] then L.d_strln "FRAME TYP = " ;
  Prover.d_typings split.frame_typ ;
  L.d_ln () ;
  if split.missing_typ <> [] then L.d_strln "MISSING TYP = " ;
  Prover.d_typings split.missing_typ ;
  L.d_ln () ;
  L.d_strln "------------------------------------------------------------" ;
  L.d_decrease_indent 1

let print_results tenv actual_pre results =
  L.d_strln "***** RESULTS FUNCTION CALL *******" ;
  Propset.d actual_pre (Propset.from_proplist tenv results) ;
  L.d_strln "***** END RESULTS FUNCTION CALL *******"

(***************)

(** Rename the variables in the spec. *)
let spec_rename_vars pname spec =
  let prop_add_callee_suffix p =
    let f = function Exp.Lvar pv -> Exp.Lvar (Pvar.to_callee pname pv) | e -> e in
    Prop.prop_expmap f p
  in
  let jprop_add_callee_suffix = function
    | Specs.Jprop.Prop (n, p)
     -> Specs.Jprop.Prop (n, prop_add_callee_suffix p)
    | Specs.Jprop.Joined (n, p, jp1, jp2)
     -> Specs.Jprop.Joined (n, prop_add_callee_suffix p, jp1, jp2)
  in
  let fav = Sil.fav_new () in
  Specs.Jprop.fav_add fav spec.Specs.pre ;
  List.iter ~f:(fun (p, _) -> Prop.prop_fav_add fav p) spec.Specs.posts ;
  let ids = Sil.fav_to_list fav in
  let ids' = List.map ~f:(fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Sil.subst_of_list (List.map ~f:(fun (i, i') -> (i, Exp.Var i')) ids') in
  let pre' = Specs.Jprop.jprop_sub ren_sub spec.Specs.pre in
  let posts' = List.map ~f:(fun (p, path) -> (Prop.prop_sub ren_sub p, path)) spec.Specs.posts in
  let pre'' = jprop_add_callee_suffix pre' in
  let posts'' = List.map ~f:(fun (p, path) -> (prop_add_callee_suffix p, path)) posts' in
  {Specs.pre= pre''; Specs.posts= posts''; Specs.visited= spec.Specs.visited}

(** Find and number the specs for [proc_name],
    after renaming their vars, and also return the parameters *)
let spec_find_rename trace_call summary : (int * Prop.exposed Specs.spec) list * Pvar.t list =
  let proc_name = Specs.get_proc_name summary in
  try
    let count = ref 0 in
    let f spec = incr count ; (!count, spec_rename_vars proc_name spec) in
    let specs = Specs.get_specs_from_payload summary in
    let formals = Specs.get_formals summary in
    if List.is_empty specs then (
      trace_call Specs.CallStats.CR_not_found ;
      raise
        (Exceptions.Precondition_not_found
           (Localise.verbatim_desc (Typ.Procname.to_string proc_name), __POS__)) ) ;
    let formal_parameters = List.map ~f:(fun (x, _) -> Pvar.mk_callee x proc_name) formals in
    (List.map ~f specs, formal_parameters)
  with Not_found ->
    L.d_strln
      ("ERROR: found no entry for procedure " ^ Typ.Procname.to_string proc_name ^ ". Give up...") ;
    raise
      (Exceptions.Precondition_not_found
         (Localise.verbatim_desc (Typ.Procname.to_string proc_name), __POS__))

(** Process a splitting coming straight from a call to the prover:
    change the instantiating substitution so that it returns primed vars,
    except for vars occurring in the missing part, where it returns
    footprint vars. *)
let process_splitting actual_pre sub1 sub2 frame missing_pi missing_sigma frame_fld missing_fld
    frame_typ missing_typ =
  let hpred_has_only_footprint_vars hpred =
    let fav = Sil.fav_new () in
    Sil.hpred_fav_add fav hpred ; Sil.fav_for_all fav Ident.is_footprint
  in
  let sub = Sil.sub_join sub1 sub2 in
  let sub1_inverse =
    let sub1_list = Sil.sub_to_list sub1 in
    let sub1_list' = List.filter ~f:(function _, Exp.Var _ -> true | _ -> false) sub1_list in
    let sub1_inverse_list =
      List.map ~f:(function id, Exp.Var id' -> (id', Exp.Var id) | _ -> assert false) sub1_list'
    in
    Sil.exp_subst_of_list_duplicates sub1_inverse_list
  in
  let fav_actual_pre =
    let fav_sub2 =
      (* vars which represent expansions of fields *)
      let fav = Sil.fav_new () in
      List.iter ~f:(Sil.exp_fav_add fav) (Sil.sub_range sub2) ;
      let filter id = Int.equal (Ident.get_stamp id) (-1) in
      Sil.fav_filter_ident fav filter ; fav
    in
    let fav_pre = Prop.prop_fav actual_pre in
    Sil.ident_list_fav_add (Sil.fav_to_list fav_sub2) fav_pre ;
    fav_pre
  in
  let fav_missing = Prop.sigma_fav (Prop.sigma_sub (`Exp sub) missing_sigma) in
  Prop.pi_fav_add fav_missing (Prop.pi_sub (`Exp sub) missing_pi) ;
  let fav_missing_primed =
    let filter id = Ident.is_primed id && not (Sil.fav_mem fav_actual_pre id) in
    Sil.fav_copy_filter_ident fav_missing filter
  in
  let fav_missing_fld = Prop.sigma_fav (Prop.sigma_sub (`Exp sub) missing_fld) in
  let map_var_to_pre_var_or_fresh id =
    match Sil.exp_sub (`Exp sub1_inverse) (Exp.Var id) with
    | Exp.Var id'
     -> if Sil.fav_mem fav_actual_pre id' || Ident.is_path id'
           (* a path id represents a position in the pre *)
        then Exp.Var id'
        else Exp.Var (Ident.create_fresh Ident.kprimed)
    | _
     -> assert false
  in
  let sub_list = Sil.sub_to_list sub in
  let fav_sub_list =
    let fav_sub = Sil.fav_new () in
    List.iter ~f:(fun (_, e) -> Sil.exp_fav_add fav_sub e) sub_list ;
    Sil.fav_to_list fav_sub
  in
  let sub1 =
    let f id =
      if Sil.fav_mem fav_actual_pre id then (id, Exp.Var id)
      else if Ident.is_normal id then (id, map_var_to_pre_var_or_fresh id)
      else if Sil.fav_mem fav_missing_fld id then (id, Exp.Var id)
      else if Ident.is_footprint id then (id, Exp.Var id)
      else
        let dom1 = Sil.sub_domain sub1 in
        let rng1 = Sil.sub_range sub1 in
        let dom2 = Sil.sub_domain sub2 in
        let rng2 = Sil.sub_range sub2 in
        let vars_actual_pre =
          List.map ~f:(fun id -> Exp.Var id) (Sil.fav_to_list fav_actual_pre)
        in
        L.d_str "fav_actual_pre: " ;
        Sil.d_exp_list vars_actual_pre ;
        L.d_ln () ;
        L.d_str "Dom(Sub1): " ;
        Sil.d_exp_list (List.map ~f:(fun id -> Exp.Var id) dom1) ;
        L.d_ln () ;
        L.d_str "Ran(Sub1): " ;
        Sil.d_exp_list rng1 ;
        L.d_ln () ;
        L.d_str "Dom(Sub2): " ;
        Sil.d_exp_list (List.map ~f:(fun id -> Exp.Var id) dom2) ;
        L.d_ln () ;
        L.d_str "Ran(Sub2): " ;
        Sil.d_exp_list rng2 ;
        L.d_ln () ;
        L.d_str "Don't know about id: " ;
        Sil.d_exp (Exp.Var id) ;
        L.d_ln () ;
        assert false
    in
    Sil.subst_of_list (List.map ~f fav_sub_list)
  in
  let sub2_list =
    let f id = (id, Exp.Var (Ident.create_fresh Ident.kfootprint)) in
    List.map ~f (Sil.fav_to_list fav_missing_primed)
  in
  let sub_list' = List.map ~f:(fun (id, e) -> (id, Sil.exp_sub sub1 e)) sub_list in
  let sub' = Sil.subst_of_list (sub2_list @ sub_list') in
  (* normalize everything w.r.t sub' *)
  let norm_missing_pi = Prop.pi_sub sub' missing_pi in
  let norm_missing_sigma = Prop.sigma_sub sub' missing_sigma in
  let norm_frame_fld = Prop.sigma_sub sub' frame_fld in
  let norm_frame_typ =
    List.map ~f:(fun (e, te) -> (Sil.exp_sub sub' e, Sil.exp_sub sub' te)) frame_typ
  in
  let norm_missing_typ =
    List.map ~f:(fun (e, te) -> (Sil.exp_sub sub' e, Sil.exp_sub sub' te)) missing_typ
  in
  let norm_missing_fld =
    let sigma = Prop.sigma_sub sub' missing_fld in
    let filter hpred =
      if not (hpred_has_only_footprint_vars hpred) then (
        L.d_warning "Missing fields hpred has non-footprint vars: " ;
        Sil.d_hpred hpred ;
        L.d_ln () ;
        false )
      else
        match hpred with
        | Sil.Hpointsto (Exp.Var _, _, _)
         -> true
        | Sil.Hpointsto (Exp.Lvar pvar, _, _)
         -> Pvar.is_global pvar
        | _
         -> L.d_warning "Missing fields in complex pred: " ; Sil.d_hpred hpred ; L.d_ln () ; false
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

(** Check whether an inst represents a dereference without null check,
    and return the line number and path position *)
let find_dereference_without_null_check_in_inst = function
  | Sil.Iupdate (Some true, _, n, pos) | Sil.Irearrange (Some true, _, n, pos)
   -> Some (n, pos)
  | _
   -> None

(** Check whether a sexp contains a dereference without null check,
    and return the line number and path position *)
let rec find_dereference_without_null_check_in_sexp = function
  | Sil.Eexp (_, inst)
   -> find_dereference_without_null_check_in_inst inst
  | Sil.Estruct (fsel, inst)
   -> let res = find_dereference_without_null_check_in_inst inst in
      if is_none res then find_dereference_without_null_check_in_sexp_list (List.map ~f:snd fsel)
      else res
  | Sil.Earray (_, esel, inst)
   -> let res = find_dereference_without_null_check_in_inst inst in
      if is_none res then find_dereference_without_null_check_in_sexp_list (List.map ~f:snd esel)
      else res

and find_dereference_without_null_check_in_sexp_list = function
  | []
   -> None
  | se :: sel ->
    match find_dereference_without_null_check_in_sexp se with
    | None
     -> find_dereference_without_null_check_in_sexp_list sel
    | Some x
     -> Some x

(** Check dereferences implicit in the spec pre.
    In case of dereference error, return [Some(deref_error, description)], otherwise [None] *)
let check_dereferences tenv callee_pname actual_pre sub spec_pre formal_params =
  let check_dereference e sexp =
    let e_sub = Sil.exp_sub sub e in
    let desc use_buckets deref_str =
      let error_desc =
        Errdesc.explain_dereference_as_caller_expression tenv ~use_buckets deref_str actual_pre
          spec_pre e (State.get_node ()) (State.get_loc ()) formal_params
      in
      L.d_strln_color Red "found error in dereference" ;
      L.d_strln "spec_pre:" ;
      Prop.d_prop spec_pre ;
      L.d_ln () ;
      L.d_str "exp " ;
      Sil.d_exp e ;
      L.d_strln (" desc: " ^ F.asprintf "%a" Localise.pp_error_desc error_desc) ;
      error_desc
    in
    let deref_no_null_check_pos =
      if Exp.equal e_sub Exp.zero then
        match find_dereference_without_null_check_in_sexp sexp with
        | Some (_, pos)
         -> Some pos
        | None
         -> None
      else None
    in
    if deref_no_null_check_pos <> None then
      (* only report a dereference null error if we know
         there was a dereference without null check *)
      match deref_no_null_check_pos with
      | Some pos
       -> Some (Deref_null pos, desc true (Localise.deref_str_null (Some callee_pname)))
      | None
       -> assert false
    else if (* Check if the dereferenced expr has the dangling uninitialized attribute. *)
            (* In that case it raise a dangling pointer dereferece *)
            Attribute.has_dangling_uninit tenv spec_pre e
    then Some (Deref_undef_exp, desc false (Localise.deref_str_dangling (Some PredSymb.DAuninit)))
    else if Exp.equal e_sub Exp.minus_one then
      Some (Deref_minusone, desc true (Localise.deref_str_dangling None))
    else
      match Attribute.get_resource tenv actual_pre e_sub with
      | Some Apred (Aresource ({ra_kind= Rrelease} as ra), _)
       -> Some (Deref_freed ra, desc true (Localise.deref_str_freed ra))
      | _ ->
        match Attribute.get_undef tenv actual_pre e_sub with
        | Some Apred (Aundef (s, _, loc, pos), _)
         -> Some (Deref_undef (s, loc, pos), desc false (Localise.deref_str_undef (s, loc)))
        | _
         -> None
  in
  let check_hpred = function
    | Sil.Hpointsto (lexp, se, _)
     -> check_dereference (Exp.root_of_lexp lexp) se
    | _
     -> None
  in
  let deref_err_list =
    List.fold
      ~f:(fun deref_errs hpred ->
        match check_hpred hpred with Some reason -> reason :: deref_errs | None -> deref_errs)
      ~init:[] spec_pre.Prop.sigma
  in
  match deref_err_list with
  | []
   -> None
  | deref_err :: _
   -> if Config.angelic_execution then
        (* In angelic mode, prefer to report Deref_null over other kinds of deref errors. this
         * makes sure we report a NULL_DEREFERENCE instead of
           a less interesting PRECONDITION_NOT_MET
         * whenever possible *)
        (* TOOD (t4893533): use this trick outside of angelic mode and in other parts of the code *)
        match
          List.find
            ~f:(fun err -> match err with Deref_null _, _ -> true | _ -> false)
            deref_err_list
        with
        | Some x
         -> Some x
        | None
         -> Some deref_err
      else Some deref_err

let post_process_sigma tenv (sigma: Sil.hpred list) loc : Sil.hpred list =
  let map_inst inst = Sil.inst_new_loc loc inst in
  let do_hpred (_, _, hpred) = Sil.hpred_instmap map_inst hpred in
  (* update the location of instrumentations *)
  List.map ~f:(fun hpred -> do_hpred (Prover.expand_hpred_pointer tenv false hpred)) sigma

(** check for interprocedural path errors in the post *)
let check_path_errors_in_post tenv caller_pname post post_path =
  let check_attr atom =
    match atom with
    | Sil.Apred (Adiv0 path_pos, [e])
     -> if Prover.check_zero tenv e then
          let desc =
            Errdesc.explain_divide_by_zero tenv e (State.get_node ()) (State.get_loc ())
          in
          let new_path, path_pos_opt =
            let current_path, _ = State.get_path () in
            if Paths.Path.contains_position post_path path_pos then (post_path, Some path_pos)
            else (current_path, None)
            (* position not found, only use the path up to the callee *)
          in
          State.set_path new_path path_pos_opt ;
          let exn = Exceptions.Divide_by_zero (desc, __POS__) in
          Reporting.log_warning_deprecated caller_pname exn
    | _
     -> ()
  in
  List.iter ~f:check_attr (Attribute.get_all post)

(** Post process the instantiated post after the function call so that
    x.f |-> se becomes x |-> \{ f: se \}.
    Also, update any Aresource attributes to refer to the caller *)
let post_process_post tenv caller_pname callee_pname loc actual_pre
    ((post: Prop.exposed Prop.t), post_path) =
  let actual_pre_has_freed_attribute e =
    match Attribute.get_resource tenv actual_pre e with
    | Some Apred (Aresource {ra_kind= Rrelease}, _)
     -> true
    | _
     -> false
  in
  let atom_update_alloc_attribute = function
    | Sil.Apred (Aresource ra, [e])
      when not
             ( PredSymb.equal_res_act_kind ra.ra_kind PredSymb.Rrelease
             && actual_pre_has_freed_attribute e )
     -> (* unless it was already freed before the call *)
        let vpath, _ = Errdesc.vpath_find tenv post e in
        let ra' = {ra with ra_pname= callee_pname; ra_loc= loc; ra_vpath= vpath} in
        Sil.Apred (Aresource ra', [e])
    | a
     -> a
  in
  let prop' = Prop.set post ~sigma:(post_process_sigma tenv post.Prop.sigma loc) in
  let pi' = List.map ~f:atom_update_alloc_attribute prop'.Prop.pi in
  (* update alloc attributes to refer to the caller *)
  let post' = Prop.set prop' ~pi:pi' in
  check_path_errors_in_post tenv caller_pname post' post_path ; (post', post_path)

let hpred_lhs_compare hpred1 hpred2 =
  match (hpred1, hpred2) with
  | Sil.Hpointsto (e1, _, _), Sil.Hpointsto (e2, _, _)
   -> Exp.compare e1 e2
  | Sil.Hpointsto _, _
   -> -1
  | _, Sil.Hpointsto _
   -> 1
  | hpred1, hpred2
   -> Sil.compare_hpred hpred1 hpred2

(** set the inst everywhere in a sexp *)
let rec sexp_set_inst inst = function
  | Sil.Eexp (e, _)
   -> Sil.Eexp (e, inst)
  | Sil.Estruct (fsel, _)
   -> Sil.Estruct (List.map ~f:(fun (f, se) -> (f, sexp_set_inst inst se)) fsel, inst)
  | Sil.Earray (len, esel, _)
   -> Sil.Earray (len, List.map ~f:(fun (e, se) -> (e, sexp_set_inst inst se)) esel, inst)

let rec fsel_star_fld fsel1 fsel2 =
  match (fsel1, fsel2) with
  | [], fsel2
   -> fsel2
  | fsel1, []
   -> fsel1
  | (f1, se1) :: fsel1', (f2, se2) :: fsel2' ->
    match Typ.Fieldname.compare f1 f2 with
    | 0
     -> (f1, sexp_star_fld se1 se2) :: fsel_star_fld fsel1' fsel2'
    | n when n < 0
     -> (f1, se1) :: fsel_star_fld fsel1' fsel2
    | _
     -> (f2, se2) :: fsel_star_fld fsel1 fsel2'

and array_content_star se1 se2 =
  try sexp_star_fld se1 se2
  with exn when SymOp.exn_not_failure exn -> se1

(* let postcondition override *)
and esel_star_fld esel1 esel2 =
  match (esel1, esel2) with
  | [], esel2
   -> (* don't know whether element is read or written in fun call with array *)
      List.map ~f:(fun (e, se) -> (e, sexp_set_inst Sil.Inone se)) esel2
  | esel1, []
   -> esel1
  | (e1, se1) :: esel1', (e2, se2) :: esel2' ->
    match Exp.compare e1 e2 with
    | 0
     -> (e1, array_content_star se1 se2) :: esel_star_fld esel1' esel2'
    | n when n < 0
     -> (e1, se1) :: esel_star_fld esel1' esel2
    | _
     -> let se2' = sexp_set_inst Sil.Inone se2 in
        (* don't know whether element is read or written in fun call with array *)
        (e2, se2')
        :: esel_star_fld esel1 esel2'

and sexp_star_fld se1 se2 : Sil.strexp =
  (* L.d_str "sexp_star_fld "; Sil.d_sexp se1; L.d_str " "; Sil.d_sexp se2; L.d_ln (); *)
  match (se1, se2) with
  | Sil.Estruct (fsel1, _), Sil.Estruct (fsel2, inst2)
   -> Sil.Estruct (fsel_star_fld fsel1 fsel2, inst2)
  | Sil.Earray (len1, esel1, _), Sil.Earray (_, esel2, inst2)
   -> Sil.Earray (len1, esel_star_fld esel1 esel2, inst2)
  | Sil.Eexp (_, inst1), Sil.Earray (len2, esel2, _)
   -> let esel1 = [(Exp.zero, se1)] in
      Sil.Earray (len2, esel_star_fld esel1 esel2, inst1)
  | _
   -> L.d_str "cannot star " ;
      Sil.d_sexp se1 ;
      L.d_str " and " ;
      Sil.d_sexp se2 ;
      L.d_ln () ;
      assert false

let texp_star tenv texp1 texp2 =
  let rec ftal_sub ftal1 ftal2 =
    match (ftal1, ftal2) with
    | [], _
     -> true
    | _, []
     -> false
    | (f1, _, _) :: ftal1', (f2, _, _) :: ftal2' ->
      match Typ.Fieldname.compare f1 f2 with
      | n when n < 0
       -> false
      | 0
       -> ftal_sub ftal1' ftal2'
      | _
       -> ftal_sub ftal1 ftal2'
  in
  let typ_star (t1: Typ.t) (t2: Typ.t) =
    match (t1.desc, t2.desc) with
    | Tstruct name1, Tstruct name2 when Typ.Name.is_same_type name1 name2 -> (
      match (Tenv.lookup tenv name1, Tenv.lookup tenv name2) with
      | Some {fields= fields1}, Some {fields= fields2} when ftal_sub fields1 fields2
       -> t2
      | _
       -> t1 )
    | _
     -> t1
  in
  match (texp1, texp2) with
  | Exp.Sizeof ({typ= t1; subtype= st1} as sizeof1), Exp.Sizeof {typ= t2; subtype= st2}
   -> Exp.Sizeof {sizeof1 with typ= typ_star t1 t2; subtype= Subtype.join st1 st2}
  | _
   -> texp1

let hpred_star_fld tenv (hpred1: Sil.hpred) (hpred2: Sil.hpred) : Sil.hpred =
  match (hpred1, hpred2) with
  | Sil.Hpointsto (e1, se1, t1), Sil.Hpointsto (_, se2, t2)
   -> (* L.d_str "hpred_star_fld t1: "; Sil.d_texp_full t1; L.d_str " t2: "; Sil.d_texp_full t2;
         L.d_str " se1: "; Sil.d_sexp se1; L.d_str " se2: "; Sil.d_sexp se2; L.d_ln (); *)
      Sil.Hpointsto (e1, sexp_star_fld se1 se2, texp_star tenv t1 t2)
  | _
   -> assert false

(** Implementation of [*] for the field-splitting model *)
let sigma_star_fld tenv (sigma1: Sil.hpred list) (sigma2: Sil.hpred list) : Sil.hpred list =
  let sigma1 = List.stable_sort ~cmp:hpred_lhs_compare sigma1 in
  let sigma2 = List.stable_sort ~cmp:hpred_lhs_compare sigma2 in
  (* L.out "@.@. computing %a@.STAR @.%a@.@." pp_sigma sigma1 pp_sigma sigma2; *)
  let rec star sg1 sg2 : Sil.hpred list =
    match (sg1, sg2) with
    | [], _
     -> []
    | sigma1, []
     -> sigma1
    | hpred1 :: sigma1', hpred2 :: sigma2' ->
      match hpred_lhs_compare hpred1 hpred2 with
      | 0
       -> hpred_star_fld tenv hpred1 hpred2 :: star sigma1' sigma2'
      | n when n < 0
       -> hpred1 :: star sigma1' sg2
      | _
       -> star sg1 sigma2'
  in
  try star sigma1 sigma2
  with exn when SymOp.exn_not_failure exn ->
    L.d_str "cannot star " ;
    Prop.d_sigma sigma1 ;
    L.d_str " and " ;
    Prop.d_sigma sigma2 ;
    L.d_ln () ;
    raise (Exceptions.Cannot_star __POS__)

let hpred_typing_lhs_compare hpred1 (e2, _) =
  match hpred1 with Sil.Hpointsto (e1, _, _) -> Exp.compare e1 e2 | _ -> -1

let hpred_star_typing (hpred1: Sil.hpred) (_, te2) : Sil.hpred =
  match hpred1 with
  | Sil.Hpointsto (e1, se1, _)
   -> Sil.Hpointsto (e1, se1, te2)
  | _
   -> assert false

(** Implementation of [*] between predicates and typings *)
let sigma_star_typ (sigma1: Sil.hpred list) (typings2: (Exp.t * Exp.t) list) : Sil.hpred list =
  let typing_lhs_compare (e1, _) (e2, _) = Exp.compare e1 e2 in
  let sigma1 = List.stable_sort ~cmp:hpred_lhs_compare sigma1 in
  let typings2 = List.stable_sort ~cmp:typing_lhs_compare typings2 in
  let rec star sg1 typ2 : Sil.hpred list =
    match (sg1, typ2) with
    | [], _
     -> []
    | sigma1, []
     -> sigma1
    | hpred1 :: sigma1', typing2 :: typings2' ->
      match hpred_typing_lhs_compare hpred1 typing2 with
      | 0
       -> hpred_star_typing hpred1 typing2 :: star sigma1' typings2'
      | n when n < 0
       -> hpred1 :: star sigma1' typ2
      | _
       -> star sg1 typings2'
  in
  try star sigma1 typings2
  with exn when SymOp.exn_not_failure exn ->
    L.d_str "cannot star " ;
    Prop.d_sigma sigma1 ;
    L.d_str " and " ;
    Prover.d_typings typings2 ;
    L.d_ln () ;
    raise (Exceptions.Cannot_star __POS__)

(** [prop_footprint_add_pi_sigma_starfld_sigma prop pi sigma missing_fld]
    extends the footprint of [prop] with [pi,sigma]
    and extends the fields of |-> with [missing_fld] *)
let prop_footprint_add_pi_sigma_starfld_sigma tenv (prop: 'a Prop.t) pi_new sigma_new missing_fld
    missing_typ : Prop.normal Prop.t option =
  let rec extend_sigma current_sigma new_sigma =
    match new_sigma with
    | []
     -> Some current_sigma
    | hpred :: new_sigma'
     -> let fav = Prop.sigma_fav [hpred] in
        (* TODO (t4893479): make this check less angelic *)
        if Sil.fav_exists fav (fun id ->
               not (Ident.is_footprint id) && not Config.angelic_execution )
        then (
          L.d_warning "found hpred with non-footprint variable, dropping the spec" ;
          L.d_ln () ;
          Sil.d_hpred hpred ;
          L.d_ln () ;
          None )
        else extend_sigma (hpred :: current_sigma) new_sigma'
  in
  let rec extend_pi current_pi new_pi =
    match new_pi with
    | []
     -> current_pi
    | a :: new_pi'
     -> let fav = Prop.pi_fav [a] in
        if Sil.fav_exists fav (fun id -> not (Ident.is_footprint id)) then (
          L.d_warning "dropping atom with non-footprint variable" ;
          L.d_ln () ;
          Sil.d_atom a ;
          L.d_ln () ;
          extend_pi current_pi new_pi' )
        else extend_pi (a :: current_pi) new_pi'
  in
  let pi_fp' = extend_pi prop.Prop.pi_fp pi_new in
  match extend_sigma prop.Prop.sigma_fp sigma_new with
  | None
   -> None
  | Some sigma'
   -> let sigma_fp' = sigma_star_fld tenv sigma' missing_fld in
      let sigma_fp'' = sigma_star_typ sigma_fp' missing_typ in
      let pi' = pi_new @ prop.Prop.pi in
      Some (Prop.normalize tenv (Prop.set prop ~pi:pi' ~pi_fp:pi_fp' ~sigma_fp:sigma_fp''))

(** Check if the attribute change is a mismatch between a kind
    of allocation and a different kind of deallocation *)
let check_attr_dealloc_mismatch att_old att_new =
  match (att_old, att_new) with
  | ( PredSymb.Aresource ({ra_kind= Racquire; ra_res= Rmemory mk_old} as ra_old)
    , PredSymb.Aresource ({ra_kind= Rrelease; ra_res= Rmemory mk_new} as ra_new) )
    when PredSymb.compare_mem_kind mk_old mk_new <> 0
   -> let desc = Errdesc.explain_allocation_mismatch ra_old ra_new in
      raise (Exceptions.Deallocation_mismatch (desc, __POS__))
  | _
   -> ()

(** [prop_copy_footprint p1 p2] copies the footprint and pure part of [p1] into [p2] *)
let prop_copy_footprint_pure tenv p1 p2 =
  let p2' = Prop.set p2 ~pi_fp:p1.Prop.pi_fp ~sigma_fp:p1.Prop.sigma_fp in
  let pi2 = p2'.Prop.pi in
  let pi2_attr, pi2_noattr = List.partition_tf ~f:Attribute.is_pred pi2 in
  let res_noattr = Prop.set p2' ~pi:(Prop.get_pure p1 @ pi2_noattr) in
  let replace_attr prop atom =
    (* call replace_atom_attribute which deals with existing attibutes *)
    (* if [atom] represents an attribute [att], add the attribure to [prop] *)
    if Attribute.is_pred atom then
      Attribute.add_or_replace_check_changed tenv check_attr_dealloc_mismatch prop atom
    else prop
  in
  List.fold ~f:replace_attr ~init:(Prop.normalize tenv res_noattr) pi2_attr

(** check if an expression is an exception *)
let exp_is_exn = function Exp.Exn _ -> true | _ -> false

(** check if a prop is an exception *)
let prop_is_exn pname prop =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let is_exn = function
    | Sil.Hpointsto (e1, Sil.Eexp (e2, _), _) when Exp.equal e1 ret_pvar
     -> exp_is_exn e2
    | _
     -> false
  in
  List.exists ~f:is_exn prop.Prop.sigma

(** when prop is an exception, return the exception name *)
let prop_get_exn_name pname prop =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let rec search_exn e = function
    | []
     -> None
    | (Sil.Hpointsto (e1, _, Sizeof {typ= {desc= Tstruct name}})) :: _ when Exp.equal e1 e
     -> Some name
    | _ :: tl
     -> search_exn e tl
  in
  let rec find_exn_name hpreds = function
    | []
     -> None
    | (Sil.Hpointsto (e1, Sil.Eexp (Exp.Exn e2, _), _)) :: _ when Exp.equal e1 ret_pvar
     -> search_exn e2 hpreds
    | _ :: tl
     -> find_exn_name hpreds tl
  in
  let hpreds = prop.Prop.sigma in
  find_exn_name hpreds hpreds

(** search in prop for some assignment of global errors *)
let lookup_custom_errors prop =
  let rec search_error = function
    | []
     -> None
    | (Sil.Hpointsto (Exp.Lvar var, Sil.Eexp (Exp.Const Const.Cstr error_str, _), _)) :: _
      when Pvar.equal var Sil.custom_error
     -> Some error_str
    | _ :: tl
     -> search_error tl
  in
  search_error prop.Prop.sigma

(** set a prop to an exception sexp *)
let prop_set_exn tenv pname prop se_exn =
  let ret_pvar = Exp.Lvar (Pvar.get_ret_pvar pname) in
  let map_hpred = function
    | Sil.Hpointsto (e, _, t) when Exp.equal e ret_pvar
     -> Sil.Hpointsto (e, se_exn, t)
    | hpred
     -> hpred
  in
  let sigma' = List.map ~f:map_hpred prop.Prop.sigma in
  Prop.normalize tenv (Prop.set prop ~sigma:sigma')

(** Include a subtrace for a procedure call if the callee is not a model. *)
let include_subtrace callee_pname =
  match Specs.proc_resolve_attributes callee_pname with
  | Some attrs
   -> not attrs.ProcAttributes.is_model
      && SourceFile.is_under_project_root attrs.ProcAttributes.loc.Location.file
  | None
   -> false

(** combine the spec's post with a splitting and actual precondition *)
let combine tenv ret_id (posts: ('a Prop.t * Paths.Path.t) list) actual_pre path_pre split
    caller_pdesc callee_pname loc =
  let caller_pname = Procdesc.get_proc_name caller_pdesc in
  let instantiated_post =
    let posts' =
      if !Config.footprint && List.is_empty posts then
        (* in case of divergence, produce a prop *)
        (* with updated footprint and inconsistent current *)
        [(Prop.set Prop.prop_emp ~pi:[Sil.Aneq (Exp.zero, Exp.zero)], path_pre)]
      else
        List.map
          ~f:(fun (p, path_post) ->
            (p, Paths.Path.add_call (include_subtrace callee_pname) path_pre callee_pname path_post))
          posts
    in
    List.map
      ~f:(fun (p, path) ->
        post_process_post tenv caller_pname callee_pname loc actual_pre
          (Prop.prop_sub split.sub p, path))
      posts'
  in
  L.d_increase_indent 1 ;
  L.d_strln "New footprint:" ;
  Prop.d_pi_sigma split.missing_pi split.missing_sigma ;
  L.d_ln () ;
  L.d_strln "Frame fld:" ;
  Prop.d_sigma split.frame_fld ;
  L.d_ln () ;
  if split.frame_typ <> [] then (
    L.d_strln "Frame typ:" ; Prover.d_typings split.frame_typ ; L.d_ln () ) ;
  L.d_strln "Missing fld:" ;
  Prop.d_sigma split.missing_fld ;
  L.d_ln () ;
  if split.missing_typ <> [] then (
    L.d_strln "Missing typ:" ; Prover.d_typings split.missing_typ ; L.d_ln () ) ;
  L.d_strln "Instantiated frame:" ;
  Prop.d_sigma split.frame ;
  L.d_ln () ;
  L.d_strln "Instantiated post:" ;
  Propgraph.d_proplist Prop.prop_emp (List.map ~f:fst instantiated_post) ;
  L.d_decrease_indent 1 ;
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
          | Sil.Aeq (Exp.Var id', Exp.Const Const.Cint i)
           -> Ident.equal id id' && IntLit.isnull i
          | _
           -> false
        in
        List.exists ~f:filter split.missing_pi
      in
      let f (e, inst_opt) =
        match (e, inst_opt) with
        | Exp.Var id, Some inst when id_assigned_to_null id
         -> let inst' = Sil.inst_set_null_case_flag inst in
            (e, Some inst')
        | _
         -> (e, inst_opt)
      in
      Sil.hpred_list_expmap f sigma
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
      | None
       -> post_p2
      | Some iter
       -> let filter = function
            | Sil.Hpointsto (e, _, _) when Exp.equal e callee_ret_pvar
             -> Some ()
            | _
             -> None
          in
          match Prop.prop_iter_find iter filter with
          | None
           -> post_p2
          | Some iter' ->
            match (fst (Prop.prop_iter_current tenv iter'), ret_id) with
            | Sil.Hpointsto (_, Sil.Eexp (e', inst), _), _ when exp_is_exn e'
             -> (* resuls is an exception: set in caller *)
                let p = Prop.prop_iter_remove_curr_then_to_prop tenv iter' in
                prop_set_exn tenv caller_pname p (Sil.Eexp (e', inst))
            | Sil.Hpointsto (_, Sil.Eexp (e', _), _), Some (id, _)
             -> let p = Prop.prop_iter_remove_curr_then_to_prop tenv iter' in
                Prop.conjoin_eq tenv e' (Exp.Var id) p
            | Sil.Hpointsto (_, Sil.Estruct (ftl, _), _), _
              when Int.equal (List.length ftl) (if is_none ret_id then 0 else 1)
             -> (* TODO(jjb): Is this case dead? *)
                let rec do_ftl_ids p = function
                  | [], None
                   -> p
                  | (_, Sil.Eexp (e', _)) :: ftl', Some (ret_id, _)
                   -> let p' = Prop.conjoin_eq tenv e' (Exp.Var ret_id) p in
                      do_ftl_ids p' (ftl', None)
                  | _
                   -> p
                in
                let p = Prop.prop_iter_remove_curr_then_to_prop tenv iter' in
                do_ftl_ids p (ftl, ret_id)
            | Sil.Hpointsto _, _
             -> (* returning nothing or unexpected sexp, turning into nondet *)
                Prop.prop_iter_remove_curr_then_to_prop tenv iter'
            | _
             -> assert false
    in
    let post_p4 =
      if !Config.footprint then
        prop_footprint_add_pi_sigma_starfld_sigma tenv post_p3 split.missing_pi split.missing_sigma
          split.missing_fld split.missing_typ
      else Some post_p3
    in
    post_p4
  in
  let _results = List.map ~f:(fun (p, path) -> (compute_result p, path)) instantiated_post in
  if List.exists ~f:(fun (x, _) -> is_none x) _results then (* at least one combine failed *)
    None
  else
    let results =
      List.map ~f:(function Some x, path -> (x, path) | None, _ -> assert false) _results
    in
    print_results tenv actual_pre (List.map ~f:fst results) ;
    Some results

(** Construct the actual precondition: add to the current state a copy
    of the (callee's) formal parameters instantiated with the actual
    parameters. *)
let mk_actual_precondition tenv prop actual_params formal_params =
  let formals_actuals =
    let rec comb fpars apars =
      match (fpars, apars) with
      | f :: fpars', a :: apars'
       -> (f, a) :: comb fpars' apars'
      | [], _
       -> ( if apars <> [] then
              let str =
                "more actual pars than formal pars in fun call ("
                ^ string_of_int (List.length actual_params) ^ " vs "
                ^ string_of_int (List.length formal_params) ^ ")"
              in
              L.d_warning str ; L.d_ln () ) ;
          []
      | _ :: _, []
       -> raise (Exceptions.Wrong_argument_number __POS__)
    in
    comb formal_params actual_params
  in
  let mk_instantiation (formal_var, (actual_e, actual_t)) =
    Prop.mk_ptsto tenv (Exp.Lvar formal_var) (Sil.Eexp (actual_e, Sil.inst_actual_precondition))
      (Exp.Sizeof {typ= actual_t; nbytes= None; dynamic_length= None; subtype= Subtype.exact})
  in
  let instantiated_formals = List.map ~f:mk_instantiation formals_actuals in
  let actual_pre = Prop.prop_sigma_star prop instantiated_formals in
  Prop.normalize tenv actual_pre

let mk_posts tenv ret_id_opt prop callee_pname posts =
  if is_none ret_id_opt then posts
  else
    let mk_getter_idempotent posts =
      (* if we have seen a previous call to the same function, only use specs whose return value
           is consistent with constraints on the return value of the previous call w.r.t to
           nullness. meant to eliminate false NPE warnings from the common
           "if (get() != null) get().something()" pattern *)
      let last_call_ret_non_null =
        List.exists
          ~f:(function
              | Sil.Apred (Aretval (pname, _), [exp]) when Typ.Procname.equal callee_pname pname
               -> Prover.check_disequal tenv prop exp Exp.zero
              | _
               -> false)
          (Attribute.get_all prop)
      in
      if last_call_ret_non_null then
        let returns_null prop =
          List.exists
            ~f:(function
                | Sil.Hpointsto (Exp.Lvar pvar, Sil.Eexp (e, _), _) when Pvar.is_return pvar
                 -> Prover.check_equal tenv (Prop.normalize tenv prop) e Exp.zero
                | _
                 -> false)
            prop.Prop.sigma
        in
        List.filter ~f:(fun (prop, _) -> not (returns_null prop)) posts
      else posts
    in
    if Config.idempotent_getters && Config.curr_language_is Config.Java then
      mk_getter_idempotent posts
    else posts

(** Check if actual_pre * missing_footprint |- false *)
let inconsistent_actualpre_missing tenv actual_pre split_opt =
  match split_opt with
  | Some split
   -> let prop' = Prop.normalize tenv (Prop.prop_sigma_star actual_pre split.missing_sigma) in
      let prop'' = List.fold ~f:(Prop.prop_atom_and tenv) ~init:prop' split.missing_pi in
      Prover.check_inconsistency tenv prop''
  | None
   -> false

let class_cast_exn tenv pname_opt texp1 texp2 exp ml_loc =
  let desc =
    Errdesc.explain_class_cast_exception tenv pname_opt texp1 texp2 exp (State.get_node ())
      (State.get_loc ())
  in
  Exceptions.Class_cast_exception (desc, ml_loc)

let create_cast_exception tenv ml_loc pname_opt texp1 texp2 exp =
  class_cast_exn tenv pname_opt texp1 texp2 exp ml_loc

let get_check_exn tenv check callee_pname loc ml_loc =
  match check with
  | Prover.Bounds_check
   -> let desc = Localise.desc_precondition_not_met (Some Localise.Pnm_bounds) callee_pname loc in
      Exceptions.Precondition_not_met (desc, ml_loc)
  | Prover.Class_cast_check (texp1, texp2, exp)
   -> class_cast_exn tenv (Some callee_pname) texp1 texp2 exp ml_loc

let check_uninitialize_dangling_deref tenv callee_pname actual_pre sub formal_params props =
  List.iter
    ~f:(fun (p, _) ->
      match check_dereferences tenv callee_pname actual_pre sub p formal_params with
      | Some (Deref_undef_exp, desc)
       -> raise (Exceptions.Dangling_pointer_dereference (Some PredSymb.DAuninit, desc, __POS__))
      | _
       -> ())
    props

(** Perform symbolic execution for a single spec *)
let exe_spec tenv ret_id_opt (n, nspecs) caller_pdesc callee_pname loc prop path_pre
    (spec: Prop.exposed Specs.spec) actual_params formal_params : abduction_res =
  let caller_pname = Procdesc.get_proc_name caller_pdesc in
  let posts = mk_posts tenv ret_id_opt prop callee_pname spec.Specs.posts in
  let actual_pre = mk_actual_precondition tenv prop actual_params formal_params in
  let spec_pre = Specs.Jprop.to_prop spec.Specs.pre in
  L.d_strln ("EXECUTING SPEC " ^ string_of_int n ^ "/" ^ string_of_int nspecs) ;
  L.d_strln "ACTUAL PRECONDITION =" ;
  L.d_increase_indent 1 ;
  Prop.d_prop actual_pre ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  L.d_strln "SPEC =" ;
  L.d_increase_indent 1 ;
  Specs.d_spec spec ;
  L.d_decrease_indent 1 ;
  L.d_ln () ;
  SymOp.pay () ;
  (* pay one symop *)
  match Prover.check_implication_for_footprint caller_pname tenv actual_pre spec_pre with
  | Prover.ImplFail checks
   -> Invalid_res (Prover_checks checks)
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
      , missing_typ )
   -> let log_check_exn check =
        let exn = get_check_exn tenv check callee_pname loc __POS__ in
        Reporting.log_warning_deprecated caller_pname exn
      in
      let do_split () =
        process_splitting actual_pre sub1 sub2 frame missing_pi missing_sigma frame_fld missing_fld
          frame_typ missing_typ
      in
      let report_valid_res split =
        match
          combine tenv ret_id_opt posts actual_pre path_pre split caller_pdesc callee_pname loc
        with
        | None
         -> Invalid_res Cannot_combine
        | Some results
         -> (* After combining we check that we have not added
               a points-to of initialized variables.*)
            check_uninitialize_dangling_deref tenv callee_pname actual_pre split.sub formal_params
              results ;
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
      let subbed_pre = Prop.prop_sub (`Exp sub1) actual_pre in
      match check_dereferences tenv callee_pname subbed_pre (`Exp sub2) spec_pre formal_params with
      | Some (Deref_undef _, _) when Config.angelic_execution
       -> let split = do_split () in
          report_valid_res split
      | Some (deref_error, desc)
       -> let rec join_paths = function
            | []
             -> None
            | (_, p) :: l ->
              match join_paths l with None -> Some p | Some p' -> Some (Paths.Path.join p p')
          in
          let pjoin = join_paths posts in
          (* join the paths from the posts *)
          Invalid_res (Dereference_error (deref_error, desc, pjoin))
      | None
       -> let split = do_split () in
          (* check if a missing_fld hpred is about a hidden field *)
          let hpred_missing_hidden = function
            | Sil.Hpointsto (_, Sil.Estruct ([(fld, _)], _), _)
             -> Typ.Fieldname.is_hidden fld
            | _
             -> false
          in
          (* missing fields minus hidden fields *)
          let missing_fld_nohidden =
            List.filter ~f:(fun hp -> not (hpred_missing_hidden hp)) missing_fld
          in
          if not !Config.footprint && split.missing_sigma <> [] then (
            L.d_strln "Implication error: missing_sigma not empty in re-execution" ;
            Invalid_res Missing_sigma_not_empty )
          else if not !Config.footprint && missing_fld_nohidden <> [] then (
            L.d_strln "Implication error: missing_fld not empty in re-execution" ;
            Invalid_res Missing_fld_not_empty )
          else report_valid_res split

let remove_constant_string_class tenv prop =
  let filter = function
    | Sil.Hpointsto (Exp.Const (Const.Cstr _ | Const.Cclass _), _, _)
     -> false
    | _
     -> true
  in
  let sigma = List.filter ~f:filter prop.Prop.sigma in
  let sigmafp = List.filter ~f:filter prop.Prop.sigma_fp in
  let prop' = Prop.set prop ~sigma ~sigma_fp:sigmafp in
  Prop.normalize tenv prop'

(** existentially quantify the path identifier generated
    by the prover to keep track of expansions of lhs paths
    and remove pointsto's whose lhs is a constant string *)
let quantify_path_idents_remove_constant_strings tenv (prop: Prop.normal Prop.t)
    : Prop.normal Prop.t =
  let fav = Prop.prop_fav prop in
  Sil.fav_filter_ident fav Ident.is_path ;
  remove_constant_string_class tenv (Prop.exist_quantify tenv fav prop)

(** Strengthen the footprint by adding pure facts from the current part *)
let prop_pure_to_footprint tenv (p: 'a Prop.t) : Prop.normal Prop.t =
  let is_footprint_atom_not_attribute a =
    not (Attribute.is_pred a)
    &&
    let a_fav = Sil.atom_fav a in
    Sil.fav_for_all a_fav Ident.is_footprint
  in
  let pure = Prop.get_pure p in
  let new_footprint_atoms = List.filter ~f:is_footprint_atom_not_attribute pure in
  if List.is_empty new_footprint_atoms then p
  else
    (* add pure fact to footprint *)
    let filtered_pi_fp =
      List.filter (p.Prop.pi_fp @ new_footprint_atoms) ~f:(fun a -> not (Sil.atom_has_local_addr a))
    in
    Prop.normalize tenv (Prop.set p ~pi_fp:filtered_pi_fp)

(** post-process the raw result of a function call *)
let exe_call_postprocess tenv ret_id trace_call callee_pname callee_attrs loc results =
  let filter_valid_res = function Invalid_res _ -> false | Valid_res _ -> true in
  let valid_res0, invalid_res0 = List.partition_tf ~f:filter_valid_res results in
  let valid_res =
    List.map ~f:(function Valid_res cr -> cr | Invalid_res _ -> assert false) valid_res0
  in
  let invalid_res =
    List.map ~f:(function Valid_res _ -> assert false | Invalid_res ir -> ir) invalid_res0
  in
  let valid_res_miss_pi, valid_res_no_miss_pi =
    List.partition_tf ~f:(fun vr -> vr.vr_pi <> []) valid_res
  in
  let _, valid_res_cons_pre_missing =
    List.partition_tf ~f:(fun vr -> vr.incons_pre_missing) valid_res
  in
  let deref_errors =
    List.filter ~f:(function Dereference_error _ -> true | _ -> false) invalid_res
  in
  let print_pi pi = L.d_str "pi: " ; Prop.d_pi pi ; L.d_ln () in
  let call_desc kind_opt = Localise.desc_precondition_not_met kind_opt callee_pname loc in
  let res_with_path_idents =
    if !Config.footprint then
      if List.is_empty valid_res_cons_pre_missing then
        (* no valid results where actual pre and missing are consistent *)
        match deref_errors with
        | error :: _
         -> (
            (* dereference error detected *)
            let extend_path path_opt path_pos_opt =
              match path_opt with
              | None
               -> ()
              | Some path_post
               -> let old_path, _ = State.get_path () in
                  let new_path =
                    Paths.Path.add_call (include_subtrace callee_pname) old_path callee_pname
                      path_post
                  in
                  State.set_path new_path path_pos_opt
            in
            match error with
            | Dereference_error (Deref_minusone, desc, path_opt)
             -> trace_call Specs.CallStats.CR_not_met ;
                extend_path path_opt None ;
                raise
                  (Exceptions.Dangling_pointer_dereference (Some PredSymb.DAminusone, desc, __POS__))
            | Dereference_error (Deref_undef_exp, desc, path_opt)
             -> trace_call Specs.CallStats.CR_not_met ;
                extend_path path_opt None ;
                raise
                  (Exceptions.Dangling_pointer_dereference (Some PredSymb.DAuninit, desc, __POS__))
            | Dereference_error (Deref_null pos, desc, path_opt)
             -> trace_call Specs.CallStats.CR_not_met ;
                extend_path path_opt (Some pos) ;
                if Localise.is_parameter_not_null_checked_desc desc then
                  raise (Exceptions.Parameter_not_null_checked (desc, __POS__))
                else if Localise.is_field_not_null_checked_desc desc then
                  raise (Exceptions.Field_not_null_checked (desc, __POS__))
                else if Localise.is_double_lock_desc desc then
                  raise (Exceptions.Double_lock (desc, __POS__))
                else if Localise.is_empty_vector_access_desc desc then
                  raise (Exceptions.Empty_vector_access (desc, __POS__))
                else raise (Exceptions.Null_dereference (desc, __POS__))
            | Dereference_error (Deref_freed _, desc, path_opt)
             -> trace_call Specs.CallStats.CR_not_met ;
                extend_path path_opt None ;
                raise (Exceptions.Use_after_free (desc, __POS__))
            | Dereference_error (Deref_undef (_, _, pos), desc, path_opt)
             -> trace_call Specs.CallStats.CR_not_met ;
                extend_path path_opt (Some pos) ;
                raise (Exceptions.Skip_pointer_dereference (desc, __POS__))
            | Prover_checks _ | Cannot_combine | Missing_sigma_not_empty | Missing_fld_not_empty
             -> trace_call Specs.CallStats.CR_not_met ;
                assert false )
        | []
         -> (* no dereference error detected *)
            let desc =
              if List.exists ~f:(function Cannot_combine -> true | _ -> false) invalid_res then
                call_desc (Some Localise.Pnm_dangling)
              else if List.exists
                        ~f:(function
                            | Prover_checks (check :: _)
                             -> trace_call Specs.CallStats.CR_not_met ;
                                let exn = get_check_exn tenv check callee_pname loc __POS__ in
                                raise exn
                            | _
                             -> false)
                        invalid_res
              then call_desc (Some Localise.Pnm_bounds)
              else call_desc None
            in
            trace_call Specs.CallStats.CR_not_met ;
            raise (Exceptions.Precondition_not_met (desc, __POS__))
      else
        (* combine the valid results, and store diverging states *)
        let process_valid_res vr =
          let save_diverging_states () =
            if not vr.incons_pre_missing && List.is_empty vr.vr_cons_res then
              (* no consistent results on one spec: divergence *)
              let incons_res =
                List.map
                  ~f:(fun (p, path) -> (prop_pure_to_footprint tenv p, path))
                  vr.vr_incons_res
              in
              State.add_diverging_states (Paths.PathSet.from_renamed_list incons_res)
          in
          save_diverging_states () ; vr.vr_cons_res
        in
        List.map
          ~f:(fun (p, path) -> (prop_pure_to_footprint tenv p, path))
          (List.concat_map ~f:process_valid_res valid_res)
    else if valid_res_no_miss_pi <> [] then
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
      | None
       -> trace_call Specs.CallStats.CR_not_met ;
          raise (Exceptions.Precondition_not_met (call_desc None, __POS__))
      | Some cover
       -> L.d_strln "Found minimum cover" ;
          List.iter ~f:print_pi (List.map ~f:fst cover) ;
          List.concat_map ~f:snd cover )
  in
  trace_call Specs.CallStats.CR_success ;
  let res =
    List.map
      ~f:(fun (p, path) -> (quantify_path_idents_remove_constant_strings tenv p, path))
      res_with_path_idents
  in
  let ret_annot, _ = callee_attrs.ProcAttributes.method_annotation in
  let returns_nullable ret_annot = Annotations.ia_is_nullable ret_annot in
  let should_add_ret_attr _ =
    let is_likely_getter = function
      | Typ.Procname.Java pn_java
       -> Int.equal (List.length (Typ.Procname.java_get_parameters pn_java)) 0
      | _
       -> false
    in
    Config.idempotent_getters && Config.curr_language_is Config.Java
    && is_likely_getter callee_pname
    || returns_nullable ret_annot
  in
  match ret_id with
  | Some (ret_id, _) when should_add_ret_attr ()
   -> (* add attribute to remember what function call a return id came from *)
      let ret_var = Exp.Var ret_id in
      let mark_id_as_retval (p, path) =
        let att_retval = PredSymb.Aretval (callee_pname, ret_annot) in
        (Attribute.add tenv p att_retval [ret_var], path)
      in
      List.map ~f:mark_id_as_retval res
  | _
   -> res

(** Execute the function call and return the list of results with return value *)
let exe_function_call callee_summary tenv ret_id_opt caller_pdesc callee_pname loc actual_params
    prop path =
  let callee_attrs = Specs.get_attributes callee_summary in
  let caller_pname = Procdesc.get_proc_name caller_pdesc in
  let caller_summary = Specs.get_summary_unsafe "exe_function_call" caller_pname in
  let trace_call res =
    Specs.CallStats.trace caller_summary.Specs.stats.Specs.call_stats callee_pname loc res
      !Config.footprint
  in
  let spec_list, formal_params = spec_find_rename trace_call callee_summary in
  let nspecs = List.length spec_list in
  L.d_strln
    ("Found " ^ string_of_int nspecs ^ " specs for function " ^ Typ.Procname.to_string callee_pname) ;
  L.d_strln ("START EXECUTING SPECS FOR " ^ Typ.Procname.to_string callee_pname ^ " from state") ;
  Prop.d_prop prop ;
  L.d_ln () ;
  let exe_one_spec (n, spec) =
    exe_spec tenv ret_id_opt (n, nspecs) caller_pdesc callee_pname loc prop path spec actual_params
      formal_params
  in
  let results = List.map ~f:exe_one_spec spec_list in
  exe_call_postprocess tenv ret_id_opt trace_call callee_pname callee_attrs loc results
