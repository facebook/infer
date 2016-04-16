(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Interprocedural footprint analysis *)

module L = Logging
module F = Format

type splitting = {
  sub: Sil.subst;
  frame: Sil.hpred list;
  missing_pi: Sil.atom list;
  missing_sigma: Sil.hpred list;
  frame_fld : Sil.hpred list;
  missing_fld : Sil.hpred list;
  frame_typ : (Sil.exp * Sil.exp) list;
  missing_typ : (Sil.exp * Sil.exp) list;
}

type deref_error =
  | Deref_freed of Sil.res_action (** dereference a freed pointer *)
  | Deref_minusone (** dereference -1 *)
  | Deref_null of Sil.path_pos (** dereference null *)
  (** dereference a value coming from the given undefined function *)
  | Deref_undef of Procname.t * Location.t * Sil.path_pos
  | Deref_undef_exp (** dereference an undefined expression *)

type invalid_res =
  (** dereference error and description *)
  | Dereference_error of deref_error * Localise.error_desc * Paths.Path.t option

  (** the abduction prover failed some checks *)
  | Prover_checks of Prover.check list

  (** cannot combine actual pre with splitting and post *)
  | Cannot_combine

  (** missing_fld not empty in re-execution mode *)
  | Missing_fld_not_empty

  (** missing sigma not empty in re-execution mode *)
  | Missing_sigma_not_empty

type valid_res =
  { incons_pre_missing : bool; (** whether the actual pre is consistent with the missing part *)
    vr_pi: Sil.atom list; (** missing pi *)
    vr_sigma: Sil.hpred list; (** missing sigma *)
    vr_cons_res : (Prop.normal Prop.t * Paths.Path.t) list; (** consistent result props *)
    vr_incons_res : (Prop.normal Prop.t * Paths.Path.t) list; (** inconsistent result props *) }

(** Result of (bi)-abduction on a single spec.
    A result is invalid if no splitting was found,
    or if combine failed, or if we are in re - execution mode and the sigma
    part of the splitting is not empty.
    A valid result contains the missing pi ans sigma, as well as the resulting props. *)
type abduction_res =
  | Valid_res of valid_res (** valid result for a function cal *)
  | Invalid_res of invalid_res (** reason for invalid result *)

(**************** printing functions ****************)
let d_splitting split =
  L.d_strln "Actual splitting";
  L.d_increase_indent 1;
  L.d_strln "------------------------------------------------------------";
  L.d_strln "SUB = "; Prop.d_sub split.sub; L.d_ln ();
  L.d_strln "FRAME ="; Prop.d_sigma split.frame; L.d_ln ();
  L.d_strln "MISSING ="; Prop.d_pi_sigma split.missing_pi split.missing_sigma; L.d_ln ();
  L.d_strln "FRAME FLD = "; Prop.d_sigma split.frame_fld; L.d_ln ();
  L.d_strln "MISSING FLD = "; Prop.d_sigma split.missing_fld; L.d_ln ();
  if split.frame_typ <> []
  then L.d_strln "FRAME TYP = "; Prover.d_typings split.frame_typ; L.d_ln ();
  if split.missing_typ <> []
  then L.d_strln "MISSING TYP = "; Prover.d_typings split.missing_typ; L.d_ln ();
  L.d_strln "------------------------------------------------------------";
  L.d_decrease_indent 1

let print_results actual_pre results =
  L.d_strln "***** RESULTS FUNCTION CALL *******";
  Propset.d actual_pre (Propset.from_proplist results);
  L.d_strln "***** END RESULTS FUNCTION CALL *******"
(***************)

(** Rename the variables in the spec. *)
let spec_rename_vars pname spec =
  let prop_add_callee_suffix p =
    let f = function
      | Sil.Lvar pv ->
          Sil.Lvar (Pvar.to_callee pname pv)
      | e -> e in
    Prop.prop_expmap f p in
  let jprop_add_callee_suffix = function
    | Specs.Jprop.Prop (n, p) ->
        Specs.Jprop.Prop (n, prop_add_callee_suffix p)
    | Specs.Jprop.Joined (n, p, jp1, jp2) ->
        Specs.Jprop.Joined (n, prop_add_callee_suffix p, jp1, jp2) in
  let fav = Sil.fav_new () in
  Specs.Jprop.fav_add fav spec.Specs.pre;
  IList.iter (fun (p, _) -> Prop.prop_fav_add fav p) spec.Specs.posts;
  let ids = Sil.fav_to_list fav in
  let ids' = IList.map (fun i -> (i, Ident.create_fresh Ident.kprimed)) ids in
  let ren_sub = Sil.sub_of_list (IList.map (fun (i, i') -> (i, Sil.Var i')) ids') in
  let pre' = Specs.Jprop.jprop_sub ren_sub spec.Specs.pre in
  let posts' = IList.map (fun (p, path) -> (Prop.prop_sub ren_sub p, path)) spec.Specs.posts in
  let pre'' = jprop_add_callee_suffix pre' in
  let posts'' = IList.map (fun (p, path) -> (prop_add_callee_suffix p, path)) posts' in
  { Specs.pre = pre''; Specs.posts = posts''; Specs.visited = spec.Specs.visited }

(** Find and number the specs for [proc_name],
    after renaming their vars, and also return the parameters *)
let spec_find_rename trace_call (proc_name : Procname.t)
  : (int * Prop.exposed Specs.spec) list * Pvar.t list =
  try
    let count = ref 0 in
    let f spec =
      incr count; (!count, spec_rename_vars proc_name spec) in
    let specs, formals = Specs.get_specs_formals proc_name in
    if specs == [] then
      begin
        trace_call Specs.CallStats.CR_not_found;
        raise (Exceptions.Precondition_not_found
                 (Localise.verbatim_desc (Procname.to_string proc_name), __POS__))
      end;
    let formal_parameters =
      IList.map (fun (x, _) -> Pvar.mk_callee x proc_name) formals in
    IList.map f specs, formal_parameters
  with Not_found -> begin
      L.d_strln
        ("ERROR: found no entry for procedure " ^
         Procname.to_string proc_name ^
         ". Give up...");
      raise (Exceptions.Precondition_not_found
               (Localise.verbatim_desc (Procname.to_string proc_name), __POS__))
    end

(** Process a splitting coming straight from a call to the prover:
    change the instantiating substitution so that it returns primed vars,
    except for vars occurring in the missing part, where it returns
    footprint vars. *)
let process_splitting
    actual_pre sub1 sub2 frame missing_pi missing_sigma
    frame_fld missing_fld frame_typ missing_typ =

  let hpred_has_only_footprint_vars hpred =
    let fav = Sil.fav_new () in
    Sil.hpred_fav_add fav hpred;
    Sil.fav_for_all fav Ident.is_footprint in
  let sub = Sil.sub_join sub1 sub2 in
  let sub1_inverse =
    let sub1_list = Sil.sub_to_list sub1 in
    let sub1_list' = IList.filter (function (_, Sil.Var _) -> true | _ -> false) sub1_list in
    let sub1_inverse_list =
      IList.map
        (function (id, Sil.Var id') -> (id', Sil.Var id) | _ -> assert false)
        sub1_list'
    in Sil.sub_of_list_duplicates sub1_inverse_list in
  let fav_actual_pre =
    let fav_sub2 = (* vars which represent expansions of fields *)
      let fav = Sil.fav_new () in
      IList.iter (Sil.exp_fav_add fav) (Sil.sub_range sub2);
      let filter id = Ident.get_stamp id = - 1 in
      Sil.fav_filter_ident fav filter;
      fav in
    let fav_pre = Prop.prop_fav actual_pre in
    Sil.ident_list_fav_add (Sil.fav_to_list fav_sub2) fav_pre;
    fav_pre in

  let fav_missing = Prop.sigma_fav (Prop.sigma_sub sub missing_sigma) in
  Prop.pi_fav_add fav_missing (Prop.pi_sub sub missing_pi);
  let fav_missing_primed =
    let filter id = Ident.is_primed id && not (Sil.fav_mem fav_actual_pre id)
    in Sil.fav_copy_filter_ident fav_missing filter in
  let fav_missing_fld = Prop.sigma_fav (Prop.sigma_sub sub missing_fld) in

  let map_var_to_pre_var_or_fresh id =
    match Sil.exp_sub sub1_inverse (Sil.Var id) with
    | Sil.Var id' ->
        if Sil.fav_mem fav_actual_pre id' || Ident.is_path id'
        (** a path id represents a position in the pre *)
        then Sil.Var id'
        else Sil.Var (Ident.create_fresh Ident.kprimed)
    | _ -> assert false in

  let sub_list = Sil.sub_to_list sub in
  let fav_sub_list =
    let fav_sub = Sil.fav_new () in
    IList.iter (fun (_, e) -> Sil.exp_fav_add fav_sub e) sub_list;
    Sil.fav_to_list fav_sub in
  let sub1 =
    let f id =
      if Sil.fav_mem fav_actual_pre id then (id, Sil.Var id)
      else if Ident.is_normal id then (id, map_var_to_pre_var_or_fresh id)
      else if Sil.fav_mem fav_missing_fld id then (id, Sil.Var id)
      else if Ident.is_footprint id then (id, Sil.Var id)
      else begin
        let dom1 = Sil.sub_domain sub1 in
        let rng1 = Sil.sub_range sub1 in
        let dom2 = Sil.sub_domain sub2 in
        let rng2 = Sil.sub_range sub2 in
        let vars_actual_pre = IList.map (fun id -> Sil.Var id) (Sil.fav_to_list fav_actual_pre) in
        L.d_str "fav_actual_pre: "; Sil.d_exp_list vars_actual_pre; L.d_ln ();
        L.d_str "Dom(Sub1): "; Sil.d_exp_list (IList.map (fun id -> Sil.Var id) dom1); L.d_ln ();
        L.d_str "Ran(Sub1): "; Sil.d_exp_list rng1; L.d_ln ();
        L.d_str "Dom(Sub2): "; Sil.d_exp_list (IList.map (fun id -> Sil.Var id) dom2); L.d_ln ();
        L.d_str "Ran(Sub2): "; Sil.d_exp_list rng2; L.d_ln ();
        L.d_str "Don't know about id: "; Sil.d_exp (Sil.Var id); L.d_ln ();
        assert false;
      end
    in Sil.sub_of_list (IList.map f fav_sub_list) in
  let sub2_list =
    let f id = (id, Sil.Var (Ident.create_fresh Ident.kfootprint))
    in IList.map f (Sil.fav_to_list fav_missing_primed) in
  let sub_list' =
    IList.map (fun (id, e) -> (id, Sil.exp_sub sub1 e)) sub_list in
  let sub' = Sil.sub_of_list (sub2_list @ sub_list') in
  (* normalize everything w.r.t sub' *)
  let norm_missing_pi = Prop.pi_sub sub' missing_pi in
  let norm_missing_sigma = Prop.sigma_sub sub' missing_sigma in
  let norm_frame_fld = Prop.sigma_sub sub' frame_fld in
  let norm_frame_typ =
    IList.map (fun (e, te) -> Sil.exp_sub sub' e, Sil.exp_sub sub' te) frame_typ in
  let norm_missing_typ =
    IList.map (fun (e, te) -> Sil.exp_sub sub' e, Sil.exp_sub sub' te) missing_typ in
  let norm_missing_fld =
    let sigma = Prop.sigma_sub sub' missing_fld in
    let filter hpred =
      if not (hpred_has_only_footprint_vars hpred) then
        begin
          L.d_warning "Missing fields hpred has non-footprint vars: "; Sil.d_hpred hpred; L.d_ln ();
          false
        end
      else match hpred with
        | Sil.Hpointsto(Sil.Var _, _, _) -> true
        | Sil.Hpointsto(Sil.Lvar pvar, _, _) -> Pvar.is_global pvar
        | _ ->
            L.d_warning "Missing fields in complex pred: "; Sil.d_hpred hpred; L.d_ln ();
            false in
    IList.filter filter sigma in
  let norm_frame = Prop.sigma_sub sub' frame in
  { sub =           sub';
    frame =         norm_frame;
    missing_pi =    norm_missing_pi;
    missing_sigma = norm_missing_sigma;
    frame_fld =     norm_frame_fld;
    missing_fld =   norm_missing_fld;
    frame_typ =     norm_frame_typ;
    missing_typ =   norm_missing_typ; }

(** Check whether an inst represents a dereference without null check,
    and return the line number and path position *)
let find_dereference_without_null_check_in_inst = function
  | Sil.Iupdate (Some true, _, n, pos)
  | Sil.Irearrange (Some true, _, n, pos) -> Some (n, pos)
  | _ -> None

(** Check whether a sexp contains a dereference without null check,
    and return the line number and path position *)
let rec find_dereference_without_null_check_in_sexp = function
  | Sil.Eexp (_, inst) -> find_dereference_without_null_check_in_inst inst
  | Sil.Estruct (fsel, inst) ->
      let res = find_dereference_without_null_check_in_inst inst in
      if res = None then
        find_dereference_without_null_check_in_sexp_list (IList.map snd fsel)
      else res
  | Sil.Earray (_, esel, inst) ->
      let res = find_dereference_without_null_check_in_inst inst in
      if res = None then
        find_dereference_without_null_check_in_sexp_list (IList.map snd esel)
      else res
and find_dereference_without_null_check_in_sexp_list = function
  | [] -> None
  | se:: sel ->
      (match find_dereference_without_null_check_in_sexp se with
       | None -> find_dereference_without_null_check_in_sexp_list sel
       | Some x -> Some x)

(** Check dereferences implicit in the spec pre.
    In case of dereference error, return [Some(deref_error, description)], otherwise [None] *)
let check_dereferences callee_pname actual_pre sub spec_pre formal_params =
  let check_dereference e sexp =
    let e_sub = Sil.exp_sub sub e in
    let desc use_buckets deref_str =
      let error_desc =
        Errdesc.explain_dereference_as_caller_expression
          ~use_buckets
          deref_str actual_pre spec_pre e (State.get_node ()) (State.get_loc ()) formal_params in
      (L.d_strln_color Red) "found error in dereference";
      L.d_strln "spec_pre:"; Prop.d_prop spec_pre; L.d_ln();
      L.d_str "exp "; Sil.d_exp e;
      L.d_strln (" desc: " ^ (pp_to_string Localise.pp_error_desc error_desc));
      error_desc in
    let deref_no_null_check_pos =
      if Sil.exp_equal e_sub Sil.exp_zero then
        match find_dereference_without_null_check_in_sexp sexp with
        | Some (_, pos) -> Some pos
        | None -> None
      else None in
    if deref_no_null_check_pos != None
    then
      (* only report a dereference null error if we know
         there was a dereference without null check *)
      match deref_no_null_check_pos with
      | Some pos -> Some (Deref_null pos, desc true (Localise.deref_str_null (Some callee_pname)))
      | None -> assert false
    else
      (* Check if the dereferenced expr has the dangling uninitialized attribute. *)
      (* In that case it raise a dangling pointer dereferece *)
    if Prop.has_dangling_uninit_attribute spec_pre e then
      Some (Deref_undef_exp, desc false (Localise.deref_str_dangling (Some Sil.DAuninit)) )
    else if Sil.exp_equal e_sub Sil.exp_minus_one
    then Some (Deref_minusone, desc true (Localise.deref_str_dangling None))
    else match Prop.get_resource_attribute actual_pre e_sub with
      | Some (Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease } as ra)) ->
          Some (Deref_freed ra, desc true (Localise.deref_str_freed ra))
      | _ ->
          (match Prop.get_undef_attribute actual_pre e_sub with
           | Some (Sil.Aundef (s, loc, pos)) ->
               Some (Deref_undef (s, loc, pos), desc false (Localise.deref_str_undef (s, loc)))
           | _ -> None) in
  let check_hpred = function
    | Sil.Hpointsto (lexp, se, _) ->
        check_dereference (Sil.root_of_lexp lexp) se
    | _ -> None in
  let deref_err_list = IList.fold_left (fun deref_errs hpred -> match check_hpred hpred with
      | Some reason -> reason :: deref_errs
      | None -> deref_errs
    ) [] (Prop.get_sigma spec_pre) in
  match deref_err_list with
  | [] -> None
  | deref_err :: _ ->
      if !Config.angelic_execution then
        (* In angelic mode, prefer to report Deref_null over other kinds of deref errors. this
         * makes sure we report a NULL_DEREFERENCE instead of
           a less interesting PRECONDITION_NOT_MET
         * whenever possible *)
        (* TOOD (t4893533): use this trick outside of angelic mode and in other parts of the code *)
        Some
          (try
             IList.find
               (fun err -> match err with
                  | (Deref_null _, _) -> true
                  | _ -> false )
               deref_err_list
           with Not_found -> deref_err)
      else Some deref_err

let post_process_sigma (sigma: Sil.hpred list) loc : Sil.hpred list =
  let map_inst inst = Sil.inst_new_loc loc inst in
  let do_hpred (_, _, hpred) = Sil.hpred_instmap map_inst hpred in
  (** update the location of instrumentations *)
  IList.map (fun hpred -> do_hpred (Prover.expand_hpred_pointer false hpred)) sigma

(** check for interprocedural path errors in the post *)
let check_path_errors_in_post caller_pname post post_path =
  let check_attr (e, att) = match att with
    | Sil.Adiv0 path_pos ->
        if Prover.check_zero e then
          let desc = Errdesc.explain_divide_by_zero e (State.get_node ()) (State.get_loc ()) in
          let new_path, path_pos_opt =
            let current_path, _ = State.get_path () in
            if Paths.Path.contains_position post_path path_pos
            then post_path, Some path_pos
            else current_path, None in (* position not found, only use the path up to the callee *)
          State.set_path new_path path_pos_opt;
          let exn = Exceptions.Divide_by_zero (desc, __POS__) in
          let pre_opt = State.get_normalized_pre (fun _ p -> p) (* Abs.abstract_no_symop *) in
          Reporting.log_warning caller_pname ~pre: pre_opt exn
    | _ -> () in
  IList.iter check_attr (Prop.get_all_attributes post)

(** Post process the instantiated post after the function call so that
    x.f |-> se becomes x |-> \{ f: se \}.
    Also, update any Aresource attributes to refer to the caller *)
let post_process_post
    caller_pname callee_pname loc actual_pre ((post: Prop.exposed Prop.t), post_path) =
  let actual_pre_has_freed_attribute e = match Prop.get_resource_attribute actual_pre e with
    | Some (Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease })) -> true
    | _ -> false in
  let atom_update_alloc_attribute = function
    | Sil.Aneq (e , Sil.Const (Sil.Cattribute (Sil.Aresource ra)))
    | Sil.Aneq (Sil.Const (Sil.Cattribute (Sil.Aresource ra)), e)
      when not (ra.Sil.ra_kind = Sil.Rrelease && actual_pre_has_freed_attribute e) ->
        (* unless it was already freed before the call *)
        let vpath, _ = Errdesc.vpath_find post e in
        let ra' = { ra with Sil.ra_pname = callee_pname; Sil.ra_loc = loc; Sil.ra_vpath = vpath } in
        let c = Sil.Const (Sil.Cattribute (Sil.Aresource ra')) in
        Sil.Aneq (e, c)
    | a -> a in
  let prop' = Prop.replace_sigma (post_process_sigma (Prop.get_sigma post) loc) post in
  let pi' = IList.map atom_update_alloc_attribute (Prop.get_pi prop') in
  (* update alloc attributes to refer to the caller *)
  let post' = Prop.replace_pi pi' prop' in
  check_path_errors_in_post caller_pname post' post_path;
  post', post_path

let hpred_lhs_compare hpred1 hpred2 = match hpred1, hpred2 with
  | Sil.Hpointsto(e1, _, _), Sil.Hpointsto(e2, _, _) -> Sil.exp_compare e1 e2
  | Sil.Hpointsto _, _ -> - 1
  | _, Sil.Hpointsto _ -> 1
  | hpred1, hpred2 -> Sil.hpred_compare hpred1 hpred2

(** set the inst everywhere in a sexp *)
let rec sexp_set_inst inst = function
  | Sil.Eexp (e, _) ->
      Sil.Eexp (e, inst)
  | Sil.Estruct (fsel, _) ->
      Sil.Estruct ((IList.map (fun (f, se) -> (f, sexp_set_inst inst se)) fsel), inst)
  | Sil.Earray (size, esel, _) ->
      Sil.Earray (size, IList.map (fun (e, se) -> (e, sexp_set_inst inst se)) esel, inst)

let rec fsel_star_fld fsel1 fsel2 = match fsel1, fsel2 with
  | [], fsel2 -> fsel2
  | fsel1,[] -> fsel1
  | (f1, se1):: fsel1', (f2, se2):: fsel2' ->
      (match Ident.fieldname_compare f1 f2 with
       | 0 -> (f1, sexp_star_fld se1 se2) :: fsel_star_fld fsel1' fsel2'
       | n when n < 0 -> (f1, se1) :: fsel_star_fld fsel1' fsel2
       | _ -> (f2, se2) :: fsel_star_fld fsel1 fsel2')

and array_content_star se1 se2 =
  try sexp_star_fld se1 se2 with
  | exn when SymOp.exn_not_failure exn -> se1 (* let postcondition override *)

and esel_star_fld esel1 esel2 = match esel1, esel2 with
  | [], esel2 -> (* don't know whether element is read or written in fun call with array *)
      IList.map (fun (e, se) -> (e, sexp_set_inst Sil.Inone se)) esel2
  | esel1,[] -> esel1
  | (e1, se1):: esel1', (e2, se2):: esel2' ->
      (match Sil.exp_compare e1 e2 with
       | 0 -> (e1, array_content_star se1 se2) :: esel_star_fld esel1' esel2'
       | n when n < 0 -> (e1, se1) :: esel_star_fld esel1' esel2
       | _ ->
           let se2' = sexp_set_inst Sil.Inone se2 in
           (* don't know whether element is read or written in fun call with array *)
           (e2, se2') :: esel_star_fld esel1 esel2')

and sexp_star_fld se1 se2 : Sil.strexp =
  (* L.d_str "sexp_star_fld "; Sil.d_sexp se1; L.d_str " "; Sil.d_sexp se2; L.d_ln (); *)
  match se1, se2 with
  | Sil.Estruct (fsel1, _), Sil.Estruct (fsel2, inst2) ->
      Sil.Estruct (fsel_star_fld fsel1 fsel2, inst2)
  | Sil.Earray (size1, esel1, _), Sil.Earray (_, esel2, inst2) ->
      Sil.Earray (size1, esel_star_fld esel1 esel2, inst2)
  | Sil.Eexp (_, inst1), Sil.Earray (size2, esel2, _) ->
      let esel1 = [(Sil.exp_zero, se1)] in
      Sil.Earray (size2, esel_star_fld esel1 esel2, inst1)
  | _ ->
      L.d_str "cannot star ";
      Sil.d_sexp se1; L.d_str " and "; Sil.d_sexp se2;
      L.d_ln ();
      assert false

let texp_star texp1 texp2 =
  let rec ftal_sub ftal1 ftal2 = match ftal1, ftal2 with
    | [], _ -> true
    | _, [] -> false
    | (f1, _, _):: ftal1', (f2, _, _):: ftal2' ->
        begin match Ident.fieldname_compare f1 f2 with
          | n when n < 0 -> false
          | 0 -> ftal_sub ftal1' ftal2'
          | _ -> ftal_sub ftal1 ftal2' end in
  let typ_star t1 t2 = match t1, t2 with
    | Sil.Tstruct { Sil.instance_fields = instance_fields1; csu = csu1 },
      Sil.Tstruct { Sil.instance_fields = instance_fields2; csu = csu2 } when csu1 = csu2 ->
        if ftal_sub instance_fields1 instance_fields2 then t2 else t1
    | _ -> t1 in
  match texp1, texp2 with
  | Sil.Sizeof (t1, st1), Sil.Sizeof (t2, st2) ->
      Sil.Sizeof (typ_star t1 t2, Sil.Subtype.join st1 st2)
  | _ ->
      texp1

let hpred_star_fld (hpred1 : Sil.hpred) (hpred2 : Sil.hpred) : Sil.hpred =
  match hpred1, hpred2 with
  | Sil.Hpointsto(e1, se1, t1), Sil.Hpointsto(_, se2, t2) ->
      (* L.d_str "hpred_star_fld t1: "; Sil.d_texp_full t1; L.d_str " t2: "; Sil.d_texp_full t2;
         L.d_str " se1: "; Sil.d_sexp se1; L.d_str " se2: "; Sil.d_sexp se2; L.d_ln (); *)
      Sil.Hpointsto(e1, sexp_star_fld se1 se2, texp_star t1 t2)
  | _ -> assert false

(** Implementation of [*] for the field-splitting model *)
let sigma_star_fld (sigma1 : Sil.hpred list) (sigma2 : Sil.hpred list) : Sil.hpred list =
  let sigma1 = IList.stable_sort hpred_lhs_compare sigma1 in
  let sigma2 = IList.stable_sort hpred_lhs_compare sigma2 in
  (* L.out "@.@. computing %a@.STAR @.%a@.@." pp_sigma sigma1 pp_sigma sigma2; *)
  let rec star sg1 sg2 : Sil.hpred list =
    match sg1, sg2 with
    | [], _ -> []
    | sigma1,[] -> sigma1
    | hpred1:: sigma1', hpred2:: sigma2' ->
        begin
          match hpred_lhs_compare hpred1 hpred2 with
          | 0 -> hpred_star_fld hpred1 hpred2 :: star sigma1' sigma2'
          | n when n < 0 -> hpred1 :: star sigma1' sg2
          | _ -> star sg1 sigma2'
        end
  in
  try star sigma1 sigma2
  with exn when SymOp.exn_not_failure exn ->
    L.d_str "cannot star ";
    Prop.d_sigma sigma1; L.d_str " and "; Prop.d_sigma sigma2;
    L.d_ln ();
    raise (Prop.Cannot_star __POS__)

let hpred_typing_lhs_compare hpred1 (e2, _) = match hpred1 with
  | Sil.Hpointsto(e1, _, _) -> Sil.exp_compare e1 e2
  | _ -> - 1

let hpred_star_typing (hpred1 : Sil.hpred) (_, te2) : Sil.hpred =
  match hpred1 with
  | Sil.Hpointsto(e1, se1, _) -> Sil.Hpointsto (e1, se1, te2)
  | _ -> assert false

(** Implementation of [*] between predicates and typings *)
let sigma_star_typ
    (sigma1 : Sil.hpred list) (typings2 : (Sil.exp * Sil.exp) list) : Sil.hpred list =
  let typing_lhs_compare (e1, _) (e2, _) = Sil.exp_compare e1 e2 in
  let sigma1 = IList.stable_sort hpred_lhs_compare sigma1 in
  let typings2 = IList.stable_sort typing_lhs_compare typings2 in
  let rec star sg1 typ2 : Sil.hpred list =
    match sg1, typ2 with
    | [], _ -> []
    | sigma1,[] -> sigma1
    | hpred1:: sigma1', typing2:: typings2' ->
        begin
          match hpred_typing_lhs_compare hpred1 typing2 with
          | 0 -> hpred_star_typing hpred1 typing2 :: star sigma1' typings2'
          | n when n < 0 -> hpred1 :: star sigma1' typ2
          | _ -> star sg1 typings2'
        end in
  try star sigma1 typings2
  with exn when SymOp.exn_not_failure exn ->
    L.d_str "cannot star ";
    Prop.d_sigma sigma1; L.d_str " and "; Prover.d_typings typings2;
    L.d_ln ();
    raise (Prop.Cannot_star __POS__)

(** [prop_footprint_add_pi_sigma_starfld_sigma prop pi sigma missing_fld]
    extends the footprint of [prop] with [pi,sigma]
    and extends the fields of |-> with [missing_fld] *)
let prop_footprint_add_pi_sigma_starfld_sigma
    (prop : 'a Prop.t) pi_new sigma_new missing_fld missing_typ : Prop.normal Prop.t option =
  let rec extend_sigma current_sigma new_sigma = match new_sigma with
    | [] -> Some current_sigma
    | hpred :: new_sigma' ->
        let fav = Prop.sigma_fav [hpred] in
        (* TODO (t4893479): make this check less angelic *)
        if Sil.fav_exists fav
            (fun id -> not (Ident.is_footprint id) && not !Config.angelic_execution)
        then begin
          L.d_warning "found hpred with non-footprint variable, dropping the spec";
          L.d_ln (); Sil.d_hpred hpred; L.d_ln ();
          None
        end
        else extend_sigma (hpred :: current_sigma) new_sigma' in
  let rec extend_pi current_pi new_pi = match new_pi with
    | [] -> current_pi
    | a :: new_pi' ->
        let fav = Prop.pi_fav [a] in
        if Sil.fav_exists fav (fun id -> not (Ident.is_footprint id))
        then begin
          L.d_warning "dropping atom with non-footprint variable";
          L.d_ln (); Sil.d_atom a; L.d_ln ();
          extend_pi current_pi new_pi'
        end
        else extend_pi (a :: current_pi) new_pi' in
  let foot_pi' = extend_pi (Prop.get_pi_footprint prop) pi_new in
  match extend_sigma (Prop.get_sigma_footprint prop) sigma_new with
  | None -> None
  | Some sigma' ->
      let foot_sigma' = sigma_star_fld sigma' missing_fld in
      let foot_sigma'' = sigma_star_typ foot_sigma' missing_typ in
      let pi' = pi_new @ Prop.get_pi prop in
      let prop' =
        Prop.replace_sigma_footprint foot_sigma'' (Prop.replace_pi_footprint foot_pi' prop) in
      let prop'' = Prop.replace_pi pi' prop' in
      Some (Prop.normalize prop'')

(** Check if the attribute change is a mismatch between a kind
    of allocation and a different kind of deallocation *)
let check_attr_dealloc_mismatch att_old att_new = match att_old, att_new with
  | Sil.Aresource ({ Sil.ra_kind = Sil.Racquire; Sil.ra_res = Sil.Rmemory mk_old } as ra_old),
    Sil.Aresource ({ Sil.ra_kind = Sil.Rrelease; Sil.ra_res = Sil.Rmemory mk_new } as ra_new)
    when Sil.mem_kind_compare mk_old mk_new <> 0 ->
      let desc = Errdesc.explain_allocation_mismatch ra_old ra_new in
      raise (Exceptions.Deallocation_mismatch (desc, __POS__))
  | _ -> ()

(** [prop_copy_footprint p1 p2] copies the footprint and pure part of [p1] into [p2] *)
let prop_copy_footprint_pure p1 p2 =
  let p2' =
    Prop.replace_sigma_footprint
      (Prop.get_sigma_footprint p1) (Prop.replace_pi_footprint (Prop.get_pi_footprint p1) p2) in
  let pi2 = Prop.get_pi p2' in
  let pi2_attr, pi2_noattr = IList.partition Prop.atom_is_attribute pi2 in
  let res_noattr = Prop.replace_pi (Prop.get_pure p1 @ pi2_noattr) p2' in
  let replace_attr prop atom = (* call replace_atom_attribute which deals with existing attibutes *)
    (** if [atom] represents an attribute [att], add the attribure to [prop] *)
    match Prop.atom_get_exp_attribute atom with
    | None -> prop
    | Some (exp, att) ->
        Prop.add_or_replace_exp_attribute_check_changed check_attr_dealloc_mismatch prop exp att in
  IList.fold_left replace_attr (Prop.normalize res_noattr) pi2_attr

(** check if an expression is an exception *)
let exp_is_exn = function
  | Sil.Const Sil.Cexn _ -> true
  | _ -> false

(** check if a prop is an exception *)
let prop_is_exn pname prop =
  let ret_pvar = Sil.Lvar (Pvar.get_ret_pvar pname) in
  let is_exn = function
    | Sil.Hpointsto (e1, Sil.Eexp(e2, _), _) when Sil.exp_equal e1 ret_pvar ->
        exp_is_exn e2
    | _ -> false in
  IList.exists is_exn (Prop.get_sigma prop)

(** when prop is an exception, return the exception name *)
let prop_get_exn_name pname prop =
  let ret_pvar = Sil.Lvar (Pvar.get_ret_pvar pname) in
  let exn_name = ref (Typename.Java.from_string "") in
  let find_exn_name e =
    let do_hpred = function
      | Sil.Hpointsto (e1, _, Sil.Sizeof (Sil.Tstruct  { Sil.struct_name = Some name }, _))
        when Sil.exp_equal e1 e ->
          let found_exn_name = Typename.TN_csu (Csu.Class Csu.Java, name) in
          exn_name := found_exn_name
      | _ -> () in
    IList.iter do_hpred (Prop.get_sigma prop) in
  let find_ret () =
    let do_hpred = function
      | Sil.Hpointsto (e1, Sil.Eexp(Sil.Const (Sil.Cexn e2), _), _)
        when Sil.exp_equal e1 ret_pvar ->
          find_exn_name e2
      | _ -> () in
    IList.iter do_hpred (Prop.get_sigma prop) in
  find_ret ();
  !exn_name

(** search in prop for some assignment of global errors *)
let lookup_custom_errors prop =
  let rec search_error = function
    | [] -> None
    | Sil.Hpointsto (Sil.Lvar var, Sil.Eexp (Sil.Const (Sil.Cstr error_str), _), _) :: _
      when Pvar.equal var Sil.custom_error -> Some error_str
    | _ :: tl -> search_error tl in
  search_error (Prop.get_sigma prop)

(** set a prop to an exception sexp *)
let prop_set_exn pname prop se_exn =
  let ret_pvar = Sil.Lvar (Pvar.get_ret_pvar pname) in
  let map_hpred = function
    | Sil.Hpointsto (e, _, t) when Sil.exp_equal e ret_pvar ->
        Sil.Hpointsto(e, se_exn, t)
    | hpred -> hpred in
  let sigma' = IList.map map_hpred (Prop.get_sigma prop) in
  Prop.normalize (Prop.replace_sigma sigma' prop)

(** Include a subtrace for a procedure call if the callee is not a model. *)
let include_subtrace callee_pname =
  Specs.get_origin callee_pname <> Specs.Models

(** combine the spec's post with a splitting and actual precondition *)
let combine
    ret_ids (posts: ('a Prop.t * Paths.Path.t) list)
    actual_pre path_pre split
    caller_pdesc callee_pname loc =
  let caller_pname = Cfg.Procdesc.get_proc_name caller_pdesc in
  let instantiated_post =
    let posts' =
      if !Config.footprint && posts = []
      then (* in case of divergence, produce a prop *)
        (* with updated footprint and inconsistent current *)
        [(Prop.replace_pi [Sil.Aneq (Sil.exp_zero, Sil.exp_zero)] Prop.prop_emp, path_pre)]
      else
        IList.map
          (fun (p, path_post) ->
             (p,
              Paths.Path.add_call
                (include_subtrace callee_pname)
                path_pre
                callee_pname
                path_post))
          posts in
    IList.map
      (fun (p, path) ->
         (post_process_post
            caller_pname callee_pname loc actual_pre (Prop.prop_sub split.sub p, path)))
      posts' in
  L.d_increase_indent 1;
  L.d_strln "New footprint:"; Prop.d_pi_sigma split.missing_pi split.missing_sigma; L.d_ln ();
  L.d_strln "Frame fld:"; Prop.d_sigma split.frame_fld; L.d_ln ();
  if split.frame_typ <> []
  then
    begin L.d_strln "Frame typ:";
      Prover.d_typings split.frame_typ; L.d_ln () end;
  L.d_strln "Missing fld:"; Prop.d_sigma split.missing_fld; L.d_ln ();
  if split.missing_typ <> []
  then
    begin L.d_strln "Missing typ:";
      Prover.d_typings split.missing_typ; L.d_ln (); end;
  L.d_strln "Instantiated frame:"; Prop.d_sigma split.frame; L.d_ln ();
  L.d_strln "Instantiated post:";
  Propgraph.d_proplist Prop.prop_emp (IList.map fst instantiated_post);
  L.d_decrease_indent 1; L.d_ln ();
  let compute_result post_p =
    let post_p' =
      let post_sigma = sigma_star_fld (Prop.get_sigma post_p) split.frame_fld in
      let post_sigma' = sigma_star_typ post_sigma split.frame_typ in
      Prop.replace_sigma post_sigma' post_p in
    let post_p1 = Prop.prop_sigma_star (prop_copy_footprint_pure actual_pre post_p') split.frame in

    let handle_null_case_analysis sigma =
      let id_assigned_to_null id =
        let filter = function
          | Sil.Aeq (Sil.Var id', Sil.Const (Sil.Cint i)) ->
              Ident.equal id id' && Sil.Int.isnull i
          | _ -> false in
        IList.exists filter split.missing_pi in
      let f (e, inst_opt) = match e, inst_opt with
        | Sil.Var id, Some inst when id_assigned_to_null id ->
            let inst' = Sil.inst_set_null_case_flag inst in
            (e, Some inst')
        | _ -> (e, inst_opt) in
      Sil.hpred_list_expmap f sigma in

    let post_p2 =
      let post_p1_sigma = Prop.get_sigma post_p1 in
      let post_p1_sigma' = handle_null_case_analysis post_p1_sigma in
      let post_p1' = Prop.replace_sigma post_p1_sigma' post_p1 in
      Prop.normalize (Prop.replace_pi (Prop.get_pi post_p1 @ split.missing_pi) post_p1') in

    let post_p3 = (** replace [result|callee] with an aux variable dedicated to this proc *)
      let callee_ret_pvar =
        Sil.Lvar (Pvar.to_callee callee_pname (Pvar.get_ret_pvar callee_pname)) in
      match Prop.prop_iter_create post_p2 with
      | None -> post_p2
      | Some iter ->
          let filter = function
            | Sil.Hpointsto (e, _, _) when Sil.exp_equal e callee_ret_pvar -> Some ()
            | _ -> None in
          match Prop.prop_iter_find iter filter with
          | None -> post_p2
          | Some iter' ->
              match fst (Prop.prop_iter_current iter') with
              | Sil.Hpointsto (_, Sil.Eexp (e', inst), _) when exp_is_exn e' ->
                  (* resuls is an exception: set in caller *)
                  let p = Prop.prop_iter_remove_curr_then_to_prop iter' in
                  prop_set_exn caller_pname p (Sil.Eexp (e', inst))
              | Sil.Hpointsto (_, Sil.Eexp (e', _), _) when IList.length ret_ids = 1 ->
                  let p = Prop.prop_iter_remove_curr_then_to_prop iter' in
                  Prop.conjoin_eq e' (Sil.Var (IList.hd ret_ids)) p
              | Sil.Hpointsto (_, Sil.Estruct (ftl, _), _)
                when IList.length ftl = IList.length ret_ids ->
                  let rec do_ftl_ids p = function
                    | [], [] -> p
                    | (_, Sil.Eexp (e', _)):: ftl', ret_id:: ret_ids' ->
                        let p' = Prop.conjoin_eq e' (Sil.Var ret_id) p in
                        do_ftl_ids p' (ftl', ret_ids')
                    | _ -> p in
                  let p = Prop.prop_iter_remove_curr_then_to_prop iter' in
                  do_ftl_ids p (ftl, ret_ids)
              | Sil.Hpointsto _ -> (** returning nothing or unexpected sexp, turning into nondet *)
                  Prop.prop_iter_remove_curr_then_to_prop iter'
              | _ -> assert false in
    let post_p4 =
      if !Config.footprint
      then
        prop_footprint_add_pi_sigma_starfld_sigma
          post_p3
          split.missing_pi
          split.missing_sigma
          split.missing_fld
          split.missing_typ
      else Some post_p3 in
    post_p4 in
  let _results = IList.map (fun (p, path) -> (compute_result p, path)) instantiated_post in
  if IList.exists (fun (x, _) -> x = None) _results then (* at least one combine failed *)
    None
  else
    let results =
      IList.map (function (Some x, path) -> (x, path) | (None, _) -> assert false)
        _results in
    print_results actual_pre (IList.map fst results);
    Some results

(* add tainting attribute to a pvar in a prop *)
let add_tainting_attribute att pvar_param prop =
  IList.fold_left
    (fun prop_acc hpred ->
       match hpred with
       | Sil.Hpointsto (Sil.Lvar pvar, (Sil.Eexp (rhs, _)), _)
         when Pvar.equal pvar pvar_param ->
           L.d_strln ("TAINT ANALYSIS: setting taint/untaint attribute of parameter " ^
                      (Pvar.to_string pvar));
           Prop.add_or_replace_exp_attribute prop_acc rhs att
       | _ -> prop_acc)
    prop (Prop.get_sigma prop)

(* add tainting attributes to a list of paramenters *)
let add_tainting_att_param_list prop param_nums formal_params att =
  try
    IList.map (fun n -> IList.nth formal_params n) param_nums
    |> IList.fold_left (fun prop param -> add_tainting_attribute att param prop) prop
  with Failure _ | Invalid_argument _  ->
    L.d_strln ("TAINT ANALYSIS: WARNING, tainting framework " ^
               "specifies incorrect parameter number " ^
               " to be set as tainted/untainted ");
    prop

(* Set Ataint attribute to list of parameters in a prop *)
let add_param_taint proc_name formal_params prop param_nums =
  let formal_params' = IList.map
      (fun (p, _) -> Pvar.mk p proc_name) formal_params in
  (* TODO: add taint_kind as part of specification format in taint.ml *)
  let taint_info = { Sil.taint_source = proc_name; taint_kind = Unknown; } in
  add_tainting_att_param_list prop param_nums formal_params' (Sil.Ataint taint_info)

(* add Auntaint attribute to a callee_pname precondition *)
let mk_pre pre formal_params callee_pname callee_attrs =
  if !Config.taint_analysis then
    add_tainting_att_param_list
      (Prop.normalize pre)
      (Taint.accepts_sensitive_params callee_pname (Some callee_attrs))
      formal_params
      Sil.Auntaint
    |> Prop.expose
  else pre

(** Construct the actual precondition: add to the current state a copy
    of the (callee's) formal parameters instantiated with the actual
    parameters. *)
let mk_actual_precondition prop actual_params formal_params =
  let formals_actuals =
    let rec comb fpars apars = match fpars, apars with
      | f:: fpars', a:: apars' -> (f, a) :: comb fpars' apars'
      | [], _ ->
          if apars != [] then
            begin
              let str =
                "more actual pars than formal pars in fun call (" ^
                string_of_int (IList.length actual_params) ^
                " vs " ^
                string_of_int (IList.length formal_params) ^
                ")" in
              L.d_warning str; L.d_ln ()
            end;
          []
      | _:: _,[] -> raise (Exceptions.Wrong_argument_number __POS__) in
    comb formal_params actual_params in
  let mk_instantiation (formal_var, (actual_e, actual_t)) =
    Prop.mk_ptsto
      (Sil.Lvar formal_var)
      (Sil.Eexp (actual_e, Sil.inst_actual_precondition))
      (Sil.Sizeof (actual_t, Sil.Subtype.exact)) in
  let instantiated_formals = IList.map mk_instantiation formals_actuals in
  let actual_pre = Prop.prop_sigma_star prop instantiated_formals in
  Prop.normalize actual_pre

let mk_posts ret_ids prop callee_pname callee_attrs posts =
  match ret_ids with
  | [ret_id] ->
      let mk_getter_idempotent posts =
        (* if we have seen a previous call to the same function, only use specs whose return value
           is consistent with constraints on the return value of the previous call w.r.t to
           nullness. meant to eliminate false NPE warnings from the common
           "if (get() != null) get().something()" pattern *)
        let last_call_ret_non_null =
          IList.exists
            (fun (exp, attr) ->
               match attr with
               | Sil.Aretval pname when Procname.equal callee_pname pname ->
                   Prover.check_disequal prop exp Sil.exp_zero
               | _ -> false)
            (Prop.get_all_attributes prop) in
        if last_call_ret_non_null then
          let returns_null prop =
            IList.exists
              (function
                | Sil.Hpointsto (Sil.Lvar pvar, Sil.Eexp (e, _), _) when Pvar.is_return pvar ->
                    Prover.check_equal (Prop.normalize prop) e Sil.exp_zero
                | _ -> false)
              (Prop.get_sigma prop) in
          IList.filter (fun (prop, _) -> not (returns_null prop)) posts
        else posts in
      let mk_retval_tainted posts =
        if Taint.returns_tainted callee_pname (Some callee_attrs) then
          let taint_retval (prop, path) =
            let prop_normal = Prop.normalize prop in
            let prop' =
              Prop.add_or_replace_exp_attribute prop_normal
                (Sil.Var ret_id)
                (Sil.Ataint { Sil.taint_source = callee_pname; taint_kind = Unknown })
              |> Prop.expose in
            (prop', path) in
          IList.map taint_retval posts
        else posts in
      let posts' =
        if !Config.idempotent_getters && !Config.curr_language = Config.Java
        then mk_getter_idempotent posts
        else posts in
      if !Config.taint_analysis then mk_retval_tainted posts' else posts'
  | _ -> posts


(** Check if actual_pre * missing_footprint |- false *)
let inconsistent_actualpre_missing actual_pre split_opt =
  match split_opt with
  | Some split ->
      let prop'= Prop.normalize (Prop.prop_sigma_star actual_pre split.missing_sigma) in
      let prop''= IList.fold_left Prop.prop_atom_and prop' split.missing_pi in
      Prover.check_inconsistency prop''
  | None -> false

(* perform the taint analysis check by comparing the taint atoms in [calling_pi] with the untaint
   atoms required by the [missing_pi] computed during abduction *)
let do_taint_check caller_pname callee_pname calling_prop missing_pi sub =
  let calling_pi = Prop.get_pi calling_prop in
  (* get a version of [missing_pi] whose var names match the names in calling pi *)
  let missing_pi_sub = Prop.pi_sub sub missing_pi in
  let combined_pi = calling_pi @ missing_pi_sub in
  (* build a map from exp -> [taint attrs, untaint attrs], keeping only exprs with both kinds of
     attrs (we will flag errors on those exprs) *)
  let collect_taint_untaint_exprs acc_map atom = match Prop.atom_get_exp_attribute atom with
    | Some (e, Sil.Ataint _) ->
        let taint_atoms, untaint_atoms = try Sil.ExpMap.find e acc_map with Not_found -> ([], []) in
        Sil.ExpMap.add e (atom :: taint_atoms, untaint_atoms) acc_map
    | Some (e, Sil.Auntaint) ->
        let taint_atoms, untaint_atoms = try Sil.ExpMap.find e acc_map with Not_found -> ([], []) in
        Sil.ExpMap.add e (taint_atoms, atom :: untaint_atoms) acc_map
    | _ -> acc_map in
  let taint_untaint_exp_map =
    IList.fold_left
      collect_taint_untaint_exprs
      Sil.ExpMap.empty
      combined_pi
    |> Sil.ExpMap.filter (fun _ (taint, untaint) -> taint <> []  && untaint <> []) in
  (* TODO: in the future, we will have a richer taint domain that will require making sure that the
     "kind" (e.g. security, privacy) of the taint and untaint match, but for now we don't look at
     the untaint atoms *)
  let report_taint_errors e (taint_atoms, _untaint_atoms) =
    let report_one_error taint_atom =
      let taint_info = match Prop.atom_get_exp_attribute taint_atom with
        | Some (_, Sil.Ataint taint_info) -> taint_info
        | _ -> failwith "Expected to get taint attr on atom" in
      let err_desc =
        Errdesc.explain_tainted_value_reaching_sensitive_function
          calling_prop
          e
          taint_info
          callee_pname
          (State.get_loc ()) in
      let exn =
        Exceptions.Tainted_value_reaching_sensitive_function
          (err_desc, __POS__) in
      Reporting.log_warning caller_pname exn in
    IList.iter report_one_error taint_atoms in
  Sil.ExpMap.iter report_taint_errors taint_untaint_exp_map;
  (* filter out UNTAINT(e) atoms from [missing_pi] such that we have already reported a taint
     error on e. without doing this, we will get PRECONDITION_NOT_MET (and failed spec
     inference), which is bad. instead, what this does is effectively assume that the UNTAINT(e)
     precondition was met, and continue with the analysis under this assumption. this makes sense
     because we are reporting the taint error, but propagating a *safe* postcondition w.r.t to
     tainting. *)
  let not_untaint_atom atom = not
      (Sil.ExpMap.exists
         (fun _ (_, untaint_atoms) ->
            IList.exists
              (fun a -> Sil.atom_equal atom a)
              untaint_atoms)
         taint_untaint_exp_map) in
  IList.filter not_untaint_atom missing_pi_sub

let class_cast_exn pname_opt texp1 texp2 exp ml_loc =
  let desc =
    Errdesc.explain_class_cast_exception
      pname_opt texp1 texp2 exp (State.get_node ()) (State.get_loc ()) in
  Exceptions.Class_cast_exception (desc, ml_loc)

let raise_cast_exception ml_loc pname_opt texp1 texp2 exp =
  let exn = class_cast_exn pname_opt texp1 texp2 exp ml_loc in
  raise exn

let get_check_exn check callee_pname loc ml_loc = match check with
  | Prover.Bounds_check ->
      let desc = Localise.desc_precondition_not_met (Some Localise.Pnm_bounds) callee_pname loc in
      Exceptions.Precondition_not_met (desc, ml_loc)
  | Prover.Class_cast_check (texp1, texp2, exp) ->
      class_cast_exn (Some callee_pname) texp1 texp2 exp ml_loc

let check_uninitialize_dangling_deref callee_pname actual_pre sub formal_params props =
  IList.iter (fun (p, _ ) ->
      match check_dereferences callee_pname actual_pre sub p formal_params with
      | Some (Deref_undef_exp, desc) ->
          raise (Exceptions.Dangling_pointer_dereference (Some Sil.DAuninit, desc, __POS__))
      | _ -> ()) props

(** Perform symbolic execution for a single spec *)
let exe_spec
    tenv ret_ids (n, nspecs) caller_pdesc callee_pname  callee_attrs loc prop path_pre
    (spec : Prop.exposed Specs.spec) actual_params formal_params : abduction_res =
  let caller_pname = Cfg.Procdesc.get_proc_name caller_pdesc in
  let posts = mk_posts ret_ids prop callee_pname callee_attrs spec.Specs.posts in
  let actual_pre = mk_actual_precondition prop actual_params formal_params in
  let spec_pre =
    mk_pre (Specs.Jprop.to_prop spec.Specs.pre) formal_params callee_pname callee_attrs in
  L.d_strln ("EXECUTING SPEC " ^ string_of_int n ^ "/" ^ string_of_int nspecs);
  L.d_strln "ACTUAL PRECONDITION =";
  L.d_increase_indent 1; Prop.d_prop actual_pre; L.d_decrease_indent 1; L.d_ln ();
  L.d_strln "SPEC =";
  L.d_increase_indent 1; Specs.d_spec spec; L.d_decrease_indent 1; L.d_ln ();
  SymOp.pay(); (* pay one symop *)
  match Prover.check_implication_for_footprint caller_pname tenv actual_pre spec_pre with
  | Prover.ImplFail checks -> Invalid_res (Prover_checks checks)
  | Prover.ImplOK
      (checks, sub1, sub2, frame, missing_pi, missing_sigma,
       frame_fld, missing_fld, frame_typ, missing_typ) ->
      let log_check_exn check =
        let exn = get_check_exn check callee_pname loc __POS__ in
        Reporting.log_warning caller_pname exn in
      let do_split () =
        let missing_pi' =
          if !Config.taint_analysis then
            do_taint_check caller_pname callee_pname actual_pre missing_pi sub2
          else missing_pi in
        process_splitting
          actual_pre sub1 sub2 frame missing_pi' missing_sigma
          frame_fld missing_fld frame_typ missing_typ in
      let report_valid_res split =
        match combine
                ret_ids posts
                actual_pre path_pre split
                caller_pdesc callee_pname loc with
        | None -> Invalid_res Cannot_combine
        | Some results ->
            (* After combining we check that we have not added
               a points-to of initialized variables.*)
            check_uninitialize_dangling_deref
              callee_pname actual_pre split.sub formal_params results;
            let inconsistent_results, consistent_results =
              IList.partition (fun (p, _) -> Prover.check_inconsistency p) results in
            let incons_pre_missing = inconsistent_actualpre_missing actual_pre (Some split) in
            Valid_res { incons_pre_missing = incons_pre_missing;
                        vr_pi = split.missing_pi;
                        vr_sigma = split.missing_sigma;
                        vr_cons_res = consistent_results;
                        vr_incons_res = inconsistent_results } in
      begin
        IList.iter log_check_exn checks;
        let subbed_pre = (Prop.prop_sub sub1 actual_pre) in
        match check_dereferences callee_pname subbed_pre sub2 spec_pre formal_params with
        | Some (Deref_undef _, _) when !Config.angelic_execution ->
            let split = do_split () in
            report_valid_res split
        | Some (deref_error, desc) ->
            let rec join_paths = function
              | [] -> None
              | (_, p):: l ->
                  (match join_paths l with
                   | None -> Some p
                   | Some p' -> Some (Paths.Path.join p p')) in
            let pjoin = join_paths posts in (* join the paths from the posts *)
            Invalid_res (Dereference_error (deref_error, desc, pjoin))
        | None ->
            let split = do_split () in
            (* check if a missing_fld hpred is about a hidden field *)
            let hpred_missing_hidden = function
              | Sil.Hpointsto (_, Sil.Estruct ([(fld, _)], _), _) -> Ident.fieldname_is_hidden fld
              | _ -> false in
            (* missing fields minus hidden fields *)
            let missing_fld_nohidden =
              IList.filter (fun hp -> not (hpred_missing_hidden hp)) missing_fld in
            if !Config.footprint = false && split.missing_sigma != [] then
              begin
                L.d_strln "Implication error: missing_sigma not empty in re-execution";
                Invalid_res Missing_sigma_not_empty
              end
            else if !Config.footprint = false && missing_fld_nohidden != [] then
              begin
                L.d_strln "Implication error: missing_fld not empty in re-execution";
                Invalid_res Missing_fld_not_empty
              end
            else report_valid_res split
      end

let remove_constant_string_class prop =
  let filter = function
    | Sil.Hpointsto (Sil.Const (Sil.Cstr _ | Sil.Cclass _), _, _) -> false
    | _ -> true in
  let sigma = IList.filter filter (Prop.get_sigma prop) in
  let sigmafp = IList.filter filter (Prop.get_sigma_footprint prop) in
  let prop' = Prop.replace_sigma_footprint sigmafp (Prop.replace_sigma sigma prop) in
  Prop.normalize prop'

(** existentially quantify the path identifier generated
    by the prover to keep track of expansions of lhs paths
    and remove pointsto's whose lhs is a constant string *)
let quantify_path_idents_remove_constant_strings (prop: Prop.normal Prop.t) : Prop.normal Prop.t =
  let fav = Prop.prop_fav prop in
  Sil.fav_filter_ident fav Ident.is_path;
  remove_constant_string_class (Prop.exist_quantify fav prop)

(** Strengthen the footprint by adding pure facts from the current part *)
let prop_pure_to_footprint (p: 'a Prop.t) : Prop.normal Prop.t =
  let is_footprint_atom_not_attribute a =
    not (Prop.atom_is_attribute a)
    &&
    let a_fav = Sil.atom_fav a in
    Sil.fav_for_all a_fav Ident.is_footprint in
  let pure = Prop.get_pure p in
  let new_footprint_atoms = IList.filter is_footprint_atom_not_attribute pure in
  if new_footprint_atoms == []
  then p
  else (** add pure fact to footprint *)
    Prop.normalize (Prop.replace_pi_footprint (Prop.get_pi_footprint p @ new_footprint_atoms) p)

(** post-process the raw result of a function call *)
let exe_call_postprocess ret_ids trace_call callee_pname loc results =
  let filter_valid_res = function
    | Invalid_res _ -> false
    | Valid_res _ -> true in
  let valid_res0, invalid_res0 =
    IList.partition filter_valid_res results in
  let valid_res =
    IList.map (function Valid_res cr -> cr | Invalid_res _ -> assert false) valid_res0 in
  let invalid_res =
    IList.map (function Valid_res _ -> assert false | Invalid_res ir -> ir) invalid_res0 in
  let valid_res_miss_pi, valid_res_no_miss_pi =
    IList.partition (fun vr -> vr.vr_pi != []) valid_res in
  let _, valid_res_cons_pre_missing =
    IList.partition (fun vr -> vr.incons_pre_missing) valid_res in
  let deref_errors = IList.filter (function Dereference_error _ -> true | _ -> false) invalid_res in
  let print_pi pi =
    L.d_str "pi: "; Prop.d_pi pi; L.d_ln () in
  let call_desc kind_opt = Localise.desc_precondition_not_met kind_opt callee_pname loc in
  let res_with_path_idents =
    if !Config.footprint then
      begin
        if valid_res_cons_pre_missing == [] then
          (* no valid results where actual pre and missing are consistent *)
          begin
            if deref_errors <> [] then (* dereference error detected *)
              let extend_path path_opt path_pos_opt = match path_opt with
                | None -> ()
                | Some path_post ->
                    let old_path, _ = State.get_path () in
                    let new_path =
                      Paths.Path.add_call
                        (include_subtrace callee_pname) old_path callee_pname path_post in
                    State.set_path new_path path_pos_opt in
              match IList.hd deref_errors with
              | Dereference_error (Deref_minusone, desc, path_opt) ->
                  trace_call Specs.CallStats.CR_not_met;
                  extend_path path_opt None;
                  raise (Exceptions.Dangling_pointer_dereference
                           (Some Sil.DAminusone, desc, __POS__))
              | Dereference_error (Deref_undef_exp, desc, path_opt) ->
                  trace_call Specs.CallStats.CR_not_met;
                  extend_path path_opt None;
                  raise (Exceptions.Dangling_pointer_dereference
                           (Some Sil.DAuninit, desc, __POS__))
              | Dereference_error (Deref_null pos, desc, path_opt) ->
                  trace_call Specs.CallStats.CR_not_met;
                  extend_path path_opt (Some pos);
                  if Localise.is_parameter_not_null_checked_desc desc then
                    raise (Exceptions.Parameter_not_null_checked (desc, __POS__))
                  else if Localise.is_field_not_null_checked_desc desc then
                    raise (Exceptions.Field_not_null_checked (desc, __POS__))
                  else raise (Exceptions.Null_dereference (desc, __POS__))
              | Dereference_error (Deref_freed _, desc, path_opt) ->
                  trace_call Specs.CallStats.CR_not_met;
                  extend_path path_opt None;
                  raise (Exceptions.Use_after_free (desc, __POS__))
              | Dereference_error (Deref_undef (_, _, pos), desc, path_opt) ->
                  trace_call Specs.CallStats.CR_not_met;
                  extend_path path_opt (Some pos);
                  raise (Exceptions.Skip_pointer_dereference (desc, __POS__))
              | Prover_checks _
              | Cannot_combine
              | Missing_sigma_not_empty
              | Missing_fld_not_empty ->
                  trace_call Specs.CallStats.CR_not_met;
                  assert false
            else (* no dereference error detected *)
              let desc =
                if IList.exists (function Cannot_combine -> true | _ -> false) invalid_res then
                  call_desc (Some Localise.Pnm_dangling)
                else if IList.exists (function
                    | Prover_checks (check :: _) ->
                        trace_call Specs.CallStats.CR_not_met;
                        let exn = get_check_exn check callee_pname loc __POS__ in
                        raise exn
                    | _ -> false) invalid_res then
                  call_desc (Some Localise.Pnm_bounds)
                else call_desc None in
              trace_call Specs.CallStats.CR_not_met;
              raise (Exceptions.Precondition_not_met (desc, __POS__))
          end
        else (* combine the valid results, and store diverging states *)
          let process_valid_res vr =
            let save_diverging_states () =
              if not vr.incons_pre_missing && vr.vr_cons_res = []
              then (* no consistent results on one spec: divergence *)
                let incons_res =
                  IList.map
                    (fun (p, path) -> (prop_pure_to_footprint p, path))
                    vr.vr_incons_res in
                State.add_diverging_states (Paths.PathSet.from_renamed_list incons_res) in
            save_diverging_states ();
            vr.vr_cons_res in
          IList.map
            (fun (p, path) -> (prop_pure_to_footprint p, path))
            (IList.flatten (IList.map process_valid_res valid_res))
      end
    else if valid_res_no_miss_pi != [] then
      IList.flatten (IList.map (fun vr -> vr.vr_cons_res) valid_res_no_miss_pi)
    else if valid_res_miss_pi == [] then
      raise (Exceptions.Precondition_not_met (call_desc None, __POS__))
    else
      begin
        L.d_strln "Missing pure facts for the function call:";
        IList.iter print_pi (IList.map (fun vr -> vr.vr_pi) valid_res_miss_pi);
        match
          Prover.find_minimum_pure_cover
            (IList.map (fun vr -> (vr.vr_pi, vr.vr_cons_res)) valid_res_miss_pi) with
        | None ->
            trace_call Specs.CallStats.CR_not_met;
            raise (Exceptions.Precondition_not_met (call_desc None, __POS__))
        | Some cover ->
            L.d_strln "Found minimum cover";
            IList.iter print_pi (IList.map fst cover);
            IList.flatten (IList.map snd cover)
      end in
  trace_call Specs.CallStats.CR_success;
  let res =
    IList.map
      (fun (p, path) -> (quantify_path_idents_remove_constant_strings p, path))
      res_with_path_idents in
  let should_add_ret_attr _ =
    let is_likely_getter = function
      | Procname.Java pn_java ->
          IList.length (Procname.java_get_parameters pn_java) = 0
      | _ ->
          false in
    !Config.idempotent_getters &&
    !Config.curr_language = Config.Java &&
    is_likely_getter callee_pname in
  match ret_ids with
  | [ret_id] when should_add_ret_attr ()->
      (* add attribute to remember what function call a return id came from *)
      let ret_var = Sil.Var ret_id in
      let mark_id_as_retval (p, path) =
        let att_retval = Sil.Aretval callee_pname in
        Prop.set_exp_attribute p ret_var att_retval, path in
      IList.map mark_id_as_retval res
  | _ -> res

(** Execute the function call and return the list of results with return value *)
let exe_function_call
    callee_attrs tenv ret_ids caller_pdesc callee_pname loc actual_params prop path =
  let caller_pname = Cfg.Procdesc.get_proc_name caller_pdesc in
  let trace_call res =
    match Specs.get_summary caller_pname with
    | None -> ()
    | Some summary ->
        Specs.CallStats.trace
          summary.Specs.stats.Specs.call_stats callee_pname loc res !Config.footprint in
  let spec_list, formal_params = spec_find_rename trace_call callee_pname in
  let nspecs = IList.length spec_list in
  L.d_strln
    ("Found " ^
     string_of_int nspecs ^
     " specs for function " ^
     Procname.to_string callee_pname);
  L.d_strln ("START EXECUTING SPECS FOR " ^ Procname.to_string callee_pname ^ " from state");
  Prop.d_prop prop; L.d_ln ();
  let exe_one_spec (n, spec) =
    exe_spec
      tenv
      ret_ids
      (n, nspecs)
      caller_pdesc
      callee_pname
      callee_attrs
      loc
      prop
      path
      spec
      actual_params
      formal_params in
  let results = IList.map exe_one_spec spec_list in
  exe_call_postprocess ret_ids trace_call callee_pname loc results

(*
let check_splitting_precondition sub1 sub2 =
  let dom1 = Sil.sub_domain sub1 in
  let rng1 = Sil.sub_range sub1 in
  let dom2 = Sil.sub_domain sub2 in
  let rng2 = Sil.sub_range sub2 in
  let overlap = IList.exists (fun id -> IList.exists (Ident.equal id) dom1) dom2 in
  if overlap then begin
    L.d_str "Dom(Sub1): "; Sil.d_exp_list (IList.map (fun id -> Sil.Var id) dom1); L.d_ln ();
    L.d_str "Ran(Sub1): "; Sil.d_exp_list rng1; L.d_ln ();
    L.d_str "Dom(Sub2): "; Sil.d_exp_list (IList.map (fun id -> Sil.Var id) dom2); L.d_ln ();
    L.d_str "Ran(Sub2): "; Sil.d_exp_list rng2; L.d_ln ();
    assert false
  end

(** check whether 0|->- occurs in sigma *)
let sigma_has_null_pointer sigma =
  let hpred_null_pointer = function
    | Sil.Hpointsto (e, _, _) ->
        Sil.exp_equal e Sil.exp_zero
    | _ -> false in
  IList.exists hpred_null_pointer sigma
*)
