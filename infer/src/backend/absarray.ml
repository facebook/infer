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

(** Abstraction for Arrays *)

module L = Logging
module F = Format

type sigma = Sil.hpred list

(** Matcher for the sigma part specialized to strexps *)
module StrexpMatch : sig
  (** path through a strexp *)
  type path

  (** convert a path into a list of expressions *)
  val path_to_exps : path -> Sil.exp list

  (** create a path from a root and a list of offsets *)
  val path_from_exp_offsets : Sil.exp -> Sil.offset list -> path

  (** path to the root, size, elements and type of a new_array *)
  type strexp_data = path * Sil.strexp * Sil.typ

  (** sigma with info about a current array *)
  type t

  (** Find a strexp at the given path. Can raise [Not_found] *)
  val find_path : sigma -> path -> t

  (** Find a strexp with the given property. *)
  val find : sigma -> (strexp_data -> bool) -> t list

  (** Get the array *)
  val get_data : t -> strexp_data

  (** Replace the strexp at a given position by a new strexp *)
  val replace_strexp : bool -> t -> Sil.strexp -> sigma

  (** Replace the index in the array at a given position with the new index *)
  val replace_index : bool -> t -> Sil.exp -> Sil.exp -> sigma
(*
  (** Get the partition of the sigma: the unmatched part of the sigma and the matched hpred *)
  val get_sigma_partition : t -> sigma * Sil.hpred

  (** Replace the strexp and the unmatched part of the sigma by the givn inputs *)
  val replace_strexp_sigma : bool -> t -> Sil.strexp -> sigma -> sigma
*)
end = struct

  (** syntactic offset *)
  type syn_offset = Field of Ident.fieldname * Sil.typ | Index of Sil.exp

  (** path through an Estruct *)
  type path = Sil.exp * (syn_offset list)

  (** Find a strexp and a type at the given syntactic offset list *)
  let rec get_strexp_at_syn_offsets se t syn_offs =
    match se, t, syn_offs with
    | _, _, [] -> (se, t)
    | Sil.Estruct (fsel, _), Sil.Tstruct { Sil.instance_fields }, Field (fld, _) :: syn_offs' ->
        let se' = snd (IList.find (fun (f', _) -> Sil.fld_equal f' fld) fsel) in
        let t' = (fun (_,y,_) -> y)
            (IList.find (fun (f', _, _) ->
                 Sil.fld_equal f' fld) instance_fields) in
        get_strexp_at_syn_offsets se' t' syn_offs'
    | Sil.Earray (_, esel, _), Sil.Tarray(t', _), Index ind :: syn_offs' ->
        let se' = snd (IList.find (fun (i', _) -> Sil.exp_equal i' ind) esel) in
        get_strexp_at_syn_offsets se' t' syn_offs'
    | _ ->
        L.d_strln "Failure of get_strexp_at_syn_offsets";
        L.d_str "se: "; Sil.d_sexp se; L.d_ln ();
        L.d_str "t: "; Sil.d_typ_full t; L.d_ln ();
        assert false

  (** Replace a strexp at the given syntactic offset list *)
  let rec replace_strexp_at_syn_offsets se t syn_offs update =
    match se, t, syn_offs with
    | _, _, [] ->
        update se
    | Sil.Estruct (fsel, inst), Sil.Tstruct { Sil.instance_fields }, Field (fld, _) :: syn_offs' ->
        let se' = snd (IList.find (fun (f', _) -> Sil.fld_equal f' fld) fsel) in
        let t' = (fun (_,y,_) -> y)
            (IList.find (fun (f', _, _) ->
                 Sil.fld_equal f' fld) instance_fields) in
        let se_mod = replace_strexp_at_syn_offsets se' t' syn_offs' update in
        let fsel' = IList.map (fun (f'', se'') -> if Sil.fld_equal f'' fld then (fld, se_mod) else (f'', se'')) fsel in
        Sil.Estruct (fsel', inst)
    | Sil.Earray (size, esel, inst), Sil.Tarray (t', _), Index idx :: syn_offs' ->
        let se' = snd (IList.find (fun (i', _) -> Sil.exp_equal i' idx) esel) in
        let se_mod = replace_strexp_at_syn_offsets se' t' syn_offs' update in
        let esel' = IList.map (fun ese -> if Sil.exp_equal (fst ese) idx then (idx, se_mod) else ese) esel in
        Sil.Earray (size, esel', inst)
    | _ -> assert false

  (** convert a path into an expression *)
  let path_to_exps (root, syn_offs_in) =
    let rec convert acc = function
      | [] -> acc
      | Field (f, t) :: syn_offs' ->
          let acc' = IList.map (fun e -> Sil.Lfield (e, f, t)) acc in
          convert acc' syn_offs'
      | Index idx :: syn_offs' ->
          let acc' = IList.map (fun e -> Sil.Lindex (e, idx)) acc in
          convert acc' syn_offs' in
    begin
      convert [root] syn_offs_in
    end

  (** create a path from a root and a list of offsets *)
  let path_from_exp_offsets root offs =
    let offset_to_syn_offset = function
      | Sil.Off_fld (fld, typ) -> Field (fld, typ)
      | Sil.Off_index idx -> Index idx in
    let syn_offs = IList.map offset_to_syn_offset offs in
    (root, syn_offs)

  (** path to the root, size, elements and type of a new_array *)
  type strexp_data = path * Sil.strexp * Sil.typ

  (** Store hpred using physical equality, and offset list for an array *)
  type t = sigma * Sil.hpred * (syn_offset list)

  (** Find an array at the given path. Can raise [Not_found] *)
  let find_path sigma (root, syn_offs) : t =
    let filter = function
      | Sil.Hpointsto (e, _, _) -> Sil.exp_equal root e
      | _ -> false in
    let hpred = IList.find filter sigma in
    (sigma, hpred, syn_offs)

  (** Find a sub strexp with the given property. Can raise [Not_found] *)
  let find (sigma : sigma) (pred : strexp_data -> bool) : t list =
    let found = ref [] in
    let rec find_offset_sexp sigma_other hpred root offs se typ =
      let offs' = IList.rev offs in
      let path = (root, offs') in
      if pred (path, se, typ) then found := (sigma, hpred, offs') :: !found
      else begin
        match se, typ with
        | Sil.Estruct (fsel, _), Sil.Tstruct { Sil.instance_fields } ->
            find_offset_fsel sigma_other hpred root offs fsel instance_fields typ
        | Sil.Earray (_, esel, _), Sil.Tarray (t, _) ->
            find_offset_esel sigma_other hpred root offs esel t
        | _ -> ()
      end
    and find_offset_fsel sigma_other hpred root offs fsel ftal typ = match fsel with
      | [] -> ()
      | (f, se) :: fsel' ->
          begin
            try
              let t = (fun (_,y,_) -> y) (IList.find (fun (f', _, _) -> Sil.fld_equal f' f) ftal) in
              find_offset_sexp sigma_other hpred root ((Field (f, typ)) :: offs) se t
            with Not_found ->
              L.d_strln ("Can't find field " ^ (Ident.fieldname_to_string f) ^ " in StrexpMatch.find")
          end;
          find_offset_fsel sigma_other hpred root offs fsel' ftal typ
    and find_offset_esel sigma_other hpred root offs esel t = match esel with
      | [] -> ()
      | (ind, se) :: esel' ->
          begin
            find_offset_sexp sigma_other hpred root ((Index ind):: offs) se t;
            find_offset_esel sigma_other hpred root offs esel' t
          end in
    let rec iterate sigma_seen = function
      | [] -> ()
      | hpred :: sigma_rest ->
          begin
            match hpred with
            | Sil.Hpointsto (root, se, te) ->
                let sigma_other = sigma_seen @ sigma_rest in
                find_offset_sexp sigma_other hpred root [] se (Sil.texp_to_typ None te)
            | _ -> ()
          end;
          iterate (hpred:: sigma_seen) sigma_rest in
    begin
      iterate [] sigma;
      !found
    end

  (** Get the matched strexp *)
  let get_data ((_ , hpred, syn_offs) : t) = match hpred with
    | Sil.Hpointsto (root, se, te) ->
        let t = Sil.texp_to_typ None te in
        let se', t' = get_strexp_at_syn_offsets se t syn_offs in
        let path' = (root, syn_offs) in
        (path', se', t')
    | _ -> assert false

  (** Replace the current hpred *)
  let replace_hpred ((sigma, hpred, _) : t) hpred' =
    IList.map (fun hpred'' -> if hpred''== hpred then hpred' else hpred'') sigma

  (** Replace the strexp at the given offset in the given hpred *)
  let hpred_replace_strexp footprint_part hpred syn_offs update =
    let update se' =
      let se_in = update se' in
      match se', se_in with
      | Sil.Earray (size, esel, _), Sil.Earray (_, esel_in, inst2) ->
          let orig_indices = IList.map fst esel in
          let index_is_not_new idx = IList.exists (Sil.exp_equal idx) orig_indices in
          let process_index idx =
            if index_is_not_new idx then idx else (Sil.array_clean_new_index footprint_part idx) in
          let esel_in' = IList.map (fun (idx, se) -> process_index idx, se) esel_in in
          Sil.Earray (size, esel_in', inst2)
      | _, _ -> se_in in
    begin
      match hpred with
      | Sil.Hpointsto (root, se, te) ->
          let t = Sil.texp_to_typ None te in
          let se' = replace_strexp_at_syn_offsets se t syn_offs update in
          Sil.Hpointsto (root, se', te)
      | _ -> assert false
    end

  (** Replace the strexp at a given position by a new strexp *)
  let replace_strexp footprint_part ((sigma, hpred, syn_offs) : t) se_in =
    let update _ = se_in in
    let hpred' = hpred_replace_strexp footprint_part hpred syn_offs update in
    replace_hpred (sigma, hpred, syn_offs) hpred'

  (** Replace the index in the array at a given position with the new index *)
  let replace_index footprint_part ((sigma, hpred, syn_offs) : t) (index: Sil.exp) (index': Sil.exp) =
    let update se' =
      match se' with
      | Sil.Earray (size, esel, inst) ->
          let esel' = IList.map (fun (e', se') -> if Sil.exp_equal e' index then (index', se') else (e', se')) esel in
          Sil.Earray (size, esel', inst)
      | _ -> assert false in
    let hpred' = hpred_replace_strexp footprint_part hpred syn_offs update in
    replace_hpred (sigma, hpred, syn_offs) hpred'
(*
  (** Get the partition of the sigma: the unmatched part of the sigma and the matched hpred *)
  let get_sigma_partition (sigma, hpred, _) =
    let sigma_unmatched = IList.filter (fun hpred' -> not (hpred' == hpred)) sigma in
    (sigma_unmatched, hpred)

  (** Replace the strexp and the unmatched part of the sigma by the given inputs *)
  let replace_strexp_sigma footprint_part ((_, hpred, syn_offs) : t) se_in sigma_in =
    let new_sigma = hpred :: sigma_in in
    let sigma' = replace_strexp footprint_part (new_sigma, hpred, syn_offs) se_in in
    IList.sort Sil.hpred_compare sigma'
*)
end

(** This function renames expressions in [p]. The renaming is, roughly
    speaking, to replace [path.i] by [path.i'] for all (i, i') in [map]. *)
let prop_replace_path_index
    (p: Prop.exposed Prop.t)
    (path: StrexpMatch.path)
    (map : (Sil.exp * Sil.exp) list) : Prop.exposed Prop.t
  =
  let elist_path = StrexpMatch.path_to_exps path in
  let expmap_list =
    IList.fold_left (fun acc_outer e_path ->
        IList.fold_left (fun acc_inner (old_index, new_index) ->
            let old_e_path_index = Prop.exp_normalize_prop p (Sil.Lindex(e_path, old_index)) in
            let new_e_path_index = Prop.exp_normalize_prop p (Sil.Lindex(e_path, new_index)) in
            (old_e_path_index, new_e_path_index) :: acc_inner
          ) acc_outer map
      ) [] elist_path in
  let expmap_fun e' =
    try
      let _, fresh_e = IList.find (fun (e, _) -> Sil.exp_equal e e') expmap_list in
      fresh_e
    with Not_found -> e' in
  Prop.prop_expmap expmap_fun p

(** This function uses [update] and transforms the two sigma parts of [p],
    the sigma of the current SH of [p] and that of the footprint of [p]. *)
let prop_update_sigma_and_fp_sigma
    (p : Prop.normal Prop.t)
    (update : bool -> sigma -> sigma * bool) : Prop.normal Prop.t * bool
  =
  let sigma', changed = update false (Prop.get_sigma p) in
  let ep1 = Prop.replace_sigma sigma' p in
  let ep2, changed2 =
    if !Config.footprint then
      let sigma_fp', changed' = update true (Prop.get_sigma_footprint ep1) in
      (Prop.replace_sigma_footprint sigma_fp' ep1, changed')
    else (ep1, false) in
  (Prop.normalize ep2, changed || changed2)

(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)
let array_abstraction_performed = ref false

(** This function abstracts strexps. The parameter [can_abstract] spots strexps
    where the abstraction might be applicable, and the parameter [do_abstract] does
    the abstraction to those spotted strexps. *)
let generic_strexp_abstract
    (abstraction_name : string)
    (p_in : Prop.normal Prop.t)
    (can_abstract_ : StrexpMatch.strexp_data -> bool)
    (do_abstract : bool -> Prop.normal Prop.t -> StrexpMatch.strexp_data -> Prop.normal Prop.t * bool)
  : Prop.normal Prop.t
  =
  let can_abstract data =
    let r = can_abstract_ data in
    if r then array_abstraction_performed := true;
    r in
  let find_strexp_to_abstract p0 =
    let find sigma = StrexpMatch.find sigma can_abstract in
    let matchings_cur = find (Prop.get_sigma p0) in
    let matchings_fp = find (Prop.get_sigma_footprint p0) in
    matchings_cur, matchings_fp in
  let match_select_next (matchings_cur, matchings_fp) =
    match matchings_cur, matchings_fp with
    | [], [] -> raise Not_found
    | matched :: cur', fp' -> matched, false, (cur', fp')
    | [], matched :: fp' -> matched, true, ([], fp') in
  let rec match_abstract p0 matchings_cur_fp =
    try
      let matched, footprint_part, matchings_cur_fp' = match_select_next matchings_cur_fp in
      let n = IList.length (snd matchings_cur_fp') + 1 in
      if !Config.trace_absarray then (L.d_strln ("Num of fp candidates " ^ (string_of_int n)));
      let strexp_data = StrexpMatch.get_data matched in
      let p1, changed = do_abstract footprint_part p0 strexp_data in
      if changed then (p1, true)
      else match_abstract p0 matchings_cur_fp'
    with
    | Not_found -> (p0, false) in
  let rec find_then_abstract bound p0 =
    if bound = 0 then p0
    else begin
      if !Config.trace_absarray then (L.d_strln ("Applying " ^ abstraction_name ^ " to"); Prop.d_prop p0; L.d_ln (); L.d_ln ());
      let matchings_cur_fp = find_strexp_to_abstract p0 in
      let p1, changed = match_abstract p0 matchings_cur_fp in
      if changed then find_then_abstract (bound - 1) p1 else p0
    end in
  let matchings_cur, matchings_fp = find_strexp_to_abstract p_in in
  let num_matches = (IList.length matchings_cur) + (IList.length matchings_fp) in
  begin
    find_then_abstract num_matches p_in
  end


(** Return [true] if there's a pointer to the index *)
let index_is_pointed_to (p: Prop.normal Prop.t) (path: StrexpMatch.path) (index: Sil.exp) : bool =
  let indices =
    let index_plus_one = Sil.BinOp(Sil.PlusA, index, Sil.exp_one) in
    [index; index_plus_one] in
  let add_index_to_paths =
    let elist_path = StrexpMatch.path_to_exps path in
    let add_index i e = Prop.exp_normalize_prop p (Sil.Lindex(e, i)) in
    fun i -> IList.map (add_index i) elist_path in
  let pointers = IList.flatten (IList.map add_index_to_paths indices) in
  let filter = function
    | Sil.Hpointsto (_, Sil.Eexp (e, _), _) -> IList.exists (Sil.exp_equal e) pointers
    | _ -> false in
  IList.exists filter (Prop.get_sigma p)


(** Given [p] containing an array at [path], blur [index] in it *)
let blur_array_index
    (p: Prop.normal Prop.t)
    (path: StrexpMatch.path)
    (index: Sil.exp) : Prop.normal Prop.t
  =
  try
    let fresh_index = Sil.Var (Ident.create_fresh (if !Config.footprint then Ident.kfootprint else Ident.kprimed)) in
    let p2 =
      try
        if !Config.footprint then
          begin
            let sigma_fp = Prop.get_sigma_footprint p in
            let matched_fp = StrexpMatch.find_path sigma_fp path in
            let sigma_fp' = StrexpMatch.replace_index true matched_fp index fresh_index in
            Prop.replace_sigma_footprint sigma_fp' p
          end
        else Prop.expose p
      with Not_found -> Prop.expose p in
    let p3 =
      let matched = StrexpMatch.find_path (Prop.get_sigma p) path in
      let sigma' = StrexpMatch.replace_index false matched index fresh_index in
      Prop.replace_sigma sigma' p2 in
    let p4 =
      let index_next = Sil.BinOp(Sil.PlusA, index, Sil.exp_one) in
      let fresh_index_next = Sil.BinOp (Sil.PlusA, fresh_index, Sil.exp_one) in
      let map = [(index, fresh_index); (index_next, fresh_index_next)] in
      prop_replace_path_index p3 path map in
    Prop.normalize p4
  with Not_found -> p


(** Given [p] containing an array at [root], blur [indices] in it *)
let blur_array_indices
    (p: Prop.normal Prop.t)
    (root: StrexpMatch.path)
    (indices: Sil.exp list) : Prop.normal Prop.t * bool
  =
  let f prop index = blur_array_index prop root index in
  (IList.fold_left f p indices, IList.length indices > 0)


(** Given [p] containing an array at [root], only keep [indices] in it *)
let keep_only_indices
    (p: Prop.normal Prop.t)
    (path: StrexpMatch.path)
    (indices: Sil.exp list) : Prop.normal Prop.t * bool
  =
  let prune_sigma footprint_part sigma =
    try
      let matched = StrexpMatch.find_path sigma path in
      let (_, se, _) = StrexpMatch.get_data matched in
      match se with
      | Sil.Earray (size, esel, inst) ->
          let esel', esel_leftover' = IList.partition (fun (e, _) -> IList.exists (Sil.exp_equal e) indices) esel in
          if esel_leftover' == [] then (sigma, false)
          else begin
            let se' = Sil.Earray (size, esel', inst) in
            let sigma' = StrexpMatch.replace_strexp footprint_part matched se' in
            (sigma', true)
          end
      | _ -> (sigma, false)
    with Not_found -> (sigma, false) in
  prop_update_sigma_and_fp_sigma p prune_sigma


(** If the type is array, check whether we should do abstraction *)
let array_typ_can_abstract = function
  | Sil.Tarray (Sil.Tptr (Sil.Tfun _, _), _) -> false (* don't abstract arrays of pointers *)
  | _ -> true

(** This function checks whether we can apply an abstraction to a strexp *)
let strexp_can_abstract ((_, se, typ) : StrexpMatch.strexp_data) : bool =
  let can_abstract_se = match se with
    | Sil.Earray (_, esel, _) ->
        let len = IList.length esel in
        len > 1
    | _ -> false in
  can_abstract_se && array_typ_can_abstract typ


(** This function abstracts a strexp *)
let strexp_do_abstract
    footprint_part p ((path, se_in, _) : StrexpMatch.strexp_data) : Prop.normal Prop.t * bool =
  if !Config.trace_absarray && footprint_part then (L.d_str "strexp_do_abstract (footprint)"; L.d_ln ());
  if !Config.trace_absarray && not footprint_part then (L.d_str "strexp_do_abstract (nonfootprint)"; L.d_ln ());
  let prune_and_blur d_keys keep blur path keep_keys blur_keys =
    let p2, changed2 =
      if !Config.trace_absarray then (L.d_str "keep "; d_keys keep_keys; L.d_ln ());
      keep p path keep_keys in
    let p3, changed3 =
      if blur_keys == [] then (p2, false)
      else begin
        if !Config.trace_absarray then (L.d_str "blur "; d_keys blur_keys; L.d_ln ());
        blur p2 path blur_keys
      end in
    if !Config.trace_absarray then (L.d_strln "Returns"; Prop.d_prop p3; L.d_ln (); L.d_ln ());
    (p3, changed2 || changed3) in
  let prune_and_blur_indices =
    prune_and_blur Sil.d_exp_list keep_only_indices blur_array_indices in

  let partition_abstract should_keep abstract ksel default_keys =
    let keep_ksel, remove_ksel = IList.partition should_keep ksel in
    let keep_keys, _, _ =
      IList.map fst keep_ksel, IList.map fst remove_ksel, IList.map fst ksel in
    let keep_keys' = if keep_keys == [] then default_keys else keep_keys in
    abstract keep_keys' keep_keys' in
  let do_array_footprint esel =
    (* array case footprint: keep only the last index, and blur it *)
    let should_keep (i0, _) = index_is_pointed_to p path i0 in
    let abstract = prune_and_blur_indices path in
    let default_indices =
      match IList.map fst esel with
      | [] -> []
      | indices -> [IList.hd (IList.rev indices)] (* keep last key at least *) in
    partition_abstract should_keep abstract esel default_indices in
  let do_footprint () =
    match se_in with
    | Sil.Earray (_, esel, _) -> do_array_footprint esel
    | _ -> assert false in

  let filter_abstract d_keys should_keep abstract ksel default_keys =
    let keep_ksel = IList.filter should_keep ksel in
    let keep_keys = IList.map fst keep_ksel in
    let keep_keys' = if keep_keys == [] then default_keys else keep_keys in
    if !Config.trace_absarray then (L.d_str "keep "; d_keys keep_keys'; L.d_ln ());
    abstract keep_keys' [] in
  let do_array_reexecution esel =
    (* array case re-execution: remove and blur constant and primed indices *)
    let is_pointed index = index_is_pointed_to p path index in
    let should_keep (index, _) = match index with
      | Sil.Const _ -> is_pointed index
      | Sil.Var id -> Ident.is_normal id || is_pointed index
      | _ -> false in
    let abstract = prune_and_blur_indices path in
    filter_abstract Sil.d_exp_list should_keep abstract esel [] in
  let do_reexecution () =
    match se_in with
    | Sil.Earray (_, esel, _) -> do_array_reexecution esel
    | _ -> assert false in

  if !Config.footprint then do_footprint ()
  else do_reexecution ()

let strexp_abstract (p : Prop.normal Prop.t) : Prop.normal Prop.t =
  generic_strexp_abstract "strexp_abstract" p strexp_can_abstract strexp_do_abstract

let report_error prop =
  L.d_strln "Check after array abstraction: FAIL";
  Prop.d_prop prop; L.d_ln ();
  assert false

(** Check performed after the array abstraction to see whether it was successful. Raise assert false in case of failure *)
let check_after_array_abstraction prop =
  let check_index root offs (ind, _) =
    if !Config.footprint then
      let path = StrexpMatch.path_from_exp_offsets root offs in
      index_is_pointed_to prop path ind
    else not (Sil.fav_exists (Sil.exp_fav ind) Ident.is_primed) in
  let rec check_se root offs typ = function
    | Sil.Eexp _ -> ()
    | Sil.Earray (_, esel, _) -> (* check that no more than 2 elements are in the array *)
        let typ_elem = Sil.array_typ_elem (Some Sil.Tvoid) typ in
        if IList.length esel > 2 && array_typ_can_abstract typ then
          if IList.for_all (check_index root offs) esel then ()
          else report_error prop
        else IList.iter (fun (ind, se) -> check_se root (offs @ [Sil.Off_index ind]) typ_elem se) esel
    | Sil.Estruct (fsel, _) ->
        IList.iter (fun (f, se) ->
            let typ_f = Sil.struct_typ_fld (Some Sil.Tvoid) f typ in
            check_se root (offs @ [Sil.Off_fld (f, typ)]) typ_f se) fsel in
  let check_hpred = function
    | Sil.Hpointsto (root, se, texp) ->
        let typ = Sil.texp_to_typ (Some Sil.Tvoid) texp in
        check_se root [] typ se
    | Sil.Hlseg _ | Sil.Hdllseg _ -> () in
  let check_sigma sigma = IList.iter check_hpred sigma in
  (* check_footprint_pure prop; *)
  check_sigma (Prop.get_sigma prop);
  check_sigma (Prop.get_sigma_footprint prop)

(** Apply array abstraction and check the result *)
let abstract_array_check p =
  let p_res = strexp_abstract p in
  check_after_array_abstraction p_res;
  p_res

(** remove redundant elements in an array *)
let remove_redundant_elements prop =
  Prop.d_prop prop; L.d_ln ();
  let occurs_at_most_once : Ident.t -> bool = (* the variable occurs at most once in the footprint or current part *)
    let fav_curr = Sil.fav_new () in
    let fav_foot = Sil.fav_new () in
    Sil.fav_duplicates := true;
    Sil.sub_fav_add fav_curr (Prop.get_sub prop);
    Prop.pi_fav_add fav_curr (Prop.get_pi prop);
    Prop.sigma_fav_add fav_curr (Prop.get_sigma prop);
    Prop.pi_fav_add fav_foot (Prop.get_pi_footprint prop);
    Prop.sigma_fav_add fav_foot (Prop.get_sigma_footprint prop);
    let favl_curr = Sil.fav_to_list fav_curr in
    let favl_foot = Sil.fav_to_list fav_foot in
    Sil.fav_duplicates := false;
    (* L.d_str "favl_curr "; IList.iter (fun id -> Sil.d_exp (Sil.Var id)) favl_curr; L.d_ln();
       L.d_str "favl_foot "; IList.iter (fun id -> Sil.d_exp (Sil.Var id)) favl_foot; L.d_ln(); *)
    let num_occur l id = IList.length (IList.filter (fun id' -> Ident.equal id id') l) in
    let at_most_once v =
      num_occur favl_curr v <= 1 && num_occur favl_foot v <= 1 in
    at_most_once in
  let modified = ref false in
  let filter_redundant_e_se fp_part (e, se) =
    let remove () =
      L.d_strln "kill_redundant: removing "; Sil.d_exp e; L.d_str " "; Sil.d_sexp se; L.d_ln();
      array_abstraction_performed := true;
      modified := true;
      false in
    match e, se with
    | Sil.Const (Sil.Cint i), Sil.Eexp (Sil.Var id, _) when (not fp_part || Sil.Int.iszero i) && Ident.is_normal id = false && occurs_at_most_once id ->
        remove () (* unknown value can be removed in re-execution mode or if the index is zero *)
    | Sil.Var id, Sil.Eexp _ when Ident.is_normal id = false && occurs_at_most_once id ->
        remove () (* index unknown can be removed *)
    | _ -> true in
  let remove_redundant_se fp_part = function
    | Sil.Earray (size, esel, inst) ->
        let esel' = IList.filter (filter_redundant_e_se fp_part) esel in
        Sil.Earray (size, esel', inst)
    | se -> se in
  let remove_redundant_hpred fp_part = function
    | Sil.Hpointsto (e, se, te) ->
        let se' = remove_redundant_se fp_part se in
        Sil.Hpointsto (e, se', te)
    | hpred -> hpred in
  let remove_redundant_sigma fp_part sigma = IList.map (remove_redundant_hpred fp_part) sigma in
  let sigma' = remove_redundant_sigma false (Prop.get_sigma prop) in
  let foot_sigma' = remove_redundant_sigma true (Prop.get_sigma_footprint prop) in
  if !modified then
    let prop' = Prop.replace_sigma sigma' (Prop.replace_sigma_footprint foot_sigma' prop) in
    Prop.normalize prop'
  else prop

(*
(** This function uses [update] and transforms the sigma of the
    current SH of [p] or that of the footprint of [p], depending on
    [footprint_part]. *)
let prop_update_sigma_or_fp_sigma
    (footprint_part : bool) (p : Prop.normal Prop.t) (update : bool -> sigma -> sigma * bool)
  : Prop.normal Prop.t * bool =
  let ep1, changed1 =
    if footprint_part then (Prop.expose p, false)
    else
      let sigma', changed = update false (Prop.get_sigma p) in
      (Prop.replace_sigma sigma' p, changed) in
  let ep2, changed2 =
    if not footprint_part then (ep1, false)
    else
      begin
        (if not !Config.footprint then assert false); (* always run in the footprint mode *)
        let sigma_fp', changed = update true (Prop.get_sigma_footprint ep1) in
        (Prop.replace_sigma_footprint sigma_fp' ep1, changed)
      end in
  (Prop.normalize ep2, changed1 || changed2)

let check_footprint_pure prop =
  let fav_pure = Sil.fav_new () in
  Prop.pi_fav_add fav_pure (Prop.get_pure prop @ Prop.get_pi_footprint prop);
  let fav_sigma = Sil.fav_new () in
  Prop.sigma_fav_add fav_sigma (Prop.get_sigma prop @ Prop.get_sigma_footprint prop);
  Sil.fav_filter_ident fav_pure Ident.is_footprint;
  Sil.fav_filter_ident fav_sigma Ident.is_footprint;
  if not (Sil.fav_subset_ident fav_pure fav_sigma)
  then (L.d_strln "footprint vars in pure and not in sigma"; report_error prop)
*)
