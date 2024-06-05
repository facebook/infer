(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** This function should be used before adding a new index to Earray. The [exp] is the newly created
    index. This function "cleans" [exp] according to whether it is the footprint or current part of
    the prop. The function faults in the re - execution mode, as an internal check of the tool. *)
let array_clean_new_index footprint_part new_idx =
  assert (not (footprint_part && not !BiabductionConfig.footprint)) ;
  if
    footprint_part
    && Exp.free_vars new_idx |> Sequence.exists ~f:(fun id -> not (Ident.is_footprint id))
  then (
    L.d_warning
      ( "Array index " ^ Exp.to_string new_idx
      ^ " has non-footprint vars: replaced by fresh footprint var" ) ;
    L.d_ln () ;
    let id = Ident.create_fresh Ident.kfootprint in
    Exp.Var id )
  else new_idx


(** Abstraction for Arrays *)

type sigma = Predicates.hpred list

(** Matcher for the sigma part specialized to strexps *)
module StrexpMatch : sig
  (** path through a strexp *)
  type path

  val path_to_exps : path -> Exp.t list
  (** convert a path into a list of expressions *)

  val path_from_exp_offsets : Exp.t -> Predicates.offset list -> path
  (** create a path from a root and a list of offsets *)

  (** path to the root, length, elements and type of a new_array *)
  type strexp_data = path * Predicates.strexp * Typ.t

  (** sigma with info about a current array *)
  type t

  val find_path : sigma -> path -> t
  (** Find a strexp at the given path. Can raise [Not_found_s/Caml.Not_found] *)

  val find : Tenv.t -> sigma -> (strexp_data -> bool) -> t list
  (** Find a strexp with the given property. *)

  val get_data : Tenv.t -> t -> strexp_data
  (** Get the array *)

  val replace_strexp : Tenv.t -> bool -> t -> Predicates.strexp -> sigma
  (** Replace the strexp at a given position by a new strexp *)

  val replace_index : Tenv.t -> bool -> t -> Exp.t -> Exp.t -> sigma
  (** Replace the index in the array at a given position with the new index *)
end = struct
  (** syntactic offset *)
  type syn_offset = Field of Fieldname.t * Typ.t | Index of Exp.t

  (** path through an Estruct *)
  type path = Exp.t * syn_offset list

  (** Find a strexp and a type at the given syntactic offset list *)
  let rec get_strexp_at_syn_offsets tenv se (t : Typ.t) syn_offs =
    let fail () =
      L.d_strln "Failure of get_strexp_at_syn_offsets" ;
      L.d_str "se: " ;
      Predicates.d_sexp se ;
      L.d_ln () ;
      L.d_str "t: " ;
      Typ.d_full t ;
      L.d_ln () ;
      assert false
    in
    match (se, t.desc, syn_offs) with
    | _, _, [] ->
        (se, t)
    | Predicates.Estruct (fsel, _), Tstruct name, Field (fld, _) :: syn_offs' -> (
      match Tenv.lookup tenv name with
      | Some {fields} ->
          let se' = snd (List.find_exn ~f:(fun (f', _) -> Fieldname.equal f' fld) fsel) in
          let field' = List.find_exn ~f:(fun {Struct.name= f'} -> Fieldname.equal f' fld) fields in
          let t' = field'.typ in
          get_strexp_at_syn_offsets tenv se' t' syn_offs'
      | None ->
          fail () )
    | Predicates.Earray (_, esel, _), Typ.Tarray {elt= t'}, Index ind :: syn_offs' ->
        let se' = snd (List.find_exn ~f:(fun (i', _) -> Exp.equal i' ind) esel) in
        get_strexp_at_syn_offsets tenv se' t' syn_offs'
    | _ ->
        fail ()


  (** Replace a strexp at the given syntactic offset list *)
  let rec replace_strexp_at_syn_offsets tenv se (t : Typ.t) syn_offs update =
    match (se, t.desc, syn_offs) with
    | _, _, [] ->
        update se
    | Predicates.Estruct (fsel, inst), Tstruct name, Field (fld, _) :: syn_offs' -> (
      match Tenv.lookup tenv name with
      | Some {fields} ->
          let se' = snd (List.find_exn ~f:(fun (f', _) -> Fieldname.equal f' fld) fsel) in
          let t' =
            let f' = List.find_exn ~f:(fun {Struct.name= f'} -> Fieldname.equal f' fld) fields in
            f'.typ
          in
          let se_mod = replace_strexp_at_syn_offsets tenv se' t' syn_offs' update in
          let fsel' =
            List.map
              ~f:(fun (f'', se'') -> if Fieldname.equal f'' fld then (fld, se_mod) else (f'', se''))
              fsel
          in
          Predicates.Estruct (fsel', inst)
      | None ->
          assert false )
    | Predicates.Earray (len, esel, inst), Tarray {elt= t'}, Index idx :: syn_offs' ->
        let se' = snd (List.find_exn ~f:(fun (i', _) -> Exp.equal i' idx) esel) in
        let se_mod = replace_strexp_at_syn_offsets tenv se' t' syn_offs' update in
        let esel' =
          List.map ~f:(fun ese -> if Exp.equal (fst ese) idx then (idx, se_mod) else ese) esel
        in
        Predicates.Earray (len, esel', inst)
    | _ ->
        assert false


  (** convert a path into an expression *)
  let path_to_exps (root, syn_offs_in) =
    let rec convert acc = function
      | [] ->
          acc
      | Field (f, t) :: syn_offs' ->
          let acc' = List.map ~f:(fun e -> Exp.Lfield (e, f, t)) acc in
          convert acc' syn_offs'
      | Index idx :: syn_offs' ->
          let acc' = List.map ~f:(fun e -> Exp.Lindex (e, idx)) acc in
          convert acc' syn_offs'
    in
    convert [root] syn_offs_in


  (** create a path from a root and a list of offsets *)
  let path_from_exp_offsets root offs =
    let offset_to_syn_offset = function
      | Predicates.Off_fld (fld, typ) ->
          Field (fld, typ)
      | Predicates.Off_index idx ->
          Index idx
    in
    let syn_offs = List.map ~f:offset_to_syn_offset offs in
    (root, syn_offs)


  (** path to the root, len, elements and type of a new_array *)
  type strexp_data = path * Predicates.strexp * Typ.t

  (** Store hpred using physical equality, and offset list for an array *)
  type t = sigma * Predicates.hpred * syn_offset list

  (** Find an array at the given path. Can raise [Not_found_s/Caml.Not_found] *)
  let find_path sigma (root, syn_offs) : t =
    let filter = function Predicates.Hpointsto (e, _, _) -> Exp.equal root e | _ -> false in
    let hpred = List.find_exn ~f:filter sigma in
    (sigma, hpred, syn_offs)


  (** Find a sub strexp with the given property. Can raise [Not_found_s/Caml.Not_found] *)
  let find tenv (sigma : sigma) (pred : strexp_data -> bool) : t list =
    let found = ref [] in
    let rec find_offset_sexp sigma_other hpred root offs se (typ : Typ.t) =
      let offs' = List.rev offs in
      let path = (root, offs') in
      if pred (path, se, typ) then found := (sigma, hpred, offs') :: !found
      else
        match (se, typ.desc) with
        | Predicates.Estruct (fsel, _), Tstruct name -> (
          match Tenv.lookup tenv name with
          | Some {fields} ->
              find_offset_fsel sigma_other hpred root offs fsel fields typ
          | None ->
              () )
        | Predicates.Earray (_, esel, _), Tarray {elt} ->
            find_offset_esel sigma_other hpred root offs esel elt
        | _ ->
            ()
    and find_offset_fsel sigma_other hpred root offs fsel ftal typ =
      match fsel with
      | [] ->
          ()
      | (f, se) :: fsel' ->
          ( match List.find ~f:(fun {Struct.name= f'} -> Fieldname.equal f' f) ftal with
          | Some ({Struct.typ= t} : Struct.field) ->
              find_offset_sexp sigma_other hpred root (Field (f, typ) :: offs) se t
          | None ->
              L.d_printfln "Can't find field %a in StrexpMatch.find" Fieldname.pp f ) ;
          find_offset_fsel sigma_other hpred root offs fsel' ftal typ
    and find_offset_esel sigma_other hpred root offs esel t =
      match esel with
      | [] ->
          ()
      | (ind, se) :: esel' ->
          find_offset_sexp sigma_other hpred root (Index ind :: offs) se t ;
          find_offset_esel sigma_other hpred root offs esel' t
    in
    let rec iterate sigma_seen = function
      | [] ->
          ()
      | hpred :: sigma_rest ->
          ( match hpred with
          | Predicates.Hpointsto (root, se, te) ->
              let sigma_other = sigma_seen @ sigma_rest in
              find_offset_sexp sigma_other hpred root [] se (Exp.texp_to_typ None te)
          | _ ->
              () ) ;
          iterate (hpred :: sigma_seen) sigma_rest
    in
    iterate [] sigma ;
    !found


  (** Get the matched strexp *)
  let get_data tenv ((_, hpred, syn_offs) : t) =
    match hpred with
    | Predicates.Hpointsto (root, se, te) ->
        let t = Exp.texp_to_typ None te in
        let se', t' = get_strexp_at_syn_offsets tenv se t syn_offs in
        let path' = (root, syn_offs) in
        (path', se', t')
    | _ ->
        assert false


  (** Replace the current hpred *)
  let replace_hpred ((sigma, hpred, _) : t) hpred' =
    List.map ~f:(fun hpred'' -> if phys_equal hpred'' hpred then hpred' else hpred'') sigma


  (** Replace the strexp at the given offset in the given hpred *)
  let hpred_replace_strexp tenv footprint_part hpred syn_offs update =
    let update se' =
      let se_in = update se' in
      match (se', se_in) with
      | Predicates.Earray (len, esel, _), Predicates.Earray (_, esel_in, inst2) ->
          let orig_indices = List.map ~f:fst esel in
          let index_is_not_new idx = List.exists ~f:(Exp.equal idx) orig_indices in
          let process_index idx =
            if index_is_not_new idx then idx else array_clean_new_index footprint_part idx
          in
          let esel_in' = List.map ~f:(fun (idx, se) -> (process_index idx, se)) esel_in in
          Predicates.Earray (len, esel_in', inst2)
      | _, _ ->
          se_in
    in
    match hpred with
    | Predicates.Hpointsto (root, se, te) ->
        let t = Exp.texp_to_typ None te in
        let se' = replace_strexp_at_syn_offsets tenv se t syn_offs update in
        Predicates.Hpointsto (root, se', te)
    | _ ->
        assert false


  (** Replace the strexp at a given position by a new strexp *)
  let replace_strexp tenv footprint_part ((sigma, hpred, syn_offs) : t) se_in =
    let update _ = se_in in
    let hpred' = hpred_replace_strexp tenv footprint_part hpred syn_offs update in
    replace_hpred (sigma, hpred, syn_offs) hpred'


  (** Replace the index in the array at a given position with the new index *)
  let replace_index tenv footprint_part ((sigma, hpred, syn_offs) : t) (index : Exp.t)
      (index' : Exp.t) =
    let update se' =
      match se' with
      | Predicates.Earray (len, esel, inst) ->
          let esel' =
            List.map
              ~f:(fun (e', se') -> if Exp.equal e' index then (index', se') else (e', se'))
              esel
          in
          Predicates.Earray (len, esel', inst)
      | _ ->
          assert false
    in
    let hpred' = hpred_replace_strexp tenv footprint_part hpred syn_offs update in
    replace_hpred (sigma, hpred, syn_offs) hpred'
end

(** This function renames expressions in [p]. The renaming is, roughly speaking, to replace [path.i]
    by [path.i'] for all (i, i') in [map]. *)
let prop_replace_path_index tenv (p : Prop.exposed Prop.t) (path : StrexpMatch.path)
    (map : (Exp.t * Exp.t) list) : Prop.exposed Prop.t =
  let elist_path = StrexpMatch.path_to_exps path in
  let expmap_list =
    List.fold
      ~f:(fun acc_outer e_path ->
        List.fold
          ~f:(fun acc_inner (old_index, new_index) ->
            let old_e_path_index =
              Prop.exp_normalize_prop tenv p (Exp.Lindex (e_path, old_index))
            in
            let new_e_path_index =
              Prop.exp_normalize_prop tenv p (Exp.Lindex (e_path, new_index))
            in
            (old_e_path_index, new_e_path_index) :: acc_inner )
          ~init:acc_outer map )
      ~init:[] elist_path
  in
  let expmap_fun e' =
    Option.value_map ~f:snd (List.find ~f:(fun (e, _) -> Exp.equal e e') expmap_list) ~default:e'
  in
  Prop.prop_expmap expmap_fun p


(** This function uses [update] and transforms the two sigma parts of [p], the sigma of the current
    SH of [p] and that of the footprint of [p]. *)
let prop_update_sigma_and_fp_sigma tenv (p : Prop.normal Prop.t)
    (update : bool -> sigma -> sigma * bool) : Prop.normal Prop.t * bool =
  let sigma', changed = update false p.Prop.sigma in
  let ep1 = Prop.set p ~sigma:sigma' in
  let ep2, changed2 =
    if !BiabductionConfig.footprint then
      let sigma_fp', changed' = update true ep1.Prop.sigma_fp in
      (Prop.set ep1 ~sigma_fp:sigma_fp', changed')
    else (ep1, false)
  in
  (Prop.normalize tenv ep2, changed || changed2)


(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)
let array_abstraction_performed = ref false

(** This function abstracts strexps. The parameter [can_abstract] spots strexps where the
    abstraction might be applicable, and the parameter [do_abstract] does the abstraction to those
    spotted strexps. *)
let generic_strexp_abstract tenv (abstraction_name : string) (p_in : Prop.normal Prop.t)
    (can_abstract_ : StrexpMatch.strexp_data -> bool)
    (do_abstract :
      bool -> Prop.normal Prop.t -> StrexpMatch.strexp_data -> Prop.normal Prop.t * bool ) :
    Prop.normal Prop.t =
  let can_abstract data =
    let r = can_abstract_ data in
    if r then array_abstraction_performed := true ;
    r
  in
  let find_strexp_to_abstract p0 =
    let find sigma = StrexpMatch.find tenv sigma can_abstract in
    let matchings_cur = find p0.Prop.sigma in
    let matchings_fp = find p0.Prop.sigma_fp in
    (matchings_cur, matchings_fp)
  in
  let match_select_next (matchings_cur, matchings_fp) =
    match (matchings_cur, matchings_fp) with
    | [], [] ->
        raise Caml.Not_found
    | matched :: cur', fp' ->
        (matched, false, (cur', fp'))
    | [], matched :: fp' ->
        (matched, true, ([], fp'))
  in
  let rec match_abstract p0 matchings_cur_fp =
    try
      let matched, footprint_part, matchings_cur_fp' = match_select_next matchings_cur_fp in
      let n = List.length (snd matchings_cur_fp') + 1 in
      if Config.trace_absarray then L.d_printfln "Num of fp candidates %d" n ;
      let strexp_data = StrexpMatch.get_data tenv matched in
      let p1, changed = do_abstract footprint_part p0 strexp_data in
      if changed then (p1, true) else match_abstract p0 matchings_cur_fp'
    with Caml.Not_found -> (p0, false)
  in
  let rec find_then_abstract bound p0 =
    if Int.equal bound 0 then p0
    else (
      if Config.trace_absarray then (
        L.d_printfln "Applying %s to" abstraction_name ;
        Prop.d_prop p0 ;
        L.d_ln () ;
        L.d_ln () ) ;
      let matchings_cur_fp = find_strexp_to_abstract p0 in
      let p1, changed = match_abstract p0 matchings_cur_fp in
      if changed then find_then_abstract (bound - 1) p1 else p0 )
  in
  let matchings_cur, matchings_fp = find_strexp_to_abstract p_in in
  let num_matches = List.length matchings_cur + List.length matchings_fp in
  find_then_abstract num_matches p_in


(** Return [true] if there's a pointer to the index *)
let index_is_pointed_to tenv (p : Prop.normal Prop.t) (path : StrexpMatch.path) (index : Exp.t) :
    bool =
  let indices =
    let index_plus_one = Exp.BinOp (Binop.PlusA None, index, Exp.one) in
    [index; index_plus_one]
  in
  let add_index_to_paths =
    let elist_path = StrexpMatch.path_to_exps path in
    let add_index i e = Prop.exp_normalize_prop tenv p (Exp.Lindex (e, i)) in
    fun i -> List.map ~f:(add_index i) elist_path
  in
  let pointers = List.concat_map ~f:add_index_to_paths indices in
  let filter = function
    | Predicates.Hpointsto (_, Predicates.Eexp (e, _), _) ->
        List.exists ~f:(Exp.equal e) pointers
    | _ ->
        false
  in
  List.exists ~f:filter p.Prop.sigma


(** Given [p] containing an array at [path], blur [index] in it *)
let blur_array_index tenv (p : Prop.normal Prop.t) (path : StrexpMatch.path) (index : Exp.t) :
    Prop.normal Prop.t =
  try
    let fresh_index =
      Exp.Var
        (Ident.create_fresh
           (if !BiabductionConfig.footprint then Ident.kfootprint else Ident.kprimed) )
    in
    let p2 =
      try
        if !BiabductionConfig.footprint then
          let sigma_fp = p.Prop.sigma_fp in
          let matched_fp = StrexpMatch.find_path sigma_fp path in
          let sigma_fp' = StrexpMatch.replace_index tenv true matched_fp index fresh_index in
          Prop.set p ~sigma_fp:sigma_fp'
        else Prop.expose p
      with Not_found_s _ | Caml.Not_found -> Prop.expose p
    in
    let p3 =
      let matched = StrexpMatch.find_path p.Prop.sigma path in
      let sigma' = StrexpMatch.replace_index tenv false matched index fresh_index in
      Prop.set p2 ~sigma:sigma'
    in
    let p4 =
      let index_next = Exp.BinOp (Binop.PlusA None, index, Exp.one) in
      let fresh_index_next = Exp.BinOp (Binop.PlusA None, fresh_index, Exp.one) in
      let map = [(index, fresh_index); (index_next, fresh_index_next)] in
      prop_replace_path_index tenv p3 path map
    in
    Prop.normalize tenv p4
  with Not_found_s _ | Caml.Not_found -> p


(** Given [p] containing an array at [root], blur [indices] in it *)
let blur_array_indices tenv (p : Prop.normal Prop.t) (root : StrexpMatch.path) (indices : Exp.t list)
    : Prop.normal Prop.t * bool =
  let f prop index = blur_array_index tenv prop root index in
  (List.fold ~f ~init:p indices, List.length indices > 0)


(** Given [p] containing an array at [root], only keep [indices] in it *)
let keep_only_indices tenv (p : Prop.normal Prop.t) (path : StrexpMatch.path) (indices : Exp.t list)
    : Prop.normal Prop.t * bool =
  let prune_sigma footprint_part sigma =
    try
      let matched = StrexpMatch.find_path sigma path in
      let _, se, _ = StrexpMatch.get_data tenv matched in
      match se with
      | Predicates.Earray (len, esel, inst) ->
          let esel', esel_leftover' =
            List.partition_tf ~f:(fun (e, _) -> List.exists ~f:(Exp.equal e) indices) esel
          in
          if List.is_empty esel_leftover' then (sigma, false)
          else
            let se' = Predicates.Earray (len, esel', inst) in
            let sigma' = StrexpMatch.replace_strexp tenv footprint_part matched se' in
            (sigma', true)
      | _ ->
          (sigma, false)
    with Not_found_s _ | Caml.Not_found -> (sigma, false)
  in
  prop_update_sigma_and_fp_sigma tenv p prune_sigma


(** If the type is array, check whether we should do abstraction *)
let array_typ_can_abstract {Typ.desc} =
  match desc with
  | Tarray {elt= {desc= Tptr ({desc= Tfun}, _)}} ->
      false (* don't abstract arrays of pointers *)
  | _ ->
      true


(** This function checks whether we can apply an abstraction to a strexp *)
let strexp_can_abstract ((_, se, typ) : StrexpMatch.strexp_data) : bool =
  let can_abstract_se =
    match se with
    | Predicates.Earray (_, esel, _) ->
        let len = List.length esel in
        len > 1
    | _ ->
        false
  in
  can_abstract_se && array_typ_can_abstract typ


(** This function abstracts a strexp *)
let strexp_do_abstract tenv footprint_part p ((path, se_in, _) : StrexpMatch.strexp_data) :
    Prop.normal Prop.t * bool =
  if Config.trace_absarray && footprint_part then L.d_strln "strexp_do_abstract (footprint)" ;
  if Config.trace_absarray && not footprint_part then L.d_strln "strexp_do_abstract (nonfootprint)" ;
  let prune_and_blur d_keys keep blur path keep_keys blur_keys =
    let p2, changed2 =
      if Config.trace_absarray then (
        L.d_str "keep " ;
        d_keys keep_keys ;
        L.d_ln () ) ;
      keep p path keep_keys
    in
    let p3, changed3 =
      if List.is_empty blur_keys then (p2, false)
      else (
        if Config.trace_absarray then (
          L.d_str "blur " ;
          d_keys blur_keys ;
          L.d_ln () ) ;
        blur p2 path blur_keys )
    in
    if Config.trace_absarray then (
      L.d_strln "Returns" ;
      Prop.d_prop p3 ;
      L.d_ln () ;
      L.d_ln () ) ;
    (p3, changed2 || changed3)
  in
  let prune_and_blur_indices =
    prune_and_blur Exp.d_list (keep_only_indices tenv) (blur_array_indices tenv)
  in
  let partition_abstract should_keep abstract ksel default_keys =
    let keep_ksel, remove_ksel = List.partition_tf ~f:should_keep ksel in
    let keep_keys, _, _ =
      (List.map ~f:fst keep_ksel, List.map ~f:fst remove_ksel, List.map ~f:fst ksel)
    in
    let keep_keys' = if List.is_empty keep_keys then default_keys else keep_keys in
    abstract keep_keys' keep_keys'
  in
  let do_array_footprint esel =
    (* array case footprint: keep only the last index, and blur it *)
    let should_keep (i0, _) = index_is_pointed_to tenv p path i0 in
    let abstract = prune_and_blur_indices path in
    let default_indices =
      match List.map ~f:fst esel with [] -> [] | indices -> [List.last_exn indices]
      (* keep last key at least *)
    in
    partition_abstract should_keep abstract esel default_indices
  in
  let do_footprint () =
    match se_in with Predicates.Earray (_, esel, _) -> do_array_footprint esel | _ -> assert false
  in
  let filter_abstract d_keys should_keep abstract ksel default_keys =
    let keep_ksel = List.filter ~f:should_keep ksel in
    let keep_keys = List.map ~f:fst keep_ksel in
    let keep_keys' = if List.is_empty keep_keys then default_keys else keep_keys in
    if Config.trace_absarray then (
      L.d_str "keep " ;
      d_keys keep_keys' ;
      L.d_ln () ) ;
    abstract keep_keys' []
  in
  let do_array_reexecution esel =
    (* array case re-execution: remove and blur constant and primed indices *)
    let is_pointed index = index_is_pointed_to tenv p path index in
    let should_keep (index, _) =
      match index with
      | Exp.Const _ ->
          is_pointed index
      | Exp.Var id ->
          Ident.is_normal id || is_pointed index
      | _ ->
          false
    in
    let abstract = prune_and_blur_indices path in
    filter_abstract Exp.d_list should_keep abstract esel []
  in
  let do_reexecution () =
    match se_in with
    | Predicates.Earray (_, esel, _) ->
        do_array_reexecution esel
    | _ ->
        assert false
  in
  if !BiabductionConfig.footprint then do_footprint () else do_reexecution ()


let strexp_abstract tenv (p : Prop.normal Prop.t) : Prop.normal Prop.t =
  generic_strexp_abstract tenv "strexp_abstract" p strexp_can_abstract (strexp_do_abstract tenv)


let report_error prop =
  L.d_strln "Check after array abstraction: FAIL" ;
  Prop.d_prop prop ;
  L.d_ln () ;
  assert false


(** Check performed after the array abstraction to see whether it was successful. Raise assert false
    in case of failure *)
let check_after_array_abstraction tenv prop =
  let lookup = Tenv.lookup tenv in
  let check_index root offs (ind, _) =
    if !BiabductionConfig.footprint then
      let path = StrexpMatch.path_from_exp_offsets root offs in
      index_is_pointed_to tenv prop path ind
    else not (Exp.free_vars ind |> Sequence.exists ~f:Ident.is_primed)
  in
  let rec check_se root offs typ = function
    | Predicates.Eexp _ ->
        ()
    | Predicates.Earray (_, esel, _) ->
        (* check that no more than 2 elements are in the array *)
        let typ_elem = Typ.array_elem (Some StdTyp.void) typ in
        if List.length esel > 2 && array_typ_can_abstract typ then (
          if not (List.for_all ~f:(check_index root offs) esel) then report_error prop )
        else
          List.iter
            ~f:(fun (ind, se) -> check_se root (offs @ [Predicates.Off_index ind]) typ_elem se)
            esel
    | Predicates.Estruct (fsel, _) ->
        List.iter
          ~f:(fun (f, se) ->
            let typ_f = Struct.fld_typ ~lookup ~default:StdTyp.void f typ in
            check_se root (offs @ [Predicates.Off_fld (f, typ)]) typ_f se )
          fsel
  in
  let check_hpred = function
    | Predicates.Hpointsto (root, se, texp) ->
        let typ = Exp.texp_to_typ (Some StdTyp.void) texp in
        check_se root [] typ se
    | Predicates.Hlseg _ | Predicates.Hdllseg _ ->
        ()
  in
  let check_sigma sigma = List.iter ~f:check_hpred sigma in
  (* check_footprint_pure prop; *)
  check_sigma prop.Prop.sigma ;
  check_sigma prop.Prop.sigma_fp


(** Apply array abstraction and check the result *)
let abstract_array_check tenv p =
  let p_res = strexp_abstract tenv p in
  check_after_array_abstraction tenv p_res ;
  p_res


(** remove redundant elements in an array *)
let remove_redundant_elements tenv prop =
  Prop.d_prop prop ;
  L.d_ln () ;
  let occurs_at_most_once : Ident.t -> bool =
    let fav_curr =
      let ( @@@ ) = Sequence.append in
      Predicates.subst_free_vars prop.Prop.sub
      @@@ Prop.pi_free_vars prop.Prop.pi
      @@@ Prop.sigma_free_vars prop.Prop.sigma
    in
    let fav_foot =
      Sequence.append (Prop.pi_free_vars prop.Prop.pi_fp) (Prop.sigma_free_vars prop.Prop.sigma_fp)
    in
    let at_most_once seq id =
      Sequence.filter seq ~f:(Ident.equal id) |> Sequence.length_is_bounded_by ~max:1
    in
    let at_most_once_in_curr_or_foot v = at_most_once fav_curr v && at_most_once fav_foot v in
    at_most_once_in_curr_or_foot
  in
  let modified = ref false in
  let filter_redundant_e_se fp_part (e, se) =
    let remove () =
      L.d_strln "kill_redundant: removing " ;
      Exp.d_exp e ;
      L.d_str " " ;
      Predicates.d_sexp se ;
      L.d_ln () ;
      array_abstraction_performed := true ;
      modified := true ;
      false
    in
    match (e, se) with
    | Exp.Const (Const.Cint i), Predicates.Eexp (Exp.Var id, _)
      when ((not fp_part) || IntLit.iszero i)
           && (not (Ident.is_normal id))
           && occurs_at_most_once id ->
        remove () (* unknown value can be removed in re-execution mode or if the index is zero *)
    | Exp.Var id, Predicates.Eexp _ when (not (Ident.is_normal id)) && occurs_at_most_once id ->
        remove () (* index unknown can be removed *)
    | _ ->
        true
  in
  let remove_redundant_se fp_part = function
    | Predicates.Earray (len, esel, inst) ->
        let esel' = List.filter ~f:(filter_redundant_e_se fp_part) esel in
        Predicates.Earray (len, esel', inst)
    | se ->
        se
  in
  let remove_redundant_hpred fp_part = function
    | Predicates.Hpointsto (e, se, te) ->
        let se' = remove_redundant_se fp_part se in
        Predicates.Hpointsto (e, se', te)
    | hpred ->
        hpred
  in
  let remove_redundant_sigma fp_part sigma = List.map ~f:(remove_redundant_hpred fp_part) sigma in
  let sigma' = remove_redundant_sigma false prop.Prop.sigma in
  let sigma_fp' = remove_redundant_sigma true prop.Prop.sigma_fp in
  if !modified then
    let prop' = Prop.set prop ~sigma:sigma' ~sigma_fp:sigma_fp' in
    Prop.normalize tenv prop'
  else prop
