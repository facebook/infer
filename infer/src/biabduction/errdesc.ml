(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Create descriptions of analysis errors *)

module L = Logging
module F = Format
module DExp = DecompiledExp

let vector_matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::vector"]

let is_one_of_classes = QualifiedCppName.Match.match_qualifiers

let is_method_of_objc_cpp_class pname matcher =
  match pname with
  | Procname.ObjC_Cpp objc_cpp ->
      let class_qual_opt = Procname.ObjC_Cpp.get_class_qualifiers objc_cpp in
      is_one_of_classes matcher class_qual_opt
  | _ ->
      false


let is_vector_method pname = is_method_of_objc_cpp_class pname vector_matcher

let is_special_field matcher field_name_opt field =
  let field_name = Fieldname.get_field_name field in
  let field_ok =
    match field_name_opt with
    | Some field_name' ->
        String.equal field_name' field_name
    | None ->
        true
  in
  field_ok
  && (not (Fieldname.is_java field))
  && Fieldname.get_class_name field |> Typ.Name.qual_name |> is_one_of_classes matcher


(** Check whether the hpred is a |-> representing a resource in the Racquire state *)
let hpred_is_open_resource tenv prop = function
  | Predicates.Hpointsto (e, _, _) -> (
    match Attribute.get_resource tenv prop e with
    | Some (Apred (Aresource {ra_kind= Racquire; ra_res= res}, _)) ->
        Some res
    | _ ->
        None )
  | _ ->
      None


let verbose = Config.trace_error

(** Special case for C++, where we translate code like [struct X; X getX() { X x; return X; }] as
    [void getX(struct X * frontend_generated_pvar)]. This lets us recognize that X was returned from
    getX *)
let find_struct_by_value_assignment node pvar =
  if Pvar.is_frontend_tmp pvar then
    let find_instr node = function
      | Sil.Call (_, (Const (Cfun pname) | Closure {name= pname}), args, loc, cf) -> (
        match List.last args with
        | Some (Exp.Lvar last_arg, _) when Pvar.equal pvar last_arg ->
            Some (node, pname, loc, cf)
        | _ ->
            None )
      | _ ->
          None
    in
    Procdesc.Node.find_in_node_or_preds node ~f:find_instr
  else None


(** Find the Load instruction used to declare normal variable [id], and return the expression
    dereferenced to initialize [id] *)
let rec find_normal_variable_load_ tenv (seen : Exp.Set.t) node id : DExp.t option =
  let find_declaration node = function
    | Sil.Load {id= id0; e} when Ident.equal id id0 ->
        if verbose then (
          L.d_str "find_normal_variable_load defining " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        exp_lv_dexp_ tenv seen node e
    | Sil.Call ((id0, _), (Exp.Const (Const.Cfun pn) | Closure {name= pn}), (e, _) :: _, _, _)
      when Ident.equal id id0 && Procname.equal pn (Procname.from_string_c_fun "__cast") ->
        if verbose then (
          L.d_str "find_normal_variable_load cast on " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        exp_rv_dexp_ tenv seen node e
    | Sil.Call
        ( (id0, _)
        , ((Exp.Const (Const.Cfun pname) | Closure {name= pname}) as fun_exp)
        , args
        , loc
        , call_flags )
      when Ident.equal id id0 ->
        if verbose then (
          L.d_str "find_normal_variable_load function call " ;
          Exp.d_exp fun_exp ;
          L.d_ln () ) ;
        let fun_dexp = DExp.Dconst (Const.Cfun pname) in
        let args_dexp =
          let args_dexpo = List.map ~f:(fun (e, _) -> exp_rv_dexp_ tenv seen node e) args in
          if List.exists ~f:is_none args_dexpo then []
          else
            let unNone = function Some x -> x | None -> assert false in
            List.map ~f:unNone args_dexpo
        in
        Some (DExp.Dretcall (fun_dexp, args_dexp, loc, call_flags))
    | Sil.Store {e1= Exp.Lvar pvar; e2= Exp.Var id0}
      when Config.is_checker_enabled Biabduction
           && Ident.equal id id0
           && not (Pvar.is_frontend_tmp pvar) ->
        (* this case is a hack to make bucketing continue to work in the presence of copy
           propagation. previously, we would have code like:
           n1 = foo(); x = n1; n2 = x; n2.toString(), but copy-propagation will optimize this to:
           n1 = foo(); x = n1; n1.toString(). This case allows us to recognize the association
           between n1 and x. Checkers don't use copy-prop, so they don't need this. *)
        Some (DExp.Dpvar pvar)
    | _ ->
        None
  in
  let res = Procdesc.Node.find_in_node_or_preds node ~f:find_declaration in
  if verbose && is_none res then
    L.d_printfln "find_normal_variable_load could not find %a in node %a" Ident.pp id
      Procdesc.Node.pp node ;
  res


(** describe lvalue [e] as a dexp *)
and exp_lv_dexp_ tenv (seen_ : Exp.Set.t) node e : DExp.t option =
  if Exp.Set.mem e seen_ then (
    L.d_str "exp_lv_dexp: cycle detected" ;
    Exp.d_exp e ;
    L.d_ln () ;
    None )
  else
    let seen = Exp.Set.add e seen_ in
    match Prop.exp_normalize_noabs tenv Predicates.sub_empty e with
    | Exp.Const c ->
        if verbose then (
          L.d_str "exp_lv_dexp: constant " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some (DExp.Dderef (DExp.Dconst c))
    | Exp.BinOp (Binop.PlusPI, e1, e2) -> (
        if verbose then (
          L.d_str "exp_lv_dexp: (e1 +PI e2) " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        match (exp_lv_dexp_ tenv seen node e1, exp_rv_dexp_ tenv seen node e2) with
        | Some de1, Some de2 ->
            Some (DExp.Dbinop (Binop.PlusPI, de1, de2))
        | _ ->
            None )
    | Exp.Var id when Ident.is_normal id -> (
        if verbose then (
          L.d_str "exp_lv_dexp: normal var " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        match find_normal_variable_load_ tenv seen node id with
        | None ->
            None
        | Some de ->
            Some (DExp.Dderef de) )
    | Exp.Lvar pvar ->
        if verbose then (
          L.d_str "exp_lv_dexp: program var " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        if Pvar.is_frontend_tmp pvar then
          match Decompile.find_program_variable_assignment node pvar with
          | None -> (
            match find_struct_by_value_assignment node pvar with
            | Some (_, pname, loc, call_flags) ->
                Some (DExp.Dfcall (DExp.Dconst (Cfun pname), [], loc, call_flags))
            | None ->
                None )
          | Some (node', id) -> (
            match Decompile.find_normal_variable_funcall node' id with
            | Some (fun_exp, eargs, loc, call_flags) ->
                let fun_dexpo = exp_rv_dexp_ tenv seen node' fun_exp in
                let blame_args = List.map ~f:(exp_rv_dexp_ tenv seen node') eargs in
                if List.exists ~f:is_none (fun_dexpo :: blame_args) then None
                else
                  let unNone = function Some x -> x | None -> assert false in
                  let args = List.map ~f:unNone blame_args in
                  Some (DExp.Dfcall (unNone fun_dexpo, args, loc, call_flags))
            | None ->
                exp_rv_dexp_ tenv seen node' (Exp.Var id) )
        else Some (DExp.Dpvar pvar)
    | Exp.Lfield (Exp.Var id, f, _) when Ident.is_normal id -> (
        if verbose then (
          L.d_str "exp_lv_dexp: Lfield with var " ;
          Exp.d_exp (Exp.Var id) ;
          L.d_printfln " %a" Fieldname.pp f ) ;
        match find_normal_variable_load_ tenv seen node id with
        | None ->
            None
        | Some de ->
            Some (DExp.Darrow (de, f)) )
    | Exp.Lfield (e1, f, _) -> (
        if verbose then (
          L.d_str "exp_lv_dexp: Lfield " ;
          Exp.d_exp e1 ;
          L.d_printfln " %a" Fieldname.pp f ) ;
        match exp_lv_dexp_ tenv seen node e1 with
        | None ->
            None
        | Some de ->
            Some (DExp.Ddot (de, f)) )
    | Exp.Lindex (e1, e2) -> (
        if verbose then (
          L.d_str "exp_lv_dexp: Lindex " ;
          Exp.d_exp e1 ;
          L.d_str " " ;
          Exp.d_exp e2 ;
          L.d_ln () ) ;
        match (exp_lv_dexp_ tenv seen node e1, exp_rv_dexp_ tenv seen node e2) with
        | None, _ ->
            None
        | Some de1, None ->
            (* even if the index is unknown, the array info is useful for bound errors *)
            Some (DExp.Darray (de1, DExp.Dunknown))
        | Some de1, Some de2 ->
            Some (DExp.Darray (de1, de2)) )
    | _ ->
        if verbose then (
          L.d_str "exp_lv_dexp: no match for  " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        None


(** describe rvalue [e] as a dexp *)
and exp_rv_dexp_ tenv (seen_ : Exp.Set.t) node e : DExp.t option =
  if Exp.Set.mem e seen_ then (
    L.d_str "exp_rv_dexp: cycle detected" ;
    Exp.d_exp e ;
    L.d_ln () ;
    None )
  else
    let seen = Exp.Set.add e seen_ in
    match e with
    | Exp.Const c ->
        if verbose then (
          L.d_str "exp_rv_dexp: constant " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some (DExp.Dconst c)
    | Exp.Lvar pv ->
        if verbose then (
          L.d_str "exp_rv_dexp: program var " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        if Pvar.is_frontend_tmp pv then
          exp_lv_dexp_ tenv seen_ (* avoid spurious cycle detection *) node e
        else Some (DExp.Dpvaraddr pv)
    | Exp.Var id when Ident.is_normal id ->
        if verbose then (
          L.d_str "exp_rv_dexp: normal var " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        find_normal_variable_load_ tenv seen node id
    | Exp.Lfield (e1, f, _) -> (
        if verbose then (
          L.d_str "exp_rv_dexp: Lfield " ;
          Exp.d_exp e1 ;
          L.d_printfln " %a" Fieldname.pp f ) ;
        match exp_rv_dexp_ tenv seen node e1 with
        | None ->
            None
        | Some de ->
            Some (DExp.Ddot (de, f)) )
    | Exp.Lindex (e1, e2) -> (
        if verbose then (
          L.d_str "exp_rv_dexp: Lindex " ;
          Exp.d_exp e1 ;
          L.d_str " " ;
          Exp.d_exp e2 ;
          L.d_ln () ) ;
        match (exp_rv_dexp_ tenv seen node e1, exp_rv_dexp_ tenv seen node e2) with
        | None, _ | _, None ->
            None
        | Some de1, Some de2 ->
            Some (DExp.Darray (de1, de2)) )
    | Exp.BinOp (op, e1, e2) -> (
        if verbose then (
          L.d_str "exp_rv_dexp: BinOp " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        match (exp_rv_dexp_ tenv seen node e1, exp_rv_dexp_ tenv seen node e2) with
        | None, _ | _, None ->
            None
        | Some de1, Some de2 ->
            Some (DExp.Dbinop (op, de1, de2)) )
    | Exp.UnOp (op, e1, _) -> (
        if verbose then (
          L.d_str "exp_rv_dexp: UnOp " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        match exp_rv_dexp_ tenv seen node e1 with
        | None ->
            None
        | Some de1 ->
            Some (DExp.Dunop (op, de1)) )
    | Exp.Cast (_, e1) ->
        if verbose then (
          L.d_str "exp_rv_dexp: Cast " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        exp_rv_dexp_ tenv seen node e1
    | Exp.Sizeof {typ; dynamic_length; subtype} ->
        if verbose then (
          L.d_str "exp_rv_dexp: type " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some
          (DExp.Dsizeof (typ, Option.bind dynamic_length ~f:(exp_rv_dexp_ tenv seen node), subtype))
    | _ ->
        if verbose then (
          L.d_str "exp_rv_dexp: no match for  " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        None


let find_normal_variable_load tenv = find_normal_variable_load_ tenv Exp.Set.empty

let exp_lv_dexp tenv = exp_lv_dexp_ tenv Exp.Set.empty

let exp_rv_dexp tenv = exp_rv_dexp_ tenv Exp.Set.empty

(** check whether the type of leaked [hpred] appears as a predicate in an inductive predicate in
    [prop] *)
let leak_from_list_abstraction hpred prop =
  let hpred_type (hpred : Predicates.hpred) =
    match hpred with
    | Hpointsto (_, _, texp) ->
        Some texp
    | Hlseg (_, {body= [Hpointsto (_, _, texp)]}, _, _, _) ->
        Some texp
    | Hdllseg (_, {body_dll= [Hpointsto (_, _, texp)]}, _, _, _, _, _) ->
        Some texp
    | _ ->
        None
  in
  let found = ref false in
  let check_hpred texp hp =
    match hpred_type hp with Some texp' when Exp.equal texp texp' -> found := true | _ -> ()
  in
  let check_hpara texp _ hpara = List.iter ~f:(check_hpred texp) hpara.Predicates.body in
  let check_hpara_dll texp _ hpara = List.iter ~f:(check_hpred texp) hpara.Predicates.body_dll in
  match hpred_type hpred with
  | Some texp ->
      let env = Prop.prop_pred_env prop in
      Predicates.Env.iter env (check_hpara texp) (check_hpara_dll texp) ;
      if !found then (
        L.d_str "leak_from_list_abstraction of predicate of type " ;
        Exp.d_texp_full texp ;
        L.d_ln () ) ;
      !found
  | None ->
      false


(** find the type of hpred, if any *)
let find_hpred_typ hpred =
  match hpred with Predicates.Hpointsto (_, _, texp) -> Some texp | _ -> None


(** find the type of pvar and remove the pointer, if any *)
let find_typ_without_ptr prop pvar =
  let res = ref None in
  let do_hpred = function
    | Predicates.Hpointsto (e, _, te) when Exp.equal e (Exp.Lvar pvar) ->
        res := Some te
    | _ ->
        ()
  in
  List.iter ~f:do_hpred prop.Prop.sigma ;
  !res


(** Produce a description of a leak by looking at the current state. If the current instruction is a
    variable nullify, blame the variable. If it is an abstraction, blame any variable nullify at the
    current node. If there is an alloc attribute, print the function call and line number. *)
let explain_leak tenv hpred prop alloc_att_opt bucket =
  let instro = AnalysisState.get_instr () in
  let loc = AnalysisState.get_loc_exn () in
  let node = AnalysisState.get_node_exn () in
  let node_instrs = Procdesc.Node.get_instrs node in
  let hpred_typ_opt = find_hpred_typ hpred in
  let value_str_from_pvars_vpath pvars vpath =
    if not (List.is_empty pvars) then
      let pp = Pp.seq Pvar.pp_value in
      let desc_string = F.asprintf "%a" pp pvars in
      Some desc_string
    else
      match vpath with
      | Some de when not (DExp.has_tmp_var de) ->
          Some (DExp.to_string de)
      | _ ->
          None
  in
  let res_action_opt, resource_opt, vpath =
    match alloc_att_opt with
    | Some (PredSymb.Aresource ({ra_kind= Racquire} as ra)) ->
        (Some ra, Some ra.ra_res, ra.ra_vpath)
    | _ ->
        (None, None, None)
  in
  let is_file = match resource_opt with Some PredSymb.Rfile -> true | _ -> false in
  let check_pvar pvar =
    (* check that pvar is local or global and has the same type as the leaked hpred *)
    (Pvar.is_local pvar || Pvar.is_global pvar)
    && (not (Pvar.is_frontend_tmp pvar))
    &&
    match (hpred_typ_opt, find_typ_without_ptr prop pvar) with
    | Some (Exp.Sizeof {typ= t1}), Some (Exp.Sizeof {typ= {Typ.desc= Tptr (t2, _)}}) ->
        Typ.equal t1 t2
    | Some (Exp.Sizeof {typ= {Typ.desc= Tint _}}), Some (Exp.Sizeof {typ= {Typ.desc= Tint _}})
      when is_file ->
        (* must be a file opened with "open" *)
        true
    | _ ->
        false
  in
  let value_str =
    match instro with
    | None ->
        if verbose then L.d_strln "explain_leak: no current instruction" ;
        value_str_from_pvars_vpath [] vpath
    | Some (Sil.Metadata (Nullify (pvar, _))) when check_pvar pvar -> (
        if verbose then (
          L.d_str "explain_leak: current instruction is Nullify for pvar " ;
          Pvar.d pvar ;
          L.d_ln () ) ;
        match exp_lv_dexp tenv (AnalysisState.get_node_exn ()) (Exp.Lvar pvar) with
        | Some de when not (DExp.has_tmp_var de) ->
            Some (DExp.to_string de)
        | _ ->
            None )
    | Some (Sil.Metadata (Abstract _)) ->
        if verbose then L.d_strln "explain_leak: current instruction is Abstract" ;
        let get_nullify = function
          | Sil.Metadata (Nullify (pvar, _)) when check_pvar pvar ->
              if verbose then (
                L.d_str "explain_leak: found nullify before Abstract for pvar " ;
                Pvar.d pvar ;
                L.d_ln () ) ;
              Some pvar
          | _ ->
              None
        in
        let rev_nullify_pvars =
          IContainer.rev_filter_map_to_list ~fold:Instrs.fold ~f:get_nullify node_instrs
        in
        let nullify_pvars_notmp =
          List.rev_filter ~f:(fun pvar -> not (Pvar.is_frontend_tmp pvar)) rev_nullify_pvars
        in
        value_str_from_pvars_vpath nullify_pvars_notmp vpath
    | Some (Sil.Store {e1= lexp}) when is_none vpath -> (
        if verbose then (
          L.d_str "explain_leak: current instruction Set for " ;
          Exp.d_exp lexp ;
          L.d_ln () ) ;
        match exp_lv_dexp tenv node lexp with
        | Some dexp when not (DExp.has_tmp_var dexp) ->
            Some (DExp.to_string dexp)
        | _ ->
            None )
    | Some instr ->
        if verbose then (
          L.d_str "explain_leak: case not matched in instr " ;
          Sil.d_instr instr ;
          L.d_ln () ) ;
        value_str_from_pvars_vpath [] vpath
  in
  let is_user_visible, bucket =
    (* decide whether Exn_user or Exn_developer *)
    match resource_opt with
    | Some _ ->
        (* we know it has been allocated *)
        (true, bucket)
    | None ->
        if leak_from_list_abstraction hpred prop && Option.is_some value_str then
          (* we don't know it's been allocated,
             but it's coming from list abstraction and we have a name *)
          (true, bucket)
        else (false, Some Mleak_buckets.ml_bucket_unknown_origin)
  in
  ( is_user_visible
  , Localise.desc_leak hpred_typ_opt value_str resource_opt res_action_opt loc bucket )


(** find the dexp, if any, where the given value is stored also return the type of the value if
    found *)
let vpath_find tenv prop exp_ : DExp.t option * Typ.t option =
  if verbose then (
    L.d_str "in vpath_find exp:" ;
    Exp.d_exp exp_ ;
    L.d_ln () ) ;
  let rec find sigma_acc sigma_todo exp =
    let do_fse res sigma_acc' sigma_todo' lexp texp (f, se) =
      match se with
      | Predicates.Eexp (e, _) when Exp.equal exp e -> (
          let sigma' = List.rev_append sigma_acc' sigma_todo' in
          match lexp with
          | Exp.Lvar pv ->
              let typo =
                match texp with
                | Exp.Sizeof {typ= {Typ.desc= Tstruct name}} -> (
                  match Tenv.lookup tenv name with
                  | Some {fields} ->
                      let field =
                        List.find ~f:(fun {Struct.name= f'} -> Fieldname.equal f' f) fields
                      in
                      Option.map ~f:(fun ({Struct.typ} : Struct.field) -> typ) field
                  | _ ->
                      None )
                | _ ->
                    None
              in
              res := (Some (DExp.Ddot (DExp.Dpvar pv, f)), typo)
          | Exp.Var id -> (
            match find [] sigma' (Exp.Var id) with
            | None, _ ->
                ()
            | Some de, typo ->
                res := (Some (DExp.Darrow (de, f)), typo) )
          | lexp ->
              if verbose then (
                L.d_str "vpath_find do_fse: no match on Eexp " ;
                Exp.d_exp lexp ;
                L.d_ln () ) )
      | _ ->
          ()
    in
    let do_sexp sigma_acc' sigma_todo' lexp sexp texp =
      match sexp with
      | Predicates.Eexp (e, _) when Exp.equal exp e -> (
          let sigma' = List.rev_append sigma_acc' sigma_todo' in
          match lexp with
          | Exp.Lvar pv when not (Pvar.is_frontend_tmp pv) ->
              let typo = match texp with Exp.Sizeof {typ} -> Some typ | _ -> None in
              (Some (DExp.Dpvar pv), typo)
          | Exp.Var id -> (
            match find [] sigma' (Exp.Var id) with
            | None, typo ->
                (None, typo)
            | Some de, typo ->
                (Some (DExp.Dderef de), typo) )
          | lexp ->
              if verbose then (
                L.d_str "vpath_find do_sexp: no match on Eexp " ;
                Exp.d_exp lexp ;
                L.d_ln () ) ;
              (None, None) )
      | Predicates.Estruct (fsel, _) ->
          let res = ref (None, None) in
          List.iter ~f:(do_fse res sigma_acc' sigma_todo' lexp texp) fsel ;
          !res
      | _ ->
          (None, None)
    in
    let do_hpred sigma_acc' sigma_todo' =
      let substituted_from_normal id =
        let filter = function
          | ni, Exp.Var id' ->
              Ident.is_normal ni && Ident.equal id' id
          | _ ->
              false
        in
        List.exists ~f:filter (Predicates.sub_to_list prop.Prop.sub)
      in
      function
      | Predicates.Hpointsto (Exp.Lvar pv, sexp, texp)
        when Pvar.is_local pv || Pvar.is_global pv || Pvar.is_seed pv ->
          do_sexp sigma_acc' sigma_todo' (Exp.Lvar pv) sexp texp
      | Predicates.Hpointsto (Exp.Var id, sexp, texp)
        when Ident.is_normal id || (Ident.is_footprint id && substituted_from_normal id) ->
          do_sexp sigma_acc' sigma_todo' (Exp.Var id) sexp texp
      | _ ->
          (None, None)
    in
    match sigma_todo with
    | [] ->
        (None, None)
    | hpred :: sigma_todo' -> (
      match do_hpred sigma_acc sigma_todo' hpred with
      | Some de, typo ->
          (Some de, typo)
      | None, _ ->
          find (hpred :: sigma_acc) sigma_todo' exp )
  in
  let res = find [] prop.Prop.sigma exp_ in
  ( if verbose then
      match res with
      | None, _ ->
          L.d_str "vpath_find: cannot find " ;
          Exp.d_exp exp_ ;
          L.d_ln ()
      | Some de, typo -> (
          L.d_printf "vpath_find: found %a :" DExp.pp de ;
          match typo with
          | None ->
              L.d_str " No type"
          | Some typ ->
              Typ.d_full typ ;
              L.d_ln () ) ) ;
  res


let access_opt ?(is_nullable = false) inst =
  match (inst : Predicates.inst) with
  | Iupdate (_, ncf, n, _) ->
      Some (Localise.Last_assigned (n, ncf))
  | Irearrange (_, _, n, _) ->
      Some (Localise.Last_accessed (n, is_nullable))
  | Ireturn_from_call n ->
      Some (Localise.Returned_from_call n)
  | Ialloc when Language.curr_language_is Java ->
      Some Localise.Initialized_automatically
  | inst ->
      if verbose then
        L.d_printfln "explain_dexp_access: inst is not an update %a" Predicates.pp_inst inst ;
      None


(** produce a description of the access from the instrumentation at position [dexp] in [prop] *)
let explain_dexp_access prop dexp is_nullable =
  let sigma = prop.Prop.sigma in
  let sexpo_to_inst = function
    | None ->
        None
    | Some (Predicates.Eexp (_, inst)) ->
        Some inst
    | Some se ->
        if verbose then (
          L.d_str "sexpo_to_inst: can't find inst " ;
          Predicates.d_sexp se ;
          L.d_ln () ) ;
        None
  in
  let find_ptsto (e : Exp.t) : Predicates.strexp option =
    let res = ref None in
    let do_hpred = function
      | Predicates.Hpointsto (e', se, _) when Exp.equal e e' ->
          res := Some se
      | _ ->
          ()
    in
    List.iter ~f:do_hpred sigma ;
    !res
  in
  let rec lookup_fld fsel f =
    match fsel with
    | [] ->
        if verbose then L.d_printfln "lookup_fld: can't find field %a" Fieldname.pp f ;
        None
    | (f1, se) :: fsel' ->
        if Fieldname.equal f1 f then Some se else lookup_fld fsel' f
  in
  let rec lookup_esel esel e =
    match esel with
    | [] ->
        if verbose then (
          L.d_str "lookup_esel: can't find index " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        None
    | (e1, se) :: esel' ->
        if Exp.equal e1 e then Some se else lookup_esel esel' e
  in
  let rec lookup : DExp.t -> Predicates.strexp option = function
    | DExp.Dconst c ->
        Some (Predicates.Eexp (Exp.Const c, Predicates.inst_none))
    | DExp.Darray (de1, de2) -> (
      match (lookup de1, lookup de2) with
      | None, _ | _, None ->
          None
      | Some (Predicates.Earray (_, esel, _)), Some (Predicates.Eexp (e, _)) ->
          lookup_esel esel e
      | Some se1, Some se2 ->
          if verbose then (
            L.d_str "lookup: case not matched on Darray " ;
            Predicates.d_sexp se1 ;
            L.d_str " " ;
            Predicates.d_sexp se2 ;
            L.d_ln () ) ;
          None )
    | DExp.Darrow (DExp.Dpvaraddr pvar, f) -> (
      match lookup (DExp.Dpvaraddr pvar) with
      | None ->
          None
      | Some (Predicates.Estruct (fsel, _)) ->
          lookup_fld fsel f
      | Some _ ->
          if verbose then L.d_strln "lookup: case not matched on Darrow" ;
          None )
    | DExp.Darrow (de1, f) -> (
      match lookup (DExp.Dderef de1) with
      | None ->
          None
      | Some (Predicates.Estruct (fsel, _)) ->
          lookup_fld fsel f
      | Some _ ->
          if verbose then L.d_strln "lookup: case not matched on Darrow" ;
          None )
    | DExp.Ddot (de1, f) -> (
      match lookup de1 with
      | None ->
          None
      | Some (Predicates.Estruct (fsel, _)) ->
          lookup_fld fsel f
      | Some (Predicates.Eexp (Const (Cfun _), _) as fun_strexp) ->
          Some fun_strexp
      | Some _ ->
          if verbose then L.d_strln "lookup: case not matched on Ddot" ;
          None )
    | DExp.Dpvar pvar ->
        if verbose then L.d_strln "lookup: found Dpvar" ;
        find_ptsto (Exp.Lvar pvar)
    | DExp.Dderef de -> (
      match lookup de with
      | None ->
          None
      | Some (Predicates.Eexp (e, _)) ->
          find_ptsto e
      | Some _ ->
          None )
    | DExp.Dbinop (Binop.PlusPI, DExp.Dpvar _, DExp.Dconst _) as de ->
        if verbose then L.d_printfln "lookup: case )pvar + constant) %a" DExp.pp de ;
        None
    | DExp.Dfcall (DExp.Dconst c, _, loc, _) -> (
        if verbose then L.d_strln "lookup: found Dfcall" ;
        match c with
        | Const.Cfun _ ->
            (* Treat function as an update *)
            Some (Predicates.Eexp (Exp.Const c, Predicates.Ireturn_from_call loc.Location.line))
        | _ ->
            None )
    | DExp.Dpvaraddr pvar ->
        L.d_printfln "lookup: found Dvaraddr %a" DExp.pp (DExp.Dpvaraddr pvar) ;
        find_ptsto (Exp.Lvar pvar)
    | de ->
        if verbose then L.d_printfln "lookup: unknown case not matched %a" DExp.pp de ;
        None
  in
  match sexpo_to_inst (lookup dexp) with
  | Some inst ->
      access_opt inst ~is_nullable
  | None ->
      if verbose then L.d_printfln "explain_dexp_access: cannot find inst of %a" DExp.pp dexp ;
      None


let explain_dereference_access outermost_array is_nullable de_opt_ prop =
  let de_opt =
    let rec remove_outermost_array_access = function
      (* remove outermost array access from [de] *)
      | DExp.Dbinop (Binop.PlusPI, de1, _) ->
          (* remove pointer arithmetic before array access *)
          remove_outermost_array_access de1
      | DExp.Darray (DExp.Dderef de1, _) ->
          (* array access is a deref already: remove both *)
          de1
      | DExp.Darray (de1, _) ->
          (* remove array access *)
          de1
      | DExp.Dderef de ->
          (* remove implicit array access *)
          de
      | DExp.Ddot (de, _) ->
          (* remove field access before array access *)
          remove_outermost_array_access de
      | de ->
          de
    in
    match de_opt_ with
    | None ->
        None
    | Some de ->
        Some (if outermost_array then remove_outermost_array_access de else de)
  in
  let value_str = match de_opt with Some de -> DExp.to_string de | None -> "" in
  let access_opt =
    match de_opt with Some de -> explain_dexp_access prop de is_nullable | None -> None
  in
  (value_str, access_opt)


(** Create a description of a dereference operation *)
let create_dereference_desc proc_name _ ?(use_buckets = false) ?(outermost_array = false)
    ?(is_nullable = false) ?(is_premature_nil = false) de_opt deref_str prop loc =
  let value_str, access_opt = explain_dereference_access outermost_array is_nullable de_opt prop in
  let access_opt' =
    match access_opt with
    | Some (Localise.Last_accessed _) when outermost_array ->
        None (* don't report last accessed for arrays *)
    | _ ->
        access_opt
  in
  let desc = Localise.dereference_string proc_name deref_str value_str access_opt' loc in
  let desc =
    if Language.curr_language_is Clang && not is_premature_nil then
      match de_opt with
      | Some (DExp.Dretcall (Dconst (Cfun pname), this_dexp :: _, loc, _)) ->
          if is_vector_method pname then
            Localise.desc_empty_vector_access (Some pname) (DExp.to_string this_dexp) loc
          else desc
      | Some (DExp.Darrow (dexp, fieldname)) | Some (DExp.Ddot (dexp, fieldname)) ->
          if
            is_special_field vector_matcher (Some "beginPtr") fieldname
            || is_special_field vector_matcher (Some "endPtr") fieldname
          then Localise.desc_empty_vector_access None (DExp.to_string dexp) loc
          else desc
      | _ ->
          desc
    else desc
  in
  if use_buckets then Buckets.classify_access desc access_opt' de_opt is_nullable else desc


let rec find_outermost_dereference tenv node e =
  match e with
  | Exp.Const _ ->
      if verbose then (
        L.d_str "find_outermost_dereference: constant " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      exp_lv_dexp tenv node e
  | Exp.Var id when Ident.is_normal id ->
      (* look up the normal variable declaration *)
      if verbose then (
        L.d_str "find_outermost_dereference: normal var " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      find_normal_variable_load tenv node id
  | Exp.Lfield (e', _, _) ->
      if verbose then (
        L.d_str "find_outermost_dereference: Lfield " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      find_outermost_dereference tenv node e'
  | Exp.Lindex (e', _) ->
      if verbose then (
        L.d_str "find_outermost_dereference: Lindex " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      find_outermost_dereference tenv node e'
  | Exp.Lvar _ ->
      if verbose then (
        L.d_str "find_outermost_dereference: Lvar " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      exp_lv_dexp tenv node e
  | Exp.BinOp (Binop.PlusPI, Exp.Lvar _, _) ->
      if verbose then (
        L.d_str "find_outermost_dereference: Lvar+index " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      exp_lv_dexp tenv node e
  | Exp.Cast (_, e') ->
      if verbose then (
        L.d_str "find_outermost_dereference: cast " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      find_outermost_dereference tenv node e'
  | _ ->
      if verbose then (
        L.d_str "find_outermost_dereference: no match for " ;
        Exp.d_exp e ;
        L.d_ln () ) ;
      None


(** explain memory access performed by the current instruction if outermost_array is true, the
    outermost array access is removed if outermost_dereference is true, stop at the outermost
    dereference (skipping e.g. outermost field access) *)
let explain_access_ proc_name tenv ?(use_buckets = false) ?(outermost_array = false)
    ?(outermost_dereference = false) ?(is_nullable = false) ?(is_premature_nil = false) deref_str
    prop loc =
  let find_exp_dereferenced () =
    match AnalysisState.get_instr () with
    | Some (Sil.Store {e1= e}) ->
        if verbose then (
          L.d_str "explain_dereference Sil.Store " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some e
    | Some (Sil.Load {e}) ->
        if verbose then (
          L.d_str "explain_dereference Binop.Leteref " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some e
    | Some (Sil.Call (_, (Exp.Const (Const.Cfun fn) | Closure {name= fn}), [(e, _)], _, _))
      when List.exists ~f:(Procname.equal fn)
             [BuiltinDecl.free; BuiltinDecl.__delete; BuiltinDecl.__delete_array] ->
        if verbose then (
          L.d_str "explain_dereference Sil.Call " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some e
    | Some (Sil.Call (_, (Exp.Var _ as e), _, _, _)) ->
        if verbose then (
          L.d_str "explain_dereference Sil.Call " ;
          Exp.d_exp e ;
          L.d_ln () ) ;
        Some e
    | _ ->
        None
  in
  let node = AnalysisState.get_node_exn () in
  match find_exp_dereferenced () with
  | None ->
      if verbose then L.d_strln "_explain_access: find_exp_dereferenced returned None" ;
      Localise.no_desc
  | Some e ->
      L.d_strln "Finding deref'd exp" ;
      let de_opt =
        if outermost_dereference then find_outermost_dereference tenv node e
        else exp_lv_dexp tenv node e
      in
      create_dereference_desc proc_name tenv ~use_buckets ~outermost_array ~is_nullable
        ~is_premature_nil de_opt deref_str prop loc


(** Produce a description of which expression is dereferenced in the current instruction, if any.
    The subexpression to focus on is obtained by removing field and index accesses. *)
let explain_dereference proc_name tenv ?(use_buckets = false) ?(is_nullable = false)
    ?(is_premature_nil = false) deref_str prop loc =
  explain_access_ proc_name tenv ~use_buckets ~outermost_array:false ~outermost_dereference:true
    ~is_nullable ~is_premature_nil deref_str prop loc


(** Produce a description of the array access performed in the current instruction, if any. The
    subexpression to focus on is obtained by removing the outermost array access. *)
let explain_array_access tenv deref_str prop loc =
  explain_access_ tenv ~outermost_array:true deref_str prop loc


(* offset of an expression found following a program variable *)
type pvar_off =
  (* value of a pvar *)
  | Fpvar
  (* value obtained by dereferencing the pvar and following a sequence of fields *)
  | Fstruct of Fieldname.t list

let dexp_apply_pvar_off dexp pvar_off =
  let rec add_ddot de = function [] -> de | f :: fl -> add_ddot (DExp.Ddot (de, f)) fl in
  match pvar_off with
  | Fpvar ->
      dexp
  | Fstruct (f :: fl) ->
      add_ddot (DExp.Darrow (dexp, f)) fl
  | Fstruct [] ->
      dexp


(* case should not happen *)

(** Produce a description of the nth parameter of the function call, if the current instruction is a
    function call with that parameter *)
let explain_nth_function_parameter proc_name tenv use_buckets deref_str prop n pvar_off =
  let node = AnalysisState.get_node_exn () in
  let loc = AnalysisState.get_loc_exn () in
  match AnalysisState.get_instr () with
  | Some (Sil.Call (_, _, args, _, _)) -> (
    try
      let arg = fst (List.nth_exn args (n - 1)) in
      let dexp_opt = exp_rv_dexp tenv node arg in
      let dexp_opt' =
        match dexp_opt with Some de -> Some (dexp_apply_pvar_off de pvar_off) | None -> None
      in
      create_dereference_desc proc_name tenv ~use_buckets dexp_opt' deref_str prop loc
    with exn when Exception.exn_not_failure exn -> Localise.no_desc )
  | _ ->
      Localise.no_desc


(** Find a program variable whose value is [exp] or pointing to a struct containing [exp] *)
let find_with_exp prop exp =
  let res = ref None in
  let found_in_pvar pv =
    if (not (Pvar.is_abduced pv)) && not (Pvar.is_this pv) then res := Some (pv, Fpvar)
  in
  let found_in_struct pv fld_lst =
    (* found_in_pvar has priority *)
    if is_none !res then res := Some (pv, Fstruct (List.rev fld_lst))
  in
  let rec search_struct pv fld_lst = function
    | Predicates.Eexp (e, _) ->
        if Exp.equal e exp then found_in_struct pv fld_lst
    | Predicates.Estruct (fsel, _) ->
        List.iter ~f:(fun (f, se) -> search_struct pv (f :: fld_lst) se) fsel
    | _ ->
        ()
  in
  let do_hpred_pointed_by_pvar pv e = function
    | Predicates.Hpointsto (e1, se, _) ->
        if Exp.equal e e1 then search_struct pv [] se
    | _ ->
        ()
  in
  let do_hpred = function
    | Predicates.Hpointsto (Lvar pv, Eexp (e, _), _) ->
        if Exp.equal e exp then found_in_pvar pv
        else List.iter ~f:(do_hpred_pointed_by_pvar pv e) prop.Prop.sigma
    | _ ->
        ()
  in
  List.iter ~f:do_hpred prop.Prop.sigma ;
  !res


(** return a description explaining value [exp] in [prop] in terms of a source expression using the
    formal parameters of the call *)
let explain_dereference_as_caller_expression proc_name tenv ?(use_buckets = false) deref_str
    actual_pre spec_pre exp node loc formal_params =
  let find_formal_param_number name =
    let rec find n = function
      | [] ->
          0
      | v :: pars ->
          if Mangled.equal (Pvar.get_name v) name then n else find (n + 1) pars
    in
    find 1 formal_params
  in
  match find_with_exp spec_pre exp with
  | Some (pv, pvar_off) ->
      if verbose then L.d_printfln "pvar: %s" (Pvar.to_string pv) ;
      let pv_name = Pvar.get_name pv in
      if Pvar.is_global pv then
        let dexp = exp_lv_dexp tenv node (Exp.Lvar pv) in
        create_dereference_desc proc_name tenv ~use_buckets dexp deref_str actual_pre loc
      else if Pvar.is_callee pv then (
        let position = find_formal_param_number pv_name in
        if verbose then L.d_printfln "parameter number: %d" position ;
        explain_nth_function_parameter proc_name tenv use_buckets deref_str actual_pre position
          pvar_off )
      else if Attribute.has_dangling_uninit tenv spec_pre exp then
        Localise.desc_uninitialized_dangling_pointer_deref deref_str (Pvar.to_string pv) loc
      else Localise.no_desc
  | None ->
      if verbose then (
        L.d_str "explain_dereference_as_caller_expression " ;
        Exp.d_exp exp ;
        L.d_strln ": cannot explain None" ) ;
      Localise.no_desc


(** explain a class cast exception *)
let explain_class_cast_exception tenv pname_opt typ1 typ2 exp node loc =
  let exp_str_opt =
    match exp_rv_dexp tenv node exp with Some dexp -> Some (DExp.to_string dexp) | None -> None
  in
  match (exp_rv_dexp tenv node typ1, exp_rv_dexp tenv node typ2) with
  | Some de1, Some de2 ->
      let typ_str1 = DExp.to_string de1 in
      let typ_str2 = DExp.to_string de2 in
      Localise.desc_class_cast_exception pname_opt typ_str1 typ_str2 exp_str_opt loc
  | _ ->
      Localise.no_desc


(** explain a division by zero *)
let explain_divide_by_zero tenv exp node loc =
  match exp_rv_dexp tenv node exp with
  | Some de ->
      let exp_str = DExp.to_string de in
      Localise.desc_divide_by_zero exp_str loc
  | None ->
      Localise.no_desc


let warning_err loc fmt_string =
  L.(debug Analysis Medium) ("%a: Warning: " ^^ fmt_string) Location.pp loc
