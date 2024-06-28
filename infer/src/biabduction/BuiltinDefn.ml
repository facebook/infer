(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Models for the builtin functions supported *)

open SymExec
module L = Logging

type t = Builtin.registered

(** model va_arg as always returning 0 *)
let execute___builtin_va_arg {Builtin.analysis_data; prop_; path; args; loc} : Builtin.ret_typ =
  match args with
  | [(lexp3, typ3)] ->
      let instr' = Sil.Store {e1= lexp3; typ= typ3; e2= Exp.zero; loc} in
      SymExec.instrs ~mask_errors:true analysis_data (Instrs.singleton instr') [(prop_, path)]
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let mk_empty_array len = Predicates.Earray (len, [], Predicates.inst_none)

(* Make a rearranged array. As it is rearranged when it appears in a precondition
   it requires that the function is called with the array allocated. If not infer
   return a null pointer deref *)
let mk_empty_array_rearranged len =
  Predicates.Earray
    (len, [], Predicates.inst_rearrange true (AnalysisState.get_loc_exn ()) (State.get_path_pos ()))


let extract_array_type typ =
  if Language.curr_language_is Java then
    match typ.Typ.desc with Typ.Tptr (({Typ.desc= Tarray _} as arr), _) -> Some arr | _ -> None
  else
    match typ.Typ.desc with
    | Typ.Tarray _ ->
        Some typ
    | Typ.Tptr (elt, _) ->
        Some (Typ.mk_array ~default:typ elt)
    | _ ->
        None


(** Return a result from a procedure call. *)
let return_result tenv e prop (ret_id, _) = Prop.conjoin_eq tenv e (Exp.Var ret_id) prop

(* Add an array of typ pointed to by lexp to prop_ if it doesn't already exist *)
(*  Return the new prop and the array length *)
(*  Return None if it fails to add the array *)
let add_array_to_prop ({InterproceduralAnalysis.tenv; _} as analysis_data) prop_ lexp typ =
  let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
  let hpred_opt =
    List.find
      ~f:(function Predicates.Hpointsto (e, _, _) -> Exp.equal e n_lexp | _ -> false)
      prop.Prop.sigma
  in
  match hpred_opt with
  | Some (Predicates.Hpointsto (_, Predicates.Earray (len, _, _), _)) ->
      Some (len, prop)
  | Some _ ->
      None (* e points to something but not an array *)
  | None ->
      extract_array_type typ
      |> Option.map ~f:(fun arr_typ ->
             let len = Exp.Var (Ident.create_fresh Ident.kfootprint) in
             let s = mk_empty_array_rearranged len in
             let hpred =
               Prop.mk_ptsto tenv n_lexp s
                 (Exp.Sizeof
                    { typ= arr_typ
                    ; nbytes= None
                    ; dynamic_length= None
                    ; subtype= Subtype.exact
                    ; nullable= false } )
             in
             let sigma = prop.Prop.sigma in
             let sigma_fp = prop.Prop.sigma_fp in
             let prop' = Prop.set prop ~sigma:(hpred :: sigma) in
             let prop'' = Prop.set prop' ~sigma_fp:(hpred :: sigma_fp) in
             let prop'' = Prop.normalize tenv prop'' in
             (len, prop'') )


(* Add an array in prop if it is not allocated.*)
let execute___require_allocated_array {Builtin.analysis_data; prop_; path; args} : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] -> (
    match add_array_to_prop analysis_data prop_ lexp typ with
    | None ->
        []
    | Some (_, prop) ->
        [(prop, path)] )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute___get_array_length
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | [(lexp, typ)] -> (
    match add_array_to_prop analysis_data prop_ lexp typ with
    | None ->
        []
    | Some (len, prop) ->
        [(return_result tenv len prop ret_id_typ, path)] )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute___set_array_length {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; args}
    : Builtin.ret_typ =
  match args with
  | [(lexp, typ); (len, _)] -> (
    match add_array_to_prop analysis_data prop_ lexp typ with
    | None ->
        []
    | Some (_, prop_a) -> (
        (* Invariant: prop_a has an array pointed to by lexp *)
        let n_lexp, prop__ = check_arith_norm_exp analysis_data lexp prop_a in
        let n_len, prop = check_arith_norm_exp analysis_data len prop__ in
        let hpred, sigma' =
          List.partition_tf
            ~f:(function Predicates.Hpointsto (e, _, _) -> Exp.equal e n_lexp | _ -> false)
            prop.Prop.sigma
        in
        match hpred with
        | [Predicates.Hpointsto (e, Earray (_, esel, inst), t)] ->
            let hpred' = Predicates.Hpointsto (e, Earray (n_len, esel, inst), t) in
            let prop' = Prop.set prop ~sigma:(hpred' :: sigma') in
            [(Prop.normalize tenv prop', path)]
        | _ ->
            []
        (* by construction of prop_a this case is impossible *) ) )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute___print_value {Builtin.analysis_data; prop_; path; args} : Builtin.ret_typ =
  L.(debug Analysis Medium) "__print_value: " ;
  let do_arg (lexp, _) =
    let n_lexp, _ = check_arith_norm_exp analysis_data lexp prop_ in
    L.(debug Analysis Medium) "%a " Exp.pp n_lexp
  in
  List.iter ~f:do_arg args ;
  L.(debug Analysis Medium) "@." ;
  [(prop_, path)]


let is_undefined_opt tenv prop n_lexp = Option.is_some (Attribute.get_undef tenv prop n_lexp)

(** Creates an object in the heap with a given type, when the object is not known to be null or when
    it doesn't appear already in the heap. *)
let create_type tenv n_lexp typ prop =
  let prop_type =
    match
      List.find
        ~f:(function Predicates.Hpointsto (e, _, _) -> Exp.equal e n_lexp | _ -> false)
        prop.Prop.sigma
    with
    | Some _ ->
        prop
    | None -> (
        let mhpred =
          match typ.Typ.desc with
          | Typ.Tptr (typ', _) ->
              let sexp = Predicates.Estruct ([], Predicates.inst_none) in
              let texp =
                Exp.Sizeof
                  { typ= typ'
                  ; nbytes= None
                  ; dynamic_length= None
                  ; subtype= Subtype.subtypes
                  ; nullable= false }
              in
              let hpred = Prop.mk_ptsto tenv n_lexp sexp texp in
              Some hpred
          | Typ.Tarray _ ->
              let len = Exp.Var (Ident.create_fresh Ident.kfootprint) in
              let sexp = mk_empty_array len in
              let texp =
                Exp.Sizeof
                  { typ
                  ; nbytes= None
                  ; dynamic_length= None
                  ; subtype= Subtype.subtypes
                  ; nullable= false }
              in
              let hpred = Prop.mk_ptsto tenv n_lexp sexp texp in
              Some hpred
          | _ ->
              None
        in
        match mhpred with
        | Some hpred ->
            let sigma = prop.Prop.sigma in
            let sigma_fp = prop.Prop.sigma_fp in
            let prop' = Prop.set prop ~sigma:(hpred :: sigma) in
            let prop'' =
              let has_normal_variables =
                Exp.free_vars n_lexp |> Sequence.exists ~f:Ident.is_normal
              in
              if is_undefined_opt tenv prop n_lexp || has_normal_variables then prop'
              else Prop.set prop' ~sigma_fp:(hpred :: sigma_fp)
            in
            let prop'' = Prop.normalize tenv prop'' in
            prop''
        | None ->
            prop )
  in
  let sil_is_null = Exp.BinOp (Binop.Eq, n_lexp, Exp.zero) in
  let sil_is_nonnull = Exp.UnOp (Unop.LNot, sil_is_null, None) in
  let null_case = Propset.to_proplist (prune tenv ~positive:true sil_is_null prop) in
  let non_null_case = Propset.to_proplist (prune tenv ~positive:true sil_is_nonnull prop_type) in
  if List.length non_null_case > 0 && !BiabductionConfig.footprint then non_null_case
  else if List.length non_null_case > 0 && is_undefined_opt tenv prop n_lexp then non_null_case
  else null_case @ non_null_case


let execute___get_type_of
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
      let props = create_type tenv n_lexp typ prop in
      let aux prop =
        let hpred_opt =
          List.find_map
            ~f:(function
              | Predicates.Hpointsto (e, _, texp) when Exp.equal e n_lexp -> Some texp | _ -> None
              )
            prop.Prop.sigma
        in
        match hpred_opt with
        | Some texp ->
            (return_result tenv texp prop ret_id_typ, path)
        | None ->
            (return_result tenv Exp.zero prop ret_id_typ, path)
      in
      List.map ~f:aux props
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(** replace the type of the ptsto rooted at [root_e] with [texp] in [prop] *)
let replace_ptsto_texp tenv prop root_e texp =
  let process_sigma sigma =
    let sigma1, sigma2 =
      List.partition_tf
        ~f:(function Predicates.Hpointsto (e, _, _) -> Exp.equal e root_e | _ -> false)
        sigma
    in
    match sigma1 with
    | [Predicates.Hpointsto (e, se, _)] ->
        Predicates.Hpointsto (e, se, texp) :: sigma2
    | _ ->
        sigma
  in
  let sigma = prop.Prop.sigma in
  let sigma_fp = prop.Prop.sigma_fp in
  let prop' = Prop.set prop ~sigma:(process_sigma sigma) in
  let prop'' = Prop.set prop' ~sigma_fp:(process_sigma sigma_fp) in
  Prop.normalize tenv prop''


let execute___instanceof_cast ~instof
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | [(val1_, typ1); (texp2_, _)] ->
      let val1, prop__ = check_arith_norm_exp analysis_data val1_ prop_ in
      let texp2, prop = check_arith_norm_exp analysis_data texp2_ prop__ in
      (* In Java, we throw an exception, in C++ we return 0 in case of a cast to a pointer, *)
      (* and throw an exception in case of a cast to a reference. *)
      let should_throw_exception =
        Language.curr_language_is Java || Language.curr_language_is CIL || Typ.is_reference typ1
      in
      let deal_with_failed_cast val1 texp1 texp2 =
        raise (Tabulation.create_cast_exception tenv __POS__ None texp1 texp2 val1)
      in
      let exe_one_prop prop =
        if Exp.equal texp2 Exp.zero then [(return_result tenv Exp.zero prop ret_id_typ, path)]
        else
          let res_opt =
            List.find
              ~f:(function Predicates.Hpointsto (e1, _, _) -> Exp.equal e1 val1 | _ -> false)
              prop.Prop.sigma
            |> Option.map ~f:(function
                 | Predicates.Hpointsto (_, _, texp1) -> (
                     let pos_type_opt, neg_type_opt =
                       SubtypingCheck.subtype_case_analysis tenv texp1 texp2
                     in
                     let mk_res type_opt res_e =
                       match type_opt with
                       | None ->
                           []
                       | Some texp1' ->
                           let prop' =
                             if Exp.equal texp1 texp1' then prop
                             else replace_ptsto_texp tenv prop val1 texp1'
                           in
                           [(return_result tenv res_e prop' ret_id_typ, path)]
                     in
                     if instof then
                       (* instanceof *)
                       let pos_res = mk_res pos_type_opt Exp.one in
                       let neg_res = mk_res neg_type_opt Exp.zero in
                       pos_res @ neg_res
                     else if (* cast *)
                             not should_throw_exception then
                       (* C++ case when negative cast returns 0 *)
                       let pos_res = mk_res pos_type_opt val1 in
                       let neg_res = mk_res neg_type_opt Exp.zero in
                       pos_res @ neg_res
                     else if !BiabductionConfig.footprint then
                       match pos_type_opt with
                       | None ->
                           deal_with_failed_cast val1 texp1 texp2
                       | Some _ ->
                           mk_res pos_type_opt val1
                     else
                       (* !BiabductionConfig.footprint is false *)
                       match neg_type_opt with
                       | Some _ ->
                           if is_undefined_opt tenv prop val1 then mk_res pos_type_opt val1
                           else deal_with_failed_cast val1 texp1 texp2
                       | None ->
                           mk_res pos_type_opt val1 )
                 | _ ->
                     [] )
          in
          match res_opt with
          | Some res ->
              res
          | None ->
              [(return_result tenv val1 prop ret_id_typ, path)]
      in
      let props = create_type tenv val1 typ1 prop in
      List.concat_map ~f:exe_one_prop props
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute___instanceof builtin_args : Builtin.ret_typ =
  execute___instanceof_cast ~instof:true builtin_args


let execute___cast builtin_args : Builtin.ret_typ =
  execute___instanceof_cast ~instof:false builtin_args


let set_resource_attribute tenv prop path n_lexp loc ra_res =
  let prop' =
    match Attribute.get_resource tenv prop n_lexp with
    | Some (Apred (Aresource ra, _)) ->
        Attribute.add_or_replace tenv prop (Apred (Aresource {ra with ra_res}, [n_lexp]))
    | _ ->
        let pname = PredSymb.mem_alloc_pname PredSymb.Mnew in
        let ra =
          {PredSymb.ra_kind= Racquire; ra_res; ra_pname= pname; ra_loc= loc; ra_vpath= None}
        in
        Attribute.add_or_replace tenv prop (Apred (Aresource ra, [n_lexp]))
  in
  [(prop', path)]


(** Set the attibute of the value as file *)
let execute___set_file_attribute
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; args; loc} : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc PredSymb.Rfile
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(** Set the resource attribute of the first real argument of method as ignore, the first argument is
    assumed to be "this" *)
let execute___method_set_ignore_attribute
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; args; loc} : Builtin.ret_typ =
  match args with
  | [_; (lexp, _)] ->
      let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc PredSymb.Rignore
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(** Set the attibute of the value as memory *)
let execute___set_mem_attribute
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; args; loc} : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc (PredSymb.Rmemory PredSymb.Mnew)
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let set_attr ({InterproceduralAnalysis.tenv; _} as analysis_data) prop path exp attr =
  let n_lexp, prop = check_arith_norm_exp analysis_data exp prop in
  [(Attribute.add_or_replace tenv prop (Apred (attr, [n_lexp])), path)]


let delete_attr ({InterproceduralAnalysis.tenv; _} as analysis_data) prop path exp attr =
  let n_lexp, prop = check_arith_norm_exp analysis_data exp prop in
  [(Attribute.remove tenv prop (Apred (attr, [n_lexp])), path)]


(** Set attibute att *)
let execute___set_attr attr {Builtin.analysis_data; prop_; path; args} : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      set_attr analysis_data prop_ path lexp attr
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(** Delete the locked attibute of the value*)
let execute___delete_locked_attribute {Builtin.analysis_data; prop_; path; args} : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      delete_attr analysis_data prop_ path lexp PredSymb.Alocked
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(** Set the attibute of the value as locked*)
let execute___set_locked_attribute builtin_args : Builtin.ret_typ =
  execute___set_attr PredSymb.Alocked builtin_args


(** Set the attibute of the value as wont leak*)
let execute___set_wont_leak_attribute builtin_args : Builtin.ret_typ =
  execute___set_attr PredSymb.Awont_leak builtin_args


let execute_exit {Builtin.prop_; path} : Builtin.ret_typ = SymExec.diverge prop_ path

let execute_free_ tenv mk ?(mark_as_freed = true) loc acc iter =
  match Prop.prop_iter_current tenv iter with
  | Predicates.Hpointsto (lexp, _, _), [] ->
      let prop = Prop.prop_iter_remove_curr_then_to_prop tenv iter in
      if mark_as_freed then
        let pname = PredSymb.mem_dealloc_pname mk in
        let ra =
          { PredSymb.ra_kind= PredSymb.Rrelease
          ; PredSymb.ra_res= PredSymb.Rmemory mk
          ; PredSymb.ra_pname= pname
          ; PredSymb.ra_loc= loc
          ; PredSymb.ra_vpath= None }
        in
        (* mark value as freed *)
        let p_res =
          Attribute.add_or_replace_check_changed tenv prop (Apred (Aresource ra, [lexp]))
        in
        p_res :: acc
      else prop :: acc
  | Predicates.Hpointsto _, _ :: _ ->
      assert false (* alignment error *)
  | _ ->
      assert false


(* should not happen *)

let execute_free_nonzero_ mk ?(mark_as_freed = true)
    ({InterproceduralAnalysis.tenv; _} as analysis_data) instr prop lexp typ loc =
  try
    match Prover.is_root tenv prop lexp lexp with
    | None ->
        L.d_strln ".... Alignment Error: Freed a non root ...." ;
        assert false
    | Some _ ->
        let prop_list =
          List.fold
            ~f:(execute_free_ tenv mk ~mark_as_freed loc)
            ~init:[]
            (Rearrange.rearrange analysis_data lexp typ prop loc)
        in
        List.rev prop_list
  with Rearrange.ARRAY_ACCESS ->
    if Int.equal Config.biabduction_array_level 0 then assert false
    else (
      L.d_strln ".... Array containing allocated heap cells ...." ;
      L.d_str "  Instr: " ;
      Sil.d_instr instr ;
      L.d_ln () ;
      L.d_str "  PROP: " ;
      Prop.d_prop prop ;
      L.d_ln () ;
      raise (Exceptions.Array_of_pointsto __POS__) )


let execute_free mk ?(mark_as_freed = true)
    {Builtin.analysis_data= {tenv; _} as analysis_data; instr; prop_; path; args; loc} :
    Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
      let prop_nonzero =
        (* case n_lexp!=0 *)
        Propset.to_proplist (prune tenv ~positive:true n_lexp prop)
      in
      let prop_zero =
        (* case n_lexp==0 *)
        Propset.to_proplist (prune tenv ~positive:false n_lexp prop)
      in
      let plist =
        prop_zero
        @ (* model: if 0 then skip else execute_free_nonzero_ *)
        List.concat_map
          ~f:(fun p ->
            execute_free_nonzero_ mk ~mark_as_freed analysis_data instr p
              (Prop.exp_normalize_prop tenv p lexp)
              typ loc )
          prop_nonzero
      in
      List.map ~f:(fun p -> (p, path)) plist
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute_alloc mk can_return_null
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args; loc} :
    Builtin.ret_typ =
  let rec evaluate_char_sizeof e =
    match e with
    | Exp.Var _ ->
        e
    | Exp.UnOp (uop, e', typ) ->
        Exp.UnOp (uop, evaluate_char_sizeof e', typ)
    | Exp.BinOp (bop, e1', e2') ->
        Exp.BinOp (bop, evaluate_char_sizeof e1', evaluate_char_sizeof e2')
    | Exp.Exn _
    | Exp.Closure _
    | Exp.Const _
    | Exp.Cast _
    | Exp.Lvar _
    | Exp.Lfield _
    | Exp.Lindex _ ->
        e
    | Exp.Sizeof {typ= {Typ.desc= Tarray {elt= {Typ.desc= Tint ik}}}; dynamic_length= Some len}
      when Typ.ikind_is_char ik ->
        evaluate_char_sizeof len
    | Exp.Sizeof
        {typ= {Typ.desc= Tarray {elt= {Typ.desc= Tint ik}; length= Some len}}; dynamic_length= None}
      when Typ.ikind_is_char ik ->
        evaluate_char_sizeof (Exp.Const (Const.Cint len))
    | Exp.Sizeof _ ->
        e
  in
  let size_exp, procname =
    match args with
    | [(size_exp, _)] ->
        (* for malloc and __new *)
        (size_exp, PredSymb.mem_alloc_pname mk)
    | [(size_exp, _); (Exp.Const (Const.Cfun pname), _)] ->
        (size_exp, pname)
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__)
  in
  let ret_id = fst ret_id_typ in
  let size_exp', prop =
    let n_size_exp, prop = check_arith_norm_exp analysis_data size_exp prop_ in
    let n_size_exp' = evaluate_char_sizeof n_size_exp in
    (Prop.exp_normalize_prop tenv prop n_size_exp', prop)
  in
  let cnt_te =
    Exp.Sizeof
      { typ= Typ.mk_array (Typ.mk (Tint Typ.IChar)) ~stride:(IntLit.of_int 1)
      ; nbytes= None
      ; dynamic_length= Some size_exp'
      ; subtype= Subtype.exact
      ; nullable= false }
  in
  let id_new = Ident.create_fresh Ident.kprimed in
  let exp_new = Exp.Var id_new in
  let ptsto_new = Prop.mk_ptsto_exp tenv Prop.Fld_init (exp_new, cnt_te, None) Predicates.Ialloc in
  let prop_plus_ptsto =
    let prop' = Prop.normalize tenv (Prop.prop_sigma_star prop [ptsto_new]) in
    let ra =
      { PredSymb.ra_kind= PredSymb.Racquire
      ; PredSymb.ra_res= PredSymb.Rmemory mk
      ; PredSymb.ra_pname= procname
      ; PredSymb.ra_loc= loc
      ; PredSymb.ra_vpath= None }
    in
    (* mark value as allocated *)
    Attribute.add_or_replace tenv prop' (Apred (Aresource ra, [exp_new]))
  in
  let prop_alloc = Prop.conjoin_eq tenv (Exp.Var ret_id) exp_new prop_plus_ptsto in
  if can_return_null then
    let prop_null = Prop.conjoin_eq tenv (Exp.Var ret_id) Exp.zero prop in
    [(prop_alloc, path); (prop_null, path)]
  else [(prop_alloc, path)]


let execute___cxx_typeid ({Builtin.analysis_data; prop_; args; loc} as r) : Builtin.ret_typ =
  match args with
  | type_info_exp :: rest -> (
      let res = execute_alloc PredSymb.Mnew false {r with args= [type_info_exp]} in
      match rest with
      | [(field_exp, _); (lexp, typ_)] ->
          let n_lexp, prop = check_arith_norm_exp analysis_data lexp prop_ in
          let typ =
            List.find
              ~f:(function Predicates.Hpointsto (e, _, _) -> Exp.equal e n_lexp | _ -> false)
              prop.Prop.sigma
            |> Option.value_map
                 ~f:(function Predicates.Hpointsto (_, _, Exp.Sizeof {typ}) -> typ | _ -> typ_)
                 ~default:typ_
          in
          let typ_string = Typ.to_string typ in
          let set_instr =
            Sil.Store {e1= field_exp; typ= StdTyp.void; e2= Exp.Const (Const.Cstr typ_string); loc}
          in
          SymExec.instrs ~mask_errors:true analysis_data (Instrs.singleton set_instr) res
      | _ ->
          res )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute_pthread_create
    ({Builtin.analysis_data= {analyze_dependency; tenv}; prop_; path; args} as builtin_args) :
    Builtin.ret_typ =
  match args with
  | [_; _; start_routine; arg] -> (
      let routine_name = Prop.exp_normalize_prop tenv prop_ (fst start_routine) in
      let routine_arg = Prop.exp_normalize_prop tenv prop_ (fst arg) in
      let pname =
        match (routine_name, snd start_routine) with
        | Exp.Lvar pvar, _ ->
            let fun_name = Pvar.get_name pvar in
            let fun_string = Mangled.to_string fun_name in
            Some (Procname.from_string_c_fun fun_string)
        | Exp.Const (Cfun pname), _ ->
            Some pname
        | _ ->
            None
      in
      match pname with
      | None ->
          L.d_str "pthread_create: unknown function " ;
          Exp.d_exp routine_name ;
          L.d_strln ", skipping call." ;
          [(prop_, path)]
      | Some pname -> (
          L.d_printfln "pthread_create: calling function %a" Procname.pp pname ;
          match analyze_dependency pname with
          | Error _ ->
              (* no precondition to check, skip *)
              [(prop_, path)]
          | Ok callee_summary ->
              SymExec.proc_call pname callee_summary
                {builtin_args with args= [(routine_arg, snd arg)]} ) )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute_skip {Builtin.prop_; path} : Builtin.ret_typ = [(prop_, path)]

let execute_scan_function skip_n_arguments ({Builtin.args; ret_id_typ} as call_args) :
    Builtin.ret_typ =
  match args with
  | _ when List.length args >= skip_n_arguments ->
      let varargs = ref args in
      varargs := IList.drop !varargs skip_n_arguments ;
      SymExec.unknown_or_scan_call ~is_scan:true ~reason:"execute scan function" (snd ret_id_typ)
        Annot.Item.empty {call_args with args= !varargs}
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute__unwrap_exception
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | [(ret_exn, _)] -> (
      let n_ret_exn, prop = check_arith_norm_exp analysis_data ret_exn prop_ in
      match n_ret_exn with
      | Exp.Exn exp ->
          let prop_with_exn = return_result tenv exp prop ret_id_typ in
          [(prop_with_exn, path)]
      | _ ->
          assert false )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute_return_first_argument
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | (arg1_, _) :: _ ->
      let arg1, prop = check_arith_norm_exp analysis_data arg1_ prop_ in
      let prop' = return_result tenv arg1 prop ret_id_typ in
      [(prop', path)]
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


let execute___split_get_nth
    {Builtin.analysis_data= {tenv; _} as analysis_data; prop_; path; ret_id_typ; args} :
    Builtin.ret_typ =
  match args with
  | [(lexp1, _); (lexp2, _); (lexp3, _)] -> (
      let n_lexp1, prop__ = check_arith_norm_exp analysis_data lexp1 prop_ in
      let n_lexp2, prop___ = check_arith_norm_exp analysis_data lexp2 prop__ in
      let n_lexp3, prop = check_arith_norm_exp analysis_data lexp3 prop___ in
      match (n_lexp1, n_lexp2, n_lexp3) with
      | Exp.Const (Const.Cstr str1), Exp.Const (Const.Cstr str2), Exp.Const (Const.Cint n_sil) -> (
          let n = IntLit.to_int_exn n_sil in
          try
            (* NOTE: [Str.regexp_string] may be expensive. *)
            let parts = Str.split (Str.regexp_string str2) str1 in
            let n_part = List.nth_exn parts n in
            let res = Exp.Const (Const.Cstr n_part) in
            [(return_result tenv res prop ret_id_typ, path)]
          with Caml.Not_found -> assert false )
      | _ ->
          [(prop, path)] )
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(* forces the expression passed as parameter to be assumed true at the point where this
   builtin is called, diverges if this causes an inconsistency *)
let execute___infer_assume {Builtin.analysis_data= {tenv}; prop_; path; args} : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let prop_assume = Prop.conjoin_eq tenv lexp (Exp.bool true) prop_ in
      if Prover.check_inconsistency tenv prop_assume then SymExec.diverge prop_assume path
      else [(prop_assume, path)]
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)


(* creates a named error state *)
let execute___infer_fail {Builtin.analysis_data= {tenv} as analysis_data; prop_; path; args; loc} :
    Builtin.ret_typ =
  let error_str =
    match args with
    | [(lexp_msg, _)] -> (
      match Prop.exp_normalize_prop tenv prop_ lexp_msg with
      | Exp.Const (Const.Cstr str) ->
          str
      | _ ->
          assert false )
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__)
  in
  let set_instr =
    Sil.Store
      { e1= Exp.Lvar Predicates.custom_error
      ; typ= StdTyp.void
      ; e2= Exp.Const (Const.Cstr error_str)
      ; loc }
  in
  SymExec.instrs ~mask_errors:true analysis_data (Instrs.singleton set_instr) [(prop_, path)]


(* translate builtin assertion failure *)
let execute___assert_fail {Builtin.analysis_data; prop_; path; args; loc} : Builtin.ret_typ =
  let error_str =
    match List.length args with
    | 4 ->
        Config.default_failure_name
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__)
  in
  let set_instr =
    Sil.Store
      { e1= Exp.Lvar Predicates.custom_error
      ; typ= StdTyp.void
      ; e2= Exp.Const (Const.Cstr error_str)
      ; loc }
  in
  SymExec.instrs ~mask_errors:true analysis_data (Instrs.singleton set_instr) [(prop_, path)]


let execute_objc_alloc_no_fail symb_state typ alloc_fun_opt {Builtin.analysis_data; ret_id_typ; loc}
    =
  let alloc_fun = Exp.Const (Const.Cfun BuiltinDecl.__objc_alloc_no_fail) in
  let ptr_typ = Typ.mk (Tptr (typ, Typ.Pk_pointer)) in
  let sizeof_typ =
    Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
  in
  let alloc_fun_exp =
    match alloc_fun_opt with
    | Some pname ->
        [(Exp.Const (Const.Cfun pname), StdTyp.void)]
    | None ->
        []
  in
  let alloc_instr =
    Sil.Call (ret_id_typ, alloc_fun, [(sizeof_typ, ptr_typ)] @ alloc_fun_exp, loc, CallFlags.default)
  in
  SymExec.instrs analysis_data (Instrs.singleton alloc_instr) symb_state


(* NSArray models *)
let execute_objc_NSArray_alloc_no_fail builtin_args symb_state pname =
  let ret_typ = snd builtin_args.Builtin.ret_id_typ in
  execute_objc_alloc_no_fail symb_state ret_typ (Some pname) builtin_args


let execute_NSArray_arrayWithObjects builtin_args =
  let n_formals = 1 in
  let res = SymExec.check_variadic_sentinel n_formals (0, 1) builtin_args in
  execute_objc_NSArray_alloc_no_fail builtin_args res BuiltinDecl.nsArray_arrayWithObjects


(* NSDictionary models *)
let execute_objc_NSDictionary_alloc_no_fail symb_state pname builtin_args =
  let ret_typ = snd builtin_args.Builtin.ret_id_typ in
  execute_objc_alloc_no_fail symb_state ret_typ (Some pname) builtin_args


let execute___objc_dictionary_literal builtin_args =
  let n_formals = 1 in
  let res' = SymExec.check_variadic_sentinel ~fails_on_nil:true n_formals (0, 1) builtin_args in
  let pname = BuiltinDecl.__objc_dictionary_literal in
  execute_objc_NSDictionary_alloc_no_fail res' pname builtin_args


let __assert_fail = Builtin.register BuiltinDecl.__assert_fail execute___assert_fail

let __builtin_add_overflow = Builtin.register BuiltinDecl.__builtin_add_overflow execute_skip

let __builtin_mul_overflow = Builtin.register BuiltinDecl.__builtin_mul_overflow execute_skip

let __builtin_sub_overflow = Builtin.register BuiltinDecl.__builtin_sub_overflow execute_skip

let __builtin_va_arg = Builtin.register BuiltinDecl.__builtin_va_arg execute___builtin_va_arg

let __builtin_va_copy = Builtin.register BuiltinDecl.__builtin_va_copy execute_skip

let __builtin_va_end = Builtin.register BuiltinDecl.__builtin_va_end execute_skip

let __builtin_va_start = Builtin.register BuiltinDecl.__builtin_va_start execute_skip

(* Ideally, offsetof needs to be properly handled here, instead of just skipped. *)
let __builtin_offsetof = Builtin.register BuiltinDecl.__builtin_offsetof execute_skip

(* [__cast(val,typ)] implements java's [typ(val)] *)
let __cast = Builtin.register BuiltinDecl.__cast execute___cast

let __cxx_typeid = Builtin.register BuiltinDecl.__cxx_typeid execute___cxx_typeid

let __delete = Builtin.register BuiltinDecl.__delete (execute_free PredSymb.Mnew)

let __delete_array = Builtin.register BuiltinDecl.__delete_array (execute_free PredSymb.Mnew_array)

let __delete_locked_attribute =
  Builtin.register BuiltinDecl.__delete_locked_attribute execute___delete_locked_attribute


let __exit = Builtin.register BuiltinDecl.__exit execute_exit

let __objc_bridge_transfer = Builtin.register BuiltinDecl.__objc_bridge_transfer execute_skip

(* return the length of the array passed as a parameter *)
let __get_array_length = Builtin.register BuiltinDecl.__get_array_length execute___get_array_length

let __get_type_of = Builtin.register BuiltinDecl.__get_type_of execute___get_type_of

(* infer assume, diverging on inconsistencies *)
let __infer_assume = Builtin.register BuiltinDecl.__infer_assume execute___infer_assume

(* externally create new errors *)
let __infer_fail = Builtin.register BuiltinDecl.__infer_fail execute___infer_fail

let __infer_skip = Builtin.register BuiltinDecl.__infer_skip execute_skip

(* [__instanceof(val,typ)] implements java's [val instanceof typ] *)
let __instanceof = Builtin.register BuiltinDecl.__instanceof execute___instanceof

(* Throw is not handled via this builtin by biabduction *)
let __java_throw = Builtin.register BuiltinDecl.__java_throw execute_skip

let __hack_throw = Builtin.register BuiltinDecl.__hack_throw execute_skip

let __method_set_ignore_attribute =
  Builtin.register BuiltinDecl.__method_set_ignore_attribute execute___method_set_ignore_attribute


let __new = Builtin.register BuiltinDecl.__new (execute_alloc PredSymb.Mnew false)

let __new_array = Builtin.register BuiltinDecl.__new_array (execute_alloc PredSymb.Mnew_array false)

(* like __objc_alloc, but does not return nil *)
let __objc_alloc_no_fail =
  Builtin.register BuiltinDecl.__objc_alloc_no_fail (execute_alloc PredSymb.Mobjc false)


let __objc_dictionary_literal =
  Builtin.register BuiltinDecl.__objc_dictionary_literal execute___objc_dictionary_literal


let __objc_get_ref_count = Builtin.register BuiltinDecl.__objc_get_ref_count execute_skip

let __objc_set_ref_count = Builtin.register BuiltinDecl.__objc_set_ref_count execute_skip

let __placement_delete = Builtin.register BuiltinDecl.__placement_delete execute_skip

let __placement_new = Builtin.register BuiltinDecl.__placement_new execute_return_first_argument

(* print a value as seen by the engine *)
let __print_value = Builtin.register BuiltinDecl.__print_value execute___print_value

(* require the parameter to point to an allocated array *)
let __require_allocated_array =
  Builtin.register BuiltinDecl.__require_allocated_array execute___require_allocated_array


let __set_array_length = Builtin.register BuiltinDecl.__set_array_length execute___set_array_length

let __set_file_attribute =
  Builtin.register BuiltinDecl.__set_file_attribute execute___set_file_attribute


let __set_locked_attribute =
  Builtin.register BuiltinDecl.__set_locked_attribute execute___set_locked_attribute


let __set_mem_attribute =
  Builtin.register BuiltinDecl.__set_mem_attribute execute___set_mem_attribute


let __set_observer_attribute =
  Builtin.register BuiltinDecl.__set_observer_attribute (execute___set_attr PredSymb.Aobserver)


let __set_unsubscribed_observer_attribute =
  Builtin.register BuiltinDecl.__set_unsubscribed_observer_attribute
    (execute___set_attr PredSymb.Aunsubscribed_observer)


let __set_wont_leak_attribute =
  Builtin.register BuiltinDecl.__set_wont_leak_attribute execute___set_wont_leak_attribute


(* splits a string given a separator and returns the nth string *)
let __split_get_nth = Builtin.register BuiltinDecl.__split_get_nth execute___split_get_nth

let __throw = Builtin.register BuiltinDecl.__throw execute_skip

let __unwrap_exception = Builtin.register BuiltinDecl.__unwrap_exception execute__unwrap_exception

let abort = Builtin.register BuiltinDecl.abort execute_exit

let exit = Builtin.register BuiltinDecl.exit execute_exit

let free = Builtin.register BuiltinDecl.free (execute_free PredSymb.Mmalloc)

let fscanf = Builtin.register BuiltinDecl.fscanf (execute_scan_function 2)

let fwscanf = Builtin.register BuiltinDecl.fwscanf (execute_scan_function 2)

let malloc =
  Builtin.register BuiltinDecl.malloc
    (execute_alloc PredSymb.Mmalloc (not Config.biabduction_unsafe_malloc))


let malloc_no_fail =
  Builtin.register BuiltinDecl.malloc_no_fail (execute_alloc PredSymb.Mmalloc false)


let nsArray_arrayWithObjects =
  Builtin.register BuiltinDecl.nsArray_arrayWithObjects execute_NSArray_arrayWithObjects


let nsArray_arrayWithObjectsCount =
  Builtin.register BuiltinDecl.nsArray_arrayWithObjectsCount execute_skip


let objc_insert_key = Builtin.register BuiltinDecl.objc_insert_key execute_skip

let objc_insert_value = Builtin.register BuiltinDecl.objc_insert_value execute_skip

let objc_autorelease_pool_pop = Builtin.register BuiltinDecl.objc_autorelease_pool_pop execute_skip

let objc_autorelease_pool_push =
  Builtin.register BuiltinDecl.objc_autorelease_pool_push execute_skip


(* model throwing exception in objc/c++ as divergence *)
let objc_cpp_throw = Builtin.register BuiltinDecl.objc_cpp_throw execute_exit

let pthread_create = Builtin.register BuiltinDecl.pthread_create execute_pthread_create

let scanf = Builtin.register BuiltinDecl.scanf (execute_scan_function 1)

let sscanf = Builtin.register BuiltinDecl.sscanf (execute_scan_function 2)

let swscanf = Builtin.register BuiltinDecl.swscanf (execute_scan_function 2)

let vfscanf = Builtin.register BuiltinDecl.vfscanf (execute_scan_function 2)

let vfwscanf = Builtin.register BuiltinDecl.vfwscanf (execute_scan_function 2)

let vscanf = Builtin.register BuiltinDecl.vscanf (execute_scan_function 1)

let vsscanf = Builtin.register BuiltinDecl.vsscanf (execute_scan_function 2)

let vswscanf = Builtin.register BuiltinDecl.vswscanf (execute_scan_function 2)

let vwscanf = Builtin.register BuiltinDecl.vwscanf (execute_scan_function 1)

let wscanf = Builtin.register BuiltinDecl.wscanf (execute_scan_function 1)

let zero_initialization = Builtin.register BuiltinDecl.zero_initialization execute_skip

(* Function exists to load module and guarantee that the side-effects of Builtin.register
   calls have been done. *)
let init () = ()
