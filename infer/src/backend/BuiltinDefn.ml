(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Models for the builtin functions supported *)

open SymExec

module L = Logging
module F = Format

type t = Builtin.registered

let execute___no_op prop path: Builtin.ret_typ =
  [(prop, path)]

(** model va_arg as always returning 0 *)
let execute___builtin_va_arg { Builtin.pdesc; tenv; prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [_; _; (lexp3, typ3)], _ ->
      let instr' = Sil.Store (lexp3, typ3, Exp.zero, loc) in
      SymExec.instrs ~mask_errors:true tenv pdesc [instr'] [(prop_, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let mk_empty_array len =
  Sil.Earray (len, [], Sil.inst_none)

(* Make a rearranged array. As it is rearranged when it appears in a precondition
   it requires that the function is called with the array allocated. If not infer
   return a null pointer deref *)
let mk_empty_array_rearranged len =
  Sil.Earray (len, [], Sil.inst_rearrange true (State.get_loc ()) (State.get_path_pos ()))

let extract_array_type typ =
  if (Config.curr_language_is Config.Java) then
    match typ with
    | Typ.Tptr (Typ.Tarray _ as arr, _) -> Some arr
    | _ -> None
  else
    match typ with
    | Typ.Tarray _ as arr -> Some arr
    | Typ.Tptr (elt, _) -> Some (Typ.Tarray (elt, None))
    | _ -> None

(** Return a result from a procedure call. *)
let return_result tenv e prop ret_id =
  match ret_id with
  | Some (ret_id, _) -> Prop.conjoin_eq tenv e (Exp.Var ret_id) prop
  | _ -> prop

(* Add an array of typ pointed to by lexp to prop_ if it doesn't already exist *)
(*  Return the new prop and the array length *)
(*  Return None if it fails to add the array *)
let add_array_to_prop tenv pdesc prop_ lexp typ =
  let pname = Procdesc.get_proc_name pdesc in
  let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
  let hpred_opt =
    List.find
      ~f:(function
          | Sil.Hpointsto(e, _, _) -> Exp.equal e n_lexp
          | _ -> false)
      prop.Prop.sigma in
  match hpred_opt with
  | Some (Sil.Hpointsto (_, Sil.Earray (len, _, _), _)) ->
      Some (len, prop)
  | Some _ ->
      None (* e points to something but not an array *)
  | None ->
      extract_array_type typ |>
      Option.map ~f:(fun arr_typ ->
          let len = Exp.Var (Ident.create_fresh Ident.kfootprint) in
          let s = mk_empty_array_rearranged len in
          let hpred =
            Prop.mk_ptsto tenv n_lexp s (Exp.Sizeof (arr_typ, Some len, Subtype.exact)) in
          let sigma = prop.Prop.sigma in
          let sigma_fp = prop.Prop.sigma_fp in
          let prop'= Prop.set prop ~sigma:(hpred:: sigma) in
          let prop''= Prop.set prop' ~sigma_fp:(hpred:: sigma_fp) in
          let prop''= Prop.normalize tenv prop'' in
          (len, prop''))

(* Add an array in prop if it is not allocated.*)
let execute___require_allocated_array { Builtin.tenv; pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      (match add_array_to_prop tenv pdesc prop_ lexp typ with
       | None -> []
       | Some (_, prop) -> [(prop, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___get_array_length { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      (match add_array_to_prop tenv pdesc prop_ lexp typ with
       | None -> []
       | Some (len, prop) -> [(return_result tenv len prop ret_id, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___set_array_length { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, typ); (len, _)], None ->
      (match add_array_to_prop tenv pdesc prop_ lexp typ with
       | None -> []
       | Some (_, prop_a) -> (* Invariant: prop_a has an array pointed to by lexp *)
           let pname = Procdesc.get_proc_name pdesc in
           let n_lexp, prop__ = check_arith_norm_exp tenv pname lexp prop_a in
           let n_len, prop = check_arith_norm_exp tenv pname len prop__ in
           let hpred, sigma' = List.partition_tf ~f:(function
               | Sil.Hpointsto(e, _, _) -> Exp.equal e n_lexp
               | _ -> false) prop.Prop.sigma in
           (match hpred with
            | [Sil.Hpointsto(e, Sil.Earray(_, esel, inst), t)] ->
                let hpred' = Sil.Hpointsto (e, Sil.Earray (n_len, esel, inst), t) in
                let prop' = Prop.set prop ~sigma:(hpred':: sigma') in
                [(Prop.normalize tenv prop', path)]
            | _ -> [] (* by construction of prop_a this case is impossible *) ))
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___print_value { Builtin.tenv; pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  L.err "__print_value: ";
  let pname = Procdesc.get_proc_name pdesc in
  let do_arg (lexp, _) =
    let n_lexp, _ = check_arith_norm_exp tenv pname lexp prop_ in
    L.err "%a " Exp.pp n_lexp in
  List.iter ~f:do_arg args;
  L.err "@.";
  [(prop_, path)]

let is_undefined_opt tenv prop n_lexp =
  let is_undef =
    Option.is_some (Attribute.get_undef tenv prop n_lexp) in
  is_undef && Config.angelic_execution

(** Creates an object in the heap with a given type, when the object is not known to be null or when
    it doesn't appear already in the heap. *)
let create_type tenv n_lexp typ prop =
  let prop_type =
    match
      List.find ~f:(function
          | Sil.Hpointsto(e, _, _) -> Exp.equal e n_lexp
          | _ -> false) prop.Prop.sigma with
    | Some _ ->
        prop
    | None ->
        let mhpred =
          match typ with
          | Typ.Tptr (typ', _) ->
              let sexp = Sil.Estruct ([], Sil.inst_none) in
              let texp = Exp.Sizeof (typ', None, Subtype.subtypes) in
              let hpred = Prop.mk_ptsto tenv n_lexp sexp texp in
              Some hpred
          | Typ.Tarray _ ->
              let len = Exp.Var (Ident.create_fresh Ident.kfootprint) in
              let sexp = mk_empty_array len in
              let texp = Exp.Sizeof (typ, None, Subtype.subtypes) in
              let hpred = Prop.mk_ptsto tenv n_lexp sexp texp in
              Some hpred
          | _ -> None in
        match mhpred with
        | Some hpred ->
            let sigma = prop.Prop.sigma in
            let sigma_fp = prop.Prop.sigma_fp in
            let prop'= Prop.set prop ~sigma:(hpred:: sigma) in
            let prop''=
              let has_normal_variables =
                Sil.fav_exists (Sil.exp_fav n_lexp) Ident.is_normal in
              if (is_undefined_opt tenv prop n_lexp) || has_normal_variables
              then prop'
              else Prop.set prop' ~sigma_fp:(hpred:: sigma_fp) in
            let prop''= Prop.normalize tenv prop'' in
            prop''
        | None -> prop in
  let sil_is_null = Exp.BinOp (Binop.Eq, n_lexp, Exp.zero) in
  let sil_is_nonnull = Exp.UnOp (Unop.LNot, sil_is_null, None) in
  let null_case = Propset.to_proplist (prune tenv ~positive:true sil_is_null prop) in
  let non_null_case = Propset.to_proplist (prune tenv ~positive:true sil_is_nonnull prop_type) in
  if ((List.length non_null_case) > 0) && (!Config.footprint) then
    non_null_case
  else if ((List.length non_null_case) > 0) && (is_undefined_opt tenv prop n_lexp) then
    non_null_case
  else null_case @ non_null_case

let execute___get_type_of { Builtin.pdesc; tenv; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      let props = create_type tenv n_lexp typ prop in
      let aux prop =
        let hpred_opt =
          List.find_map ~f:(function
              | Sil.Hpointsto(e, _, texp) when Exp.equal e n_lexp -> Some texp
              | _ -> None) prop.Prop.sigma in
        match hpred_opt with
        | Some texp ->
            ((return_result tenv texp prop ret_id), path)
        | None ->
            ((return_result tenv Exp.zero prop ret_id), path) in
      (List.map ~f:aux props)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** replace the type of the ptsto rooted at [root_e] with [texp] in [prop] *)
let replace_ptsto_texp tenv prop root_e texp =
  let process_sigma sigma =
    let sigma1, sigma2 =
      List.partition_tf ~f:(function
          | Sil.Hpointsto(e, _, _) -> Exp.equal e root_e
          | _ -> false) sigma in
    match sigma1 with
    | [Sil.Hpointsto(e, se, _)] -> (Sil.Hpointsto (e, se, texp)) :: sigma2
    | _ -> sigma in
  let sigma = prop.Prop.sigma in
  let sigma_fp = prop.Prop.sigma_fp in
  let prop'= Prop.set prop ~sigma:(process_sigma sigma) in
  let prop''= Prop.set prop' ~sigma_fp:(process_sigma sigma_fp) in
  Prop.normalize tenv prop''

let execute___instanceof_cast ~instof
    { Builtin.pdesc; tenv; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(val1_, typ1); (texp2_, _)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let val1, prop__ = check_arith_norm_exp tenv pname val1_ prop_ in
      let texp2, prop = check_arith_norm_exp tenv pname texp2_ prop__ in
      let is_cast_to_reference =
        match typ1 with
        | Typ.Tptr (_, Typ.Pk_reference) -> true
        | _ -> false in
      (* In Java, we throw an exception, in C++ we return 0 in case of a cast to a pointer, *)
      (* and throw an exception in case of a cast to a reference. *)
      let should_throw_exception =
        Config.curr_language_is Config.Java || is_cast_to_reference in
      let deal_with_failed_cast val1 texp1 texp2 =
        raise
          (Tabulation.create_cast_exception
             tenv __POS__ None texp1 texp2 val1) in
      let exe_one_prop prop =
        if Exp.equal texp2 Exp.zero then
          [(return_result tenv Exp.zero prop ret_id, path)]
        else
          let res_opt =
            List.find ~f:(function
                | Sil.Hpointsto (e1, _, _) -> Exp.equal e1 val1
                | _ -> false) prop.Prop.sigma |>
            Option.map ~f:(function
                | Sil.Hpointsto (_, _, texp1) ->
                    let pos_type_opt, neg_type_opt =
                      Prover.Subtyping_check.subtype_case_analysis tenv texp1 texp2 in
                    let mk_res type_opt res_e = match type_opt with
                      | None -> []
                      | Some texp1' ->
                          let prop' =
                            if Exp.equal texp1 texp1' then prop
                            else replace_ptsto_texp tenv prop val1 texp1' in
                          [(return_result tenv res_e prop' ret_id, path)] in
                    if instof then (* instanceof *)
                      let pos_res = mk_res pos_type_opt Exp.one in
                      let neg_res = mk_res neg_type_opt Exp.zero in
                      pos_res @ neg_res
                    else (* cast *)
                    if not should_throw_exception then (* C++ case when negative cast returns 0 *)
                      let pos_res = mk_res pos_type_opt val1 in
                      let neg_res = mk_res neg_type_opt Exp.zero in
                      pos_res @ neg_res
                    else
                      begin
                        if !Config.footprint then
                          match pos_type_opt with
                          | None -> deal_with_failed_cast val1 texp1 texp2
                          | Some _ -> mk_res pos_type_opt val1
                        else (* !Config.footprint is false *)
                          match neg_type_opt with
                          | Some _ ->
                              if is_undefined_opt tenv prop val1 then mk_res pos_type_opt val1
                              else deal_with_failed_cast val1 texp1 texp2
                          | None -> mk_res pos_type_opt val1
                      end
                | _ -> []
              ) in
          match res_opt with
          | Some res ->
              res
          | None ->
              [(return_result tenv val1 prop ret_id, path)] in
      let props = create_type tenv val1 typ1 prop in
      List.concat_map ~f:exe_one_prop props
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___instanceof builtin_args
  : Builtin.ret_typ =
  execute___instanceof_cast ~instof:true builtin_args

let execute___cast builtin_args
  : Builtin.ret_typ =
  execute___instanceof_cast ~instof:false builtin_args

let set_resource_attribute tenv prop path n_lexp loc ra_res =
  let prop' = match Attribute.get_resource tenv prop n_lexp with
    | Some (Apred (Aresource ra, _)) ->
        Attribute.add_or_replace tenv prop (Apred (Aresource { ra with ra_res }, [n_lexp]))
    | _ ->
        let pname = PredSymb.mem_alloc_pname PredSymb.Mnew in
        let ra =
          { PredSymb.
            ra_kind = Racquire;
            ra_res = ra_res;
            ra_pname = pname;
            ra_loc = loc;
            ra_vpath = None } in
        Attribute.add_or_replace tenv prop (Apred (Aresource ra, [n_lexp])) in
  [(prop', path)]

(** Set the attibute of the value as file *)
let execute___set_file_attribute { Builtin.tenv; pdesc; prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, _)], _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc PredSymb.Rfile
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as lock *)
let execute___set_lock_attribute { Builtin.tenv; pdesc; prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, _)], _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc PredSymb.Rlock
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the resource attribute of the first real argument of method as ignore, the first argument is
    assumed to be "this" *)
let execute___method_set_ignore_attribute { Builtin.tenv; pdesc; prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [_ ; (lexp, _)], _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc PredSymb.Rignore
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as memory *)
let execute___set_mem_attribute { Builtin.tenv; pdesc; prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, _)], _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      set_resource_attribute tenv prop path n_lexp loc (PredSymb.Rmemory PredSymb.Mnew)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** report an error if [lexp] is tainted; otherwise, add untained([lexp]) as a precondition *)
let execute___check_untainted
    { Builtin.tenv; pdesc; prop_; path; ret_id; args; proc_name = callee_pname; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, _)], _ ->
      let caller_pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv caller_pname lexp prop_ in
      [(check_untainted tenv n_lexp PredSymb.Tk_unknown caller_pname callee_pname prop, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** take a pointer to a struct, and return the value of a hidden field in the struct *)
let execute___get_hidden_field { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
      let ret_val = ref None in
      let return_val p = match !ret_val with
        | Some e -> return_result tenv e p ret_id
        | None -> p in
      let foot_var = lazy (Exp.Var (Ident.create_fresh Ident.kfootprint)) in
      let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
      let has_fld_hidden fsel = List.exists ~f:filter_fld_hidden fsel in
      let do_hpred in_foot hpred = match hpred with
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Exp.equal e n_lexp && (not (has_fld_hidden fsel)) ->
            let foot_e = Lazy.force foot_var in
            ret_val := Some foot_e;
            let se = Sil.Eexp(foot_e, Sil.inst_none) in
            let fsel' = (Ident.fieldname_hidden, se) :: fsel in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | Sil.Hpointsto(e, Sil.Estruct (fsel, _), _)
          when Exp.equal e n_lexp && not in_foot && has_fld_hidden fsel ->
            let set_ret_val () =
              match List.find ~f:filter_fld_hidden fsel with
              | Some (_, Sil.Eexp(e, _)) ->
                  ret_val := Some e
              | _ ->
                  () in
            set_ret_val();
            hpred
        | _ -> hpred in
      let sigma' = List.map ~f:(do_hpred false) prop.Prop.sigma in
      let sigma_fp' = List.map ~f:(do_hpred true) prop.Prop.sigma_fp in
      let prop' = Prop.set prop ~sigma:sigma' ~sigma_fp:sigma_fp' in
      let prop'' = return_val (Prop.normalize tenv prop') in
      [(prop'', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** take a pointer to a struct and a value,
    and set a hidden field in the struct to the given value *)
let execute___set_hidden_field { Builtin.tenv; pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp1, _); (lexp2, _)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp1, prop__ = check_arith_norm_exp tenv pname lexp1 prop_ in
      let n_lexp2, prop = check_arith_norm_exp tenv pname lexp2 prop__ in
      let foot_var = lazy (Exp.Var (Ident.create_fresh Ident.kfootprint)) in
      let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
      let has_fld_hidden fsel = List.exists ~f:filter_fld_hidden fsel in
      let do_hpred in_foot hpred = match hpred with
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Exp.equal e n_lexp1 && not in_foot ->
            let se = Sil.Eexp(n_lexp2, Sil.inst_none) in
            let fsel' =
              (Ident.fieldname_hidden, se) ::
              (List.filter ~f:(fun x -> not (filter_fld_hidden x)) fsel) in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Exp.equal e n_lexp1 && in_foot && not (has_fld_hidden fsel) ->
            let foot_e = Lazy.force foot_var in
            let se = Sil.Eexp(foot_e, Sil.inst_none) in
            let fsel' = (Ident.fieldname_hidden, se) :: fsel in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | _ -> hpred in
      let sigma' = List.map ~f:(do_hpred false) prop.Prop.sigma in
      let sigma_fp' = List.map ~f:(do_hpred true) prop.Prop.sigma_fp in
      let prop' = Prop.set prop ~sigma:sigma' ~sigma_fp:sigma_fp' in
      let prop'' = Prop.normalize tenv prop' in
      [(prop'', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* Update the objective-c hidden counter by applying the operation op and the operand delta.*)
(* Eg. op=+/- delta is an integer *)
let execute___objc_counter_update
    ~mask_errors op delta
    { Builtin.pdesc; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, (Typ.Tstruct _ as typ | Tptr (Tstruct _ as typ, _)))] ->
      (* Assumes that lexp is a temp n$1 that has the value of the object. *)
      (* This is the case as a call f(o) it's translates as n$1=*&o; f(n$1) *)
      (* n$2 = *n$1.hidden *)
      let tmp = Ident.create_fresh Ident.knormal in
      let hidden_field = Exp.Lfield (lexp, Ident.fieldname_hidden, typ) in
      let counter_to_tmp = Sil.Load (tmp, hidden_field, typ, loc) in
      (* *n$1.hidden = (n$2 +/- delta) *)
      let update_counter =
        Sil.Store (hidden_field, typ, BinOp (op, Var tmp, Const (Cint delta)), loc) in
      let update_counter_instrs =
        [ counter_to_tmp; update_counter; Sil.Remove_temps([tmp], loc) ] in
      SymExec.instrs ~mask_errors tenv pdesc update_counter_instrs [(prop_, path)]
  | [(_, typ)] ->
      L.d_str ("Trying to update hidden field of non-struct value. Type: " ^ (Typ.to_string typ));
      assert false
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* Given a list of args checks if the first is the flag indicating whether is a call to
   retain/release for which we have to suppress NPE report or not. If the flag is present it is
   removed from the list of args. *)
let get_suppress_npe_flag args =
  match args with
  | (Exp.Const (Const.Cint i), Typ.Tint Typ.IBool):: args' when IntLit.isone i ->
      false, args' (* this is a CFRelease/CFRetain *)
  | _ -> true, args

let execute___objc_retain_impl ({ Builtin.tenv; prop_; args; ret_id; } as builtin_args)
  : Builtin.ret_typ =
  let mask_errors, args' = get_suppress_npe_flag args in
  match args' with
  | [(lexp, _)] ->
      let prop = return_result tenv lexp prop_ ret_id in
      execute___objc_counter_update
        ~mask_errors (Binop.PlusA) (IntLit.one)
        { builtin_args with Builtin.prop_ = prop; args = args'; }
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___objc_retain builtin_args
  : Builtin.ret_typ =
  if Config.objc_memory_model_on then
    execute___objc_retain_impl builtin_args
  else execute___no_op builtin_args.Builtin.prop_ builtin_args.Builtin.path

let execute___objc_retain_cf builtin_args
  : Builtin.ret_typ =
  execute___objc_retain_impl builtin_args

let execute___objc_release_impl
    ({ Builtin.args; }
     as builtin_args)
  : Builtin.ret_typ =
  let mask_errors, args' = get_suppress_npe_flag args in
  execute___objc_counter_update
    ~mask_errors Binop.MinusA IntLit.one
    { builtin_args with Builtin.args = args'; }

let execute___objc_release builtin_args
  : Builtin.ret_typ =
  if Config.objc_memory_model_on then
    execute___objc_release_impl builtin_args
  else execute___no_op builtin_args.Builtin.prop_ builtin_args.Builtin.path

let execute___objc_release_cf builtin_args
  : Builtin.ret_typ =
  execute___objc_release_impl builtin_args

(** Set the attibute of the value as objc autoreleased *)
let execute___set_autorelease_attribute
    { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args, ret_id with
  | [(lexp, _)], _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let prop = return_result tenv lexp prop_ ret_id in
      if Config.objc_memory_model_on then
        let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop in
        let prop' = Attribute.add_or_replace tenv prop (Apred (Aautorelease, [n_lexp])) in
        [(prop', path)]
      else execute___no_op prop path
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Release all the objects in the pool *)
let execute___release_autorelease_pool
    ({ Builtin.tenv; prop_; path; } as builtin_args)
  : Builtin.ret_typ =
  if Config.objc_memory_model_on then
    let autoreleased_objects = Attribute.get_for_symb prop_ Aautorelease in
    let prop_without_attribute = Attribute.remove_for_attr tenv prop_ Aautorelease in
    let call_release res atom =
      match res, atom with
      | ((prop', path') :: _, Sil.Apred (_, exp :: _)) ->
          List.find ~f:(function
              | Sil.Hpointsto(e1, _, _) -> Exp.equal e1 exp
              | _ -> false) prop_.Prop.sigma |>
          Option.value_map ~f:(function
              | Sil.Hpointsto (_, _, Exp.Sizeof (typ, _, _)) ->
                  let res1 =
                    execute___objc_release
                      { builtin_args with
                        Builtin.args = [(exp, typ)];
                        prop_ = prop';
                        path = path'; } in
                  res1
              | _ -> res
            )
            ~default:res
      | _ -> res in
    List.fold ~f:call_release ~init:[(prop_without_attribute, path)] autoreleased_objects
  else execute___no_op prop_ path

let set_attr tenv pdesc prop path exp attr =
  let pname = Procdesc.get_proc_name pdesc in
  let n_lexp, prop = check_arith_norm_exp tenv pname exp prop in
  [(Attribute.add_or_replace tenv prop (Apred (attr, [n_lexp])), path)]

let delete_attr tenv pdesc prop path exp attr =
  let pname = Procdesc.get_proc_name pdesc in
  let n_lexp, prop = check_arith_norm_exp tenv pname exp prop in
  [(Attribute.remove tenv prop (Apred (attr, [n_lexp])), path)]


(** Set attibute att *)
let execute___set_attr attr { Builtin.tenv; pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] -> set_attr tenv pdesc prop_ path lexp attr
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Delete the locked attibute of the value*)
let execute___delete_locked_attribute { Builtin.tenv; prop_; pdesc; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] -> delete_attr tenv pdesc prop_ path lexp PredSymb.Alocked
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)


(** Set the attibute of the value as locked*)
let execute___set_locked_attribute builtin_args
  : Builtin.ret_typ =
  execute___set_attr (PredSymb.Alocked) builtin_args

(** Set the attibute of the value as resource/unlocked*)
let execute___set_unlocked_attribute
    ({ Builtin.pdesc; loc; } as builtin_args)
  : Builtin.ret_typ =
  let pname = Procdesc.get_proc_name pdesc in
  (* ra_kind = Rrelease in following indicates unlocked *)
  let ra = {
    PredSymb.ra_kind = PredSymb.Rrelease;
    ra_res = PredSymb.Rlock;
    ra_pname = pname;
    ra_loc = loc;
    ra_vpath = None; } in
  execute___set_attr (PredSymb.Aresource ra) builtin_args

(** Set the attibute of the value as tainted *)
let execute___set_taint_attribute
    ({ Builtin.tenv; pdesc; args; prop_; path; })
  : Builtin.ret_typ =
  match args with
  | (exp, _) :: [(Exp.Const (Const.Cstr taint_kind_str), _)] ->
      let taint_source = Procdesc.get_proc_name pdesc in
      let taint_kind = match taint_kind_str with
        | "UnverifiedSSLSocket" -> PredSymb.Tk_unverified_SSL_socket
        | "SharedPreferenceData" -> PredSymb.Tk_shared_preferences_data
        | other_str -> failwith ("Unrecognized taint kind " ^ other_str) in
      set_attr tenv pdesc prop_ path exp (PredSymb.Ataint { PredSymb.taint_source; taint_kind})
  | _ ->
      (* note: we can also get this if [taint_kind] is not a string literal *)
      raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as tainted *)
let execute___set_untaint_attribute
    ({ Builtin.tenv; pdesc; args; prop_; path; })
  : Builtin.ret_typ =
  match args with
  | (exp, _) :: [] ->
      let taint_source = Procdesc.get_proc_name pdesc in
      let taint_kind = PredSymb.Tk_unknown in (* TODO: change builtin to specify taint kind *)
      set_attr tenv pdesc prop_ path exp (PredSymb.Auntaint { PredSymb.taint_source; taint_kind})
  | _ ->
      raise (Exceptions.Wrong_argument_number __POS__)

let execute___objc_cast { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(val1_, _); (texp2_, _)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let val1, prop__ = check_arith_norm_exp tenv pname val1_ prop_ in
      let texp2, prop = check_arith_norm_exp tenv pname texp2_ prop__ in
      (match
         List.find ~f:(function
             | Sil.Hpointsto(e1, _, _) -> Exp.equal e1 val1
             | _ -> false) prop.Prop.sigma |>
         Option.map ~f:(fun hpred -> match hpred, texp2 with
             | Sil.Hpointsto (val1, _, _), Exp.Sizeof _ ->
                 let prop' = replace_ptsto_texp tenv prop val1 texp2 in
                 [(return_result tenv val1 prop' ret_id, path)]
             | _ -> [(return_result tenv val1 prop ret_id, path)]
           )
       with
       | Some res ->
           res
       | None ->
           [(return_result tenv val1 prop ret_id, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_abort { Builtin.proc_name; }
  : Builtin.ret_typ =
  raise
    (Exceptions.Precondition_not_found
       (Localise.verbatim_desc (Typ.Procname.to_string proc_name), __POS__))

let execute_exit { Builtin.prop_; path; }
  : Builtin.ret_typ =
  SymExec.diverge prop_ path

let _execute_free tenv mk loc acc iter =
  match Prop.prop_iter_current tenv iter with
  | (Sil.Hpointsto(lexp, _, _), []) ->
      let prop = Prop.prop_iter_remove_curr_then_to_prop tenv iter in
      let pname = PredSymb.mem_dealloc_pname mk in
      let ra =
        { PredSymb.ra_kind = PredSymb.Rrelease;
          PredSymb.ra_res = PredSymb.Rmemory mk;
          PredSymb.ra_pname = pname;
          PredSymb.ra_loc = loc;
          PredSymb.ra_vpath = None } in
      (* mark value as freed *)
      let p_res =
        Attribute.add_or_replace_check_changed tenv
          Tabulation.check_attr_dealloc_mismatch prop (Apred (Aresource ra, [lexp])) in
      p_res :: acc
  | (Sil.Hpointsto _, _ :: _) -> assert false (* alignment error *)
  | _ -> assert false (* should not happen *)

let _execute_free_nonzero mk pdesc tenv instr prop lexp typ loc =
  try
    begin
      match Prover.is_root tenv prop lexp lexp with
      | None ->
          L.d_strln ".... Alignment Error: Freed a non root ....";
          assert false
      | Some _ ->
          let prop_list =
            List.fold ~f:(_execute_free tenv mk loc) ~init:[]
              (Rearrange.rearrange pdesc tenv lexp typ prop loc) in
          List.rev prop_list
    end
  with Rearrange.ARRAY_ACCESS ->
    if (Int.equal Config.array_level 0) then assert false
    else begin
      L.d_strln ".... Array containing allocated heap cells ....";
      L.d_str "  Instr: "; Sil.d_instr instr; L.d_ln ();
      L.d_str "  PROP: "; Prop.d_prop prop; L.d_ln ();
      raise (Exceptions.Array_of_pointsto __POS__)
    end

let execute_free mk { Builtin.pdesc; instr; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      begin
        let pname = Procdesc.get_proc_name pdesc in
        let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
        let prop_nonzero = (* case n_lexp!=0 *)
          Propset.to_proplist (prune tenv ~positive:true n_lexp prop) in
        let prop_zero = (* case n_lexp==0 *)
          Propset.to_proplist (prune tenv ~positive:false n_lexp prop) in
        let plist =
          prop_zero @ (* model: if 0 then skip else _execute_free_nonzero *)
          List.concat_map
            ~f:(fun p ->
                _execute_free_nonzero mk pdesc tenv instr p
                  (Prop.exp_normalize_prop tenv p lexp) typ loc)
            prop_nonzero in
        List.map ~f:(fun p -> (p, path)) plist
      end
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_alloc mk can_return_null
    { Builtin.pdesc; tenv;  prop_; path; ret_id; args; loc; }
  : Builtin.ret_typ =
  let pname = Procdesc.get_proc_name pdesc in
  let rec evaluate_char_sizeof e = match e with
    | Exp.Var _ -> e
    | Exp.UnOp (uop, e', typ) ->
        Exp.UnOp (uop, evaluate_char_sizeof e', typ)
    | Exp.BinOp (bop, e1', e2') ->
        Exp.BinOp (bop, evaluate_char_sizeof e1', evaluate_char_sizeof e2')
    | Exp.Exn _ | Exp.Closure _ | Exp.Const _ | Exp.Cast _ | Exp.Lvar _ | Exp.Lfield _
    | Exp.Lindex _ -> e
    | Exp.Sizeof (Typ.Tarray (Typ.Tint ik, _), Some len, _) when Typ.ikind_is_char ik ->
        evaluate_char_sizeof len
    | Exp.Sizeof (Typ.Tarray (Typ.Tint ik, Some len), None, _) when Typ.ikind_is_char ik ->
        evaluate_char_sizeof (Exp.Const (Const.Cint len))
    | Exp.Sizeof _ -> e in
  let size_exp, procname = match args with
    | [(Exp.Sizeof (Tstruct (ObjcClass _ as name) as s, len, subt), _)] ->
        let struct_type =
          match AttributesTable.get_correct_type_from_objc_class_name name with
          | Some struct_type -> struct_type
          | None -> s in
        Exp.Sizeof (struct_type, len, subt), pname
    | [(size_exp, _)] -> (* for malloc and __new *)
        size_exp, PredSymb.mem_alloc_pname mk
    | [(size_exp, _); (Exp.Const (Const.Cfun pname), _)] ->
        size_exp, pname
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let ret_id = match ret_id with
    | Some (ret_id, _) -> ret_id
    | _ -> Ident.create_fresh Ident.kprimed in
  let size_exp', prop =
    let n_size_exp, prop = check_arith_norm_exp tenv pname size_exp prop_ in
    let n_size_exp' = evaluate_char_sizeof n_size_exp in
    Prop.exp_normalize_prop tenv prop n_size_exp', prop in
  let cnt_te =
    Exp.Sizeof (Typ.Tarray (Typ.Tint Typ.IChar, None), Some size_exp', Subtype.exact) in
  let id_new = Ident.create_fresh Ident.kprimed in
  let exp_new = Exp.Var id_new in
  let ptsto_new =
    Prop.mk_ptsto_exp tenv Prop.Fld_init (exp_new, cnt_te, None) Sil.Ialloc in
  let prop_plus_ptsto =
    let prop' = Prop.normalize tenv (Prop.prop_sigma_star prop [ptsto_new]) in
    let ra =
      { PredSymb.ra_kind = PredSymb.Racquire;
        PredSymb.ra_res = PredSymb.Rmemory mk;
        PredSymb.ra_pname = procname;
        PredSymb.ra_loc = loc;
        PredSymb.ra_vpath = None } in
    (* mark value as allocated *)
    Attribute.add_or_replace tenv prop' (Apred (Aresource ra, [exp_new])) in
  let prop_alloc = Prop.conjoin_eq tenv (Exp.Var ret_id) exp_new prop_plus_ptsto in
  if can_return_null then
    let prop_null = Prop.conjoin_eq tenv (Exp.Var ret_id) Exp.zero prop in
    [(prop_alloc, path); (prop_null, path)]
  else [(prop_alloc, path)]

let execute___cxx_typeid ({ Builtin.pdesc; tenv; prop_; args; loc} as r)
  : Builtin.ret_typ =
  match args with
  | type_info_exp :: rest ->
      (let res = execute_alloc PredSymb.Mnew false { r with args = [type_info_exp] } in
       match rest with
       | [(field_exp, _); (lexp, typ_)] ->
           let pname = Procdesc.get_proc_name pdesc in
           let n_lexp, prop = check_arith_norm_exp tenv pname lexp prop_ in
           let typ =
             List.find ~f:(function
                 | Sil.Hpointsto (e, _, _) -> Exp.equal e n_lexp
                 | _ -> false) prop.Prop.sigma |>
             Option.value_map ~f:(function
                 | Sil.Hpointsto (_, _, Exp.Sizeof (dynamic_type, _, _)) -> dynamic_type
                 | _ -> typ_
               )
               ~default:typ_ in
           let typ_string = Typ.to_string typ in
           let set_instr =
             Sil.Store (field_exp, Typ.Tvoid, Exp.Const (Const.Cstr typ_string), loc) in
           SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] res
       | _ -> res)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_pthread_create ({ Builtin.tenv; prop_; path; args; } as builtin_args)
  : Builtin.ret_typ =
  match args with
  | [_; _; start_routine; arg] ->
      let routine_name = Prop.exp_normalize_prop tenv prop_ (fst start_routine) in
      let routine_arg = Prop.exp_normalize_prop tenv prop_ (fst arg) in
      (match routine_name, (snd start_routine) with
       | Exp.Lvar pvar, _ ->
           let fun_name = Pvar.get_name pvar in
           let fun_string = Mangled.to_string fun_name in
           L.d_strln ("pthread_create: calling function " ^ fun_string);
           begin
             match Specs.get_summary (Typ.Procname.from_string_c_fun fun_string) with
             | None -> assert false
             | Some callee_summary ->
                 SymExec.proc_call callee_summary
                   { builtin_args with args = [(routine_arg, snd arg)] }
           end
       | _ ->
           L.d_str "pthread_create: unknown function ";
           Sil.d_exp routine_name; L.d_strln ", skipping call.";
           [(prop_, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_skip { Builtin.prop_; path; } : Builtin.ret_typ =
  [(prop_, path)]

let execute_scan_function skip_n_arguments ({ Builtin.args } as call_args)
  : Builtin.ret_typ =
  match args with
  | _ when List.length args >= skip_n_arguments ->
      let varargs = ref args in
      varargs := List.drop !varargs skip_n_arguments;
      SymExec.unknown_or_scan_call
        ~is_scan:true
        None
        Annot.Item.empty
        { call_args with args = !varargs }
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute__unwrap_exception { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(ret_exn, _)] ->
      begin
        let pname = Procdesc.get_proc_name pdesc in
        let n_ret_exn, prop = check_arith_norm_exp tenv pname ret_exn prop_ in
        match n_ret_exn with
        | Exp.Exn exp ->
            let prop_with_exn = return_result tenv exp prop ret_id in
            [(prop_with_exn, path)]
        | _ -> assert false
      end
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_return_first_argument { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | (arg1_, _):: _ ->
      let pname = Procdesc.get_proc_name pdesc in
      let arg1, prop = check_arith_norm_exp tenv pname arg1_ prop_ in
      let prop' = return_result tenv arg1 prop ret_id in
      [(prop', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___split_get_nth { Builtin.tenv; pdesc; prop_; path; ret_id; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp1, _); (lexp2, _); (lexp3, _)] ->
      let pname = Procdesc.get_proc_name pdesc in
      let n_lexp1, prop__ = check_arith_norm_exp tenv pname lexp1 prop_ in
      let n_lexp2, prop___ = check_arith_norm_exp tenv pname lexp2 prop__ in
      let n_lexp3, prop = check_arith_norm_exp tenv pname lexp3 prop___ in
      (match n_lexp1, n_lexp2, n_lexp3 with
       | Exp.Const (Const.Cstr str1), Exp.Const (Const.Cstr str2), Exp.Const (Const.Cint n_sil) ->
           (let n = IntLit.to_int n_sil in
            try
              let parts = Str.split (Str.regexp_string str2) str1 in
              let n_part = List.nth_exn parts n in
              let res = Exp.Const (Const.Cstr n_part) in
              [(return_result tenv res prop ret_id, path)]
            with Not_found -> assert false)
       | _ -> [(prop, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* forces the expression passed as parameter to be assumed true at the point where this
   builtin is called, diverges if this causes an inconsistency *)
let execute___infer_assume { Builtin.tenv; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let prop_assume = Prop.conjoin_eq tenv lexp (Exp.bool true) prop_ in
      if Prover.check_inconsistency tenv prop_assume
      then SymExec.diverge prop_assume path
      else [(prop_assume, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* creates a named error state *)
let execute___infer_fail { Builtin.pdesc; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  let error_str =
    match args with
    | [(lexp_msg, _)] ->
        begin
          match Prop.exp_normalize_prop tenv prop_ lexp_msg with
          | Exp.Const (Const.Cstr str) -> str
          | _ -> assert false
        end
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let set_instr =
    Sil.Store (Exp.Lvar Sil.custom_error, Typ.Tvoid, Exp.Const (Const.Cstr error_str), loc) in
  SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] [(prop_, path)]

(* translate builtin assertion failure *)
let execute___assert_fail { Builtin.pdesc; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  let error_str =
    match List.length args with
    | 4 ->
        Config.default_failure_name
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let set_instr =
    Sil.Store (Exp.Lvar Sil.custom_error, Typ.Tvoid, Exp.Const (Const.Cstr error_str), loc) in
  SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] [(prop_, path)]

let execute_objc_alloc_no_fail
    symb_state typ alloc_fun_opt
    { Builtin.pdesc; tenv; ret_id; loc; } =
  let alloc_fun = Exp.Const (Const.Cfun BuiltinDecl.__objc_alloc_no_fail) in
  let ptr_typ = Typ.Tptr (typ, Typ.Pk_pointer) in
  let sizeof_typ = Exp.Sizeof (typ, None, Subtype.exact) in
  let alloc_fun_exp =
    match alloc_fun_opt with
    | Some pname -> [Exp.Const (Const.Cfun pname), Typ.Tvoid]
    | None -> [] in
  let alloc_instr =
    Sil.Call
      (ret_id, alloc_fun, [(sizeof_typ, ptr_typ)] @ alloc_fun_exp, loc, CallFlags.default) in
  SymExec.instrs tenv pdesc [alloc_instr] symb_state

(* NSArray models *)
let execute_objc_NSArray_alloc_no_fail builtin_args symb_state pname =
  let ret_typ =
    match builtin_args.Builtin.ret_id with
    | Some (_, typ) -> typ
    | None -> Typ.Tptr (Tvoid, Pk_pointer) in
  execute_objc_alloc_no_fail symb_state ret_typ (Some pname) builtin_args

let execute_NSArray_arrayWithObjects_count builtin_args =
  let n_formals = 1 in
  let res = SymExec.check_variadic_sentinel ~fails_on_nil: true n_formals (0,1) builtin_args in
  execute_objc_NSArray_alloc_no_fail builtin_args res BuiltinDecl.nsArray_arrayWithObjectsCount

let execute_NSArray_arrayWithObjects builtin_args =
  let n_formals = 1 in
  let res = SymExec.check_variadic_sentinel n_formals (0,1) builtin_args in
  execute_objc_NSArray_alloc_no_fail builtin_args res BuiltinDecl.nsArray_arrayWithObjects


(* NSDictionary models *)
let execute_objc_NSDictionary_alloc_no_fail symb_state pname builtin_args =
  let ret_typ =
    match builtin_args.Builtin.ret_id with
    | Some (_, typ) -> typ
    | None -> Typ.Tptr (Tvoid, Pk_pointer) in
  execute_objc_alloc_no_fail symb_state ret_typ (Some pname) builtin_args

let execute___objc_dictionary_literal builtin_args =
  let n_formals = 1 in
  let res' = SymExec.check_variadic_sentinel ~fails_on_nil: true n_formals (0,1) builtin_args in
  let pname = BuiltinDecl.__objc_dictionary_literal in
  execute_objc_NSDictionary_alloc_no_fail res' pname builtin_args

let __assert_fail = Builtin.register BuiltinDecl.__assert_fail execute___assert_fail

let __builtin_va_arg = Builtin.register BuiltinDecl.__builtin_va_arg execute___builtin_va_arg

let __builtin_va_copy = Builtin.register BuiltinDecl.__builtin_va_copy execute_skip

let __builtin_va_end = Builtin.register BuiltinDecl.__builtin_va_end execute_skip

let __builtin_va_start = Builtin.register BuiltinDecl.__builtin_va_start execute_skip

(* [__cast(val,typ)] implements java's [typ(val)] *)
let __cast = Builtin.register BuiltinDecl.__cast execute___cast

(* report a taint error if the parameter is tainted, and assume it is untainted afterward *)
let __check_untainted = Builtin.register BuiltinDecl.__check_untainted execute___check_untainted

let __cxx_typeid = Builtin.register BuiltinDecl.__cxx_typeid execute___cxx_typeid

let __delete = Builtin.register BuiltinDecl.__delete (execute_free PredSymb.Mnew)

let __delete_array = Builtin.register BuiltinDecl.__delete_array
    (execute_free PredSymb.Mnew_array)

let __delete_locked_attribute = Builtin.register BuiltinDecl.__delete_locked_attribute
    execute___delete_locked_attribute

let __exit = Builtin.register BuiltinDecl.__exit execute_exit

(* return the length of the array passed as a parameter *)
let __get_array_length = Builtin.register BuiltinDecl.__get_array_length execute___get_array_length

let __get_hidden_field = Builtin.register BuiltinDecl.__get_hidden_field execute___get_hidden_field

let __get_type_of = Builtin.register BuiltinDecl.__get_type_of execute___get_type_of

(* infer assume, diverging on inconsistencies *)
let __infer_assume = Builtin.register BuiltinDecl.__infer_assume execute___infer_assume

(* externally create new errors *)
let __infer_fail = Builtin.register BuiltinDecl.__infer_fail execute___infer_fail

(* [__instanceof(val,typ)] implements java's [val instanceof typ] *)
let __instanceof = Builtin.register BuiltinDecl.__instanceof execute___instanceof

let __method_set_ignore_attribute = Builtin.register BuiltinDecl.__method_set_ignore_attribute
    execute___method_set_ignore_attribute

let __new = Builtin.register BuiltinDecl.__new (execute_alloc PredSymb.Mnew false)

let __new_array = Builtin.register BuiltinDecl.__new_array (execute_alloc PredSymb.Mnew_array false)

let __objc_alloc = Builtin.register BuiltinDecl.__objc_alloc (execute_alloc PredSymb.Mobjc true)

(* like __objc_alloc, but does not return nil *)
let __objc_alloc_no_fail = Builtin.register BuiltinDecl.__objc_alloc_no_fail
    (execute_alloc PredSymb.Mobjc false)

let __objc_cast = Builtin.register BuiltinDecl.__objc_cast execute___objc_cast

let __objc_dictionary_literal = Builtin.register BuiltinDecl.__objc_dictionary_literal
    execute___objc_dictionary_literal

let __objc_release = Builtin.register BuiltinDecl.__objc_release execute___objc_release

let __objc_release_autorelease_pool = Builtin.register BuiltinDecl.__objc_release_autorelease_pool
    execute___release_autorelease_pool

let __objc_release_cf = Builtin.register BuiltinDecl.__objc_release_cf execute___objc_release_cf

let __objc_retain = Builtin.register BuiltinDecl.__objc_retain execute___objc_retain

let __objc_retain_cf = Builtin.register BuiltinDecl.__objc_retain_cf execute___objc_retain_cf

let __placement_delete = Builtin.register BuiltinDecl.__placement_delete execute_skip

let __placement_new = Builtin.register BuiltinDecl.__placement_new execute_return_first_argument

(* print a value as seen by the engine *)
let __print_value = Builtin.register BuiltinDecl.__print_value execute___print_value

(* require the parameter to point to an allocated array *)
let __require_allocated_array = Builtin.register BuiltinDecl.__require_allocated_array
    execute___require_allocated_array

let __set_array_length = Builtin.register BuiltinDecl.__set_array_length execute___set_array_length

let __set_autorelease_attribute = Builtin.register BuiltinDecl.__set_autorelease_attribute
    execute___set_autorelease_attribute

let __set_file_attribute = Builtin.register BuiltinDecl.__set_file_attribute
    execute___set_file_attribute

(* set a hidden field in the struct to the given value *)
let __set_hidden_field = Builtin.register BuiltinDecl.__set_hidden_field execute___set_hidden_field

let __set_lock_attribute = Builtin.register BuiltinDecl.__set_lock_attribute
    execute___set_lock_attribute

let __set_locked_attribute = Builtin.register BuiltinDecl.__set_locked_attribute
    execute___set_locked_attribute

let __set_mem_attribute = Builtin.register BuiltinDecl.__set_mem_attribute
    execute___set_mem_attribute

let __set_observer_attribute = Builtin.register BuiltinDecl.__set_observer_attribute
    (execute___set_attr PredSymb.Aobserver)

let __set_taint_attribute = Builtin.register BuiltinDecl.__set_taint_attribute
    execute___set_taint_attribute

let __set_unlocked_attribute = Builtin.register BuiltinDecl.__set_unlocked_attribute
    execute___set_unlocked_attribute

let __set_unsubscribed_observer_attribute = Builtin.register
    BuiltinDecl.__set_unsubscribed_observer_attribute
    (execute___set_attr PredSymb.Aunsubscribed_observer)

let __set_untaint_attribute = Builtin.register
    BuiltinDecl.__set_untaint_attribute execute___set_untaint_attribute

(* splits a string given a separator and returns the nth string *)
let __split_get_nth = Builtin.register BuiltinDecl.__split_get_nth execute___split_get_nth

let __throw = Builtin.register BuiltinDecl.__throw execute_skip

let __unwrap_exception = Builtin.register BuiltinDecl.__unwrap_exception execute__unwrap_exception

let abort = Builtin.register BuiltinDecl.abort execute_abort

let exit = Builtin.register BuiltinDecl.exit execute_exit

let free = Builtin.register BuiltinDecl.free (execute_free PredSymb.Mmalloc)

let fscanf = Builtin.register BuiltinDecl.fscanf (execute_scan_function 2)

let fwscanf = Builtin.register BuiltinDecl.fwscanf (execute_scan_function 2)

let malloc = Builtin.register BuiltinDecl.malloc (execute_alloc PredSymb.Mmalloc
                                                    (not Config.unsafe_malloc))

let malloc_no_fail = Builtin.register BuiltinDecl.malloc_no_fail
    (execute_alloc PredSymb.Mmalloc false)

let nsArray_arrayWithObjects = Builtin.register BuiltinDecl.nsArray_arrayWithObjects
    execute_NSArray_arrayWithObjects

let nsArray_arrayWithObjectsCount = Builtin.register BuiltinDecl.nsArray_arrayWithObjectsCount
    execute_NSArray_arrayWithObjects_count

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

(* Function exists to load module and guarantee that the side-effects of Builtin.register
   calls have been done. *)
let init () = ()
