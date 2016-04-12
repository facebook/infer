(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Models for the builtin functions supported *)

open SymExec

module L = Logging
module F = Format

let execute___no_op prop path: Builtin.ret_typ =
  [(prop, path)]

(** model va_arg as always returning 0 *)
let execute___builtin_va_arg { Builtin.pdesc; tenv; prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [_; _; (lexp3, typ3)], _ ->
      let instr' = Sil.Set (lexp3, typ3, Sil.exp_zero, loc) in
      SymExec.instrs ~mask_errors:true tenv pdesc [instr'] [(prop_, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let mk_empty_array size =
  Sil.Earray (size, [], Sil.inst_none)

(* Make a rearranged array. As it is rearranged when it appears in a precondition
   it requires that the function is called with the array allocated. If not infer
   return a null pointer deref *)
let mk_empty_array_rearranged size =
  Sil.Earray (size, [], Sil.inst_rearrange true (State.get_loc ()) (State.get_path_pos ()))

let extract_array_type typ =
  if (!Config.curr_language = Config.Java) then
    match typ with
    | Sil.Tptr ( Sil.Tarray (typ', _), _) -> Some typ'
    | _ -> None
  else
    match typ with
    | Sil.Tptr (typ', _) | Sil.Tarray (typ', _) ->
        Some typ'
    | _ -> None

(** Return a result from a procedure call. *)
let return_result e prop ret_ids =
  match ret_ids with
  | [ret_id] -> Prop.conjoin_eq e (Sil.Var ret_id) prop
  | _ -> prop

(* Add an array of typ pointed to by lexp to prop_ if it doesnt exist alread*)
(*  Return the new prop and the array size *)
(*  Return None if it fails to add the array *)
let add_array_to_prop pdesc prop_ lexp typ =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
  begin
    try
      let hpred = IList.find (function
          | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
          | _ -> false) (Prop.get_sigma prop) in
      match hpred with
      | Sil.Hpointsto(_, Sil.Earray(size, _, _), _) ->
          Some (size, prop)
      | _ -> None (* e points to something but not an array *)
    with Not_found -> (* e is not allocated, so we can add the array *)
      let otyp' = (extract_array_type typ) in
      match otyp' with
      | Some typ' ->
          let size = Sil.Var(Ident.create_fresh Ident.kfootprint) in
          let s = mk_empty_array_rearranged size in
          let hpred =
            Prop.mk_ptsto n_lexp s (Sil.Sizeof(Sil.Tarray(typ', size), Sil.Subtype.exact)) in
          let sigma = Prop.get_sigma prop in
          let sigma_fp = Prop.get_sigma_footprint prop in
          let prop'= Prop.replace_sigma (hpred:: sigma) prop in
          let prop''= Prop.replace_sigma_footprint (hpred:: sigma_fp) prop' in
          let prop''= Prop.normalize prop'' in
          Some (size, prop'')
      | _ -> None
  end

(* Add an array in prop if it is not allocated.*)
let execute___require_allocated_array { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] when IList.length ret_ids <= 1 ->
      (match add_array_to_prop pdesc prop_ lexp typ with
       | None -> []
       | Some (_, prop) -> [(prop, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___get_array_size { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] when IList.length ret_ids <= 1 ->
      (match add_array_to_prop pdesc prop_ lexp typ with
       | None -> []
       | Some (size, prop) -> [(return_result size prop ret_ids, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___set_array_size { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, typ); (size, _)], []->
      (match add_array_to_prop pdesc prop_ lexp typ with
       | None -> []
       | Some (_, prop_a) -> (* Invariant: prop_a has an array pointed to by lexp *)
           let pname = Cfg.Procdesc.get_proc_name pdesc in
           let n_lexp, prop__ = check_arith_norm_exp pname lexp prop_a in
           let n_size, prop = check_arith_norm_exp pname size prop__ in
           let hpred, sigma' = IList.partition (function
               | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
               | _ -> false) (Prop.get_sigma prop) in
           (match hpred with
            | [Sil.Hpointsto(e, Sil.Earray(_, esel, inst), t)] ->
                let hpred' = Sil.Hpointsto (e, Sil.Earray (n_size, esel, inst), t) in
                let prop' = Prop.replace_sigma (hpred':: sigma') prop in
                [(Prop.normalize prop', path)]
            | _ -> [])) (* by construction of prop_a this case is impossible *)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___print_value { Builtin.pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  L.err "__print_value: ";
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let do_arg (lexp, _) =
    let n_lexp, _ = check_arith_norm_exp pname lexp prop_ in
    L.err "%a " (Sil.pp_exp pe_text) n_lexp in
  IList.iter do_arg args;
  L.err "@.";
  [(prop_, path)]

let is_undefined_opt prop n_lexp =
  let is_undef =
    Option.is_some (Prop.get_undef_attribute prop n_lexp) in
  is_undef && (!Config.angelic_execution || !Config.optimistic_cast)

(** Creates an object in the heap with a given type, when the object is not known to be null or when
    it doesn't appear already in the heap. *)
let create_type tenv n_lexp typ prop =
  let prop_type =
    try
      let _ = IList.find (function
          | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
          | _ -> false) (Prop.get_sigma prop) in
      prop
    with Not_found ->
      let mhpred =
        match typ with
        | Sil.Tptr (typ', _) ->
            let sexp = Sil.Estruct ([], Sil.inst_none) in
            let typ'' = Tenv.expand_type tenv typ' in
            let texp = Sil.Sizeof (typ'', Sil.Subtype.subtypes) in
            let hpred = Prop.mk_ptsto n_lexp sexp texp in
            Some hpred
        | Sil.Tarray _ ->
            let size = Sil.Var(Ident.create_fresh Ident.kfootprint) in
            let sexp = mk_empty_array size in
            let texp = Sil.Sizeof (typ, Sil.Subtype.subtypes) in
            let hpred = Prop.mk_ptsto n_lexp sexp texp in
            Some hpred
        | _ -> None in
      match mhpred with
      | Some hpred ->
          let sigma = Prop.get_sigma prop in
          let sigma_fp = Prop.get_sigma_footprint prop in
          let prop'= Prop.replace_sigma (hpred:: sigma) prop in
          let prop''=
            let has_normal_variables =
              Sil.fav_exists (Sil.exp_fav n_lexp) Ident.is_normal in
            if (is_undefined_opt prop n_lexp) || has_normal_variables
            then prop'
            else Prop.replace_sigma_footprint (hpred:: sigma_fp) prop' in
          let prop''= Prop.normalize prop'' in
          prop''
      | None -> prop in
  let sil_is_null = Sil.BinOp (Sil.Eq, n_lexp, Sil.exp_zero) in
  let sil_is_nonnull = Sil.UnOp (Sil.LNot, sil_is_null, None) in
  let null_case = Propset.to_proplist (prune ~positive:true sil_is_null prop) in
  let non_null_case = Propset.to_proplist (prune ~positive:true sil_is_nonnull prop_type) in
  if ((IList.length non_null_case) > 0) && (!Config.footprint) then
    non_null_case
  else if ((IList.length non_null_case) > 0) && (is_undefined_opt prop n_lexp) then
    non_null_case
  else null_case @ non_null_case

let execute___get_type_of { Builtin.pdesc; tenv; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] when IList.length ret_ids <= 1 ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      let props = create_type tenv n_lexp typ prop in
      let aux prop =
        begin
          try
            let hpred = IList.find (function
                | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e n_lexp
                | _ -> false) (Prop.get_sigma prop) in
            match hpred with
            | Sil.Hpointsto(_, _, texp) ->
                (return_result texp prop ret_ids), path
            | _ -> assert false
          with Not_found -> (return_result Sil.exp_zero prop ret_ids), path
        end in
      (IList.map aux props)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** replace the type of the ptsto rooted at [root_e] with [texp] in [prop] *)
let replace_ptsto_texp prop root_e texp =
  let process_sigma sigma =
    let sigma1, sigma2 =
      IList.partition (function
          | Sil.Hpointsto(e, _, _) -> Sil.exp_equal e root_e
          | _ -> false) sigma in
    match sigma1 with
    | [Sil.Hpointsto(e, se, _)] -> (Sil.Hpointsto (e, se, texp)) :: sigma2
    | _ -> sigma in
  let sigma = Prop.get_sigma prop in
  let sigma_fp = Prop.get_sigma_footprint prop in
  let prop'= Prop.replace_sigma (process_sigma sigma) prop in
  let prop''= Prop.replace_sigma_footprint (process_sigma sigma_fp) prop' in
  Prop.normalize prop''

let execute___instanceof_cast ~instof
    { Builtin.pdesc; tenv; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(val1_, typ1); (texp2_, _)] when IList.length ret_ids <= 1 ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let val1, prop__ = check_arith_norm_exp pname val1_ prop_ in
      let texp2, prop = check_arith_norm_exp pname texp2_ prop__ in
      let is_cast_to_reference =
        match typ1 with
        | Sil.Tptr (_, Sil.Pk_reference) -> true
        | _ -> false in
      (* In Java, we throw an exception, in C++ we return 0 in case of a cast to a pointer, *)
      (* and throw an exception in case of a cast to a reference. *)
      let should_throw_exception =
        !Config.curr_language = Config.Java || is_cast_to_reference in
      let deal_with_failed_cast val1 _ texp1 texp2 =
        Tabulation.raise_cast_exception
          __POS__ None texp1 texp2 val1 in
      let exe_one_prop prop =
        if Sil.exp_equal texp2 Sil.exp_zero then
          [(return_result Sil.exp_zero prop ret_ids, path)]
        else
          begin
            try
              let hpred = IList.find (function
                  | Sil.Hpointsto (e1, _, _) -> Sil.exp_equal e1 val1
                  | _ -> false) (Prop.get_sigma prop) in
              match hpred with
              | Sil.Hpointsto (_, _, texp1) ->
                  let pos_type_opt, neg_type_opt =
                    Prover.Subtyping_check.subtype_case_analysis tenv texp1 texp2 in
                  let mk_res type_opt res_e = match type_opt with
                    | None -> []
                    | Some texp1' ->
                        let prop' =
                          if Sil.exp_equal texp1 texp1' then prop
                          else replace_ptsto_texp prop val1 texp1' in
                        [(return_result res_e prop' ret_ids, path)] in
                  if instof then (* instanceof *)
                    begin
                      let pos_res = mk_res pos_type_opt Sil.exp_one in
                      let neg_res = mk_res neg_type_opt Sil.exp_zero in
                      pos_res @ neg_res
                    end
                  else (* cast *)
                  if not should_throw_exception then (* C++ case when negative cast returns 0 *)
                    let pos_res = mk_res pos_type_opt val1 in
                    let neg_res = mk_res neg_type_opt Sil.exp_zero in
                    pos_res @ neg_res
                  else
                    begin
                      if (!Config.footprint = true) then
                        begin
                          match pos_type_opt with
                          | None -> deal_with_failed_cast val1 typ1 texp1 texp2
                          | Some _ -> mk_res pos_type_opt val1
                        end
                      else (* !Config.footprint = false *)
                        begin
                          match neg_type_opt with
                          | Some _ ->
                              if is_undefined_opt prop val1 then mk_res pos_type_opt val1
                              else deal_with_failed_cast val1 typ1 texp1 texp2
                          | None -> mk_res pos_type_opt val1
                        end
                    end
              | _ -> []
            with Not_found ->
              [(return_result val1 prop ret_ids, path)]
          end in
      let props = create_type tenv val1 typ1 prop in
      IList.flatten (IList.map exe_one_prop props)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___instanceof builtin_args
  : Builtin.ret_typ =
  execute___instanceof_cast ~instof:true builtin_args

let execute___cast builtin_args
  : Builtin.ret_typ =
  execute___instanceof_cast ~instof:false builtin_args

let set_resource_attribute prop path n_lexp loc ra_res =
  let prop' = match Prop.get_resource_attribute prop n_lexp with
    | Some (Sil.Aresource (_ as ra)) ->
        Prop.add_or_replace_exp_attribute
          prop
          n_lexp
          (Sil.Aresource { ra with Sil.ra_res = ra_res })
    | _ ->
        ( let pname = Sil.mem_alloc_pname Sil.Mnew in
          let ra =
            { Sil.ra_kind = Sil.Racquire;
              Sil.ra_res = ra_res;
              Sil.ra_pname = pname;
              Sil.ra_loc = loc;
              Sil.ra_vpath = None } in
          Prop.add_or_replace_exp_attribute prop n_lexp (Sil.Aresource ra)) in
  [(prop', path)]

(** Set the attibute of the value as file *)
let execute___set_file_attribute { Builtin.pdesc; prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, _)], _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      set_resource_attribute prop path n_lexp loc Sil.Rfile
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as lock *)
let execute___set_lock_attribute { Builtin.pdesc; prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, _)], _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      set_resource_attribute prop path n_lexp loc Sil.Rlock
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the resource attribute of the first real argument of method as ignore, the first argument is
    assumed to be "this" *)
let execute___method_set_ignore_attribute
    { Builtin.pdesc; prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [_ ; (lexp, _)], _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      set_resource_attribute prop path n_lexp loc Sil.Rignore
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as memory *)
let execute___set_mem_attribute { Builtin.pdesc; prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, _)], _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      set_resource_attribute prop path n_lexp loc (Sil.Rmemory Sil.Mnew)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** report an error if [lexp] is tainted; otherwise, add untained([lexp]) as a precondition *)
let execute___check_untainted
    { Builtin.pdesc; prop_; path; ret_ids; args; proc_name = callee_pname; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, _)], _ ->
      let caller_pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp caller_pname lexp prop_ in
      [(check_untainted n_lexp caller_pname callee_pname prop, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** take a pointer to a struct, and return the value of a hidden field in the struct *)
let execute___get_hidden_field { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
      let ret_val = ref None in
      let return_val p = match !ret_val with
        | Some e -> return_result e p ret_ids
        | None -> p in
      let foot_var = lazy (Sil.Var (Ident.create_fresh Ident.kfootprint)) in
      let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
      let has_fld_hidden fsel = IList.exists filter_fld_hidden fsel in
      let do_hpred in_foot hpred = match hpred with
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Sil.exp_equal e n_lexp && (not (has_fld_hidden fsel)) ->
            let foot_e = Lazy.force foot_var in
            ret_val := Some foot_e;
            let se = Sil.Eexp(foot_e, Sil.inst_none) in
            let fsel' = (Ident.fieldname_hidden, se) :: fsel in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | Sil.Hpointsto(e, Sil.Estruct (fsel, _), _)
          when Sil.exp_equal e n_lexp && not in_foot && has_fld_hidden fsel ->
            let set_ret_val () =
              match IList.find filter_fld_hidden fsel with
              | _, Sil.Eexp(e, _) -> ret_val := Some e
              | _ -> () in
            set_ret_val();
            hpred
        | _ -> hpred in
      let sigma' = IList.map (do_hpred false) (Prop.get_sigma prop) in
      let sigma_fp' = IList.map (do_hpred true) (Prop.get_sigma_footprint prop) in
      let prop' = Prop.replace_sigma_footprint sigma_fp' (Prop.replace_sigma sigma' prop) in
      let prop'' = return_val (Prop.normalize prop') in
      [(prop'', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** take a pointer to a struct and a value,
    and set a hidden field in the struct to the given value *)
let execute___set_hidden_field { Builtin.pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp1, _); (lexp2, _)] ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp1, prop__ = check_arith_norm_exp pname lexp1 prop_ in
      let n_lexp2, prop = check_arith_norm_exp pname lexp2 prop__ in
      let foot_var = lazy (Sil.Var (Ident.create_fresh Ident.kfootprint)) in
      let filter_fld_hidden (f, _ ) = Ident.fieldname_is_hidden f in
      let has_fld_hidden fsel = IList.exists filter_fld_hidden fsel in
      let do_hpred in_foot hpred = match hpred with
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Sil.exp_equal e n_lexp1 && not in_foot ->
            let se = Sil.Eexp(n_lexp2, Sil.inst_none) in
            let fsel' =
              (Ident.fieldname_hidden, se) ::
              (IList.filter (fun x -> not (filter_fld_hidden x)) fsel) in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | Sil.Hpointsto(e, Sil.Estruct (fsel, inst), texp)
          when Sil.exp_equal e n_lexp1 && in_foot && not (has_fld_hidden fsel) ->
            let foot_e = Lazy.force foot_var in
            let se = Sil.Eexp(foot_e, Sil.inst_none) in
            let fsel' = (Ident.fieldname_hidden, se) :: fsel in
            Sil.Hpointsto(e, Sil.Estruct (fsel', inst), texp)
        | _ -> hpred in
      let sigma' = IList.map (do_hpred false) (Prop.get_sigma prop) in
      let sigma_fp' = IList.map (do_hpred true) (Prop.get_sigma_footprint prop) in
      let prop' = Prop.replace_sigma_footprint sigma_fp' (Prop.replace_sigma sigma' prop) in
      let prop'' = Prop.normalize prop' in
      [(prop'', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* Update the objective-c hidden counter by applying the operation op and the operand delta.*)
(* Eg. op=+/- delta is an integer *)
let execute___objc_counter_update
    ~mask_errors op delta
    { Builtin.pdesc; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, typ)] ->
      let typ' = (match Tenv.expand_type tenv typ with
          | Sil.Tstruct _ as s -> s
          | Sil.Tptr(t, _) -> Tenv.expand_type tenv t
          | s' ->
              L.d_str
                ("Trying to update hidden field of not a struc. Type: " ^
                 (Sil.typ_to_string s'));
              assert false) in
      (* Assumes that lexp is a temp n$1 that has the value of the object. *)
      (* This is the case as a call f(o) it's translates as n$1=*&o; f(n$1) *)
      (* n$2 = *n$1.hidden *)
      let tmp = Ident.create_fresh Ident.knormal in
      let hidden_field = Sil.Lfield(lexp, Ident.fieldname_hidden, typ') in
      let counter_to_tmp = Sil.Letderef(tmp, hidden_field, typ', loc) in
      (* *n$1.hidden = (n$2 +/- delta) *)
      let update_counter =
        Sil.Set
          (hidden_field,
           typ',
           Sil.BinOp(op, Sil.Var tmp, Sil.Const (Sil.Cint delta)),
           loc) in
      let update_counter_instrs =
        [ counter_to_tmp; update_counter; Sil.Remove_temps([tmp], loc) ] in
      SymExec.instrs ~mask_errors tenv pdesc update_counter_instrs [(prop_, path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* Given a list of args checks if the first is the flag indicating whether is a call to
   retain/release for which we have to suppress NPE report or not. If the flag is present it is
   removed from the list of args. *)
let get_suppress_npe_flag args =
  match args with
  | (Sil.Const (Sil.Cint i), Sil.Tint Sil.IBool):: args' when Sil.Int.isone i ->
      false, args' (* this is a CFRelease/CFRetain *)
  | _ -> true, args

let execute___objc_retain_impl
    ({ Builtin.prop_; args; ret_ids; } as builtin_args)
  : Builtin.ret_typ =
  let mask_errors, args' = get_suppress_npe_flag args in
  match args' with
  | [(lexp, _)] ->
      let prop = return_result lexp prop_ ret_ids in
      execute___objc_counter_update
        ~mask_errors (Sil.PlusA) (Sil.Int.one)
        { builtin_args with Builtin.prop_ = prop; args = args'; }
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___objc_retain builtin_args
  : Builtin.ret_typ =
  if !Config.objc_memory_model_on then
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
    ~mask_errors Sil.MinusA Sil.Int.one
    { builtin_args with Builtin.args = args'; }

let execute___objc_release builtin_args
  : Builtin.ret_typ =
  if !Config.objc_memory_model_on then
    execute___objc_release_impl builtin_args
  else execute___no_op builtin_args.Builtin.prop_ builtin_args.Builtin.path

let execute___objc_release_cf builtin_args
  : Builtin.ret_typ =
  execute___objc_release_impl builtin_args

(** Set the attibute of the value as objc autoreleased *)
let execute___set_autorelease_attribute
    { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args, ret_ids with
  | [(lexp, _)], _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let prop = return_result lexp prop_ ret_ids in
      if !Config.objc_memory_model_on then
        let n_lexp, prop = check_arith_norm_exp pname lexp prop in
        let prop' = Prop.add_or_replace_exp_attribute prop n_lexp Sil.Aautorelease in
        [(prop', path)]
      else execute___no_op prop path
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Release all the objects in the pool *)
let execute___release_autorelease_pool
    ({ Builtin.prop_; path; } as builtin_args)
  : Builtin.ret_typ =
  if !Config.objc_memory_model_on then
    let autoreleased_objects = Prop.get_atoms_with_attribute Sil.Aautorelease prop_ in
    let prop_without_attribute = Prop.remove_attribute Sil.Aautorelease prop_ in
    let call_release res exp =
      match res with
      | (prop', path'):: _ ->
          (try
             let hpred = IList.find (function
                 | Sil.Hpointsto(e1, _, _) -> Sil.exp_equal e1 exp
                 | _ -> false) (Prop.get_sigma prop_) in
             match hpred with
             | Sil.Hpointsto(_, _, Sil.Sizeof (typ, _)) ->
                 let res1 =
                   execute___objc_release
                     { builtin_args with
                       Builtin.args = [(exp, typ)];
                       prop_ = prop';
                       path = path'; } in
                 res1
             | _ -> res
           with Not_found -> res)
      | [] -> res in
    IList.fold_left call_release [(prop_without_attribute, path)] autoreleased_objects
  else execute___no_op prop_ path

let set_attr pdesc prop path exp attr =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let n_lexp, prop = check_arith_norm_exp pname exp prop in
  [(Prop.add_or_replace_exp_attribute prop n_lexp attr, path)]

(** Set attibute att *)
let execute___set_attr attr { Builtin.pdesc; prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] -> set_attr pdesc prop_ path lexp attr
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(** Set the attibute of the value as resource/locked*)
let execute___set_locked_attribute
    ({ Builtin.pdesc; loc; } as builtin_args)
  : Builtin.ret_typ =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  (* ra_kind = Racquire in following indicates locked *)
  let ra = {
    Sil.ra_kind = Sil.Racquire;
    ra_res = Sil.Rlock;
    ra_pname = pname;
    ra_loc = loc;
    ra_vpath = None; } in
  execute___set_attr (Sil.Aresource ra) builtin_args

(** Set the attibute of the value as resource/unlocked*)
let execute___set_unlocked_attribute
    ({ Builtin.pdesc; loc; } as builtin_args)
  : Builtin.ret_typ =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  (* ra_kind = Rrelease in following indicates unlocked *)
  let ra = {
    Sil.ra_kind = Sil.Rrelease;
    ra_res = Sil.Rlock;
    ra_pname = pname;
    ra_loc = loc;
    ra_vpath = None; } in
  execute___set_attr (Sil.Aresource ra) builtin_args

(** Set the attibute of the value as tainted *)
let execute___set_taint_attribute
    ({ Builtin.pdesc; args; prop_; path; })
  : Builtin.ret_typ =
  match args with
  | (exp, _) :: [(Sil.Const (Sil.Cstr taint_kind_str), _)] ->
      let taint_source = Cfg.Procdesc.get_proc_name pdesc in
      let taint_kind = match taint_kind_str with
        | "UnverifiedSSLSocket" -> Sil.UnverifiedSSLSocket
        | "SharedPreferenceData" -> Sil.SharedPreferencesData
        | other_str -> failwith ("Unrecognized taint kind " ^ other_str) in
      set_attr pdesc prop_ path exp (Sil.Ataint { Sil.taint_source; taint_kind})
  | _ ->
      (* note: we can also get this if [taint_kind] is not a string literal *)
      raise (Exceptions.Wrong_argument_number __POS__)

let execute___objc_cast { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(val1_, _); (texp2_, _)] when IList.length ret_ids <= 1 ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let val1, prop__ = check_arith_norm_exp pname val1_ prop_ in
      let texp2, prop = check_arith_norm_exp pname texp2_ prop__ in
      (try
         let hpred = IList.find (function
             | Sil.Hpointsto(e1, _, _) -> Sil.exp_equal e1 val1
             | _ -> false) (Prop.get_sigma prop) in
         match hpred, texp2 with
         | Sil.Hpointsto(val1, _, _), Sil.Sizeof (_, _) ->
             let prop' = replace_ptsto_texp prop val1 texp2 in
             [(return_result val1 prop' ret_ids, path)]
         | _ -> [(return_result val1 prop ret_ids, path)]
       with Not_found -> [(return_result val1 prop ret_ids, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_abort { Builtin.proc_name; }
  : Builtin.ret_typ =
  raise
    (Exceptions.Precondition_not_found
       (Localise.verbatim_desc (Procname.to_string proc_name), __POS__))

let execute_exit { Builtin.prop_; path; }
  : Builtin.ret_typ =
  SymExec.diverge prop_ path

let _execute_free mk loc acc iter =
  match Prop.prop_iter_current iter with
  | (Sil.Hpointsto(lexp, _, _), []) ->
      let prop = Prop.prop_iter_remove_curr_then_to_prop iter in
      let pname = Sil.mem_dealloc_pname mk in
      let ra =
        { Sil.ra_kind = Sil.Rrelease;
          Sil.ra_res = Sil.Rmemory mk;
          Sil.ra_pname = pname;
          Sil.ra_loc = loc;
          Sil.ra_vpath = None } in
      (* mark value as freed *)
      let p_res =
        Prop.add_or_replace_exp_attribute_check_changed
          Tabulation.check_attr_dealloc_mismatch
          prop
          lexp
          (Sil.Aresource ra) in
      p_res :: acc
  | (Sil.Hpointsto _, _ :: _) -> assert false (* alignment error *)
  | _ -> assert false (* should not happen *)

let _execute_free_nonzero mk pdesc tenv instr prop lexp typ loc =
  try
    begin
      match Prover.is_root prop lexp lexp with
      | None ->
          L.d_strln ".... Alignment Error: Freed a non root ....";
          assert false
      | Some _ ->
          let prop_list =
            IList.fold_left (_execute_free mk loc) []
              (Rearrange.rearrange pdesc tenv lexp typ prop loc) in
          IList.rev prop_list
    end
  with Rearrange.ARRAY_ACCESS ->
    if (!Config.array_level = 0) then assert false
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
        let pname = Cfg.Procdesc.get_proc_name pdesc in
        let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
        let prop_nonzero = (* case n_lexp!=0 *)
          Propset.to_proplist (prune ~positive:true n_lexp prop) in
        let prop_zero = (* case n_lexp==0 *)
          Propset.to_proplist (prune ~positive:false n_lexp prop) in
        let plist =
          prop_zero @ (* model: if 0 then skip else _execute_free_nonzero *)
          IList.flatten (IList.map (fun p ->
              _execute_free_nonzero mk pdesc tenv instr p
                (Prop.exp_normalize_prop p lexp) typ loc) prop_nonzero) in
        IList.map (fun p -> (p, path)) plist
      end
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_alloc mk can_return_null
    { Builtin.pdesc; tenv;  prop_; path; ret_ids; args; loc; }
  : Builtin.ret_typ =
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  let rec evaluate_char_sizeof e = match e with
    | Sil.Var _ -> e
    | Sil.UnOp (uop, e', typ) ->
        Sil.UnOp (uop, evaluate_char_sizeof e', typ)
    | Sil.BinOp (bop, e1', e2') ->
        Sil.BinOp (bop, evaluate_char_sizeof e1', evaluate_char_sizeof e2')
    | Sil.Const _ | Sil.Cast _ | Sil.Lvar _ | Sil.Lfield _ | Sil.Lindex _ -> e
    | Sil.Sizeof (Sil.Tarray(Sil.Tint ik, size), _) when Sil.ikind_is_char ik ->
        evaluate_char_sizeof size
    | Sil.Sizeof _ -> e in
  let handle_sizeof_exp size_exp =
    Sil.Sizeof (Sil.Tarray (Sil.Tint Sil.IChar, size_exp), Sil.Subtype.exact) in
  let size_exp = match args with
    | [(size_exp, _)] -> (* for malloc and __new *)
        size_exp
    | [(num_obj, _); (base_exp, _)] -> (* for __new_array *)
        Sil.BinOp (Sil.Mult, num_obj, base_exp)
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let ret_id = match ret_ids with
    | [ret_id] -> ret_id
    | _ -> Ident.create_fresh Ident.kprimed in
  let size_exp', prop =
    let n_size_exp, prop = check_arith_norm_exp pname size_exp prop_ in
    let n_size_exp' = evaluate_char_sizeof n_size_exp in
    Prop.exp_normalize_prop prop n_size_exp', prop in
  let cnt_te = handle_sizeof_exp size_exp' in
  let id_new = Ident.create_fresh Ident.kprimed in
  let exp_new = Sil.Var id_new in
  let ptsto_new =
    Prop.mk_ptsto_exp (Some tenv) Prop.Fld_init (exp_new, cnt_te, None) Sil.Ialloc in
  let prop_plus_ptsto =
    let pname = Sil.mem_alloc_pname mk in
    let prop' = Prop.normalize (Prop.prop_sigma_star prop [ptsto_new]) in
    let ra =
      { Sil.ra_kind = Sil.Racquire;
        Sil.ra_res = Sil.Rmemory mk;
        Sil.ra_pname = pname;
        Sil.ra_loc = loc;
        Sil.ra_vpath = None } in
    (* mark value as allocated *)
    Prop.add_or_replace_exp_attribute prop' exp_new (Sil.Aresource ra) in
  let prop_alloc = Prop.conjoin_eq (Sil.Var ret_id) exp_new prop_plus_ptsto in
  if can_return_null then
    let prop_null = Prop.conjoin_eq (Sil.Var ret_id) Sil.exp_zero prop in
    [(prop_alloc, path); (prop_null, path)]
  else [(prop_alloc, path)]

let execute___cxx_typeid ({ Builtin.pdesc; tenv; prop_; args; loc} as r)
  : Builtin.ret_typ =
  match args with
  | type_info_exp :: rest ->
      (let res = execute_alloc Sil.Mnew false { r with args = [type_info_exp] } in
       match rest with
       | [(field_exp, _); (lexp, typ)] ->
           let pname = Cfg.Procdesc.get_proc_name pdesc in
           let n_lexp, prop = check_arith_norm_exp pname lexp prop_ in
           let typ =
             try
               let hpred = IList.find (function
                   | Sil.Hpointsto (e, _, _) -> Sil.exp_equal e n_lexp
                   | _ -> false) (Prop.get_sigma prop) in
               match hpred with
               | Sil.Hpointsto (_, _, Sil.Sizeof (dynamic_type, _)) -> dynamic_type
               | _ -> typ
             with Not_found -> typ in
           let typ_string = Sil.typ_to_string typ in
           let set_instr = Sil.Set (field_exp, Sil.Tvoid, Sil.Const (Sil.Cstr typ_string), loc) in
           SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] res
       | _ -> res)
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_pthread_create ({ Builtin.prop_; path; args; } as builtin_args)
  : Builtin.ret_typ =
  match args with
  | [_; _; start_routine; arg] ->
      let routine_name = Prop.exp_normalize_prop prop_ (fst start_routine) in
      let routine_arg = Prop.exp_normalize_prop prop_ (fst arg) in
      (match routine_name, (snd start_routine) with
       | Sil.Lvar pvar, _ ->
           let fun_name = Pvar.get_name pvar in
           let fun_string = Mangled.to_string fun_name in
           L.d_strln ("pthread_create: calling function " ^ fun_string);
           begin
             match Specs.get_summary (Procname.from_string_c_fun fun_string) with
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
  | _ when IList.length args >= skip_n_arguments ->
      let varargs = ref args in
      for _ = 1 to skip_n_arguments do varargs := IList.tl !varargs done;
      SymExec.unknown_or_scan_call ~is_scan:true None { call_args with args = !varargs }
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute__unwrap_exception { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(ret_exn, _)] ->
      begin
        let pname = Cfg.Procdesc.get_proc_name pdesc in
        let n_ret_exn, prop = check_arith_norm_exp pname ret_exn prop_ in
        match n_ret_exn with
        | Sil.Const (Sil.Cexn exp) ->
            let prop_with_exn = return_result exp prop ret_ids in
            [(prop_with_exn, path)]
        | _ -> assert false
      end
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute_return_first_argument { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | (arg1_, _):: _ ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let arg1, prop = check_arith_norm_exp pname arg1_ prop_ in
      let prop' = return_result arg1 prop ret_ids in
      [(prop', path)]
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

let execute___split_get_nth { Builtin.pdesc; prop_; path; ret_ids; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp1, _); (lexp2, _); (lexp3, _)] ->
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let n_lexp1, prop__ = check_arith_norm_exp pname lexp1 prop_ in
      let n_lexp2, prop___ = check_arith_norm_exp pname lexp2 prop__ in
      let n_lexp3, prop = check_arith_norm_exp pname lexp3 prop___ in
      (match n_lexp1, n_lexp2, n_lexp3 with
       | Sil.Const (Sil.Cstr str1), Sil.Const (Sil.Cstr str2), Sil.Const (Sil.Cint n_sil) ->
           (let n = Sil.Int.to_int n_sil in
            try
              let parts = Str.split (Str.regexp_string str2) str1 in
              let n_part = IList.nth parts n in
              let res = Sil.Const (Sil.Cstr n_part) in
              [(return_result res prop ret_ids, path)]
            with Not_found -> assert false)
       | _ -> [(prop, path)])
  | _ -> raise (Exceptions.Wrong_argument_number __POS__)

(* forces the expression passed as parameter to be assumed true at the point where this
   builtin is called, diverges if this causes an inconsistency *)
let execute___infer_assume { Builtin.prop_; path; args; }
  : Builtin.ret_typ =
  match args with
  | [(lexp, _)] ->
      let prop_assume = Prop.conjoin_eq lexp (Sil.exp_bool true) prop_ in
      if Prover.check_inconsistency prop_assume
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
          match Prop.exp_normalize_prop prop_ lexp_msg with
          | Sil.Const (Sil.Cstr str) -> str
          | _ -> assert false
        end
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let set_instr =
    Sil.Set (Sil.Lvar Sil.custom_error, Sil.Tvoid, Sil.Const (Sil.Cstr error_str), loc) in
  SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] [(prop_, path)]

(* translate builtin assertion failure *)
let execute___assert_fail { Builtin.pdesc; tenv; prop_; path; args; loc; }
  : Builtin.ret_typ =
  let error_str =
    match args with
    | l when IList.length l = 4 ->
        Config.default_failure_name
    | _ ->
        raise (Exceptions.Wrong_argument_number __POS__) in
  let set_instr =
    Sil.Set (Sil.Lvar Sil.custom_error, Sil.Tvoid, Sil.Const (Sil.Cstr error_str), loc) in
  SymExec.instrs ~mask_errors:true tenv pdesc [set_instr] [(prop_, path)]

let __assert_fail = Builtin.register
    "__assert_fail" execute___assert_fail
let _ = Builtin.register
    (* model for va_arg *)
    "__builtin_va_arg" execute___builtin_va_arg
let _ = Builtin.register
    "__builtin_va_copy" execute_skip
let _ = Builtin.register
    (* model va_end as skip *)
    "__builtin_va_end" execute_skip
let _ = Builtin.register
    "__builtin_va_start" execute_skip
let __cast = Builtin.register
    (* [__cast(val,typ)] implements java's [typ(val)] *)
    "__cast" execute___cast
let _ = Builtin.register
    (* report a taint error if the parameter is tainted, and assume it is untainted afterward *)
    "__check_untainted" execute___check_untainted
let __delete = Builtin.register
    (* like free *)
    "__delete" (execute_free Sil.Mnew)
let __delete_array = Builtin.register
    (* like free *)
    "__delete_array" (execute_free Sil.Mnew_array)
let __exit = Builtin.register
    (* _exit from C library *)
    "_exit" execute_exit
let __get_array_size = Builtin.register
    (* return the size of the array passed as a parameter *)
    "__get_array_size" execute___get_array_size
let __require_allocated_array = Builtin.register
    (* require the parameter to point to an allocated array *)
    "__require_allocated_array" execute___require_allocated_array
let _ = Builtin.register
    (* return the value of a hidden field in the struct *)
    "__get_hidden_field" execute___get_hidden_field
let __get_type_of = Builtin.register
    (* return the get the type of the allocated object passed as a parameter *)
    "__get_type_of" execute___get_type_of
let __infer_assume = Builtin.register
    (* infer assume, diverging on inconsistencies *)
    "__infer_assume" execute___infer_assume
let __infer_fail = Builtin.register
    (* externally create new errors *)
    "__infer_fail" execute___infer_fail
let __instanceof = Builtin.register
    (* [__instanceof(val,typ)] implements java's [val instanceof typ] *)
    "__instanceof" execute___instanceof
let _ = Builtin.register
    "__method_set_ignore_attribute" execute___method_set_ignore_attribute
let __new = Builtin.register
    (* like malloc, but always succeeds *)
    "__new" (execute_alloc Sil.Mnew false)
let __new_array = Builtin.register
    (* like malloc, but always succeeds *)
    "__new_array" (execute_alloc Sil.Mnew_array false)
let __objc_alloc = Builtin.register
    (* Objective C alloc *)
    "__objc_alloc" (execute_alloc Sil.Mobjc true)
let __objc_alloc_no_fail = Builtin.register
    (* like __objc_alloc, but does not return nil *)
    "__objc_alloc_no_fail" (execute_alloc Sil.Mobjc false)
let __objc_cast = Builtin.register
    (* objective-c "cast" *)
    "__objc_cast" execute___objc_cast
let __objc_release = Builtin.register
    (* objective-c "release" *)
    "__objc_release" execute___objc_release
let __objc_release_autorelease_pool = Builtin.register
    (* set the attribute of the parameter as autorelease *)
    "__objc_release_autorelease_pool" execute___release_autorelease_pool
let __objc_release_cf = Builtin.register
    (* objective-c "release" *)
    "__objc_release_cf" execute___objc_release_cf
let __objc_retain = Builtin.register
    (* objective-c "retain" *)
    "__objc_retain" execute___objc_retain
let __objc_retain_cf = Builtin.register
    "__objc_retain_cf" execute___objc_retain_cf
let __cxx_typeid = Builtin.register
    (* C++ "typeid" *)
    "__cxx_typeid" execute___cxx_typeid
let __placement_delete = Builtin.register
    (* placement delete is skip *)
    "__placement_delete" execute_skip
let __placement_new = Builtin.register
    (* placement new returns the first argument *)
    "__placement_new" execute_return_first_argument
let _ = Builtin.register
    (* print a value as seen by the engine *)
    "__print_value" execute___print_value
let __set_array_size = Builtin.register
    (* set the size of the array passed as a parameter *)
    "__set_array_size" execute___set_array_size
let __set_autorelease_attribute = Builtin.register
    (* set the attribute of the parameter as autorelease *)
    "__set_autorelease_attribute" execute___set_autorelease_attribute
let __set_file_attribute = Builtin.register
    (* set the attribute of the parameter as file *)
    "__set_file_attribute" execute___set_file_attribute
let __set_lock_attribute = Builtin.register
    (* set the attribute of the parameter as file *)
    "__set_lock_attribute" execute___set_lock_attribute
let __set_mem_attribute = Builtin.register
    (* set the attribute of the parameter as memory *)
    "__set_mem_attribute" execute___set_mem_attribute
let __set_observer_attribute = Builtin.register
    (* set the observer attribute of the parameter *)
    "__set_observer_attribute" (execute___set_attr Sil.Aobserver)
let __set_unsubscribed_observer_attribute = Builtin.register
    (* set the unregistered observer attribute of the parameter *)
    "__set_unsubscribed_observer_attribute"
    (execute___set_attr Sil.Aunsubscribed_observer)
let __split_get_nth = Builtin.register
    (* splits a string given a separator and returns the nth string *)
    "__split_get_nth" execute___split_get_nth
let _ = Builtin.register
    (* set a hidden field in the struct to the given value *)
    "__set_hidden_field" execute___set_hidden_field
let _ = Builtin.register
    (* set the attribute of the parameter as tainted *)
    "__set_taint_attribute" execute___set_taint_attribute
let _ = Builtin.register
    (* set the attribute of the parameter as untainted *)
    "__set_untaint_attribute" (execute___set_attr Sil.Auntaint)
let __set_locked_attribute = Builtin.register
    (* set the attribute of the parameter as locked *)
    "__set_locked_attribute" execute___set_locked_attribute
let __set_unlocked_attribute = Builtin.register
    (* set the attribute of the parameter as unlocked *)
    "__set_unlocked_attribute" execute___set_unlocked_attribute
let _ = Builtin.register
    "__throw" execute_skip
let __unwrap_exception = Builtin.register
    (* unwrap an exception *)
    "__unwrap_exception" execute__unwrap_exception
let _ = Builtin.register
    (* abort from C library *)
    "abort" execute_abort
let _ = Builtin.register
    (* exit from C library *)
    "exit" execute_exit
let _ = Builtin.register
    (* free from C library, requires allocated memory *)
    "free" (execute_free Sil.Mmalloc)
let _ = Builtin.register
    (* fscanf from C library *)
    "fscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vsscanf from C library *)
    "fwscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* malloc from C library *)
    "malloc" (execute_alloc Sil.Mmalloc true)
let malloc_no_fail = Builtin.register
    (* malloc from ObjC library *)
    "malloc_no_fail" (execute_alloc Sil.Mmalloc false)
let _ = Builtin.register
    (* register execution handler for pthread_create *)
    "pthread_create" execute_pthread_create
let _ = Builtin.register
    (* scanf from C library *)
    "scanf" (execute_scan_function 1)
let _ = Builtin.register
    (* sscanf from C library *)
    "sscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vsscanf from C library *)
    "swscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vfwscanf from C library *)
    "vfscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vsscanf from C library *)
    "vfwscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vscanf from C library *)
    "vscanf" (execute_scan_function 1)
let _ = Builtin.register
    (* vsscanf from C library *)
    "vsscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vsscanf from C library *)
    "vswscanf" (execute_scan_function 2)
let _ = Builtin.register
    (* vsscanf from C library *)
    "vwscanf" (execute_scan_function 1)
let _ = Builtin.register
    (* vsscanf from C library *)
    "wscanf" (execute_scan_function 1)

let execute_objc_alloc_no_fail
    symb_state typ
    { Builtin.pdesc; tenv; ret_ids; loc; } =
  let alloc_fun = Sil.Const (Sil.Cfun __objc_alloc_no_fail) in
  let ptr_typ = Sil.Tptr (typ, Sil.Pk_pointer) in
  let sizeof_typ = Sil.Sizeof (typ, Sil.Subtype.exact) in
  let alloc_instr = Sil.Call (ret_ids, alloc_fun, [sizeof_typ, ptr_typ], loc, Sil.cf_default) in
  SymExec.instrs tenv pdesc [alloc_instr] symb_state

let execute_objc_NSArray_alloc_no_fail
    ({ Builtin.tenv; } as builtin_args) symb_state =
  let nsarray_typ_ =
    Sil.Tvar (Typename.TN_csu (Csu.Class Csu.Objc, Mangled.from_string "NSArray")) in
  let nsarray_typ = Tenv.expand_type tenv nsarray_typ_ in
  execute_objc_alloc_no_fail symb_state nsarray_typ builtin_args

let execute_NSArray_arrayWithObjects_count builtin_args =
  let n_formals = 1 in
  let res = SymExec.check_variadic_sentinel ~fails_on_nil: true n_formals (0,1) builtin_args in
  execute_objc_NSArray_alloc_no_fail builtin_args res

let execute_NSArray_arrayWithObjects builtin_args =
  let n_formals = 1 in
  let res = SymExec.check_variadic_sentinel n_formals (0,1) builtin_args in
  execute_objc_NSArray_alloc_no_fail builtin_args res

let _ =
  let method_kind = Procname.mangled_of_objc_method_kind Procname.Class_objc_method in
  Builtin.register_procname
    (Procname.ObjC_Cpp
       (Procname.objc_cpp "NSArray" "arrayWithObjects:count:" method_kind))
    execute_NSArray_arrayWithObjects_count
let _ =
  let method_kind = Procname.mangled_of_objc_method_kind Procname.Class_objc_method in
  Builtin.register_procname
    (Procname.ObjC_Cpp
       (Procname.objc_cpp "NSArray" "arrayWithObjects:" method_kind))
    execute_NSArray_arrayWithObjects

let execute_objc_NSDictionary_alloc_no_fail
    symb_state
    ({ Builtin.tenv; } as builtin_args) =
  let nsdictionary_typ_ =
    Sil.Tvar (Typename.TN_csu (Csu.Class Csu.Objc, Mangled.from_string "NSDictionary")) in
  let nsdictionary_typ =
    Tenv.expand_type tenv nsdictionary_typ_ in
  execute_objc_alloc_no_fail symb_state nsdictionary_typ builtin_args

let execute___objc_dictionary_literal builtin_args =
  let n_formals = 1 in
  let res' = SymExec.check_variadic_sentinel ~fails_on_nil: true n_formals (0,1) builtin_args in
  execute_objc_NSDictionary_alloc_no_fail res' builtin_args

let __objc_dictionary_literal =
  let method_kind = Procname.mangled_of_objc_method_kind Procname.Class_objc_method in
  let pname =
    Procname.ObjC_Cpp
      (Procname.objc_cpp "NSDictionary" "__objc_dictionary_literal:" method_kind) in
  Builtin.register_procname pname execute___objc_dictionary_literal;
  pname
