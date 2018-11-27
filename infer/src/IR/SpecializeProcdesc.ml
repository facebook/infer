(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(** Applies f_instr_list to all the instructions in all the nodes of the cfg *)
let convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list =
  let callee_start_node = Procdesc.get_start_node callee_pdesc
  and callee_exit_node = Procdesc.get_exit_node callee_pdesc in
  let node_map = ref Procdesc.NodeMap.empty in
  let rec convert_node node =
    let loc = Procdesc.Node.get_loc node
    and kind = Procdesc.Node.get_kind node
    and instrs = f_instr_list (Procdesc.Node.get_instrs node) in
    Procdesc.create_node_from_not_reversed resolved_pdesc loc kind instrs
  and loop callee_nodes =
    match callee_nodes with
    | [] ->
        []
    | node :: other_node ->
        let converted_node =
          try Procdesc.NodeMap.find node !node_map with Caml.Not_found ->
            let new_node = convert_node node
            and successors = Procdesc.Node.get_succs node
            and exn_nodes = Procdesc.Node.get_exn node in
            node_map := Procdesc.NodeMap.add node new_node !node_map ;
            if Procdesc.Node.equal node callee_start_node then
              Procdesc.set_start_node resolved_pdesc new_node ;
            if Procdesc.Node.equal node callee_exit_node then
              Procdesc.set_exit_node resolved_pdesc new_node ;
            Procdesc.node_set_succs_exn callee_pdesc new_node (loop successors) (loop exn_nodes) ;
            new_node
        in
        converted_node :: loop other_node
  in
  ignore (loop [callee_start_node]) ;
  resolved_pdesc


(** clone a procedure description and apply the type substitutions where
      the parameters are used *)
let with_formals_types_proc callee_pdesc resolved_pdesc substitutions =
  let resolved_pname = Procdesc.get_proc_name resolved_pdesc in
  let convert_pvar pvar = Pvar.mk (Pvar.get_name pvar) resolved_pname in
  let mk_ptr_typ typename =
    (* Only consider pointers from objects for now *)
    Typ.mk (Tptr (Typ.mk (Tstruct typename), Typ.Pk_pointer))
  in
  let convert_exp = function
    | Exp.Lvar origin_pvar ->
        Exp.Lvar (convert_pvar origin_pvar)
    | exp ->
        exp
  in
  let subst_map = ref Ident.Map.empty in
  let redirect_typename origin_id =
    try Some (Ident.Map.find origin_id !subst_map) with Caml.Not_found -> None
  in
  let convert_instr = function
    | Sil.Load
        ( id
        , (Exp.Lvar origin_pvar as origin_exp)
        , {Typ.desc= Tptr ({desc= Tstruct origin_typename}, Pk_pointer)}
        , loc ) ->
        let specialized_typname =
          try Mangled.Map.find (Pvar.get_name origin_pvar) substitutions with Caml.Not_found ->
            origin_typename
        in
        subst_map := Ident.Map.add id specialized_typname !subst_map ;
        Some (Sil.Load (id, convert_exp origin_exp, mk_ptr_typ specialized_typname, loc))
    | Sil.Load (id, (Exp.Var origin_id as origin_exp), ({Typ.desc= Tstruct _} as origin_typ), loc)
      ->
        let updated_typ : Typ.t =
          try Typ.mk ~default:origin_typ (Tstruct (Ident.Map.find origin_id !subst_map))
          with Caml.Not_found -> origin_typ
        in
        Some (Sil.Load (id, convert_exp origin_exp, updated_typ, loc))
    | Sil.Load (id, origin_exp, origin_typ, loc) ->
        Some (Sil.Load (id, convert_exp origin_exp, origin_typ, loc))
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        Some set_instr
    | Sil.Call
        ( return_ids
        , Exp.Const (Const.Cfun callee_pname)
        , (Exp.Var id, _) :: origin_args
        , loc
        , call_flags )
      when call_flags.CallFlags.cf_virtual && redirect_typename id <> None ->
        let redirected_typename = Option.value_exn (redirect_typename id) in
        let redirected_typ = mk_ptr_typ redirected_typename in
        let redirected_pname = Typ.Procname.replace_class callee_pname redirected_typename in
        let args =
          let other_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
          (Exp.Var id, redirected_typ) :: other_args
        in
        let call_instr =
          Sil.Call (return_ids, Exp.Const (Const.Cfun redirected_pname), args, loc, call_flags)
        in
        Some call_instr
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
        let call_instr =
          Sil.Call (return_ids, convert_exp origin_call_exp, converted_args, loc, call_flags)
        in
        Some call_instr
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        Some (Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind))
    | Sil.Nullify _ | Abstract _ | ExitScope _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        None
  in
  let f_instr_list instrs = Instrs.filter_map ~f:convert_instr instrs in
  convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list


exception UnmatchedParameters

(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)
let with_formals_types ?(has_clang_model = false) callee_pdesc resolved_pname args =
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  let resolved_params, substitutions =
    match
      List.fold2 ~init:([], Mangled.Map.empty) callee_attributes.formals args
        ~f:(fun (params, subts) (param_name, param_typ) (_, arg_typ) ->
          match arg_typ.Typ.desc with
          | Tptr ({desc= Tstruct typename}, Pk_pointer) ->
              (* Replace the type of the parameter by the type of the argument *)
              ((param_name, arg_typ) :: params, Mangled.Map.add param_name typename subts)
          | _ ->
              ((param_name, param_typ) :: params, subts) )
    with
    | Ok result ->
        result
    | Unequal_lengths ->
        L.(debug Analysis Medium)
          "Call mismatch: method %a has %i paramters but is called with %i arguments@."
          Typ.Procname.pp resolved_pname
          (List.length callee_attributes.formals)
          (List.length args) ;
        raise UnmatchedParameters
  in
  let translation_unit =
    (* If it is a model, and we are using the procdesc stored in the summary, the default translation unit
       won't be useful because we don't store that tenv, so we aim to find the source file of the caller to
       use its tenv. *)
    if has_clang_model then
      let pname = Procdesc.get_proc_name callee_pdesc in
      match Attributes.find_file_capturing_procedure pname with
      | Some (source_file, _) ->
          source_file
      | None ->
          Logging.die InternalError
            "specialize_types should only be called with defined procedures, but we cannot find \
             the captured file of procname %a"
            Typ.Procname.pp pname
    else callee_attributes.translation_unit
  in
  let resolved_attributes =
    { callee_attributes with
      formals= List.rev resolved_params
    ; proc_name= resolved_pname
    ; is_specialized= true
    ; translation_unit }
  in
  let resolved_proc_desc = Procdesc.from_proc_attributes resolved_attributes in
  let resolved_proc_desc = with_formals_types_proc callee_pdesc resolved_proc_desc substitutions in
  (* The attributes here are used to retrieve the per-file type environment for Clang languages.
     The analysis for Java is using a global type environment *)
  if not (Typ.Procname.is_java resolved_pname) then
    Attributes.store ~proc_desc:(Some resolved_proc_desc) resolved_attributes ;
  resolved_proc_desc


let with_block_args_instrs resolved_pdesc substitutions =
  let resolved_pname = Procdesc.get_proc_name resolved_pdesc in
  let convert_pvar pvar = Pvar.mk (Pvar.get_name pvar) resolved_pname in
  let convert_exp exp =
    match exp with
    | Exp.Lvar origin_pvar ->
        let new_pvar = convert_pvar origin_pvar in
        Exp.Lvar new_pvar
    | Exp.Lfield (Exp.Lvar origin_pvar, fname, typ) ->
        let new_pvar = convert_pvar origin_pvar in
        Exp.Lfield (Exp.Lvar new_pvar, fname, typ)
    | _ ->
        exp
  in
  let convert_instr (instrs, id_map) instr =
    let get_block_name_and_load_captured_vars_instrs block_var loc =
      let block_name, extra_formals = Mangled.Map.find block_var substitutions in
      let dead_vars, id_exp_typs, load_instrs =
        List.map extra_formals ~f:(fun (var, typ) ->
            let id = Ident.create_fresh_specialized_with_blocks Ident.knormal in
            let pvar = Pvar.mk var resolved_pname in
            (Var.of_id id, (Exp.Var id, pvar, typ), Sil.Load (id, Exp.Lvar pvar, typ, loc)) )
        |> List.unzip3
      in
      let remove_temps_instr = Sil.ExitScope (dead_vars, loc) in
      (block_name, id_exp_typs, load_instrs, remove_temps_instr)
    in
    let convert_generic_call return_ids exp origin_args loc call_flags =
      let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
      let call_instr = Sil.Call (return_ids, exp, converted_args, loc, call_flags) in
      (call_instr :: instrs, id_map)
    in
    match instr with
    | Sil.Load (id, Exp.Lvar block_param, _, _)
      when Mangled.Map.mem (Pvar.get_name block_param) substitutions ->
        let id_map = Ident.Map.add id (Pvar.get_name block_param) id_map in
        (* we don't need the load the block param instruction anymore *)
        (instrs, id_map)
    | Sil.Load (id, origin_exp, origin_typ, loc) ->
        (Sil.Load (id, convert_exp origin_exp, origin_typ, loc) :: instrs, id_map)
    | Sil.Store (assignee_exp, origin_typ, Exp.Var id, loc) when Ident.Map.mem id id_map ->
        let block_param = Ident.Map.find id id_map in
        let block_name, id_exp_typs, load_instrs, remove_temps_instr =
          get_block_name_and_load_captured_vars_instrs block_param loc
        in
        let closure = Exp.Closure {name= block_name; captured_vars= id_exp_typs} in
        let instr = Sil.Store (assignee_exp, origin_typ, closure, loc) in
        ((remove_temps_instr :: instr :: load_instrs) @ instrs, id_map)
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        (set_instr :: instrs, id_map)
    | Sil.Call (return_ids, Exp.Var id, origin_args, loc, call_flags) -> (
      try
        let block_name, id_exp_typs, load_instrs, remove_temps_instr =
          let block_var = Ident.Map.find id id_map in
          get_block_name_and_load_captured_vars_instrs block_var loc
        in
        let call_instr =
          let id_exps = List.map ~f:(fun (id, _, typ) -> (id, typ)) id_exp_typs in
          let converted_args =
            List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args
          in
          Sil.Call
            ( return_ids
            , Exp.Const (Const.Cfun block_name)
            , id_exps @ converted_args
            , loc
            , call_flags )
        in
        let instrs = (remove_temps_instr :: call_instr :: load_instrs) @ instrs in
        (instrs, id_map)
      with Caml.Not_found ->
        convert_generic_call return_ids (Exp.Var id) origin_args loc call_flags )
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        convert_generic_call return_ids origin_call_exp origin_args loc call_flags
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        (Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind) :: instrs, id_map)
    | Sil.Nullify _ | Abstract _ | Sil.ExitScope _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        (instrs, id_map)
  in
  let f_instr_list instrs =
    let rev_instrs, _ = Instrs.fold ~f:convert_instr ~init:([], Ident.Map.empty) instrs in
    Instrs.of_rev_list rev_instrs
  in
  f_instr_list


let append_no_duplicates_formals_and_annot =
  Staged.unstage
    (IList.append_no_duplicates ~cmp:(fun ((name1, _), _) ((name2, _), _) ->
         Mangled.compare name1 name2 ))


let with_block_args callee_pdesc pname_with_block_args block_args =
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  (* Substitution from a block parameter to the block name and the new formals
  that correspond to the captured variables *)
  let substitutions : (Typ.Procname.t * (Mangled.t * Typ.t) list) Mangled.Map.t =
    List.fold2_exn callee_attributes.formals block_args ~init:Mangled.Map.empty
      ~f:(fun subts (param_name, _) block_arg_opt ->
        match block_arg_opt with
        | Some (cl : Exp.closure) ->
            let formals_from_captured =
              List.map
                ~f:(fun (_, var, typ) ->
                  (* Here we create fresh names for the new formals, based on the names of the captured
                   variables annotated with the name of the caller method *)
                  (Pvar.get_name_of_local_with_procname var, typ) )
                cl.captured_vars
            in
            Mangled.Map.add param_name (cl.name, formals_from_captured) subts
        | None ->
            subts )
  in
  (* Extend formals with fresh variables for the captured variables of the block arguments,
    without duplications. *)
  let new_formals_blocks_captured_vars, extended_formals_annots =
    let new_formals_blocks_captured_vars_with_annots =
      let formals_annots =
        List.zip_exn callee_attributes.formals callee_attributes.method_annotation.params
      in
      List.fold formals_annots ~init:[] ~f:(fun acc ((param_name, typ), annot) ->
          try
            let _, captured = Mangled.Map.find param_name substitutions in
            append_no_duplicates_formals_and_annot acc
              (List.map captured ~f:(fun captured_var -> (captured_var, Annot.Item.empty)))
          with Caml.Not_found ->
            append_no_duplicates_formals_and_annot acc [((param_name, typ), annot)] )
    in
    List.unzip new_formals_blocks_captured_vars_with_annots
  in
  let translation_unit =
    let pname = Procdesc.get_proc_name callee_pdesc in
    match Attributes.find_file_capturing_procedure pname with
    | Some (source_file, _) ->
        source_file
    | None ->
        Logging.die InternalError
          "specialize_with_block_args ahould only be called with defined procedures, but we \
           cannot find the captured file of procname %a"
          Typ.Procname.pp pname
  in
  let resolved_attributes =
    { callee_attributes with
      proc_name= pname_with_block_args
    ; is_defined= true
    ; formals= new_formals_blocks_captured_vars
    ; method_annotation=
        {return= callee_attributes.method_annotation.return; params= extended_formals_annots}
    ; translation_unit }
  in
  let resolved_pdesc = Procdesc.from_proc_attributes resolved_attributes in
  Logging.(debug Analysis Verbose)
    "signature of base method %a@." Procdesc.pp_signature callee_pdesc ;
  Logging.(debug Analysis Verbose)
    "signature of specialized method %a@." Procdesc.pp_signature resolved_pdesc ;
  let proc_desc =
    convert_cfg ~callee_pdesc ~resolved_pdesc
      ~f_instr_list:(with_block_args_instrs resolved_pdesc substitutions)
  in
  Attributes.store ~proc_desc:(Some proc_desc) resolved_attributes ;
  proc_desc
