(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
          try Procdesc.NodeMap.find node !node_map
          with Caml.Not_found ->
            let new_node = convert_node node
            and successors = Procdesc.Node.get_succs node
            and exn_nodes = Procdesc.Node.get_exn node in
            node_map := Procdesc.NodeMap.add node new_node !node_map ;
            if Procdesc.Node.equal node callee_start_node then
              Procdesc.set_start_node resolved_pdesc new_node ;
            if Procdesc.Node.equal node callee_exit_node then
              Procdesc.set_exit_node resolved_pdesc new_node ;
            Procdesc.node_set_succs callee_pdesc new_node ~normal:(loop successors)
              ~exn:(loop exn_nodes) ;
            new_node
        in
        converted_node :: loop other_node
  in
  ignore (loop [callee_start_node]) ;
  resolved_pdesc


(** clone a procedure description and apply the type substitutions where the parameters are used *)
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
        { id
        ; e= Exp.Lvar origin_pvar as origin_exp
        ; typ= {Typ.desc= Tptr ({desc= Tstruct origin_typename}, Pk_pointer)}
        ; loc } ->
        let specialized_typname =
          try Mangled.Map.find (Pvar.get_name origin_pvar) substitutions
          with Caml.Not_found -> origin_typename
        in
        subst_map := Ident.Map.add id specialized_typname !subst_map ;
        let typ = mk_ptr_typ specialized_typname in
        Some (Sil.Load {id; e= convert_exp origin_exp; typ; loc})
    | Sil.Load
        {id; e= Exp.Var origin_id as origin_exp; typ= {Typ.desc= Tstruct _} as origin_typ; loc} ->
        let updated_typ : Typ.t =
          try Typ.mk ~default:origin_typ (Tstruct (Ident.Map.find origin_id !subst_map))
          with Caml.Not_found -> origin_typ
        in
        Some (Sil.Load {id; e= convert_exp origin_exp; typ= updated_typ; loc})
    | Sil.Load {id; e= origin_exp; typ; loc} ->
        Some (Sil.Load {id; e= convert_exp origin_exp; typ; loc})
    | Sil.Store {e1= assignee_exp; typ= origin_typ; e2= origin_exp; loc} ->
        let set_instr =
          Sil.Store {e1= convert_exp assignee_exp; typ= origin_typ; e2= convert_exp origin_exp; loc}
        in
        Some set_instr
    | Sil.Call
        ( return_ids
        , Exp.Const (Const.Cfun callee_pname)
        , (Exp.Var id, _) :: origin_args
        , loc
        , call_flags )
      when call_flags.CallFlags.cf_virtual && Option.is_some (redirect_typename id) ->
        let redirected_typename = Option.value_exn (redirect_typename id) in
        let redirected_typ = mk_ptr_typ redirected_typename in
        let redirected_pname = Procname.replace_class callee_pname redirected_typename in
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
    | Sil.Metadata _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        None
  in
  let f_instr_list instrs = Instrs.filter_map ~f:convert_instr instrs in
  convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list


exception UnmatchedParameters

(** Creates a copy of a procedure description and a list of type substitutions of the form (name,
    typ) where name is a parameter. The resulting proc desc is isomorphic but all the type of the
    parameters are replaced in the instructions according to the list. The virtual calls are also
    replaced to match the parameter types *)
let with_formals_types callee_pdesc resolved_pname args =
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  let resolved_params, substitutions =
    match
      List.fold2 ~init:([], Mangled.Map.empty) callee_attributes.formals args
        ~f:(fun (params, subts) (param_name, param_typ, param_annot) (_, arg_typ) ->
          match arg_typ.Typ.desc with
          | Tptr ({desc= Tstruct typename}, Pk_pointer) ->
              (* Replace the type of the parameter by the type of the argument *)
              ( (param_name, arg_typ, param_annot) :: params
              , Mangled.Map.add param_name typename subts )
          | _ ->
              ((param_name, param_typ, param_annot) :: params, subts) )
    with
    | Ok result ->
        result
    | Unequal_lengths ->
        L.(debug Analysis Medium)
          "Call mismatch: method %a has %i paramters but is called with %i arguments@." Procname.pp
          resolved_pname
          (List.length callee_attributes.formals)
          (List.length args) ;
        raise UnmatchedParameters
  in
  let resolved_attributes =
    { callee_attributes with
      formals= List.rev resolved_params
    ; proc_name= resolved_pname
    ; is_specialized= true }
  in
  let resolved_proc_desc = Procdesc.from_proc_attributes resolved_attributes in
  let resolved_proc_desc = with_formals_types_proc callee_pdesc resolved_proc_desc substitutions in
  Attributes.store ~proc_desc:(Some resolved_proc_desc) resolved_attributes ~analysis:true ;
  resolved_proc_desc
