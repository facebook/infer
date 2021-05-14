(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let get_load_self_instr location (self, self_typ) fieldname =
  let id_self = Ident.create_fresh Ident.knormal in
  let load_self_instr =
    Sil.Load {id= id_self; e= Lvar self; root_typ= self_typ; typ= self_typ; loc= location}
  in
  let class_typ = match self_typ.Typ.desc with Typ.Tptr (t, _) -> t | _ -> self_typ in
  let field_exp = Exp.Lfield (Var id_self, fieldname, class_typ) in
  (field_exp, load_self_instr)


let objc_getter tenv proc_desc location self_with_typ (fieldname, field_typ, _) =
  let field_exp, load_self_instr = get_load_self_instr location self_with_typ fieldname in
  let id_field = Ident.create_fresh Ident.knormal in
  let store_instrs =
    match field_typ with
    | {Typ.desc= Tstruct ((CStruct _ | CppClass _) as struct_name)} ->
        let ret_param = Exp.Lvar (Pvar.get_ret_param_pvar (Procdesc.get_proc_name proc_desc)) in
        Sil.Load {id= id_field; e= ret_param; root_typ= field_typ; typ= field_typ; loc= location}
        :: CStructUtils.struct_copy tenv location (Exp.Var id_field) field_exp ~typ:field_typ
             ~struct_name
    | _ ->
        let load_field_instr =
          Sil.Load {id= id_field; e= field_exp; root_typ= field_typ; typ= field_typ; loc= location}
        in
        let exp_var = Exp.Lvar (Procdesc.get_ret_var proc_desc) in
        let return_exp =
          Sil.Store
            {e1= exp_var; root_typ= field_typ; typ= field_typ; e2= Exp.Var id_field; loc= location}
        in
        [load_field_instr; return_exp]
  in
  load_self_instr :: store_instrs


let objc_setter tenv location self_with_typ (var, var_typ) (fieldname, field_typ, _) =
  let field_exp, load_self_instr = get_load_self_instr location self_with_typ fieldname in
  let store_instrs =
    match field_typ with
    | {Typ.desc= Tstruct ((CStruct _ | CppClass _) as struct_name)} ->
        CStructUtils.struct_copy tenv location field_exp (Exp.Lvar var) ~typ:field_typ ~struct_name
    | _ ->
        let id_field = Ident.create_fresh Ident.knormal in
        let load_var_instr =
          Sil.Load {id= id_field; e= Lvar var; root_typ= var_typ; typ= var_typ; loc= location}
        in
        let store_exp =
          Sil.Store
            {e1= field_exp; root_typ= field_typ; typ= field_typ; e2= Exp.Var id_field; loc= location}
        in
        [load_var_instr; store_exp]
  in
  load_self_instr :: store_instrs


let process_getter_setter tenv proc_name proc_desc =
  let location = Procdesc.get_loc proc_desc in
  let formals = Procdesc.get_formals proc_desc in
  let attributes = Procdesc.get_attributes proc_desc in
  Ident.NameGenerator.reset () ;
  let getter_setter_instrs =
    match (attributes.ProcAttributes.objc_accessor, formals) with
    | Some (Objc_getter field), (self, self_typ) :: _ ->
        let self_var = Pvar.mk self proc_name in
        objc_getter tenv proc_desc location (self_var, self_typ) field
    | Some (Objc_setter field), [(self, self_typ); (var_name, var_typ)] ->
        let self_var = Pvar.mk self proc_name in
        let var = Pvar.mk var_name proc_name in
        objc_setter tenv location (self_var, self_typ) (var, var_typ) field
    | _ ->
        []
  in
  if not (List.is_empty getter_setter_instrs) then (
    let new_attributes = {attributes with is_defined= true} in
    Procdesc.set_attributes proc_desc new_attributes ;
    let start_node = Procdesc.create_node proc_desc location Procdesc.Node.Start_node [] in
    let exit_node = Procdesc.create_node proc_desc location Procdesc.Node.Exit_node [] in
    Procdesc.set_start_node proc_desc start_node ;
    Procdesc.set_exit_node proc_desc exit_node ;
    let node_name = Procdesc.Node.BinaryOperatorStmt "Node" in
    let node_kind = Procdesc.Node.Stmt_node node_name in
    let getter_setter_node =
      Procdesc.create_node proc_desc location node_kind getter_setter_instrs
    in
    Procdesc.node_set_succs proc_desc start_node ~normal:[getter_setter_node] ~exn:[] ;
    Procdesc.node_set_succs proc_desc getter_setter_node ~normal:[exit_node] ~exn:[] )


let process cfg tenv = Procname.Hash.iter (process_getter_setter tenv) cfg
