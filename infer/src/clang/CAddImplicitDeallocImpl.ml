(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let get_dealloc_call_field (self_var, self_typ) location instrs (fieldname, field_typ, _) =
  match field_typ.Typ.desc with
  | Typ.Tptr (({desc= Tstruct name} as cls), Pk_pointer) when Typ.is_objc_class cls ->
      let field_class_dealloc_name = Procname.make_objc_dealloc name in
      let id_pvar = Ident.create_fresh Ident.knormal in
      let load_pvar_instr =
        Sil.Load {id= id_pvar; e= Lvar self_var; root_typ= self_typ; typ= self_typ; loc= location}
      in
      let id_field = Ident.create_fresh Ident.knormal in
      let class_typ = match self_typ.Typ.desc with Typ.Tptr (t, _) -> t | _ -> self_typ in
      let e = Exp.Lfield (Var id_pvar, fieldname, class_typ) in
      let load_field_instr =
        Sil.Load {id= id_field; e; root_typ= field_typ; typ= field_typ; loc= location}
      in
      let ret_id = Ident.create_fresh Ident.knormal in
      let call_instr =
        Sil.Call
          ( (ret_id, StdTyp.void)
          , Const (Cfun field_class_dealloc_name)
          , [(Var id_field, field_typ)]
          , location
          , CallFlags.default )
      in
      instrs @ [load_pvar_instr; load_field_instr; call_instr]
  | _ ->
      instrs


let process_dealloc proc_desc fields self =
  let exit_node = Procdesc.get_exit_node proc_desc in
  let location = Procdesc.Node.get_last_loc exit_node in
  let fields_dealloc_call_instrs =
    List.fold ~f:(get_dealloc_call_field self location) ~init:[] fields
  in
  let exit_pred_nodes = Procdesc.Node.get_preds exit_node in
  let node_name = Procdesc.Node.Call CFrontend_config.dealloc in
  let node_kind = Procdesc.Node.Stmt_node node_name in
  let dealloc_calls_node =
    Procdesc.create_node proc_desc location node_kind fields_dealloc_call_instrs
  in
  Procdesc.node_set_succs proc_desc dealloc_calls_node ~normal:[exit_node] ~exn:[] ;
  List.iter
    ~f:(fun node -> Procdesc.node_set_succs proc_desc node ~normal:[dealloc_calls_node] ~exn:[])
    exit_pred_nodes


let process_procdesc tenv proc_name proc_desc =
  let get_struct_procname tenv proc_name =
    match Procname.get_class_type_name proc_name with
    | Some name ->
        Tenv.lookup tenv name
    | None ->
        None
  in
  if Procdesc.is_defined proc_desc && Procname.is_objc_dealloc proc_name then
    let struct_opt = get_struct_procname tenv proc_name in
    match struct_opt with
    | Some {fields} -> (
        let formals = Procdesc.get_formals proc_desc in
        let self = List.find ~f:(fun (var, _) -> Mangled.equal var Mangled.self) formals in
        match self with
        | Some (self, typ) ->
            let self_var = Pvar.mk self proc_name in
            process_dealloc proc_desc fields (self_var, typ)
        | None ->
            () )
    | _ ->
        ()
  else ()


let process cfg tenv = Procname.Hash.iter (process_procdesc tenv) cfg
