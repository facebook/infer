(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let get_dealloc_call_field (self_var, self_typ) location instrs
    {Struct.name= fieldname; typ= field_typ} =
  match field_typ.Typ.desc with
  | Typ.Tptr (({desc= Tstruct name} as cls), Pk_pointer) when Typ.is_objc_class cls ->
      let field_class_dealloc_name = Procname.make_objc_dealloc name in
      let id_pvar = Ident.create_fresh Ident.knormal in
      let load_pvar_instr =
        Sil.Load {id= id_pvar; e= Lvar self_var; typ= self_typ; loc= location}
      in
      let id_field = Ident.create_fresh Ident.knormal in
      let class_typ = match self_typ.Typ.desc with Typ.Tptr (t, _) -> t | _ -> self_typ in
      let e = Exp.Lfield (Var id_pvar, fieldname, class_typ) in
      let load_field_instr = Sil.Load {id= id_field; e; typ= field_typ; loc= location} in
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


(* Generates code equivalent to:
   count = __objc_get_ref_count(self);
   __objc_set_ref_count(self, (count - 1));
   new_count = __objc_get_ref_count(self);
*)
let decrement_ref_count self_var self_typ location =
  let self_id = Ident.create_fresh Ident.knormal in
  let load_self_instr =
    Sil.Load {id= self_id; e= Exp.Lvar self_var; typ= self_typ; loc= location}
  in
  let self_var = Exp.Var self_id in
  let count_id = Ident.create_fresh Ident.knormal in
  let count_typ = StdTyp.uint in
  let call_get_count_instr =
    Sil.Call
      ( (count_id, count_typ)
      , Const (Cfun BuiltinDecl.__objc_get_ref_count)
      , [(self_var, self_typ)]
      , location
      , CallFlags.default )
  in
  let new_count_e = Exp.BinOp (MinusA None, Var count_id, Const (Cint IntLit.one)) in
  let ret = (Ident.create_fresh Ident.knormal, StdTyp.void) in
  let call_set_count_instr =
    Sil.Call
      ( ret
      , Const (Cfun BuiltinDecl.__objc_set_ref_count)
      , [(self_var, self_typ); (new_count_e, count_typ)]
      , location
      , CallFlags.default )
  in
  let new_count_id = Ident.create_fresh Ident.knormal in
  let call_get_new_count_instr =
    Sil.Call
      ( (new_count_id, count_typ)
      , Const (Cfun BuiltinDecl.__objc_get_ref_count)
      , [(self_var, self_typ)]
      , location
      , CallFlags.default )
  in
  let instrs =
    [load_self_instr; call_get_count_instr; call_set_count_instr; call_get_new_count_instr]
  in
  (instrs, (new_count_id, count_typ))


(* Generates code equivalent to:

   /* ref_count_node */
   // decrement ref_count
   count = __objc_get_ref_count(self);
   __objc_set_ref_count(self, (count - 1));
   new_count = __objc_get_ref_count(self);
   /* !ref_count_node */

   /* dealloc_prune_node */
   if (new_count == 0) {
   /* !dealloc_prune_node */

     /* deallocation_nodes */
     // user implemented dealloc instructions
     ...

     // field dealloc calls
     field1 = self.__field1;
     field1_typ.dealloc(field1);
     field2 = self.__field2;
     field2_typ.dealloc(field2);
     /* !deallocation_nodes */
   }

   /* not_dealloc_prune_node */
   else // new_count != 0 {
   /* !not_dealloc_prune_node */

     // do nothing
   }
*)
let dealloc_if_no_ref proc_desc (self_var, self_typ) =
  let start_node = Procdesc.get_start_node proc_desc in
  let exit_node = Procdesc.get_exit_node proc_desc in
  let location = Procdesc.Node.get_last_loc start_node in
  let ref_count_instrs, (count_id, _count_typ) = decrement_ref_count self_var self_typ location in
  let ref_count_node =
    let node_name = Procdesc.Node.Call CFrontend_config.dealloc in
    let node_kind = Procdesc.Node.Stmt_node node_name in
    Procdesc.create_node proc_desc location node_kind ref_count_instrs
  in
  let if_kind = Sil.Ik_if {terminated= true} in
  let dealloc_prune_node =
    let cond_exp = Exp.BinOp (Eq, Var count_id, Const (Cint IntLit.zero)) in
    let instr = Sil.Prune (cond_exp, location, true, if_kind) in
    let node_kind = Procdesc.Node.Prune_node (true, if_kind, PruneNodeKind_TrueBranch) in
    Procdesc.create_node proc_desc location node_kind [instr]
  in
  let not_dealloc_prune_node =
    let cond_exp = Exp.BinOp (Ne, Var count_id, Const (Cint IntLit.zero)) in
    let instr = Sil.Prune (cond_exp, location, false, if_kind) in
    let node_kind = Procdesc.Node.Prune_node (false, if_kind, PruneNodeKind_FalseBranch) in
    Procdesc.create_node proc_desc location node_kind [instr]
  in
  Procdesc.node_set_succs proc_desc not_dealloc_prune_node ~normal:[exit_node] ~exn:[] ;
  let deallocation_nodes = Procdesc.Node.get_succs start_node in
  Procdesc.node_set_succs proc_desc dealloc_prune_node ~normal:deallocation_nodes ~exn:[] ;
  Procdesc.node_set_succs proc_desc ref_count_node
    ~normal:[dealloc_prune_node; not_dealloc_prune_node]
    ~exn:[] ;
  Procdesc.node_set_succs proc_desc start_node ~normal:[ref_count_node] ~exn:[]


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
    exit_pred_nodes ;
  dealloc_if_no_ref proc_desc self


let process_procdesc tenv proc_name proc_desc =
  let get_struct_procname tenv proc_name =
    match Procname.get_class_type_name proc_name with
    | Some name ->
        Tenv.lookup tenv name
    | None ->
        None
  in
  if Procdesc.is_defined proc_desc && Procname.is_objc_dealloc proc_name then
    get_struct_procname tenv proc_name
    |> Option.iter ~f:(fun Struct.{fields} ->
           let formals = Procdesc.get_formals proc_desc in
           let self = List.find ~f:(fun (var, _, _) -> Mangled.equal var Mangled.self) formals in
           Option.iter self ~f:(fun (self, typ, _) ->
               let self_var = Pvar.mk self proc_name in
               process_dealloc proc_desc fields (self_var, typ) ) )


let process cfg tenv = Procname.Hash.iter (process_procdesc tenv) cfg
