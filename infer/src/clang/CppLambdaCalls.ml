(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let is_std_function_function procname =
  let name = F.asprintf "%a" Procname.pp_without_templates procname in
  String.is_prefix name ~prefix:"std::function::function"


let is_lambda_copy_ctor proc_name attributes =
  Procname.is_lambda proc_name && attributes.ProcAttributes.is_cpp_copy_ctor


let is_lambda_move_ctor proc_name attributes =
  Procname.is_lambda proc_name && attributes.ProcAttributes.is_cpp_move_ctor


let is_callee_lambda_copy_ctor cfg callee_procname =
  match Procname.Hash.find_opt cfg callee_procname with
  | Some procdesc ->
      let callee_attributes = Procdesc.get_attributes procdesc in
      is_lambda_copy_ctor callee_procname callee_attributes
  | None ->
      false


let updateTemps proc_desc =
  let temp_ids =
    List.filter_map
      ~f:(fun var_data -> var_data.ProcAttributes.tmp_id)
      (Procdesc.get_locals proc_desc)
  in
  Ident.update_name_generator temp_ids ;
  ProcCfg.Normal.fold_nodes proc_desc ~init:() ~f:(fun _ node ->
      let used_ids = Instrs.instrs_get_normal_vars (ProcCfg.Normal.instrs node) in
      Ident.update_name_generator used_ids )


let process_instr ~callee_procname ~proc_name proc_desc loc (exp, typ) (acc1, acc2) =
  if Exp.is_cpp_closure exp then (
    let new_var = Pvar.mk_tmp Pvar.materialized_cpp_temporary proc_name in
    let new_var_data : ProcAttributes.var_data = ProcAttributes.default_var_data new_var typ in
    let new_var_exp = Exp.Lvar new_var in
    Procdesc.append_locals proc_desc [new_var_data] ;
    let store_instr = Sil.Store {e1= new_var_exp; typ; e2= exp; loc} in
    if Procname.is_cpp_lambda callee_procname then
      let new_id = Ident.create_fresh Ident.knormal in
      let load_var_instr = Sil.Load {id= new_id; e= Lvar new_var; typ; loc} in
      ((Exp.Var new_id, typ) :: acc1, store_instr :: load_var_instr :: acc2)
    else ((new_var_exp, typ) :: acc1, store_instr :: acc2) )
  else ((exp, typ) :: acc1, acc2)


(** This does the following replacement:

    f(closure) ---> temp = closure; f(temp) for general function calls because they expect a pointer
    to a closure

    f(closure) ---> temp = closure; n = temp; f(n) for operator() because they expect the actual
    closure

    The first transformation will improve the analysis: f(closure) was not working properly The
    second transformation will improve readability of the translation. Better to evaluate the
    closure first before passing it to a function. *)
let replace_lambda_as_arg_calls proc_name proc_desc =
  let add_calls _node _ instr =
    let instrs =
      match (instr : Sil.instr) with
      | Sil.Call (ret_id_typ, Const (Cfun callee_procname), args, loc, flags) ->
          let new_args, new_instrs =
            List.fold_right ~init:([], [])
              ~f:(process_instr ~callee_procname ~proc_name proc_desc loc)
              args
          in
          List.append new_instrs
            [Sil.Call (ret_id_typ, Const (Cfun callee_procname), new_args, loc, flags)]
      | _ ->
          [instr]
    in
    Array.of_list instrs
  in
  updateTemps proc_desc ;
  ignore
    (Procdesc.replace_instrs_by_using_context proc_desc ~f:add_calls
       ~update_context:(fun () _ -> ())
       ~context_at_node:(fun _ -> ()) )


let is_lambda_type typ =
  match typ.Typ.desc with
  | Typ.Tptr ({desc= Tstruct name}, Pk_pointer) | Tstruct name ->
      Procname.is_lambda_name (Typ.Name.name name)
  | _ ->
      false


let swap_copy_ctor_lambda_calls cfg _proc_name proc_desc =
  let add_calls _node _ instr =
    let instrs =
      match (instr : Sil.instr) with
      | Sil.Call
          ( _ret_id_typ
          , Const (Cfun callee_procname)
          , [(Exp.Lvar pvar, typ); (lambda_arg, lambda_arg_typ)]
          , loc
          , _flags )
        when is_callee_lambda_copy_ctor cfg callee_procname
             || (is_std_function_function callee_procname && is_lambda_type lambda_arg_typ) ->
          let new_id = Ident.create_fresh Ident.knormal in
          let load_lambda_instr = Sil.Load {id= new_id; e= lambda_arg; typ= lambda_arg_typ; loc} in
          let store_var_instr = Sil.Store {e1= Exp.Lvar pvar; typ; e2= Exp.Var new_id; loc} in
          [load_lambda_instr; store_var_instr]
      | _ ->
          [instr]
    in
    Array.of_list instrs
  in
  updateTemps proc_desc ;
  ignore
    (Procdesc.replace_instrs_by_using_context proc_desc ~f:add_calls
       ~update_context:(fun () _ -> ())
       ~context_at_node:(fun _ -> ()) )


let remove_lambda_ctor proc_name proc_desc =
  let attributes = Procdesc.get_attributes proc_desc in
  if is_lambda_copy_ctor proc_name attributes || is_lambda_move_ctor proc_name attributes then None
  else Some proc_desc


let process_proc cfg proc_name proc_desc =
  replace_lambda_as_arg_calls proc_name proc_desc ;
  swap_copy_ctor_lambda_calls cfg proc_name proc_desc


let process cfg =
  Ident.NameGenerator.reset () ;
  Procname.Hash.iter (process_proc cfg) cfg ;
  Procname.Hash.filter_map_inplace remove_lambda_ctor cfg
