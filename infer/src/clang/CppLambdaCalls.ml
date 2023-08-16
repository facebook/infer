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


let create_temp_store_instr ~callee_procname ~proc_name proc_desc loc (exp, typ) (acc1, acc2) =
  if Exp.is_cpp_closure exp then (
    let new_var = Pvar.mk_tmp Pvar.materialized_cpp_temporary proc_name in
    let new_var_data : ProcAttributes.var_data = ProcAttributes.default_var_data new_var typ in
    let new_var_exp = Exp.Lvar new_var in
    Procdesc.append_locals proc_desc [new_var_data] ;
    let store_instr = Sil.Store {e1= new_var_exp; typ; e2= exp; loc} in
    if Procname.is_cpp_lambda callee_procname || is_std_function_function callee_procname then
      let new_id = Ident.create_fresh Ident.knormal in
      let load_var_instr = Sil.Load {id= new_id; e= Lvar new_var; typ; loc} in
      ((Exp.Var new_id, typ) :: acc1, store_instr :: load_var_instr :: acc2)
    else ((new_var_exp, typ) :: acc1, store_instr :: acc2) )
  else ((exp, typ) :: acc1, acc2)


(** This does the following replacement:

    f(closure) ---> temp = closure; f(temp) for general function calls because they expect a pointer
    to a closure

    f(closure) ---> temp = closure; n = temp; f(n) for operator() and std::function::function
    because they expect the actual closure

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
              ~f:(create_temp_store_instr ~callee_procname ~proc_name proc_desc loc)
              args
          in
          List.append new_instrs
            [Sil.Call (ret_id_typ, Const (Cfun callee_procname), new_args, loc, flags)]
      | _ ->
          [instr]
    in
    Array.of_list instrs
  in
  let temp_ids =
    List.filter_map
      ~f:(fun var_data -> var_data.ProcAttributes.tmp_id)
      (Procdesc.get_locals proc_desc)
  in
  Ident.update_name_generator temp_ids ;
  ProcCfg.Normal.fold_nodes proc_desc ~init:() ~f:(fun _ node ->
      let used_ids = Instrs.instrs_get_normal_vars (ProcCfg.Normal.instrs node) in
      Ident.update_name_generator used_ids ) ;
  ignore
    (Procdesc.replace_instrs_by_using_context proc_desc ~f:add_calls
       ~update_context:(fun () _ -> ())
       ~context_at_node:(fun _ -> ()) )


let process cfg =
  Ident.NameGenerator.reset () ;
  Procname.Hash.iter replace_lambda_as_arg_calls cfg
