(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_mockptr_initializer prefix procname =
  Procname.get_global_name_of_initializer procname
  |> Option.exists ~f:(String.is_substring ~substring:prefix)


let process_globals_initializers prefix procname proc_desc map =
  let process_code map _node instr =
    match (instr : Sil.instr) with
    | Sil.Store {e1= Exp.Lvar pvar; e2= Const (Cfun function_name)} ->
        Pvar.Map.add pvar function_name map
    | _ ->
        map
  in
  if is_mockptr_initializer prefix procname then
    Procdesc.fold_instrs proc_desc ~init:map ~f:process_code
  else map


let remove_globals_initializers prefix cfg =
  let remove_globals_initializer procname _ =
    if is_mockptr_initializer prefix procname then Procname.Hash.remove cfg procname
  in
  Procname.Hash.iter remove_globals_initializer cfg


let update_context globals_map map instr =
  match instr with
  | Sil.Load {id; e= Lvar pvar} -> (
    match Pvar.Map.find_opt pvar globals_map with
    | Some procname ->
        Ident.Map.add id procname map
    | None ->
        map )
  | _ ->
      map


let replace_mockpointers_calls globals_map _procname proc_desc =
  let add_calls _node ids_map instr =
    let instrs =
      match (instr : Sil.instr) with
      | Sil.Call (ret_id_typ, Const (Cfun procname), (Var caller_var, _) :: args, loc, flags)
        when Procname.equal BuiltinDecl.__call_c_function_ptr procname -> (
        match Ident.Map.find_opt caller_var ids_map with
        | Some procname ->
            let new_loc = {loc with macro_file_opt= None; macro_line= 0} in
            [Sil.Call (ret_id_typ, Const (Cfun procname), args, new_loc, flags)]
        | None ->
            [instr] )
      | Sil.Load {e= Lvar pvar} when Pvar.Map.mem pvar globals_map ->
          []
      | _ ->
          [instr]
    in
    Array.of_list instrs
  in
  ignore
    (Procdesc.replace_instrs_by_using_context proc_desc ~f:add_calls
       ~update_context:(update_context globals_map) ~context_at_node:(fun _ -> Ident.Map.empty ) )


let process cfg prefix =
  let globals_map = Procname.Hash.fold (process_globals_initializers prefix) cfg Pvar.Map.empty in
  Procname.Hash.iter (replace_mockpointers_calls globals_map) cfg ;
  remove_globals_initializers prefix cfg
