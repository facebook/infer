(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CFG = ProcCfg.Normal

module PPPVar = struct
  type t = Pvar.t [@@deriving compare, equal]

  let pp = Pvar.pp Pp.text
end

module VDom = AbstractDomain.Flat (PPPVar)
module Domain = AbstractDomain.SafeInvertedMap (Ident) (VDom)

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate _analysis_data _node instr =
    let open Sil in
    match instr with
    | Load {id; e= Exp.Lvar pvar} ->
        Domain.add id (VDom.v pvar) astate
    | Load {id} ->
        Domain.add id VDom.top astate
    | Call ((id, _), _, _, _, _) ->
        Domain.add id VDom.top astate
    | _ ->
        astate


  let pp_session_name node fmt =
    Format.fprintf fmt "Closure Subst Specialized Method %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let exec_instr domain proc_name formals_to_blocks_map _node instr =
  let open Sil in
  let res =
    let exec_exp exp =
      let exec_pvar pvar = Pvar.swap_proc_in_local_pvar pvar proc_name in
      match exp with Exp.Lvar origin_pvar -> Exp.Lvar (exec_pvar origin_pvar) | exp -> exp
    in
    match instr with
    | Load {id; e; root_typ; loc} ->
        [Load {id; e= exec_exp e; root_typ; typ= root_typ; loc}]
    | Store {e1; root_typ; typ; e2; loc} ->
        [Store {e1= exec_exp e1; root_typ; typ; e2= exec_exp e2; loc}]
    | Call (ret_id_typ, Var id, origin_args, loc, call_flags) -> (
        let converted_args = List.map ~f:(fun (exp, typ) -> (exec_exp exp, typ)) origin_args in
        match Option.bind ~f:VDom.get (Domain.find_opt id domain) with
        | None ->
            [instr]
        | Some pvar -> (
          match Mangled.Map.find_opt (Pvar.get_name pvar) formals_to_blocks_map with
          | Some (procname, extra_formals) ->
              let extra_args, load_instrs =
                List.map
                  ~f:(fun (name, typ) ->
                    let e = Exp.Lvar (Pvar.mk name proc_name) in
                    let id = Ident.create_fresh Ident.knormal in
                    let load_instr = Load {id; e; root_typ= typ; typ; loc} in
                    ((Exp.Var id, typ), load_instr) )
                  extra_formals
                |> List.unzip
              in
              load_instrs
              @ [ Call
                    (ret_id_typ, Const (Cfun procname), extra_args @ converted_args, loc, call_flags)
                ]
          | None ->
              [instr] ) )
    | Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        let converted_args = List.map ~f:(fun (exp, typ) -> (exec_exp exp, typ)) origin_args in
        [Call (return_ids, exec_exp origin_call_exp, converted_args, loc, call_flags)]
    | Prune (origin_exp, loc, is_true_branch, if_kind) ->
        [Prune (exec_exp origin_exp, loc, is_true_branch, if_kind)]
    | _ ->
        [instr]
  in
  Array.of_list res


let process summary =
  let pdesc = Summary.get_proc_desc summary in
  let proc_name = Procdesc.get_proc_name pdesc in
  let proc_attributes = Procdesc.get_attributes pdesc in
  match proc_attributes.ProcAttributes.specialized_with_blocks_info with
  | Some spec_with_blocks_info -> (
    match AnalysisCallbacks.get_proc_desc spec_with_blocks_info.orig_proc with
    | Some orig_proc_desc -> (
        let formals_to_blocks_map = spec_with_blocks_info.formals_to_procs_and_new_formals in
        Procdesc.shallow_copy_code_from_pdesc ~orig_pdesc:orig_proc_desc ~dest_pdesc:pdesc ;
        let analysis_data = () in
        match Analyzer.compute_post ~initial:Domain.empty analysis_data pdesc with
        | Some domain ->
            let used_ids = Domain.bindings domain |> List.map ~f:fst in
            Ident.update_name_generator used_ids ;
            Procdesc.replace_instrs_by pdesc ~f:(exec_instr domain proc_name formals_to_blocks_map)
            |> ignore ;
            ()
        | None ->
            () )
    | _ ->
        () )
  | _ ->
      ()
