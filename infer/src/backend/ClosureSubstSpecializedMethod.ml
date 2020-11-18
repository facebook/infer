(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CFG = ProcCfg.Normal
module L = Logging

module PPPVar = struct
  type t = Pvar.t [@@deriving compare, equal]

  let pp = Pvar.pp Pp.text
end

module VDom = AbstractDomain.Flat (PPPVar)
module BlockIdMap = AbstractDomain.SafeInvertedMap (Ident) (VDom)

module BlockSpec = struct
  type t = Procname.t * (Mangled.t * Typ.t) list [@@deriving compare, equal]

  let pp fmt (pname, _) = Procname.pp fmt pname
end

module SpecDom = AbstractDomain.Flat (BlockSpec)
module BlockPvarSpecMap = AbstractDomain.SafeInvertedMap (Mangled) (SpecDom)
module Domain = AbstractDomain.Pair (BlockIdMap) (BlockPvarSpecMap)

let eval_instr ((id_to_pvar_map, pvars_to_blocks_map) : Domain.t) instr =
  let open Sil in
  match instr with
  | Load {id; e= Exp.Lvar pvar} ->
      (BlockIdMap.add id (VDom.v pvar) id_to_pvar_map, pvars_to_blocks_map)
  | Store {e1= Exp.Lvar pvar; e2= Exp.Var id} ->
      let pvars_to_blocks_map =
        match Option.bind ~f:VDom.get (BlockIdMap.find_opt id id_to_pvar_map) with
        | Some block_var ->
            Option.value_map
              (BlockPvarSpecMap.find_opt (Pvar.get_name block_var) pvars_to_blocks_map)
              ~default:pvars_to_blocks_map
              ~f:(fun res -> BlockPvarSpecMap.add (Pvar.get_name pvar) res pvars_to_blocks_map)
        | None ->
            pvars_to_blocks_map
      in
      (id_to_pvar_map, pvars_to_blocks_map)
  | Load {id} ->
      (BlockIdMap.add id VDom.top id_to_pvar_map, pvars_to_blocks_map)
  | Call ((id, _), _, _, _, _) ->
      (BlockIdMap.add id VDom.top id_to_pvar_map, pvars_to_blocks_map)
  | _ ->
      (id_to_pvar_map, pvars_to_blocks_map)


module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate _ _node instr = eval_instr astate instr

  let pp_session_name node fmt =
    Format.fprintf fmt "Closure Subst Specialized Method %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let exec_instr proc_name (id_to_pvar_map, pvars_to_blocks_map) instr =
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
        match Option.bind ~f:VDom.get (BlockIdMap.find_opt id id_to_pvar_map) with
        | None ->
            [instr]
        | Some pvar -> (
          match
            BlockPvarSpecMap.find_opt (Pvar.get_name pvar) pvars_to_blocks_map
            |> Option.bind ~f:SpecDom.get
          with
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
              L.debug Capture Verbose "substituting specialized method@\n" ;
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


let analyze_at_node (map : Analyzer.invariant_map) node : Domain.t =
  match Analyzer.InvariantMap.find_opt (Procdesc.Node.get_id node) map with
  | Some abstate ->
      abstate.pre
  | None ->
      (BlockIdMap.top, BlockPvarSpecMap.top)


let process summary =
  let pdesc = Summary.get_proc_desc summary in
  let proc_name = Procdesc.get_proc_name pdesc in
  let proc_attributes = Procdesc.get_attributes pdesc in
  match proc_attributes.ProcAttributes.specialized_with_blocks_info with
  | Some spec_with_blocks_info -> (
    match AnalysisCallbacks.get_proc_desc spec_with_blocks_info.orig_proc with
    | Some orig_proc_desc ->
        let formals_to_blocks_map = spec_with_blocks_info.formals_to_procs_and_new_formals in
        Procdesc.shallow_copy_code_from_pdesc ~orig_pdesc:orig_proc_desc ~dest_pdesc:pdesc ;
        let pvars_to_blocks_map =
          Mangled.Map.map SpecDom.v formals_to_blocks_map
          |> Mangled.Map.to_seq |> BlockPvarSpecMap.of_seq
        in
        let node_cfg = CFG.from_pdesc pdesc in
        let invariant_map =
          Analyzer.exec_cfg node_cfg () ~initial:(BlockIdMap.empty, pvars_to_blocks_map)
        in
        let update_context = eval_instr in
        CFG.fold_nodes node_cfg ~init:() ~f:(fun _ node ->
            let used_ids = Instrs.instrs_get_normal_vars (CFG.instrs node) in
            Ident.update_name_generator used_ids ) ;
        let replace_instr _node = exec_instr proc_name in
        let context_at_node node = analyze_at_node invariant_map node in
        let _has_changed : bool =
          Procdesc.replace_instrs_by_using_context pdesc ~f:replace_instr ~update_context
            ~context_at_node
        in
        ()
    | _ ->
        () )
  | _ ->
      ()
