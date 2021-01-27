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

let try_keep_original ~default orig new_ ~f = if phys_equal orig new_ then default else f new_

let try_keep_original2 ~default orig1 new1 orig2 new2 ~f =
  if phys_equal orig1 new1 && phys_equal orig2 new2 then default else f new1 new2


let exec_pvar pname pvar = Pvar.swap_proc_in_local_pvar pvar pname

let exec_var pname var =
  let open Var in
  match var with
  | LogicalVar _ ->
      var
  | ProgramVar pvar ->
      try_keep_original ~default:var pvar (exec_pvar pname pvar) ~f:of_pvar


let rec exec_exp pname e =
  let open Exp in
  match e with
  | Var _ | Const _ ->
      e
  | UnOp (unop, e1, typ) ->
      try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> UnOp (unop, e1', typ))
  | BinOp (binop, e1, e2) ->
      try_keep_original2 ~default:e e1 (exec_exp pname e1) e2 (exec_exp pname e2) ~f:(fun e1' e2' ->
          BinOp (binop, e1', e2') )
  | Exn e1 ->
      try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Exn e1')
  | Closure {name; captured_vars} ->
      let updated = ref false in
      let captured_vars =
        List.map captured_vars ~f:(fun ((e, pvar, typ, captured_mode) as captured_var) ->
            try_keep_original2 ~default:captured_var e (exec_exp pname e) pvar
              (exec_pvar pname pvar) ~f:(fun e' pvar' ->
                updated := true ;
                (e', pvar', typ, captured_mode) ) )
      in
      if !updated then Closure {name; captured_vars} else e
  | Cast (typ, e1) ->
      try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Cast (typ, e1'))
  | Lvar pvar ->
      try_keep_original ~default:e pvar (exec_pvar pname pvar) ~f:(fun pvar' -> Lvar pvar')
  | Lfield (e1, fn, typ) ->
      try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Lfield (e1', fn, typ))
  | Lindex (e1, e2) ->
      try_keep_original2 ~default:e e1 (exec_exp pname e1) e2 (exec_exp pname e2) ~f:(fun e1' e2' ->
          Lindex (e1', e2') )
  | Sizeof {typ; nbytes; dynamic_length; subtype} ->
      Option.value_map dynamic_length ~default:e ~f:(fun dynamic_length ->
          try_keep_original ~default:e dynamic_length (exec_exp pname dynamic_length)
            ~f:(fun dynamic_length' ->
              Sizeof {typ; nbytes; dynamic_length= Some dynamic_length'; subtype} ) )


let exec_metadata pname metadata =
  let open Sil in
  match metadata with
  | Abstract _ | Skip ->
      metadata
  | ExitScope (vars, loc) ->
      let updated = ref false in
      let vars' =
        List.map vars ~f:(fun var ->
            try_keep_original ~default:var var (exec_var pname var) ~f:(fun var' ->
                updated := true ;
                var' ) )
      in
      if !updated then ExitScope (vars', loc) else metadata
  | Nullify (pvar, loc) ->
      try_keep_original ~default:metadata pvar (exec_pvar pname pvar) ~f:(fun pvar' ->
          Nullify (pvar', loc) )
  | VariableLifetimeBegins (pvar, typ, loc) ->
      try_keep_original ~default:metadata pvar (exec_pvar pname pvar) ~f:(fun pvar' ->
          VariableLifetimeBegins (pvar', typ, loc) )


let exec_args proc_name args =
  let updated = ref false in
  let args' =
    List.map
      ~f:(fun ((exp, typ) as exp_typ) ->
        try_keep_original ~default:exp_typ exp (exec_exp proc_name exp) ~f:(fun exp' ->
            updated := true ;
            (exp', typ) ) )
      args
  in
  if !updated then args' else args


let exec_instr proc_name (id_to_pvar_map, pvars_to_blocks_map) instr =
  let open Sil in
  let res =
    match instr with
    | Load {id; e; root_typ; loc} ->
        [ try_keep_original ~default:instr e (exec_exp proc_name e) ~f:(fun e' ->
              Load {id; e= e'; root_typ; typ= root_typ; loc} ) ]
    | Store {e1; root_typ; typ; e2; loc} ->
        [ try_keep_original2 ~default:instr e1 (exec_exp proc_name e1) e2 (exec_exp proc_name e2)
            ~f:(fun e1' e2' -> Store {e1= e1'; root_typ; typ; e2= e2'; loc}) ]
    | Call (ret_id_typ, Var id, origin_args, loc, call_flags) -> (
        let converted_args = exec_args proc_name origin_args in
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
        [ try_keep_original2 ~default:instr origin_call_exp (exec_exp proc_name origin_call_exp)
            origin_args (exec_args proc_name origin_args)
            ~f:(fun converted_call_exp converted_args ->
              Call (return_ids, converted_call_exp, converted_args, loc, call_flags) ) ]
    | Prune (origin_exp, loc, is_true_branch, if_kind) ->
        [ try_keep_original ~default:instr origin_exp (exec_exp proc_name origin_exp)
            ~f:(fun converted_exp -> Prune (converted_exp, loc, is_true_branch, if_kind)) ]
    | Metadata metadata ->
        [ try_keep_original ~default:instr metadata (exec_metadata proc_name metadata)
            ~f:(fun metadata' -> Metadata metadata') ]
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
