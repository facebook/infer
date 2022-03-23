(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CFG = ProcCfg.Normal
module L = Logging

module BlockSpec = struct
  type t = ProcAttributes.passed_block [@@deriving compare, equal]

  let pp fmt passed_block =
    match passed_block with
    | ProcAttributes.Block (pname, _) ->
        Procname.pp fmt pname
    | ProcAttributes.Fields _ ->
        Format.fprintf fmt "In fields"
end

module SpecDom = AbstractDomain.Flat (BlockSpec)
module PvarBlockSpecMap = AbstractDomain.SafeInvertedMap (Mangled) (SpecDom)
module IdBlockSpecMap = AbstractDomain.SafeInvertedMap (Ident) (SpecDom)
module Domain = AbstractDomain.Pair (IdBlockSpecMap) (PvarBlockSpecMap)

let rec get_block (id_to_block_map, pvar_to_block_map) exp =
  match exp with
  | Exp.Lvar pvar ->
      PvarBlockSpecMap.find_opt (Pvar.get_name pvar) pvar_to_block_map
  | Exp.Var id ->
      IdBlockSpecMap.find_opt id id_to_block_map
  | Exp.Lfield (e, fieldname, _) -> (
      let open IOption.Let_syntax in
      get_block (id_to_block_map, pvar_to_block_map) e
      >>= SpecDom.get
      >>= function
      | ProcAttributes.Fields field_to_passed_block_map ->
          Fieldname.Map.find_opt fieldname field_to_passed_block_map >>| SpecDom.v
      | _ ->
          None )
  | _ ->
      None


let eval_instr ((id_to_block_map, pvar_to_block_map) as maps : Domain.t) instr =
  let open Sil in
  match instr with
  | Load {id; e} | Store {e1= Exp.Var id; e2= e} -> (
    (* if [e] is associated with a known block then [id] is associated with the same block *)
    match get_block maps e with
    | None ->
        maps
    | Some passed_block ->
        let id_to_block_map = IdBlockSpecMap.add id passed_block id_to_block_map in
        (id_to_block_map, pvar_to_block_map) )
  | Store {e1= Exp.Lvar pvar; e2} -> (
    (* if [e2] is associated with a known block then [e1] is associated with the same block *)
    match get_block maps e2 with
    | None ->
        maps
    | Some passed_block ->
        let pvar_to_block_map =
          PvarBlockSpecMap.add (Pvar.get_name pvar) passed_block pvar_to_block_map
        in
        (id_to_block_map, pvar_to_block_map) )
  | Call ((id, _), Exp.Const (Cfun callee_pname), (e, _) :: _, _, _) -> (
      (* if the function called is a getter and the gotten field is associated with a known block
         then [id] is associated with the same block *)
      let passed_block =
        let open IOption.Let_syntax in
        let* attributes = Attributes.load callee_pname in
        let* accessor = attributes.ProcAttributes.objc_accessor in
        match accessor with
        | ProcAttributes.Objc_getter (fieldname, typ, _) ->
            let field_exp = Exp.Lfield (e, fieldname, typ) in
            get_block maps field_exp
        | _ ->
            None
      in
      match passed_block with
      | None ->
          maps
      | Some passed_block ->
          let id_to_block_map = IdBlockSpecMap.add id passed_block id_to_block_map in
          (id_to_block_map, pvar_to_block_map) )
  | _ ->
      maps


module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate _ _node _ instr = eval_instr astate instr

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


let closure_of_exp pname maps loc exp load_instrs =
  (* Replace [exp] with the corresponding block as a closure if the block exists:
      [f] with [f = block] is transformed into
      [Closure(block, \[block_capt1; block_capt2\])]
     Additional load instructions are created to have captured variables live in the
     current memory
  *)
  let get_block maps exp : BlockSpec.t option =
    (* Find the block associated with the current expression if it exists *)
    match get_block maps exp with Some passed_block -> SpecDom.get passed_block | None -> None
  in
  let closure_of_block (block_name, block_formals) load_instrs =
    (* Given a block, create a closure encapsulating its pname and captured vars:
          [Closure(block, \[block_capt1; block_capt2\])]
       We also create additional load instructions for the captured variables so they
       are easily accessible when matching passed captures and expected captures
    *)
    let captured_vars, load_instrs =
      List.fold block_formals ~init:([], load_instrs)
        ~f:(fun (captured_vars, load_instrs) CapturedVar.{pvar; typ; capture_mode} ->
          let pvar = Pvar.mk (Pvar.get_name pvar) pname in
          let e = Exp.Lvar pvar in
          let id = Ident.create_fresh Ident.knormal in
          let load_instr = Sil.Load {id; e; root_typ= typ; typ; loc} in
          let captured_var = (Exp.Var id, pvar, typ, capture_mode) in
          (captured_var :: captured_vars, load_instr :: load_instrs) )
    in
    (* order of captured vars matters for correspondance with the closure calls *)
    let captured_vars = List.rev captured_vars in
    (Exp.Closure {name= block_name; captured_vars}, load_instrs)
  in
  let exp = exec_exp pname exp in
  match get_block maps exp with
  | Some (Block block) ->
      closure_of_block block load_instrs
  | _ ->
      (exp, load_instrs)


let exec_metadata pname metadata =
  let open Sil in
  match metadata with
  | Abstract _ | CatchEntry _ | EndBranches | Skip | TryEntry _ | TryExit _ ->
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


let exec_args proc_name block_maps loc args load_instrs =
  let updated = ref false in
  let load_instrs = ref load_instrs in
  (* Replace arguments corresponding to a block with a closure encapsulating
     the block's pname and captured vars:
      [foo(f1, arg, f2)] with [f1 = block1] and [f2= block2] is specialized into
      [foo(Closure(block1, \[block1_capt1\]), arg, Closure(block2, \[block2_capt1; block2_capt2\]))]
  *)
  let args' =
    List.map args ~f:(fun ((exp, typ) as exp_typ) ->
        let exp', load_instrs' = closure_of_exp proc_name block_maps loc exp !load_instrs in
        try_keep_original ~default:exp_typ exp exp' ~f:(fun exp' ->
            updated := true ;
            load_instrs := load_instrs' ;
            (exp', typ) ) )
  in
  let args = if !updated then args' else args in
  (args, !load_instrs)


let exec_instr proc_name block_maps instr =
  let open Sil in
  let res =
    match instr with
    | Load {id; e; root_typ; loc} ->
        [ try_keep_original ~default:instr e (exec_exp proc_name e) ~f:(fun e' ->
              Load {id; e= e'; root_typ; typ= root_typ; loc} ) ]
    | Store {e1; root_typ; typ; e2; loc} ->
        [ try_keep_original2 ~default:instr e1 (exec_exp proc_name e1) e2 (exec_exp proc_name e2)
            ~f:(fun e1' e2' -> Store {e1= e1'; root_typ; typ; e2= e2'; loc}) ]
    | Call (ret_id_typ, origin_call_exp, origin_args, loc, call_flags) ->
        (* Call instr specialization. We want to:
           - When the function called is a known block:
            Replace the function call with a closure call encapsulating the
            block's pname and captured vars

           - When the function called has a block as argument:
            Replace the argument with a closure encapsulating the block's pname and captured vars
        *)
        let call_exp, load_instrs = closure_of_exp proc_name block_maps loc origin_call_exp [] in
        let args, load_instrs = exec_args proc_name block_maps loc origin_args load_instrs in
        load_instrs
        @ [ try_keep_original2 ~default:instr origin_call_exp call_exp origin_args args
              ~f:(fun converted_call_exp converted_args ->
                L.debug Capture Verbose "substituting specialized method@\n" ;
                Call (ret_id_typ, converted_call_exp, converted_args, loc, call_flags) ) ]
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
      (IdBlockSpecMap.top, PvarBlockSpecMap.top)


let process pdesc =
  let proc_name = Procdesc.get_proc_name pdesc in
  let proc_attributes = Procdesc.get_attributes pdesc in
  match proc_attributes.ProcAttributes.specialized_with_blocks_info with
  | Some spec_with_blocks_info -> (
    match Procdesc.load spec_with_blocks_info.ProcAttributes.orig_proc with
    | Some orig_pdesc ->
        Procdesc.deep_copy_code_from_pdesc ~orig_pdesc ~dest_pdesc:pdesc ;
        let formals_to_blocks_map = spec_with_blocks_info.formals_to_blocks in
        let pvar_to_block_map =
          Mangled.Map.map SpecDom.v formals_to_blocks_map
          |> Mangled.Map.to_seq |> PvarBlockSpecMap.of_seq
        in
        let node_cfg = CFG.from_pdesc pdesc in
        let invariant_map =
          Analyzer.exec_cfg node_cfg () ~initial:(IdBlockSpecMap.empty, pvar_to_block_map)
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
    | None ->
        () )
  | _ ->
      ()
