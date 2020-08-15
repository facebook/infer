(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** value domain, with the following concretization function [gamma]:

    {[
      gamma(VDom.top) = { any value }
      gamma(VDom.v Closure) = { a closure }
      gamma(VDom.bot) = emptyset
    ]} *)
module ExpClosure = struct
  type t = Exp.closure

  let pp fmt closure = Exp.pp fmt (Exp.Closure closure)

  let equal closure1 closure2 = Exp.equal (Exp.Closure closure1) (Exp.Closure closure2)
end

module VDom = AbstractDomain.Flat (ExpClosure)
module CFG = ProcCfg.Normal
module Domain = AbstractDomain.Map (Var) (VDom)

let get_var (astate : Domain.t) (v : Var.t) : VDom.t =
  match Domain.find_opt v astate with Some ab -> ab | None -> VDom.bottom


let rec eval_expr (astate : Domain.t) (expr : Exp.t) : VDom.t =
  match expr with
  | Var id ->
      get_var astate (Var.of_id id)
  | Closure c when Exp.is_objc_block_closure expr ->
      VDom.v c
  | Closure _ (* TODO: implement for C++ lambdas *) ->
      VDom.top
  | Cast (_, e) ->
      eval_expr astate e
  | Lvar v ->
      get_var astate (Var.of_pvar v)
  | UnOp _ | BinOp _ | Exn _ | Const _ | Lfield _ | Lindex _ | Sizeof _ ->
      VDom.top


let eval_instr (astate : Domain.t) (instr : Sil.instr) : Domain.t =
  match instr with
  | Load {id} when Ident.is_none id ->
      astate
  | Load {id; e} ->
      let aval = eval_expr astate e in
      Domain.add (Var.of_id id) aval astate
  | Store {e1= Lvar pvar; e2} ->
      let aval = eval_expr astate e2 in
      Domain.add (Var.of_pvar pvar) aval astate
  | Store _ | Prune _ | Metadata _ | Call _ ->
      astate


module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate _ _node instr = eval_instr astate instr

  let pp_session_name node fmt =
    Format.fprintf fmt "Closures Substitution %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let get_invariant_at_node (map : Analyzer.invariant_map) node : Domain.t =
  match Analyzer.InvariantMap.find_opt (Procdesc.Node.get_id node) map with
  | Some abstate ->
      abstate.pre
  | None ->
      Domain.bottom


let replace_instr node (astate : Domain.t) (instr : Sil.instr) : Sil.instr =
  let kind = `ExecNode in
  let pp_name fmt = Format.pp_print_string fmt "Closure Substitution" in
  NodePrinter.with_session (CFG.Node.underlying_node node) ~kind ~pp_name ~f:(fun () ->
      match instr with
      | Call (ret_id_typ, Var id, actual_params, loc, call_flags) -> (
          L.d_printfln "call  %a " (Sil.pp_instr Pp.text ~print_types:true) instr ;
          let aval = eval_expr astate (Var id) in
          match VDom.get aval with
          | None ->
              L.d_printfln "(no closure found)" ;
              instr
          | Some c ->
              L.d_printfln "found closure %a for variable %a\n" Exp.pp (Exp.Closure c) Ident.pp id ;
              let captured_values =
                List.map ~f:(fun (id_exp, _, typ, _) -> (id_exp, typ)) c.captured_vars
              in
              let actual_params = actual_params @ captured_values in
              let new_instr =
                Sil.Call (ret_id_typ, Const (Cfun c.name), actual_params, loc, call_flags)
              in
              L.d_printfln "replaced by call %a " (Sil.pp_instr Pp.text ~print_types:true) new_instr ;
              new_instr )
      | _ ->
          instr )


let process summary =
  let pdesc = Summary.get_proc_desc summary in
  let node_cfg = CFG.from_pdesc pdesc in
  let map = Analyzer.exec_cfg node_cfg ~initial:Domain.empty () in
  let update_context = eval_instr in
  let context_at_node node = get_invariant_at_node map node in
  let _has_changed : bool =
    Procdesc.replace_instrs_using_context pdesc ~f:replace_instr ~update_context ~context_at_node
  in
  ()
