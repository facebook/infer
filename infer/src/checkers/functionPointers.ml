(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module ProcnameSet = AbstractDomain.FiniteSet (Procname)
module Domain = AbstractDomain.Map (String) (ProcnameSet)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate () _ = function
    | Sil.Load {id= lhs_id} when Ident.is_none lhs_id ->
        astate
    | Sil.Load {id= lhs_id; e= Exp.Lvar rhs_pvar; typ= Typ.{desc= Tptr ({desc= Tfun}, _)}} ->
        let fun_ptr =
          try Domain.find (Pvar.to_string rhs_pvar) astate
          with Caml.Not_found -> ProcnameSet.empty
        in
        Domain.add (Ident.to_string lhs_id) fun_ptr astate
    | Sil.Store {e1= Lvar lhs_pvar; e2= Exp.Const (Const.Cfun pn)} ->
        (* strong update *)
        Domain.add (Pvar.to_string lhs_pvar) (ProcnameSet.singleton pn) astate
    | Sil.Load _ | Store _ | Call _ | Prune _ | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "function pointers"
end

module CFG = ProcCfg.Normal
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

let find_procname var astate =
  match Domain.find_opt (Ident.to_string var) astate with
  | Some procnames -> (
    match ProcnameSet.is_singleton_or_more procnames with
    | IContainer.Empty ->
        None
    | IContainer.Singleton procname ->
        Some procname
    | IContainer.More ->
        Some (ProcnameSet.min_elt procnames)
        (* TODO: handle multiple procnames, e.g. with non-determinism branching *) )
  | None ->
      None


let substitute_expr astate expr =
  match expr with
  | Exp.Var var -> (
    match find_procname var astate with Some pname -> Exp.Const (Const.Cfun pname) | None -> expr )
  | _ ->
      expr


let substitute_arg astate arg =
  let expr, typ = arg in
  let expr' = substitute_expr astate expr in
  if phys_equal expr' expr then arg else (expr', typ)


let substitute_function_ptrs ~function_pointers node instr =
  match instr with
  | Sil.Call (ret, e_fun, args, loc, cfs) -> (
      let node_id = CFG.Node.id node in
      match Analyzer.extract_post node_id function_pointers with
      | None ->
          instr
      | Some astate ->
          let e_fun' = substitute_expr astate e_fun in
          let args' = IList.map_changed args ~equal:phys_equal ~f:(substitute_arg astate) in
          if phys_equal e_fun' e_fun && phys_equal args' args then instr
          else Sil.Call (ret, e_fun', args', loc, cfs) )
  | _ ->
      instr


let get_function_pointers proc_desc =
  let cfg = CFG.from_pdesc proc_desc in
  Analyzer.exec_cfg cfg () ~initial:Domain.empty


let substitute proc_desc =
  let function_pointers = get_function_pointers proc_desc in
  let f = substitute_function_ptrs ~function_pointers in
  ignore (Procdesc.replace_instrs proc_desc ~f)
