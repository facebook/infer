(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module DomainData = struct
  type t = {checked: bool; captured_in_closure: Procname.t option} [@@deriving compare]

  let pp fmt {checked; captured_in_closure} =
    F.fprintf fmt "checked=%b, captured_in_closure=%a" checked (Pp.option Procname.pp)
      captured_in_closure
end

module PVars = struct
  include PrettyPrintable.MakePPMonoMap (Mangled) (DomainData)

  let compare = compare DomainData.compare
end

module Vars = struct
  include PrettyPrintable.MakePPMonoMap (Ident) (Mangled)

  let compare = compare Mangled.compare
end

module Mem = struct
  type t = {vars: Vars.t; pvars: PVars.t} [@@deriving compare]

  let pp fmt {vars; pvars} = F.fprintf fmt "Vars= %a@.PVars= %a@." Vars.pp vars PVars.pp pvars

  let find_block_param id (astate : t) = Vars.find_opt id astate.vars

  let exec_null_check_id id ({vars; pvars} as astate : t) =
    match find_block_param id astate with
    | Some name ->
        let pvars = PVars.add name {checked= true; captured_in_closure= None} pvars in
        {vars; pvars}
    | None ->
        astate


  let load id pvar _ astate =
    let name = Pvar.get_name pvar in
    let vars = Vars.add id name astate.vars in
    {astate with vars}


  let call args astate =
    let update_captured_in_closure closure_pname astate (exp, _) =
      match exp with
      | Exp.Var id -> (
        match find_block_param id astate with
        | Some name ->
            let pvars =
              PVars.update name
                (fun data_opt ->
                  match data_opt with
                  | Some data ->
                      Some {data with captured_in_closure= Some closure_pname}
                  | None ->
                      Some {checked= false; captured_in_closure= Some closure_pname} )
                astate.pvars
            in
            {astate with pvars}
        | None ->
            astate )
      | _ ->
          astate
    in
    let process_arg astate (exp, _) =
      match exp with
      | Exp.Closure {name; captured_vars} ->
          List.fold ~f:(update_captured_in_closure name) captured_vars ~init:astate
      | _ ->
          astate
    in
    List.fold ~f:process_arg args ~init:astate
end

module Domain = struct
  include AbstractDomain.FiniteSet (Mem)

  let exec_null_check_id id astate = map (Mem.exec_null_check_id id) astate

  let load id pvar loc astate = map (Mem.load id pvar loc) astate

  let call args astate = map (Mem.call args) astate
end

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = Procdesc.t

  let pp_session_name _node fmt = F.pp_print_string fmt "ComputeCapturedInfo"

  let exec_instr (astate : Domain.t) _proc_desc _cfg_node _ (instr : Sil.instr) =
    match instr with
    | Load {id; e= Lvar pvar; loc} ->
        Domain.load id pvar loc astate
    | Prune (Var id, _, _, _) ->
        Domain.exec_null_check_id id astate
    (* If (block != nil) or equivalent else branch *)
    | Prune (BinOp (Binop.Ne, e1, e2), _, _, _)
    (* If (!(block == nil)) or equivalent else branch *)
    | Prune (UnOp (LNot, BinOp (Binop.Eq, e1, e2), _), _, _, _) -> (
      match (e1, e2) with
      | Var id, e when Exp.is_null_literal e ->
          Domain.exec_null_check_id id astate
      | e, Var id when Exp.is_null_literal e ->
          Domain.exec_null_check_id id astate
      | _ ->
          astate )
    | Call (_, Exp.Const (Const.Cfun _procname), args, _loc, _call_flags) ->
        Domain.call args astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let update_captured_in_closures cfg astate_opt =
  let process_mem {Mem.pvars} =
    let process_pvar pvar {DomainData.checked; captured_in_closure} =
      match captured_in_closure with
      | Some procname ->
          let proc_desc = Procname.Hash.find cfg procname in
          let attributes = Procdesc.get_attributes proc_desc in
          let update_captured captured_var =
            if Mangled.equal pvar (Pvar.get_name captured_var.CapturedVar.pvar) then
              {captured_var with CapturedVar.context_info= Some {is_checked_for_null= checked}}
            else captured_var
          in
          let captured = List.map ~f:update_captured attributes.ProcAttributes.captured in
          let attributes = {attributes with captured} in
          Procdesc.set_attributes proc_desc attributes
      | None ->
          ()
    in
    PVars.iter process_pvar pvars
  in
  match astate_opt with Some astate -> Domain.iter process_mem astate | None -> ()


let process cfg =
  let process cfg _ proc_desc =
    let initial = Domain.singleton {Mem.vars= Vars.empty; pvars= PVars.empty} in
    let astate = Analyzer.compute_post proc_desc ~initial proc_desc in
    update_captured_in_closures cfg astate
  in
  Procname.Hash.iter (process cfg) cfg
