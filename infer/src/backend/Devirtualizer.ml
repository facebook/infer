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
      gamma(VDom.v A) = { any ref of exact class A }
      gamma(VDom.bot) = emptyset
    ]} *)
module VDom = AbstractDomain.Flat (JavaClassName)

module CFG = ProcCfg.Normal
module Domain = AbstractDomain.SafeInvertedMap (Var) (VDom)

let get_var (astate : Domain.t) (v : Var.t) : VDom.t =
  match Domain.find_opt v astate with Some ab -> ab | None -> VDom.top


let rec eval_expr (astate : Domain.t) (expr : Exp.t) : VDom.t =
  match expr with
  | Var id ->
      get_var astate (Var.of_id id)
  | UnOp _ | BinOp _ | Exn _ | Closure _ ->
      VDom.top
  | Const _ ->
      VDom.top (* could be more precise for Cstr and Cclass *)
  | Cast (_, e) ->
      eval_expr astate e (* could be more precise for final class *)
  | Lvar v ->
      get_var astate (Var.of_pvar v)
  | Lfield _ ->
      VDom.top (* could be more precise for final class *)
  | Lindex _ ->
      VDom.top
  | Sizeof _ ->
      VDom.top


let eval_fun pname args =
  (* can be extended later if we decide to handle more builtins *)
  if Procname.equal pname BuiltinDecl.__new then
    match args with
    | (_, typ) :: _ when Typ.is_pointer typ -> (
      match Typ.name (Typ.strip_ptr typ) with Some (Typ.JavaClass cn) -> VDom.v cn | _ -> VDom.top )
    | _ ->
        VDom.top
  else VDom.top


let eval_instr (astate : Domain.t) (instr : Sil.instr) : Domain.t =
  match instr with
  | Load {id} when Ident.is_none id ->
      astate
  | Load {id; e} ->
      let aval = eval_expr astate e in
      Domain.add (Var.of_id id) aval astate
  | Call ((id, _), Const (Const.Cfun pname), args, _, _) ->
      let aval = eval_fun pname args in
      Domain.add (Var.of_id id) aval astate
  | Call ((id, _), _, _, _, _) ->
      Domain.add (Var.of_id id) VDom.top astate
  | Store {e1= Lvar pvar; e2} ->
      let aval = eval_expr astate e2 in
      Domain.add (Var.of_pvar pvar) aval astate
  | Store _ | Prune _ | Metadata _ ->
      astate


module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let exec_instr astate _ _node _ instr = eval_instr astate instr

  let pp_session_name node fmt =
    Format.fprintf fmt "devirtualizer analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let analyze_at_node (map : Analyzer.invariant_map) node : Domain.t =
  match Analyzer.InvariantMap.find_opt (Procdesc.Node.get_id node) map with
  | Some abstate ->
      abstate.pre
  | None ->
      Domain.top


(* inspired from biabduction/Symexec.ml, function resolve_method  *)
let resolve_method tenv class_name proc_name =
  let method_exists pname methods = List.exists ~f:(Procname.equal pname) methods in
  Tenv.resolve_method ~method_exists tenv class_name proc_name


let process pdesc tenv =
  let node_cfg = CFG.from_pdesc pdesc in
  let all_params = Procdesc.get_pvar_formals pdesc in
  let initial =
    (* all params -> top, bottom otherwise *)
    (* we could use param type (if final) to get more precision *)
    List.fold_left ~init:Domain.empty
      ~f:(fun acc (pvar, _) -> Domain.add (Var.of_pvar pvar) VDom.top acc)
      all_params
  in
  let map = Analyzer.exec_cfg node_cfg () ~initial in
  let is_virtual_call call_flags =
    call_flags.CallFlags.cf_virtual || call_flags.CallFlags.cf_interface
  in
  let replace_instr node (astate : Domain.t) (instr : Sil.instr) : Sil.instr =
    let kind = `ExecNode in
    let pp_name fmt = Format.pp_print_string fmt "devirtualizer" in
    NodePrinter.with_session (CFG.Node.underlying_node node) ~kind ~pp_name ~f:(fun () ->
        match instr with
        | Call
            ( ret_id_typ
            , Const (Const.Cfun callee_pname)
            , ((this_expr, _) :: _ as actual_params)
            , loc
            , call_flags )
          when is_virtual_call call_flags -> (
            L.d_printfln "virtual call %a " Procname.pp callee_pname ;
            let aval = eval_expr astate this_expr in
            match VDom.get aval with
            | Some dyn_typ -> (
              match resolve_method tenv (Typ.JavaClass dyn_typ) callee_pname with
              | None, _ ->
                  L.d_printfln "(unexpected: no resolved method found)" ;
                  instr
              | Some method_info, _missed_captures ->
                  (* note: missed captures are only tracked for Hack while this preanalysis is only performed on Java *)
                  let resolved_callee_pname = Tenv.MethodInfo.get_procname method_info in
                  let resolved_call_flags =
                    {call_flags with cf_virtual= false; cf_interface= false}
                  in
                  L.d_printfln "replaced by nonvirtual <%a>\n" Procname.pp resolved_callee_pname ;
                  Sil.Call
                    ( ret_id_typ
                    , Const (Const.Cfun resolved_callee_pname)
                    , actual_params
                    , loc
                    , resolved_call_flags ) )
            | _ ->
                Logging.debug Capture Verbose "unchanged\n" ;
                instr )
        | _ ->
            instr )
  in
  let update_context = eval_instr in
  let context_at_node node = analyze_at_node map node in
  let _has_changed : bool =
    Procdesc.replace_instrs_using_context pdesc ~f:replace_instr ~update_context ~context_at_node
  in
  ()
