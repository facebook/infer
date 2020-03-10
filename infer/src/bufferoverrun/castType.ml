(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CFG = ProcCfg.Backward (ProcCfg.NormalOneInstrPerNode)
module F = Format

module Loc = struct
  type t = Ident of Ident.t | Pvar of Pvar.t [@@deriving compare]

  let pp f = function Ident id -> Ident.pp f id | Pvar pvar -> Pvar.pp Pp.text f pvar
end

module Val = struct
  include AbstractDomain.FiniteSet (struct
    include Typ

    let pp = pp Pp.text
  end)

  let is_integer_type =
    let is_integer_type = function
      | Typ.{desc= Tstruct (JavaClass name)} ->
          JavaClassName.(equal name java_lang_integer)
      | _ ->
          false
    in
    fun x -> (not (is_empty x)) && for_all is_integer_type x
end

module Dom = struct
  include AbstractDomain.Map (Loc) (Val)

  let lookup l m = Option.value (find_opt l m) ~default:Val.bottom

  let lookup_ident id m = lookup (Loc.Ident id) m

  let lookup_pvar pvar m = lookup (Loc.Pvar pvar) m

  let cast id typ m =
    let f = function None -> Some (Val.singleton typ) | Some prev -> Some (Val.add typ prev) in
    update (Loc.Ident id) f m


  let load id pvar m = add (Loc.Pvar pvar) (lookup_ident id m) m

  let store pvar id m = add (Loc.Ident id) (lookup_pvar pvar m) m
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Dom

  type extras = unit

  let exec_instr mem _pdata _node = function
    | Sil.Load {id; e= Exp.Lvar pvar} ->
        Dom.load id pvar mem
    | Sil.Store {e1= Exp.Lvar pvar; e2= Exp.Var id} ->
        Dom.store pvar id mem
    | Sil.Call (_, Const (Cfun callee_pname), (Exp.Var id, _) :: (Exp.Sizeof {typ}, _) :: _, _, _)
      when Procname.equal callee_pname BuiltinDecl.__cast ->
        Dom.cast id typ mem
    | _ ->
        mem


  let pp_session_name node f = F.fprintf f "Cast type analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let compute_invariant_map summary tenv =
  let pdata = ProcData.make summary tenv () in
  Analyzer.exec_pdesc ~do_narrowing:false ~initial:Dom.bottom pdata


type get_cast_type = Ident.t -> Val.t

let compute_get_cast_type =
  let cache_get, cache_set = Procname.UnitCache.create () in
  fun {Callbacks.exe_env; summary} ->
    let pname = Summary.get_proc_name summary in
    let casted_types =
      match cache_get pname with
      | Some casted_types ->
          casted_types
      | None ->
          let pdesc = Summary.get_proc_desc summary in
          let cfg = CFG.from_pdesc pdesc in
          let tenv = Exe_env.get_tenv exe_env pname in
          let inv_map = compute_invariant_map summary tenv in
          let exit_node_id = CFG.exit_node cfg |> CFG.Node.id in
          let casted_types =
            Analyzer.extract_post exit_node_id inv_map |> Option.value ~default:Dom.bottom
          in
          cache_set pname casted_types ; casted_types
    in
    fun id -> Dom.lookup_ident id casted_types
