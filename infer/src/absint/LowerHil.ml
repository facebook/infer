(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module type HilConfig = sig
  val include_array_indexes : bool
end

module DefaultConfig : HilConfig = struct
  let include_array_indexes = false
end

module Make
    (MakeTransferFunctions : TransferFunctions.MakeHIL)
    (HilConfig : HilConfig)
    (CFG : ProcCfg.S) =
struct
  module TransferFunctions = MakeTransferFunctions (CFG)
  module CFG = TransferFunctions.CFG
  module Domain = AbstractDomain.Pair (TransferFunctions.Domain) (IdAccessPathMapDomain)

  type extras = TransferFunctions.extras

  let pp_session_name = TransferFunctions.pp_session_name

  let pp_pre_post pre post hil_instr node =
    if Config.write_html then (
      let underyling_node = CFG.Node.underlying_node node in
      NodePrinter.start_session ~pp_name:(pp_session_name node) underyling_node ;
      let pp_post f =
        if phys_equal post pre then Format.pp_print_string f "= PRE"
        else TransferFunctions.Domain.pp f post
      in
      L.d_printfln_escaped "PRE: %a@.INSTR: %a@.POST: %t@." TransferFunctions.Domain.pp pre
        HilInstr.pp hil_instr pp_post ;
      NodePrinter.finish_session underyling_node )


  let is_java_unlock pname actuals =
    (* would check is_java, but we want to include builtins too *)
    (not (Typ.Procname.is_c_method pname))
    && match ConcurrencyModels.get_lock pname actuals with Unlock -> true | _ -> false


  let exec_instr ((actual_state, id_map) as astate) extras node instr =
    let f_resolve_id id = IdAccessPathMapDomain.find_opt id id_map in
    match
      HilInstr.of_sil ~include_array_indexes:HilConfig.include_array_indexes ~f_resolve_id instr
    with
    | Bind (id, access_path) ->
        let id_map' = IdAccessPathMapDomain.add id access_path id_map in
        if phys_equal id_map id_map' then astate else (actual_state, id_map')
    | Unbind ids ->
        let id_map' =
          List.fold ~f:(fun acc id -> IdAccessPathMapDomain.remove id acc) ~init:id_map ids
        in
        if phys_equal id_map id_map' then astate else (actual_state, id_map')
    | Instr (Call (_, Direct callee_pname, actuals, _, loc) as hil_instr)
      when is_java_unlock callee_pname actuals ->
        (* need to be careful not to move reads/writes out of a critical section due to odd
           temporaries introduced in our translation of try/synchronized in Java. to ensure this,
           "dump" all of the temporaries out of the id map, then execute the unlock instruction. *)
        let actual_state' =
          IdAccessPathMapDomain.fold
            (fun id access_expr astate_acc ->
              let lhs_access_path = AccessExpression.base (id, Typ.mk Typ.Tvoid) in
              let dummy_assign =
                HilInstr.Assign (lhs_access_path, HilExp.AccessExpression access_expr, loc)
              in
              TransferFunctions.exec_instr astate_acc extras node dummy_assign )
            id_map actual_state
        in
        let actual_state'' = TransferFunctions.exec_instr actual_state' extras node hil_instr in
        pp_pre_post actual_state actual_state'' hil_instr node ;
        (actual_state'', IdAccessPathMapDomain.empty)
    | Instr hil_instr ->
        let actual_state' = TransferFunctions.exec_instr actual_state extras node hil_instr in
        pp_pre_post actual_state actual_state' hil_instr node ;
        if phys_equal actual_state actual_state' then astate else (actual_state', id_map)
    | Ignore ->
        astate
end

module MakeAbstractInterpreterWithConfig
    (HilConfig : HilConfig)
    (CFG : ProcCfg.S)
    (MakeTransferFunctions : TransferFunctions.MakeHIL) =
struct
  module Interpreter = AbstractInterpreter.MakeRPO (Make (MakeTransferFunctions) (HilConfig) (CFG))

  let debug = AbstractInterpreter.DefaultNoExecInstr_UseFromLowerHilAbstractInterpreterOnly

  let compute_post ({ProcData.pdesc; tenv} as proc_data) ~initial =
    Preanal.do_preanalysis pdesc tenv ;
    let initial' = (initial, IdAccessPathMapDomain.empty) in
    Option.map ~f:fst (Interpreter.compute_post ~debug proc_data ~initial:initial')
end

module MakeAbstractInterpreter = MakeAbstractInterpreterWithConfig (DefaultConfig)
