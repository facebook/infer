(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type HilConfig = sig
  val include_array_indexes : bool
end

module DefaultConfig : HilConfig = struct
  let include_array_indexes = false
end

let update_id_map hil_translation id_map =
  match (hil_translation : HilInstr.translation) with
  | Bind (id, access_path) ->
      IdAccessPathMapDomain.add id access_path id_map
  | Instr (ExitScope vars) ->
      List.fold ~f:(fun acc var -> IdAccessPathMapDomain.remove var acc) ~init:id_map vars
  | Instr _ | Ignore ->
      id_map


(** HIL adds a map from temporary variables to access paths to each domain *)
module MakeHILDomain (Domain : AbstractDomain.S) = struct
  include AbstractDomain.Pair (Domain) (IdAccessPathMapDomain)

  (** hides HIL implementation details *)
  let pp fmt (astate, id_map) =
    if Config.debug_level_analysis >= 3 then
      Format.fprintf fmt "Id map: @[<h>%a@]@\n" IdAccessPathMapDomain.pp id_map ;
    Domain.pp fmt astate
end

module Make (TransferFunctions : TransferFunctions.HIL) (HilConfig : HilConfig) = struct
  module CFG = TransferFunctions.CFG
  module Domain = MakeHILDomain (TransferFunctions.Domain)

  type extras = TransferFunctions.extras

  let pp_session_name = TransferFunctions.pp_session_name

  let is_java_unlock pname actuals =
    (* would check is_java, but we want to include builtins too *)
    (not (Typ.Procname.is_c_method pname))
    && match ConcurrencyModels.get_lock_effect pname actuals with Unlock _ -> true | _ -> false


  let exec_instr_actual extras id_map node hil_instr actual_state =
    match (hil_instr : HilInstr.t) with
    | Call (_, Direct callee_pname, actuals, _, loc) as hil_instr
      when is_java_unlock callee_pname actuals ->
        (* need to be careful not to move reads/writes out of a critical section due to odd
           temporaries introduced in our translation of try/synchronized in Java. to ensure this,
           "dump" all of the temporaries out of the id map, then execute the unlock instruction. *)
        let actual_state' =
          IdAccessPathMapDomain.fold
            (fun id access_expr astate_acc ->
              let lhs_access_path = HilExp.AccessExpression.base (id, Typ.mk Typ.Tvoid) in
              let dummy_assign =
                HilInstr.Assign (lhs_access_path, HilExp.AccessExpression access_expr, loc)
              in
              TransferFunctions.exec_instr astate_acc extras node dummy_assign )
            id_map actual_state
        in
        ( TransferFunctions.exec_instr actual_state' extras node hil_instr
        , IdAccessPathMapDomain.empty )
    | hil_instr ->
        (TransferFunctions.exec_instr actual_state extras node hil_instr, id_map)


  let exec_instr ((actual_state, id_map) as astate) extras node instr =
    let f_resolve_id id = IdAccessPathMapDomain.find_opt id id_map in
    let hil_translation =
      HilInstr.of_sil ~include_array_indexes:HilConfig.include_array_indexes ~f_resolve_id instr
    in
    let actual_state', id_map' =
      match hil_translation with
      | Instr hil_instr ->
          exec_instr_actual extras id_map node hil_instr actual_state
      | Bind _ | Ignore ->
          (actual_state, id_map)
    in
    let id_map' = update_id_map hil_translation id_map' in
    if phys_equal id_map id_map' && phys_equal actual_state actual_state' then astate
    else (actual_state', id_map')
end

module type S = sig
  module Interpreter : AbstractInterpreter.S

  type domain

  val compute_post :
    Interpreter.TransferFunctions.extras ProcData.t -> initial:domain -> domain option
end

module MakeAbstractInterpreterWithConfig
    (MakeAbstractInterpreter : AbstractInterpreter.Make)
    (HilConfig : HilConfig)
    (TransferFunctions : TransferFunctions.HIL) :
  S
  with type domain = TransferFunctions.Domain.t
   and module Interpreter = MakeAbstractInterpreter(Make(TransferFunctions)(HilConfig)) = struct
  module Interpreter = MakeAbstractInterpreter (Make (TransferFunctions) (HilConfig))

  type domain = TransferFunctions.Domain.t

  let compute_post ({ProcData.pdesc; tenv} as proc_data) ~initial =
    Preanal.do_preanalysis pdesc tenv ;
    let initial' = (initial, IdAccessPathMapDomain.empty) in
    let pp_instr (_, id_map) instr =
      let f_resolve_id id = IdAccessPathMapDomain.find_opt id id_map in
      let hil_translation =
        HilInstr.of_sil ~include_array_indexes:HilConfig.include_array_indexes ~f_resolve_id instr
      in
      match hil_translation with
      | Instr hil_instr ->
          Some (fun f -> Format.fprintf f "@[<h>%a@];@;" HilInstr.pp hil_instr)
      | Bind _ | Ignore ->
          None
    in
    Interpreter.compute_post ~pp_instr proc_data ~initial:initial' |> Option.map ~f:fst
end

module MakeAbstractInterpreter (TransferFunctions : TransferFunctions.HIL) =
  MakeAbstractInterpreterWithConfig (AbstractInterpreter.MakeRPO) (DefaultConfig)
    (TransferFunctions)
