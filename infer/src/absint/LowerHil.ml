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

(** HIL adds a map from temporary variables to access paths to each domain *)
module MakeHILDomain (Domain : AbstractDomain.S) = struct
  include AbstractDomain.Pair (Domain) (Bindings)

  (** hides HIL implementation details *)
  let pp fmt (astate, id_map) =
    if Config.debug_level_analysis >= 3 then
      Format.fprintf fmt "Bindings: @[<h>%a@]@\n" Bindings.pp id_map ;
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


  let exec_instr_actual extras bindings node hil_instr actual_state =
    match (hil_instr : HilInstr.t) with
    | Call (_, Direct callee_pname, actuals, _, loc) as hil_instr
      when is_java_unlock callee_pname actuals ->
        (* need to be careful not to move reads/writes out of a critical section due to odd
           temporaries introduced in our translation of try/synchronized in Java. to ensure this,
           "dump" all of the temporaries out of the id map, then execute the unlock instruction. *)
        let actual_state' =
          Bindings.fold bindings ~init:actual_state ~f:(fun id access_expr astate_acc ->
              let lhs_access_path = HilExp.AccessExpression.base (id, Typ.mk Typ.Tvoid) in
              let dummy_assign =
                HilInstr.Assign (lhs_access_path, HilExp.AccessExpression access_expr, loc)
              in
              TransferFunctions.exec_instr astate_acc extras node dummy_assign )
        in
        (TransferFunctions.exec_instr actual_state' extras node hil_instr, Bindings.empty)
    | hil_instr ->
        (TransferFunctions.exec_instr actual_state extras node hil_instr, bindings)


  let append_bindings = IList.append_no_duplicates ~cmp:Var.compare |> Staged.unstage

  let hil_instr_of_sil bindings instr =
    let hil_translation =
      let f_resolve_id = Bindings.resolve bindings in
      HilInstr.of_sil ~include_array_indexes:HilConfig.include_array_indexes ~f_resolve_id instr
    in
    match hil_translation with
    | Ignore ->
        (None, bindings)
    | Bind (id, access_path) ->
        (None, Bindings.add id access_path bindings)
    | Instr (ExitScope (vars, loc)) ->
        let bindings, vars =
          List.fold vars ~init:(bindings, []) ~f:(fun (bindings, vars) var ->
              let bindings, vars' = Bindings.exit_scope var bindings in
              (bindings, append_bindings vars vars') )
        in
        let instr = if List.is_empty vars then None else Some (HilInstr.ExitScope (vars, loc)) in
        (instr, bindings)
    | Instr instr ->
        (Some instr, bindings)


  let exec_instr ((actual_state, bindings) as astate) extras node instr =
    let actual_state', bindings' =
      match hil_instr_of_sil bindings instr with
      | None, bindings ->
          (actual_state, bindings)
      | Some hil_instr, bindings ->
          exec_instr_actual extras bindings node hil_instr actual_state
    in
    if phys_equal bindings bindings' && phys_equal actual_state actual_state' then astate
    else (actual_state', bindings')
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
  module LowerHilInterpreter = Make (TransferFunctions) (HilConfig)
  module Interpreter = MakeAbstractInterpreter (LowerHilInterpreter)

  type domain = TransferFunctions.Domain.t

  let compute_post ({ProcData.pdesc; tenv} as proc_data) ~initial =
    Preanal.do_preanalysis pdesc tenv ;
    let initial' = (initial, Bindings.empty) in
    let pp_instr (_, bindings) instr =
      match LowerHilInterpreter.hil_instr_of_sil bindings instr with
      | Some hil_instr, _ ->
          Some (fun f -> Format.fprintf f "@[<h>%a@];@;" HilInstr.pp hil_instr)
      | None, _ ->
          None
    in
    Interpreter.compute_post ~pp_instr proc_data ~initial:initial' |> Option.map ~f:fst
end

module MakeAbstractInterpreter (TransferFunctions : TransferFunctions.HIL) =
  MakeAbstractInterpreterWithConfig (AbstractInterpreter.MakeRPO) (DefaultConfig)
    (TransferFunctions)
