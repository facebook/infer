(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

  let exec_instr (actual_state, id_map as astate) extras node instr =
    let f_resolve_id id =
      try Some (IdAccessPathMapDomain.find id id_map)
      with Not_found -> None
    in
    match
      HilInstr.of_sil ~include_array_indexes:HilConfig.include_array_indexes ~f_resolve_id instr
    with
    | Bind (id, access_path)
     -> let id_map' = IdAccessPathMapDomain.add id access_path id_map in
        if phys_equal id_map id_map' then astate else (actual_state, id_map')
    | Unbind ids
     -> let id_map' =
          List.fold ~f:(fun acc id -> IdAccessPathMapDomain.remove id acc) ~init:id_map ids
        in
        if phys_equal id_map id_map' then astate else (actual_state, id_map')
    | Instr hil_instr
     -> let actual_state' = TransferFunctions.exec_instr actual_state extras node hil_instr in
        ( if Config.write_html then
            let underyling_node = CFG.underlying_node node in
            NodePrinter.start_session underyling_node ;
            L.d_strln
              (Format.asprintf "PRE: %a@.INSTR: %a@.POST: %a@." TransferFunctions.Domain.pp
                 (fst astate) HilInstr.pp hil_instr TransferFunctions.Domain.pp actual_state') ;
            NodePrinter.finish_session underyling_node ) ;
        if phys_equal actual_state actual_state' then astate else (actual_state', id_map)
    | Ignore
     -> astate
end

module MakeDefault (MakeTransferFunctions : TransferFunctions.MakeHIL) (CFG : ProcCfg.S) =
  Make (MakeTransferFunctions) (DefaultConfig) (CFG)
