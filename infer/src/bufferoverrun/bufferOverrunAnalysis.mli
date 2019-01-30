(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CFG = ProcCfg.NormalOneInstrPerNode

module Payload : SummaryPayload.S with type t = BufferOverrunSummary.t

type invariant_map

type local_decls = AbsLoc.PowLoc.t

val cached_compute_invariant_map : Procdesc.t -> Tenv.t -> Typ.IntegerWidths.t -> invariant_map

val compute_summary : local_decls -> CFG.t -> invariant_map -> BufferOverrunSummary.memory

val extract_pre : CFG.Node.id -> invariant_map -> BufferOverrunDomain.Mem.t option

val extract_post : CFG.Node.id -> invariant_map -> BufferOverrunDomain.Mem.t option

val extract_state :
  CFG.Node.id -> invariant_map -> BufferOverrunDomain.Mem.t AbstractInterpreter.State.t option

val get_local_decls : Procdesc.t -> local_decls
