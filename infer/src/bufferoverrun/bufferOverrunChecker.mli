(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker : Callbacks.proc_callback_t

module CFG = ProcCfg.NormalOneInstrPerNode

type invariant_map

val lookup_inv_map_cache : Callbacks.proc_callback_args -> Typ.Procname.t -> invariant_map

val compute_invariant_map_and_check : Callbacks.proc_callback_args -> invariant_map * Summary.t

val extract_pre : CFG.Node.id -> invariant_map -> BufferOverrunDomain.Mem.t option

val extract_post : CFG.Node.id -> invariant_map -> BufferOverrunDomain.Mem.t option
