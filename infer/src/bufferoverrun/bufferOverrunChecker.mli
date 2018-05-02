(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val checker : Callbacks.proc_callback_t

module Summary : Summary.S with type payload = BufferOverrunDomain.Summary.t

module CFG = ProcCfg.NormalOneInstrPerNode

type invariant_map

val compute_invariant_map_and_check : Callbacks.proc_callback_args -> invariant_map * Specs.summary

val extract_pre : CFG.id -> invariant_map -> BufferOverrunDomain.Mem.t option
