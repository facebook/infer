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

type invariant_map

val compute_invariant_map : Callbacks.proc_callback_args -> invariant_map

val extract_post : invariant_map -> Procdesc.Node.t -> BufferOverrunDomain.Mem.astate option
