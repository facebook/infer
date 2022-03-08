(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val make_specialized_call_exp :
     (PulseAbstractValue.t * 'a) ProcnameDispatcher.Call.FuncArg.t list
  -> Procname.t
  -> 'b InterproceduralAnalysis.t
  -> PulseAbductiveDomain.t
  -> (Procname.t * Exp.t) option
