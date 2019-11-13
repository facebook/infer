(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module VarSet : module type of AbstractDomain.FiniteSet (Var)

module Domain = VarSet

module PreAnalysisTransferFunctions (CFG : ProcCfg.S) :
  TransferFunctions.SIL
    with module CFG = CFG
     and module Domain = Domain
     and type extras = ProcData.no_extras

val checker : Callbacks.proc_callback_args -> Summary.t
