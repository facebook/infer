(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Domain : sig
  include AbstractDomain.WithBottom

  val add : Var.t -> t -> t

  val fold : (Var.t -> 'a -> 'a) -> t -> 'a -> 'a

  val union : t -> t -> t

  val diff : t -> t -> t
end

module PreAnalysisTransferFunctions (CFG : ProcCfg.S) :
  TransferFunctions.SIL
    with module CFG = CFG
     and module Domain = Domain
     and type analysis_data = Procdesc.t

val checker : IntraproceduralAnalysis.t -> unit

val is_always_in_scope : Procdesc.t -> Pvar.t -> bool
