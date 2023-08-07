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

type t

val live_before : Procdesc.Node.id -> t -> Domain.t option

val live_after : Procdesc.Node.id -> t -> Domain.t option

val compute : Procdesc.t -> t

module ExtendedDomain : AbstractDomain.WithBottom

module PreAnalysisTransferFunctions (CFG : ProcCfg.S) :
  AbstractInterpreter.TransferFunctions
    with module CFG = CFG
     and module Domain = ExtendedDomain
     and type analysis_data = Procdesc.t

val checker : IntraproceduralAnalysis.t -> unit

val is_always_in_scope : Procdesc.t -> Pvar.t -> bool
