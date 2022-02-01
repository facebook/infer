(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Call : sig
  type t =
    { loc: Location.t
    ; pname: Procname.t
    ; node: ProcCfg.InstrNode.t
    ; args: (Exp.t * Typ.t) list
    ; ret: Ident.t * Typ.t }
  [@@deriving compare]

  val pp : Format.formatter -> t -> unit
end

type 'a interproc_analysis =
  (BufferOverrunAnalysisSummary.t option * 'a * CostDomain.summary option) InterproceduralAnalysis.t

type instantiated_cost = Cheap | NoModel | Symbolic of CostDomain.BasicCost.t

val get_cost_if_expensive : 'a interproc_analysis -> Call.t -> CostDomain.BasicCost.t option

val get_instantiated_cost : 'a interproc_analysis -> Call.t -> instantiated_cost option
