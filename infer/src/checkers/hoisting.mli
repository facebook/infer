(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
     ( BufferOverrunAnalysisSummary.t option
     * PurityDomain.ModifiedParamIndices.t AbstractDomain.Types.top_lifted option
     * CostDomain.summary option )
     InterproceduralAnalysis.t
  -> unit
