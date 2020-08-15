(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
     (PurityDomain.summary option * BufferOverrunAnalysisSummary.t option) InterproceduralAnalysis.t
  -> PurityDomain.summary option
