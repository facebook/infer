(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
     ( CostDomain.summary option
     * BufferOverrunAnalysisSummary.t option
     * PurityDomain.summary option )
     InterproceduralAnalysis.t
  -> CostDomain.summary option

val instantiate_cost :
     ?get_closure_callee_cost:(Procname.t -> CostDomain.BasicCostWithReason.t option)
  -> default_closure_cost:Ints.NonNegativeInt.t
  -> IntegerWidths.t
  -> inferbo_caller_mem:BufferOverrunDomain.Mem.t
  -> callee_pname:Procname.t
  -> callee_formals:(Pvar.t * Typ.t) list
  -> args:(Exp.t * Typ.t) list
  -> captured_vars:(Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
  -> callee_cost:CostDomain.BasicCostWithReason.t
  -> loc:Location.t
  -> CostDomain.BasicCostWithReason.t

val is_report_suppressed : Procname.t -> bool
