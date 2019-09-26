(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker : Callbacks.proc_callback_t

val instantiate_cost :
     Typ.IntegerWidths.t
  -> inferbo_caller_mem:BufferOverrunDomain.Mem.t
  -> callee_pname:Typ.Procname.t
  -> callee_formals:(Pvar.t * Typ.t) list
  -> params:(Exp.t * Typ.t) list
  -> callee_cost:CostDomain.BasicCost.t
  -> loc:Location.t
  -> CostDomain.BasicCost.t
