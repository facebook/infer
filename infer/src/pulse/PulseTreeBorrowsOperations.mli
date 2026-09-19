(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val init_formals :
     (Pvar.t * Typ.t) list
  -> tree_borrows:Specialization.Pulse.TreeBorrows.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t

val exec_load :
     id:Ident.t
  -> e:Exp.t
  -> typ:Typ.t
  -> loc:Location.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t

val exec_store :
     lhs:Exp.t
  -> rhs:Exp.t
  -> typ:Typ.t
  -> loc:Location.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t

val exec_retag :
     dst_exp:Exp.t
  -> src_exp:Exp.t
  -> is_mut:bool
  -> loc:Location.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t

val compute_specialization :
     formals:(Pvar.t * Typ.t) list
  -> Exp.t list
  -> PulseAbductiveDomain.t
  -> Specialization.Pulse.t option

val graft_call :
     callee_summary:PulseAbductiveDomain.Summary.t
  -> callee_pname:Procname.t
  -> tb_arg_exps:Exp.t list
  -> subst_map:(PulseAbstractValue.t * PulseValueHistory.t) PulseAbstractValue.Map.t
  -> ret_id:Ident.t
  -> loc:Location.t
  -> caller:PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t

val report_errors : Procdesc.t -> Errlog.t -> PulseAbductiveDomain.Summary.summary -> unit
