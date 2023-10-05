(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module CostModelEnv : sig
  type cost_model_env =
    { model_env: BufferOverrunUtils.ModelEnv.model_env
    ; get_summary: Procname.t -> CostDomain.summary option }
end

type model =
     CostModelEnv.cost_model_env
  -> ret:Ident.t * Typ.t
  -> BufferOverrunDomain.Mem.t
  -> CostDomain.BasicCost.t

val unit_cost_of : of_function:string -> Location.t -> CostDomain.BasicCost.t
(** Unit cost value *)

val cost_of_itv :
     itv:Itv.t
  -> degree_kind:Polynomials.DegreeKind.t
  -> of_function:string
  -> Location.t
  -> CostDomain.BasicCost.t
(** Translate interval to cost value *)

val string_len_range_itv :
     BufferOverrunUtils.ModelEnv.model_env
  -> Exp.t
  -> from:(Exp.t * IntegerWidths.t) option
  -> BufferOverrunDomain.Mem.t
  -> Itv.t
(** Given a string of length n and an optional starting index i (0 by default), return itv
    [0, n_u-i_l] *)

(** Module type for getting length of various data types *)
module type S = sig
  val length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
end

module Array : S

module Collection : S

module Container : S

module CString : S

module NSCollection : S
