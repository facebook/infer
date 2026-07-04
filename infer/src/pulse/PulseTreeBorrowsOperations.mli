(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

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
  dst_exp:Exp.t -> src_exp:Exp.t -> is_mut:bool -> PulseAbductiveDomain.t -> PulseAbductiveDomain.t
