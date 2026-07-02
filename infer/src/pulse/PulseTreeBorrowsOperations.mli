(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain

val exec_load :
  id:Ident.t -> e:Exp.t -> typ:Typ.t -> loc:Location.t -> AbductiveDomain.t -> AbductiveDomain.t

val exec_store :
  lhs:Exp.t -> rhs:Exp.t -> typ:Typ.t -> loc:Location.t -> AbductiveDomain.t -> AbductiveDomain.t
