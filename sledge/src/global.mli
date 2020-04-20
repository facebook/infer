(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = private {reg: Reg.t; init: Exp.t option; typ: Typ.t; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

val pp : t pp
val pp_defn : t pp

include Invariant.S with type t := t

val mk : ?init:Exp.t -> Reg.t -> Typ.t -> Loc.t -> t
