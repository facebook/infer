(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = private
  {var: Var.t; init: (Exp.t * int) option; typ: Typ.t; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

val pp : t pp
val pp_defn : t pp

include Invariant.S with type t := t

val mk : ?init:Exp.t * int -> Var.t -> Typ.t -> Loc.t -> t
