(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

open! NS

type t = {name: Global.t; init: (LlairExp.t * LlairTyp.t) option; loc: LairLoc.t}
[@@deriving compare, equal, sexp]

val pp : t pp

include Invariant.S with type t := t

val mk : ?init:LlairExp.t * LlairTyp.t -> Global.t -> LairLoc.t -> t
