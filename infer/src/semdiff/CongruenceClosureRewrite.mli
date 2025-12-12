(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver

module Var : sig
  type t = private string [@@deriving compare, equal]

  val of_string : string -> t
end

module Header : sig
  type t = private string [@@deriving compare, equal]

  val of_string : string -> t
end

type subst

val pp_subst : CC.t -> F.formatter -> subst -> unit

module Pattern : sig
  type t = Var of Var.t | Term of {header: Header.t; args: t list}

  val pp : F.formatter -> t -> unit

  val e_match : ?debug:bool -> CC.t -> t -> CC.Atom.t -> subst list
end

module Rule : sig
  type t = {lhs: Pattern.t; rhs: Pattern.t}
end
