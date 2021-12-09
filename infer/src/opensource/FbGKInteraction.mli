(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ConfigName : sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
end

val get_config : 'exp -> 'config_name option

val get_config_check : 'tenv -> 'pname -> 'args -> 'a option

val is_lazy_instance : 'pname -> bool

type known_expensiveness = KnownCheap | KnownExpensive

module ExpensivenessModel : sig
  val dispatcher : 'tenv -> 'pname -> 'args -> 'known_expensivene option
end

val action_message : string
