(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_config : is_param:'is_param -> 'exp -> 'config_name option

val get_config_check : is_param:'is_param -> 'tenv -> 'pname -> 'args -> 'a option

val is_lazy_instance : 'pname -> bool

type known_expensiveness = KnownCheap | KnownExpensive

module ExpensivenessModel : sig
  val dispatcher : 'tenv -> 'pname -> 'args -> 'known_expensivene option
end

val action_message : string
