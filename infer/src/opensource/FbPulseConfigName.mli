(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving compare, equal]

val pp : Format.formatter -> t -> unit

val has_config_read : 'pulse_value_history -> bool

val of_string : config_type:string -> string -> t
