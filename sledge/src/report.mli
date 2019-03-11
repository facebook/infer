(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

val unknown_call : Llair.term -> unit
val invalid_access_inst : Domain.t -> Llair.inst -> unit
val invalid_access_term : Domain.t -> Llair.term -> unit
