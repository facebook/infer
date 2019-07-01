(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

val unknown_call : Llair.term -> unit
val invalid_access_inst : State_domain.t -> Llair.inst -> unit
val invalid_access_term : State_domain.t -> Llair.term -> unit
