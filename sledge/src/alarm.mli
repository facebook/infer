(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind = Abort | Invalid_memory_access

type t =
  { kind: kind
  ; loc: Llair.Loc.t
  ; pp_action: Format.formatter -> unit
  ; pp_state: Format.formatter -> unit }

val pp : t pp
(** print an alarm for the user report *)

val pp_trace : t pp
(** print an error for the debug trace *)
