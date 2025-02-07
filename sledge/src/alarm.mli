(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind = Abort | Invalid_memory_access
type t

val kind : t -> kind

val v : kind -> Llair.Loc.t -> 'a pp -> 'a -> 's pp -> 's -> t
(** [v kind location pp_action action pp_state state] is a [kind] alarm
    triggered at [location] by [action] (which can be printed by
    [pp_action]) on [state] (which can be printed by [pp_state]). *)

val pp : t pp
(** print an alarm for the user report *)

val pp_trace : t pp
(** print an error for the debug trace *)
