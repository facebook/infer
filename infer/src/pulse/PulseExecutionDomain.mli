(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type exec_state =
  | AbortProgram of PulseAbductiveDomain.t
      (** represents the state at the program point that caused an error *)
  | ContinueProgram of PulseAbductiveDomain.t  (** represents the state at the program point *)
  | ExitProgram of PulseAbductiveDomain.t
      (** represents the state originating at exit/divergence. *)

include AbstractDomain.NoJoin with type t = exec_state

val continue : PulseAbductiveDomain.t -> t

val of_post : Procdesc.t -> t -> t

val mk_initial : Procdesc.t -> t
