(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t =
  | AbortProgram of PulseISLAbductiveDomain.t
      (** represents the state at the program point that caused an error *)
  | ContinueProgram of PulseISLAbductiveDomain.t  (** represents the state at the program point *)
  | ExitProgram of PulseISLAbductiveDomain.t
      (** represents the state originating at exit/divergence. *)

include AbstractDomain.NoJoin with type t := t

val continue : PulseISLAbductiveDomain.t -> t

val of_posts : Procdesc.t -> t list -> t list

val mk_initial : Procdesc.t -> t
