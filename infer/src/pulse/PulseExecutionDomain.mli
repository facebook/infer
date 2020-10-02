(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module LatentIssue = PulseLatentIssue

type t =
  | AbortProgram of AbductiveDomain.t
      (** represents the state at the program point that caused an error *)
  | ContinueProgram of AbductiveDomain.t  (** represents the state at the program point *)
  | ExitProgram of AbductiveDomain.t  (** represents the state originating at exit/divergence. *)
  | LatentAbortProgram of {astate: AbductiveDomain.t; latent_issue: LatentIssue.t}
      (** this path leads to an error but we don't have conclusive enough data to report it yet *)

include AbstractDomain.NoJoin with type t := t

val continue : AbductiveDomain.t -> t

val of_posts : Procdesc.t -> t list -> t list

val mk_initial : Procdesc.t -> t
