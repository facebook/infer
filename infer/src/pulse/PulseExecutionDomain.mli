(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module LatentIssue = PulseLatentIssue

type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t  (** represents the state at the program point *)
  | ExitProgram of AbductiveDomain.summary
      (** represents the state originating at exit/divergence. *)
  | AbortProgram of AbductiveDomain.summary
      (** represents the state at the program point that caused an error *)
  | LatentAbortProgram of {astate: AbductiveDomain.summary; latent_issue: LatentIssue.t}
      (** this path leads to an error but we don't have conclusive enough data to report it yet *)
  | ISLLatentMemoryError of 'abductive_domain_t
      (** represents the state at the program point that might cause an error; used for
          {!Config.pulse_isl} *)

type t = AbductiveDomain.t base_t

include AbstractDomain.NoJoin with type t := t

val continue : AbductiveDomain.t -> t

val mk_initial : Tenv.t -> Procdesc.t -> t

val is_unsat_cheap : t -> bool
(** see {!PulsePathCondition.is_unsat_cheap} *)

type summary = AbductiveDomain.summary base_t [@@deriving compare, equal, yojson_of]

val summary_of_posts : Tenv.t -> Procdesc.t -> t list -> summary list

val force_exit_program : Tenv.t -> Procdesc.t -> t -> t option
