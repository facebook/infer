(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | AnnotationReachability
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ClassLoads
  | Cost
  | Eradicate
  | FragmentRetainsView
  | ImmutableCast
  | Impurity
  | InefficientKeysetIterator
  | Linters
  | LithoRequiredProps
  | Liveness
  | LoopHoisting
  | NullsafeDeprecated
  | PrintfArgs
  | Pulse
  | Purity
  | Quandary
  | RacerD
  | ResourceLeakLabExercise
  | SIOF
  | SelfInBlock
  | Starvation
  | TOPL
  | Uninit
[@@deriving equal, enumerate]

(** per-language support for each checker *)
type support =
  | NoSupport  (** checker does not run at all for this language *)
  | Support  (** checker is expected to give reasonable results *)
  | ExperimentalSupport  (** checker runs but is not expected to give reasonable results *)
  | ToySupport
      (** the checker is for teaching purposes only (like experimental but with no plans to improve
          it) *)

type cli_flags =
  { long: string
        (** The flag to enable this option on the command line, without the leading "--" (like the
            [~long] argument of {!CommandLineOption} functions). *)
  ; deprecated: string list
        (** More command-line flags, similar to [~deprecated] arguments in {!CommandLineOption}. *)
  ; show_in_help: bool }

type config =
  { name: string
  ; support: Language.t -> support
  ; short_documentation: string
  ; cli_flags: cli_flags option
        (** If [None] then the checker cannot be enabled/disabled from the command line. *)
  ; enabled_by_default: bool
  ; activates: t list  (** TODO doc *) }

val config : t -> config
