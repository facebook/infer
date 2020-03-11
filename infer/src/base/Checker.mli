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
  | BufferOverrun
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
  | ResourceLeak
  | SIOF
  | SelfInBlock
  | Starvation
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

type config =
  { support: Language.t -> support
  ; short_documentation: string
  ; cli_flag: string
        (** the flag to enable this option on the command line, without the leading "--" (like the
            [~long] argument of [CommandLineOption] functions) *)
  ; show_in_help: bool
  ; enabled_by_default: bool
  ; cli_deprecated_flags: string list
        (** more command-line flags, similar to [~deprecated] arguments *) }

val config : t -> config
