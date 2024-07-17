(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | AnnotationReachability
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ConfigImpactAnalysis
  | Cost
  | DisjunctiveDemo
  | FragmentRetainsView
  | Impurity
  | InefficientKeysetIterator
  | Lineage
  | LineageShape
  | LithoRequiredProps
  | Liveness
  | LoopHoisting
  | ParameterNotNullChecked
  | Pulse
  | PurityAnalysis
  | PurityChecker
  | RacerD
  | ResourceLeakLabExercise
  | SILValidation
  | SIOF
  | ScopeLeakage
  | SelfInBlock
  | Starvation
  | Topl
[@@deriving equal, enumerate]

(** per-language support for each checker *)
type support =
  | NoSupport  (** checker does not run at all for this language *)
  | ExperimentalSupport  (** checker runs but is not expected to give reasonable results *)
  | Support  (** checker is expected to give reasonable results *)

type cli_flags =
  { deprecated: string list
        (** More command-line flags, similar to [~deprecated] arguments in {!CommandLineOption}. *)
  ; show_in_help: bool }

type kind =
  | UserFacing of
      { title: string  (** the title of the documentation web page *)
      ; markdown_body: string  (** main text of the documentation *) }
      (** can report issues to users *)
  | UserFacingDeprecated of
      { title: string  (** the title of the documentation web page *)
      ; markdown_body: string  (** main text of the documentation *)
      ; deprecation_message: string }
      (** can report issues to users but should probably be deleted from infer *)
  | Internal
      (** Analysis that only serves other analyses. Do not use to mean experimental! Please still
          document experimental checkers as they will become non-experimental. *)
  | Exercise  (** reserved for the "resource leak" lab exercise *)

type config =
  { id: string
        (** Unique identifier. Used to generate web URLs for the documentation as well as the flag
            to enable this option on the command line. *)
  ; kind: kind
  ; support: Language.t -> support
  ; short_documentation: string  (** used in man pages and as a short intro on the website *)
  ; cli_flags: cli_flags option
        (** If [None] then the checker cannot be enabled/disabled from the command line. *)
  ; enabled_by_default: bool
  ; activates: t list  (** list of checkers that get enabled when this checker is enabled *) }

val config : t -> config

val get_id : t -> string
(** [get_id c] is [(config c).id] *)

val from_id : string -> t option

val pp_manual : F.formatter -> t -> unit
(** prints a short explanation of the checker; used for the man pages *)

module Set : PrettyPrintable.PPSet with type elt = t

val get_dependencies : t -> Set.t
