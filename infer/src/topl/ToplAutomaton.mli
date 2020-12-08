(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* An automaton is a different representation for a set of TOPL properties: states and transitions
   are identified by nonnegative integers; and transitions are grouped by their source. Also, the
   meaning of transition labels does not depend on context (e.g., prefixes are now included).

   We identify states by integers because biabduction tracks integers well; for example, equality
   checks on integers are obvious, we don't need to worry about whether we should be using an
   equals() method.

   We identify transitions by integers because, in the monitor code that we generate, we use a
   boolean variable transitionN to tell if the static part of a transition guard is satisfied. The N
   is just some identifier for the transition, and integers are convenient identifiers.

   Transitions are grouped by their source to ease generation of the monitor code.
*)
type t

(** from 0 to vcount()-1, inclusive *)
type vindex = int [@@deriving compare]

(** from 0 to tcount()-1, inclusive *)
type tindex = int

type transition = {source: vindex; target: vindex; label: ToplAst.label option}

val make : ToplAst.t list -> t

val outgoing : t -> vindex -> tindex list

val is_nondet : t -> vindex -> bool

val vcount : t -> int

val transition : t -> tindex -> transition

val tfilter_map : t -> f:(transition -> 'a option) -> 'a list

val is_skip : t -> tindex -> bool
(** A transition is *skip* when it has no action, its guard is implied by all other guards, and its
    target equals its source. [is_skip automaton t] returns true when it can prove that [t] is skip.*)

val tcount : t -> int

val max_args : t -> int

val get_start_error_pairs : t -> (vindex * vindex) list
(** Return pairs [(i,j)] of vertex indices corresponding to pairs [((p, "start"), (p, "error"))] of
    vertex names, where [p] ranges over property names. POST: no vertex index occurs more than once
    in the result. *)

val registers : t -> ToplAst.register_name list

val pp_message_of_state : Format.formatter -> t * tindex -> unit
(** Print "property P reaches state E". *)

val is_start : t -> vindex -> bool

val is_error : t -> vindex -> bool

val pp_transition : Format.formatter -> transition -> unit
