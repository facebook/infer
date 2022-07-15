(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* An automaton is a different representation for a set of Topl properties: states and transitions
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
type vindex = int [@@deriving compare, equal]

(** from 0 to tcount()-1, inclusive *)
type tindex = int

type transition = {source: vindex; target: vindex; label: ToplAst.label option}

val make : ToplAst.t list -> t

val vcount : t -> int

val tcount : t -> int

val tfilter_mapi : t -> f:(tindex -> transition -> 'a option) -> 'a list

val registers : t -> ToplAst.register_name list

val message : t -> vindex -> string

val is_start : t -> vindex -> bool

val is_error : t -> vindex -> bool

val pp_transition : t -> Format.formatter -> transition -> unit

val pp_tindex : t -> Format.formatter -> tindex -> unit
