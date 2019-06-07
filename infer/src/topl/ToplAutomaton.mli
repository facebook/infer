(*
 * Copyright (c) 2019-present, Facebook, Inc.
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

   NOTE: Now, the signature is the minimal needed for code instrumentation, but the implementation
   does some extra work (such as grouping transitions by their source) that will be useful for
   code generation. (TODO: remove note once code generation is implemented.)
*)

type t

type vindex = int

type tindex = int (* from 0 to tcount()-1, inclusive *)

type transition = {source: vindex; target: vindex; label: ToplAst.label}

val make : ToplAst.t list -> t

val transition : t -> tindex -> transition

val tcount : t -> int
