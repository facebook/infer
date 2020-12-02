(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

type value = AbstractValue.t

type event = Call of {return: value option; arguments: value list}

type vertex = string

type register = string

type configuration = {vertex: vertex; memory: (register * value) list}

(** Let P be the [path_condition] in the enclosing pulse state, and let Q be the [path_condition] in
    the [simple_state] below. Then, the facts we know are P∧Q, and it should be that ∃V P∧Q,
    where V are the abstract values mentioned in the pre/post-configurations of the simple state, is
    equivalent to P. In other words, the facts in Q should not constrain program variables but may
    constrain Topl registers. *)
type simple_state =
  { pre: configuration  (** at the start of the procedure *)
  ; post: configuration  (** at the current program point *)
  ; path_condition: PathCondition.t }

(* TODO: include a hash of the automaton in a summary to avoid caching problems. *)
type state = simple_state list

let start () = (* TODO *) []

let small_step _condition _event state = (* TODO *) state

let large_step ~substitution:_ ~condition:_ ~callee_prepost:_ _state = (* TODO *) []

let pp_state _formatter _state = (* TODO *) ()
