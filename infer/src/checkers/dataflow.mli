(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

type throws =
  | DontKnow (** May or may not throw an exception. *)
  | Throws (** Definitely throws an exception. *)
  | DoesNotThrow (** Does not throw an exception. *)

(** Module type used to define the state component for a dataflow algorithm. *)
module type DFStateType = sig
  type t (** Type for state. *)
  val equal : t -> t -> bool (** Equality between states. *)
  val join : t -> t -> t (** Join two states (the old one is the first parameter). *)

  (** Perform a state transition on a node. *)
  val do_node : Tenv.t -> Cfg.Node.t -> t -> (t list) * (t list)

  val proc_throws : Procname.t -> throws (** Can proc throw an exception? *)
end

(** Type for the dataflow API. *)
module type DF = sig
  type t
  type state
  type transition =
    | Dead_state
    | Transition of state * state list * state list
  val join : state list -> state -> state
  (** Run the dataflow analysis on a procedure starting from the given state.
      Returns a function to lookup the results of the analysis on every node *)
  val run : Tenv.t -> Cfg.Procdesc.t -> state -> (Cfg.Node.t -> transition)
end

(** Functor to create an instance of a dataflow analysis. *)
module MakeDF(St: DFStateType) : DF with type state = St.t

val callback_test_dataflow : Callbacks.proc_callback_t
