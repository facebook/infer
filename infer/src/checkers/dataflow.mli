(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type throws =
  | DontKnow (** May or may not throw an exception. *)
  | Throws (** Definitely throws an exception. *)
  | DoesNotThrow (** Does not throw an exception. *)

(** Module type used to define the state component for a dataflow algorithm. *)
module type DFStateType = sig
  (** Type for state. *)
  type t

  (** Equality between states. *)
  val equal : t -> t -> bool

  (** Join two states (the old one is the first parameter). *)
  val join : t -> t -> t

  (** Perform a state transition on a node. *)
  val do_node : Tenv.t -> Procdesc.Node.t -> t -> (t list) * (t list)

  (** Can proc throw an exception? *)
  val proc_throws : Typ.Procname.t -> throws
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
  val run : Tenv.t -> Procdesc.t -> state -> (Procdesc.Node.t -> transition)
end

(** Functor to create an instance of a dataflow analysis. *)
module MakeDF(St: DFStateType) : DF with type state = St.t

val callback_test_dataflow : Callbacks.proc_callback_t
