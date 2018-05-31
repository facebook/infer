(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type throws =
  | DontKnow  (** May or may not throw an exception. *)
  | Throws  (** Definitely throws an exception. *)
  | DoesNotThrow  (** Does not throw an exception. *)

(** Module type used to define the state component for a dataflow algorithm. *)
module type DFStateType = sig
  (** Type for state. *)
  type t

  val equal : t -> t -> bool
  (** Equality between states. *)

  val join : t -> t -> t
  (** Join two states (the old one is the first parameter). *)

  val do_node : Tenv.t -> Procdesc.Node.t -> t -> t list * t list
  (** Perform a state transition on a node. *)

  val proc_throws : Typ.Procname.t -> throws
  (** Can proc throw an exception? *)
end

(** Type for the dataflow API. *)
module type DF = sig
  type t

  type state

  type transition = Dead_state | Transition of state * state list * state list

  val join : state list -> state -> state

  val run : Tenv.t -> Procdesc.t -> state -> Procdesc.Node.t -> transition
  (** Run the dataflow analysis on a procedure starting from the given state.
      Returns a function to lookup the results of the analysis on every node *)
end

(** Functor to create an instance of a dataflow analysis. *)
module MakeDF (St : DFStateType) : DF with type state = St.t
