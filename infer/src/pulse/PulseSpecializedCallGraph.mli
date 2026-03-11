(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type location

type name

type callee = {callee_name: name; callee_context: int; call_location: location}

type caller = {caller_name: name; caller_context: int}

type node = {caller: caller; callees: callee list}

type specialization = string

(** Readable version of the call graph explored by Pulse during analysis. The graph is
    context-sensitive because of specialization: the same procedure can be analyzed in different
    contexts and may not have the same callees depending on its context. The meaning of context [i]
    can be found in the [i]-th element of the [contexts] field. Context [0] is the default (no
    specialization). *)
type call_graph = {nodes: node list; contexts: specialization list}

module JsonBuilder : sig
  type t

  val make : unit -> t
  (** Create a new builder. *)

  val add : t -> Procname.t -> PulseSummary.t option -> t
  (** Add call graph edges for a caller based on its summary. *)

  val finalize : t -> call_graph
  (** Close the builder and return the resulting call graph. *)
end

val to_json : call_graph -> string
(** Serialize a call graph to a JSON string. *)

val get_missed_captures :
     get_summary:(Procname.t -> PulseSummary.t option)
  -> SpecializedProcname.t list
  -> SpecializedProcname.Set.t Typ.Name.Map.t
(** Traverse Pulse summaries reachable from the given entry points and return a map from missed
    capture types to the set of specialized procnames that reference them. *)
