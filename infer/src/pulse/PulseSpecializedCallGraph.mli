(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val nodes_of : Procname.t -> PulseSummary.t -> Specialized_call_graph_t.node list
(** builds call graph nodes for the given procedure from its Pulse summary, one per specialization
*)

val to_json : Specialized_call_graph_t.call_graph -> string
(** serializes a call graph as a JSON string *)

val get_missed_captures :
     get_summary:(Procname.t -> PulseSummary.t option)
  -> SpecializedProcname.t list
  -> SpecializedProcname.Set.t Typ.Name.Map.t
(** traverses stored Pulse summaries and creates a map from missed types to the set of specialized
    procnames where the types are missing *)
