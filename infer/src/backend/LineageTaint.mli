(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val report : lineage_source:string -> lineage_sink:string -> unit

include sig
  (** Used for tests*)

  [@@@warning "-unused-module"]

  [@@@warning "-unused-value-declaration"]

  module Private : sig
    val parse_node : string -> (Procname.t * Lineage.Vertex.t) option
  end
end
