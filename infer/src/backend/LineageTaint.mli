(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! LineageShape.StdModules

module TaintConfig : sig
  type t

  val parse :
       lineage_source:string list
    -> lineage_sink:string list
    -> lineage_sanitizers:string list
    -> lineage_limit:int option
    -> t option
end

val export_result : name:string -> fileparts:string list -> 'a Fmt.t -> 'a -> unit

val report : TaintConfig.t -> IssueLog.t

include sig
  (** Used for tests*)

  [@@@warning "-unused-module"]

  [@@@warning "-unused-type-declaration"]

  [@@@warning "-unused-value-declaration"]

  module Private : sig
    module TaintConfig : sig
      module Endpoint : sig
        type node

        val pp_node : node Fmt.t

        type t = {procname: Procname.t; node: node}
      end

      val parse_endpoint : string -> Endpoint.t option
    end
  end
end
