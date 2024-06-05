(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! LineageShape.StdModules

val report : lineage_source:string -> lineage_sink:string -> unit

include sig
  (** Used for tests*)

  [@@@warning "-unused-module"]

  [@@@warning "-unused-type-declaration"]

  [@@@warning "-unused-value-declaration"]

  module Private : sig
    module Todo : sig
      type node

      val pp_node : node Fmt.t

      type t = {procname: Procname.t; node: node}
    end

    val parse_node : string -> Todo.t option
  end
end
