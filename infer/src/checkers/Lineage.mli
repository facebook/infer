(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open LineageShape.StdModules

module PPNode : sig
  type t
end

module Local : sig
  type t
end

module Vertex : sig
  type t =
    | Local of (Local.t * PPNode.t)
    | Argument of int * FieldPath.t
    | ArgumentOf of Procname.t * int * FieldPath.t
    | Captured of int
    | CapturedBy of Procname.t * int
    | Return of FieldPath.t
    | ReturnOf of Procname.t * FieldPath.t
    | Self
    | Function of Procname.t

  val pp : t Fmt.t [@@warning "-unused-value-declaration"]
end

module G : Graph.Sig.P with type V.t = Vertex.t

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val graph : t -> G.t
end

module Out : sig
  val report_graph : Out_channel.t -> Procdesc.t -> G.t -> unit

  val report_summary : Procdesc.t -> Summary.t -> unit
end

val checker :
     (Summary.t option * LineageShape.Summary.t option) InterproceduralAnalysis.t
  -> LineageShape.Summary.t option
  -> Summary.t option
