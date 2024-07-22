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

  val pp : t Fmt.t [@@warning "-unused-value-declaration"]
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

module Edge : sig
  module Kind : sig
    type t =
      | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
      | Call  (** Target is ArgumentOf *)
      | Return  (** Source is ReturnOf *)
      | Capture  (** [X=1, F=fun()->X end] has Capture edge from X to F *)
      | Builtin  (** Edge coming from a suppressed builtin call, ultimately exported as a Copy *)
      | Summary of {callee: Procname.t; shape_is_preserved: bool}
          (** Summarizes the effect of a procedure call *)
      | DynamicCallFunction
      | DynamicCallModule
  end

  type kind = Kind.t

  type t

  val pp : t Fmt.t [@@warning "-unused-value-declaration"]

  val kind : t -> kind

  val location : t -> Location.t

  val procname : t -> Procname.t
end

module G : sig
  include Graph.Sig.P with type V.t = Vertex.t and type E.label = Edge.t

  val pp : t Fmt.t [@@warning "-unused-value-declaration"]

  val of_vertices : vertex list -> t
end

module Unified : sig
  module UVertex : sig
    type v =
      | Local of Local.t * PPNode.t
      | Argument of int * FieldPath.t
      | Return of FieldPath.t
      | Captured of int
      | Function
    [@@deriving sexp, compare, equal, hash]

    type t = {procname: Procname.t; vertex: v} [@@deriving sexp, compare, equal, hash]

    val pp : t Fmt.t [@@warning "-unused-value-declaration"]
  end

  module LocalG := G

  module G : Graph.Sig.P with type V.t = UVertex.t and type E.label = Edge.t

  val transform_e :
    (Procname.t -> LineageShape.Summary.t option) -> Procname.t -> LocalG.edge -> G.edge list

  module Dot : sig
    val pp : G.t Fmt.t
  end
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val graph : t -> G.t
end

module Out : sig
  val report_summary : Procdesc.t -> Summary.t -> unit
end

val checker :
     (Summary.t option * LineageShape.Summary.t option) InterproceduralAnalysis.t
  -> LineageShape.Summary.t option
  -> Summary.t option
