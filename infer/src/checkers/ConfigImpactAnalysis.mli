(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module UncheckedCallee : sig
  type t

  val make_err_trace : t -> Errlog.loc_trace

  val pp_without_location_list : Format.formatter -> t list -> unit
end

module UncheckedCallees : sig
  include AbstractDomain.FiniteSetS with type elt = UncheckedCallee.t

  val encode : t -> string

  val decode : string -> t

  val pp_without_location : Format.formatter -> t -> unit
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val get_unchecked_callees : t -> UncheckedCallees.t
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
