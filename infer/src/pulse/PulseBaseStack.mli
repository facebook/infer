(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface

module type S = sig
  include PrettyPrintable.PPMonoMap with type key = Var.t

  (* need to shadow the declaration in [MonoMap] even though it is unused since [Map.S.compare] has
     a different type *)
  val compare : t -> t -> int [@@warning "-unused-value-declaration"]

  val equal : t -> t -> bool
end

include S with type value = AbstractValue.t * ValueHistory.t

val yojson_of_t : t -> Yojson.Safe.t

val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t SatUnsat.t
(** replace each address in the stack by its canonical representative according to the current
    equality relation, represented by [get_var_repr] *)

val subst_var : AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t
