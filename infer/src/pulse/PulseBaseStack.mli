(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface
   
module VarAddress : sig
  type t = Var.t

  val equal : t -> t -> bool
end

module AddrHistPair : sig
  type t = AbstractValue.t * ValueHistory.t

end
     
include
  PrettyPrintable.MonoMap with type key = VarAddress.t and type value = AddrHistPair.t


(* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
val compare : t -> t -> int [@@warning "-32"]

val pp : F.formatter -> t -> unit

val get_vars : t -> VarAddress.t list

val yojson_of_t : t -> Yojson.Safe.t

