(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of CCHashSet with module Make := CCHashSet.Make

module Make (E : ELEMENT) : sig
  include module type of CCHashSet.Make (E)

  val add : t -> elt -> bool
  (** [add s x] adds [x] into [s] and returns whether [s] was changed, that
      is, [add s x = not (mem s x)] *)

  val update : t -> elt -> f:(elt option -> elt option) -> unit
end
