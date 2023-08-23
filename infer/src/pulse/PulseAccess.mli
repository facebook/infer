(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type S = sig
  type key

  include PrettyPrintable.PrintableEquatableOrderedType with type t = key MemoryAccess.t

  val is_strong_access : Tenv.t -> t -> bool

  val canonicalize : get_var_repr:(PulseAbstractValue.t -> PulseAbstractValue.t) -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  module Set : Caml.Set.S with type elt = t
end

include S with type key := PulseAbstractValue.t
