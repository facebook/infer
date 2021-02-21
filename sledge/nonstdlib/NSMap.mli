(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of NSMap_intf

type ('key, +'a, 'compare_key) t [@@deriving compare, equal, sexp]
type ('compare_key, 'compare_a) compare [@@deriving compare, equal, sexp]

module Make (Key : sig
  type t [@@deriving compare, equal, sexp_of]
end) : S with type key = Key.t

module Make_from_Comparer (Key : sig
  type t [@@deriving equal, sexp_of]

  include Comparer.S with type t := t
end) :
  S
    with type key = Key.t
    with type compare_key = Key.compare
    with type 'compare_a compare = (Key.compare, 'compare_a) compare
    with type 'a t = (Key.t, 'a, Key.compare) t
