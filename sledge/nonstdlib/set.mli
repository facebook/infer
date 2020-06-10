(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Set_intf

module Make (Elt : sig
  type t [@@deriving compare, sexp_of]
end) : S with type elt = Elt.t
