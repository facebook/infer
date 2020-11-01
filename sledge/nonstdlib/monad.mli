(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Monad_intf

module Make (M : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) : S with type 'a t = 'a M.t

module State (State : sig
  type t
end) : sig
  include S with type 'a t = State.t -> 'a * State.t

  val run : 'a t -> State.t -> 'a * State.t
end
