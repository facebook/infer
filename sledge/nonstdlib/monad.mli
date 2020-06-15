(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  module Import : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end

module State (State : sig
  type t
end) : sig
  type state = State.t

  include S with type 'a t = state -> 'a * state

  val run : 'a t -> state -> 'a * state
end
