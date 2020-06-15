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
end) =
struct
  type state = State.t
  type 'a t = state -> 'a * state

  let return a s = (a, s)

  let bind k m s =
    let a, s = m s in
    k a s

  let run m s = m s

  module Import = struct
    let ( >>= ) m k = bind k m
    let ( let* ) m k = bind k m

    let ( and* ) : 'a t -> 'b t -> ('a * 'b) t =
     fun m n s ->
      let a, s = m s in
      let b, s = n s in
      return (a, b) s

    let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
     fun m f ->
      let* a = m in
      return (f a)

    let ( let+ ) = ( >>| )
    let ( and+ ) = ( and* )
  end
end
