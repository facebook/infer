(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Monad_intf

module Make (M : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) =
struct
  include M

  let map f m = bind m (fun a -> return (f a))
  let ap m n = bind m (fun f -> bind n (fun a -> return (f a)))
  let prod m n = bind m (fun a -> bind n (fun b -> return (a, b)))

  module Import = struct
    let ( |>= ) m f = map f m
    let ( >>= ) = bind
    let ( let* ) = bind
    let ( and* ) = prod
    let ( let+ ) = ( |>= )
    let ( and+ ) = prod
  end

  let bind m ~f = bind m f
  let map m ~f = map f m
end

module State (State : sig
  type t
end) =
struct
  include Make (struct
    type 'a t = State.t -> 'a * State.t

    let return a s = (a, s)

    let bind m k s =
      let a, s = m s in
      k a s
  end)

  let run m s = m s
end
