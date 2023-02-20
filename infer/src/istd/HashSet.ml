(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

module type S = sig
  type elt

  type t

  val create : int -> t

  val singleton : elt -> t

  val add : elt -> t -> unit

  val remove : elt -> t -> unit

  val remove_all : elt Iter.t -> t -> unit

  val iter : t -> elt Iter.t

  val seq : t -> elt Seq.t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val length : t -> int
end

module Make (Key : Hashtbl.HashedType) : S with type elt = Key.t = struct
  open Hashtbl

  type elt = Key.t

  type nonrec t = (elt, unit) t

  let create n = create n

  let singleton x =
    let xs = create 1 in
    add xs x () ;
    xs


  let add x xs = replace xs x ()

  let remove x xs = remove xs x

  let iter xs f = iter (fun x _ -> f x) xs

  let seq xs = to_seq_keys xs

  let remove_all it xs = Iter.iter (fun y -> remove y xs) it

  let fold f = fold (fun x _ acc -> f x acc)

  let length = length
end
