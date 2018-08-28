(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(*
  Lists with O(1) append and rev.
*)

include
  sig
    (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)

    type +'a t

    (* O(1) time and O(1) allocation *)

    val empty : 'a t

    val singleton : 'a -> 'a t

    val of_list : 'a list -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val snoc : 'a t -> 'a -> 'a t

    val append : 'a t -> 'a t -> 'a t

    val rev : 'a t -> 'a t

    val is_empty : 'a t -> bool

    val is_singleton : 'a t -> 'a option

    val is_singleton_or_more : 'a t -> 'a IContainer.singleton_or_more

    (* O(1) best to O(N) worst time and allocation. Do not use in a loop, use [fold] instead. *)

    val hd_tl_exn : 'a t -> 'a * 'a t

    val front_last_exn : 'a t -> 'a t * 'a

    (* O(1) best to O(N) worst time, no allocation *)

    val hd_exn : 'a t -> 'a

    val last_exn : 'a t -> 'a

    val hd : 'a t -> 'a option

    val last : 'a t -> 'a option

    (* Theta(N) time, 0 best to Theta(N) worst allocation *)

    val fold_left : ('a t, 'a, 'accum) Container.fold

    val fold_right : ('a t, 'a, 'accum) Container.fold

    val fold_unordered : ('a t, 'a, 'accum) Container.fold
    (** Always better than [fold_left] when you do not care about the order. *)
end[@@warning "-32"]
