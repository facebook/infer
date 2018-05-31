(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Vector - Immutable view of an array *)

open Base

(** = 'a array but covariant since imperative operations hidden *)
type +'a t

let v (a: 'a array) : 'a t = Caml.Obj.magic a

let a (v: 'a t) : 'a array = Caml.Obj.magic v

let compare cmp x y = Array.compare cmp (a x) (a y)

let t_of_sexp a_of_sexp s = v (Array.t_of_sexp a_of_sexp s)

let sexp_of_t sexp_of_a x = Array.sexp_of_t sexp_of_a (a x)

module Infix = struct
  type +'a vector = 'a t [@@deriving compare, sexp]
end

let create ~len x = v (Array.create ~len x)

let empty = v [||]

let find x ~f = Array.find (a x) ~f

let find_exn x ~f = Array.find_exn (a x) ~f

let fold x ~init ~f = Array.fold (a x) ~init ~f

let fold_right x ~f ~init = Array.fold_right (a x) ~f ~init

let for_all x ~f = Array.for_all (a x) ~f

let for_all2_exn x y ~f = Array.for_all2_exn (a x) (a y) ~f

external get : 'a t -> int -> 'a = "%array_safe_get"

let init n ~f = v (Array.init n ~f)

let is_empty x = Array.is_empty (a x)

let iter x ~f = Array.iter (a x) ~f

let rev_iter x ~f = Array.fold_right (a x) ~init:() ~f:(fun e () -> f e)

let iter2_exn x y ~f = Array.iter2_exn (a x) (a y) ~f

let iteri x ~f = Array.iteri (a x) ~f

let length x = Array.length (a x)

let map x ~f = v (Array.map (a x) ~f)

let mapi x ~f = v (Array.mapi (a x) ~f)

let slice x i j = v (Array.slice (a x) i j)

let of_array = v

let of_list x = v (Array.of_list x)

let of_list_rev x = v (Array.of_list_rev x)

let of_option x = v (Option.to_array x)

let to_list x = Array.to_list (a x)

let to_array = a
