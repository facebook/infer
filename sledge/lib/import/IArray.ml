(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** IArray - Immutable view of an array *)

open Import0

(** = 'a array but covariant since imperative operations hidden *)
type +'a t

let v (a : 'a array) : 'a t = Obj.magic a
let a (v : 'a t) : 'a array = Obj.magic v
let _vl (al : 'a array list) : 'a t list = Obj.magic al
let al (vl : 'a t list) : 'a array list = Obj.magic vl
let compare cmp x y = Array.compare cmp (a x) (a y)
let equal cmp x y = Array.equal cmp (a x) (a y)
let hash_fold_t f s x = Hash.Builtin.hash_fold_array_frozen f s (a x)
let t_of_sexp a_of_sexp s = v (Array.t_of_sexp a_of_sexp s)
let sexp_of_t sexp_of_a x = Array.sexp_of_t sexp_of_a (a x)

module Import = struct
  type +'a iarray = 'a t [@@deriving compare, equal, hash, sexp]
end

let to_list x = Array.to_list (a x)
let to_array = a
let pp sep pp_elt fs v = List.pp sep pp_elt fs (to_list v)
let concat_map x ~f = v (Array.concat_map (a x) ~f:(fun y -> a (f y)))

let combine_adjacent ~f xs_v =
  let xs = a xs_v in
  let n = Array.length xs - 1 in
  let rec combine_adjacent_ j i xs =
    if i < n then (
      match f xs.(i - j) xs.(i + 1) with
      | None ->
          if j != 0 then xs.(i + 1 - j) <- xs.(i + 1) ;
          combine_adjacent_ j (i + 1) xs
      | Some x ->
          let xs = if j = 0 then Array.copy xs else xs in
          xs.(i - j) <- x ;
          combine_adjacent_ (j + 1) (i + 1) xs )
    else if j = 0 then xs
    else Array.sub xs ~pos:0 ~len:(n + 1 - j)
  in
  v (combine_adjacent_ 0 0 xs)

let create ~len x = v (Array.create ~len x)
let empty = v [||]

let contains_dup ~compare xs =
  Option.is_some
    (Array.find_consecutive_duplicate
       ~equal:(fun x y -> compare x y = 0)
       (Array.sorted_copy ~compare (a xs)))

let find x ~f = Array.find (a x) ~f
let find_exn x ~f = Array.find_exn (a x) ~f
let find_map x ~f = Array.find_map (a x) ~f
let fold x ~init ~f = Array.fold (a x) ~init ~f
let fold_right x ~f ~init = Array.fold_right (a x) ~f ~init
let fold_result x ~init ~f = Array.fold_result (a x) ~init ~f
let fold_until x ~init ~f ~finish = Array.fold_until (a x) ~init ~f ~finish
let fold2_exn x y ~init ~f = Array.fold2_exn (a x) (a y) ~init ~f
let exists x ~f = Array.exists (a x) ~f
let for_all x ~f = Array.for_all (a x) ~f
let for_all2_exn x y ~f = Array.for_all2_exn (a x) (a y) ~f
let filteri x ~f = v (Array.filteri (a x) ~f)

external get : 'a t -> int -> 'a = "%array_safe_get"

let last x = Array.last (a x)
let init n ~f = v (Array.init n ~f)
let is_empty x = Array.is_empty (a x)
let iter x ~f = Array.iter (a x) ~f
let rev_iter x ~f = Array.fold_right (a x) ~init:() ~f:(fun e () -> f e)
let iter2_exn x y ~f = Array.iter2_exn (a x) (a y) ~f
let iteri x ~f = Array.iteri (a x) ~f
let length x = Array.length (a x)
let map x ~f = v (Array.map (a x) ~f)

let map_preserving_phys_equal xs ~f =
  let change = ref false in
  let xs' =
    map xs ~f:(fun x ->
        let x' = f x in
        if not (x' == x) then change := true ;
        x' )
  in
  if !change then xs' else xs

let mapi x ~f = v (Array.mapi (a x) ~f)
let map2_exn x y ~f = v (Array.map2_exn (a x) (a y) ~f)
let map_inplace x ~f = Array.map_inplace (a x) ~f

let fold_map x ~init ~f =
  let s, x = Array.fold_map (a x) ~init ~f in
  (s, v x)

let fold_map_until xs ~init ~f ~finish =
  with_return (fun {return} ->
      finish
        (fold_map xs ~init ~f:(fun s x ->
             match (f s x : _ Continue_or_stop.t) with
             | Continue x -> x
             | Stop x -> return x )) )

let concat xs = v (Array.concat (al xs))
let copy x = v (Array.copy (a x))
let sub ~pos ~len x = v (Array.sub ~pos ~len (a x))
let subo ?pos ?len x = v (Array.subo ?pos ?len (a x))
let of_ x = v [|x|]
let of_array = v
let of_list x = v (Array.of_list x)
let of_list_rev x = v (Array.of_list_rev x)
let of_option x = v (Option.to_array x)
let reduce_exn x ~f = Array.reduce_exn (a x) ~f

let unzip x =
  let y, z = Array.unzip (a x) in
  (v y, v z)
