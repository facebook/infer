(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hashtbl = Caml.Hashtbl

module type S = sig
  type key

  type 'a t

  val create : initial_size:int -> max_size:int -> 'a t

  val find_opt : 'a t -> key -> 'a option

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val clear : 'a t -> unit

  val pp :
       pp_key:(F.formatter -> key -> unit)
    -> pp_v:(F.formatter -> 'a -> unit)
    -> F.formatter
    -> 'a t
    -> unit

  val bindings : 'a t -> (key * 'a) list
end

module Make (Key : Hashtbl.HashedType) = struct
  type key = Key.t

  module Hash = Hashtbl.Make (Key)

  module LRU = struct
    type t = {list: Key.t Doubly_linked.t; max: int}

    let create max =
      assert (max > 0) ;
      {list= Doubly_linked.create (); max}


    let insert_first {list; max} k =
      let new_node = Doubly_linked.insert_first list k in
      let removed_key =
        if Doubly_linked.length list > max then Doubly_linked.remove_last list else None
      in
      (new_node, removed_key)


    let use {list} n = Doubly_linked.move_to_front list n

    let remove {list} n = Doubly_linked.remove list n

    let clear {list} = Doubly_linked.clear list

    let to_list {list} = Doubly_linked.to_list list
  end

  type 'a t = {map: ('a * key Doubly_linked.Elt.t) Hash.t; lru: LRU.t}

  let create ~initial_size ~max_size = {map= Hash.create initial_size; lru= LRU.create max_size}

  let find_opt {map; lru} k =
    match Hash.find_opt map k with
    | None ->
        None
    | Some (v, e) ->
        LRU.use lru e ;
        Some v


  let replace {map; lru} k v =
    let n =
      match Hash.find_opt map k with
      | None ->
          let n, removed_key = LRU.insert_first lru k in
          Option.iter removed_key ~f:(Hash.remove map) ;
          n
      | Some (_, n) ->
          LRU.use lru n ;
          n
    in
    Hash.replace map k (v, n)


  let remove {map; lru} k =
    match Hash.find_opt map k with
    | None ->
        ()
    | Some (_, n) ->
        LRU.remove lru n ;
        Hash.remove map k


  let clear {map; lru} =
    Hash.clear map ;
    LRU.clear lru


  let pp ~pp_key ~pp_v f {map} =
    let is_first = ref true in
    let pp_key_v key (v, _node) =
      if !is_first then is_first := false else F.pp_print_string f ", " ;
      F.fprintf f "%a->%a" pp_key key pp_v v
    in
    F.pp_print_string f "{" ;
    Hash.iter pp_key_v map ;
    F.pp_print_string f "}"


  let bindings {map; lru} =
    LRU.to_list lru |> List.map ~f:(fun key -> (key, Hash.find map key |> fst))
end
