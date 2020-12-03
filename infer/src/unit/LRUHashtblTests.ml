(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module LRUHash = LRUHashtbl.Make (Int)

let inputs =
  [ ("empty", (fun () -> LRUHash.create ~initial_size:5 ~max_size:3), [])
  ; ( "singleton"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        map )
    , [(0, 10)] )
  ; ( "LRU order"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        LRUHash.replace map 1 10 ;
        LRUHash.replace map 2 10 ;
        map )
    , [(2, 10); (1, 10); (0, 10)] )
  ; ( "LRU1"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        LRUHash.replace map 1 10 ;
        LRUHash.replace map 2 10 ;
        let (_ : int option) = LRUHash.find_opt map 1 in
        LRUHash.replace map 3 10 ;
        LRUHash.replace map 4 10 ;
        map )
    , [(4, 10); (3, 10); (1, 10)] )
  ; ( "LRU2"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        LRUHash.replace map 1 10 ;
        LRUHash.replace map 2 10 ;
        LRUHash.replace map 0 20 ;
        LRUHash.replace map 3 10 ;
        map )
    , [(3, 10); (0, 20); (2, 10)] )
  ; ( "remove"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        LRUHash.replace map 1 10 ;
        LRUHash.replace map 2 10 ;
        LRUHash.remove map 1 ;
        LRUHash.replace map 3 10 ;
        map )
    , [(3, 10); (2, 10); (0, 10)] )
  ; ( "clear"
    , (fun () ->
        let map = LRUHash.create ~initial_size:5 ~max_size:3 in
        LRUHash.replace map 0 10 ;
        LRUHash.replace map 1 10 ;
        LRUHash.replace map 2 10 ;
        LRUHash.clear map ;
        map )
    , [] ) ]


let tests =
  "LRUHashtble"
  >::: List.map inputs ~f:(fun (name, input, expected) ->
           name >:: fun _ -> assert_equal (input () |> LRUHash.bindings) expected )
