(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module StringMap = Map.Make (String)

type ('r, 'a) user = {f: 'f. string -> ('r -> 'f) -> 'a} [@@unboxed]

type 'r elt = E : {name: string; get: 'r -> 'a} -> 'r elt

type 'r t = 'r elt list

type 'r sub = S : ('r, 'f) Field.t * 'f t -> 'r sub

let make ?(subfields = []) (map_poly : _ Field.user -> _) =
  let subfields =
    List.fold subfields ~init:StringMap.empty ~f:(fun acc (S (field, elts)) ->
        let name = Field.name field in
        let prefix = name ^ "." in
        let get = Field.get field in
        let elts =
          List.map elts ~f:(fun (E {name= subname; get= subget}) ->
              E {name= prefix ^ subname; get= (fun r -> get r |> subget)} )
        in
        StringMap.add_exn acc ~key:name ~data:elts )
  in
  let f field =
    let name = Field.name field in
    StringMap.find subfields name |> Option.value ~default:[E {name; get= Field.get field}]
  in
  E {name= "ALL"; get= Fn.id} :: List.concat (map_poly {f})


let map elts {f} = List.map elts ~f:(fun (E {name; get}) -> f name get)
