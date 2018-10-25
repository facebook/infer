(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = unit -> int

let make : int -> t =
 fun init ->
  let num_ref = ref init in
  let get_num () =
    let v = !num_ref in
    num_ref := v + 1 ;
    v
  in
  get_num


let next : t -> int = fun counter -> counter ()
