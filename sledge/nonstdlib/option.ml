(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Containers.Option

type 'a t = 'a option [@@deriving compare, equal, hash, sexp]

let pp fmt pp_elt fs = function
  | Some x -> Format.fprintf fs fmt pp_elt x
  | None -> ()

let map xo ~f = map f xo
let map_or xo ~default ~f = map_or ~default f xo
let bind xo ~f = bind xo f
let iter xo ~f = iter f xo
let exists xo ~f = exists f xo
let for_all xo ~f = for_all f xo
let fold xo ~init ~f = fold f init xo
