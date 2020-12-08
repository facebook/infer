(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Containers.Option
include Monad.Make (Containers.Option)

type 'a t = 'a option [@@deriving compare, equal, hash, sexp]

let pp fmt pp_elt fs = function
  | Some x -> Format.fprintf fs fmt pp_elt x
  | None -> ()

let map_or xo ~default ~f = map_or ~default f xo
let flat_map xo ~f = flat_map f xo
let iter xo ~f = iter f xo
let exists xo ~f = exists f xo
let for_all xo ~f = for_all f xo
let fold xo s ~f = fold (fun x s -> f s x) s xo
