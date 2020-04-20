(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Import0
include Core.Option

let pp fmt pp_elt fs = function
  | Some x -> Format.fprintf fs fmt pp_elt x
  | None -> ()

let or_else ~f = function None -> f () | o -> o
let cons xo xs = match xo with Some x -> x :: xs | None -> xs

module Monad_syntax = struct
  let ( let+ ) x f = map ~f x
  let ( and+ ) x y = both x y
  let ( let* ) x f = bind ~f x
  let ( and* ) x y = both x y
end
