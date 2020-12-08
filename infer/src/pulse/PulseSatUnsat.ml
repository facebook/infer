(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t = Unsat | Sat of 'a

module Types = struct
  type nonrec 'a sat_unsat_t = 'a t = Unsat | Sat of 'a
end

let map f = function Unsat -> Unsat | Sat x -> Sat (f x)

let bind f = function Unsat -> Unsat | Sat x -> f x

module Import = struct
  include Types

  let ( >>| ) x f = map f x

  let ( >>= ) x f = bind f x

  let ( let+ ) x f = map f x

  let ( let* ) x f = bind f x
end
