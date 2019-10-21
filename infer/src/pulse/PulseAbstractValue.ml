(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = int [@@deriving compare]

let equal = [%compare.equal: t]

let next_fresh = ref 1

let mk_fresh () =
  let l = !next_fresh in
  incr next_fresh ; l


let pp f l = F.fprintf f "v%d" l

let init () = next_fresh := 1

type state = int

let get_state () = !next_fresh

let set_state counter = next_fresh := counter

module PPKey = struct
  type nonrec t = t

  let compare = compare

  let pp = pp
end

module Set = PrettyPrintable.MakePPSet (PPKey)
module Map = PrettyPrintable.MakePPMap (PPKey)
