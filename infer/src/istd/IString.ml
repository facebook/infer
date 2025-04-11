(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = String
module Map = PrettyPrintable.MakeHashSexpPPMap (T)
module Set = PrettyPrintable.MakeHashSexpPPSet (T)
module Hash = Stdlib.Hashtbl.Make (T)

module PairSet = PrettyPrintable.MakeHashSexpPPSet (struct
  type t = string * string [@@deriving compare, equal, sexp, hash]

  let pp fmt (x, y) = Format.fprintf fmt "(%s, %s)" x y
end)
