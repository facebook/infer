(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Containers.Int
include Stdlib.Int

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp]
end

include T

module Infix = struct
  include Containers.Int.Infix
  include Int_compare

  external ( + ) : t -> t -> t = "%addint"
  external ( - ) : t -> t -> t = "%subint"
  external ( ~- ) : t -> t = "%negint"
  external ( * ) : t -> t -> t = "%mulint"
  external ( / ) : t -> t -> t = "%divint"

  let ( ** ) = pow

  external ( mod ) : t -> t -> t = "%modint"
  external ( land ) : t -> t -> t = "%andint"
  external ( lor ) : t -> t -> t = "%orint"
  external ( lxor ) : t -> t -> t = "%xorint"

  let lnot = lnot

  external ( lsl ) : t -> int -> t = "%lslint"
  external ( lsr ) : t -> int -> t = "%lsrint"
  external ( asr ) : t -> int -> t = "%asrint"
end

include Infix

let of_string = int_of_string_opt
let of_string_exn = int_of_string
let sign = Sign.of_int
let incr = incr
let decr = decr

module Set = NSSet.Make (T)
module Map = NSMap.Make (T)
