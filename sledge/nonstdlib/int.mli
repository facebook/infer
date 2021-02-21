(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of Containers.Int
include module type of Stdlib.Int

type t = int [@@deriving compare, equal, hash, sexp]

val of_string : string -> int option
val of_string_exn : string -> int
val sign : int -> Sign.t
val incr : int ref -> unit
val decr : int ref -> unit

module Infix : sig
  val ( -- ) : t -> t -> t iter
  val ( --^ ) : t -> t -> t iter

  include module type of NS0.Int_compare

  external ( + ) : t -> t -> t = "%addint"
  external ( - ) : t -> t -> t = "%subint"
  external ( ~- ) : t -> t = "%negint"
  external ( * ) : t -> t -> t = "%mulint"
  external ( / ) : t -> t -> t = "%divint"
  val ( ** ) : t -> t -> t
  external ( mod ) : t -> t -> t = "%modint"
  external ( land ) : t -> t -> t = "%andint"
  external ( lor ) : t -> t -> t = "%orint"
  external ( lxor ) : t -> t -> t = "%xorint"
  val lnot : t -> t
  external ( lsl ) : t -> t -> t = "%lslint"
  external ( lsr ) : t -> t -> t = "%lsrint"
  external ( asr ) : t -> t -> t = "%asrint"
end

include module type of Infix
module Set : NSSet.S with type elt = int
module Map : NSMap.S with type key = int
