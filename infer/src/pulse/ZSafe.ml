(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let protect f x = try Some (f x) with Division_by_zero | Invalid_argument _ | Z.Overflow -> None

let protect2 f x y =
  try Some (f x y) with Division_by_zero | Invalid_argument _ | Z.Overflow -> None


let yojson_of_t z = `String (Z.to_string z)

include Z

let div = protect2 Z.div

let rem = protect2 Z.rem

let div_rem = protect2 Z.div_rem

let cdiv = protect2 Z.cdiv

let fdiv = protect2 Z.fdiv

let ediv_rem = protect2 Z.ediv_rem

let ediv = protect2 Z.ediv

let erem = protect2 Z.erem

let divexact = protect2 Z.divexact

let gcd = protect2 Z.gcd

let gcdext = protect2 Z.gcdext

let lcm = protect2 Z.lcm

let powm = protect2 Z.powm

let powm_sec = protect2 Z.powm_sec

let invert = protect2 Z.invert

let ( / ) = protect2 Z.( / )

let ( /> ) = protect2 Z.( /> )

let ( /< ) = protect2 Z.( /< )

let ( /| ) = protect2 Z.( /| )

let ( mod ) = protect2 Z.( mod )
