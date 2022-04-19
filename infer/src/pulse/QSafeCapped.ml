(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Z = ZSafe
include Q

type _q = Q.t = {num: Z.t; den: Z.t} [@@deriving yojson_of]

let yojson_of_t = [%yojson_of: _q]

let not_equal q1 q2 = not (Q.equal q1 q2)

let is_one q = Q.equal q Q.one

let is_minus_one q = Q.equal q Q.minus_one

let is_zero q = Q.equal q Q.zero

let is_not_zero q = not (is_zero q)

let to_int q = Z.protect Q.to_int q

let to_int32 q = Z.protect Q.to_int32 q

let to_int64 q = Z.protect Q.to_int64 q

let to_bigint q = Z.protect Q.to_bigint q

let to_nativeint q = Z.protect Q.to_nativeint q

(** cap certain operations to prevent numerators and denominators from growing too large (>128 bits
    on a 64-bit machine) *)
let cap q = if Int.(Z.size q.num > 2 || Z.size q.den > 2) then Q.undef else q

let mul q1 q2 =
  (* {!Q.mul} does not optimise these cases *)
  if is_one q1 then q2 else if is_one q2 then q1 else Q.mul q1 q2 |> cap


let div q1 q2 =
  (* {!Q.div} does not optimise these cases *)
  if is_one q2 then q1 else Q.div q1 q2


let is_rational q = match Q.classify q with ZERO | NZERO -> true | UNDEF | INF | MINF -> false
