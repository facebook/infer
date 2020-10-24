(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0

let pp = Q.pp_print
let hash = Poly.hash
let hash_fold_t s q = Int.hash_fold_t s (hash q)
let sexp_of_t q = Sexp.Atom (Q.to_string q)
let t_of_sexp = function Sexp.Atom s -> Q.of_string s | _ -> assert false
let of_z = Q.of_bigint

let pow q = function
  | 1 -> q
  | 0 -> Q.one
  | -1 -> Q.inv q
  | n ->
      let q, n = if n < 0 then (Q.inv q, -n) else (q, n) in
      Q.make (Z.pow (Q.num q) n) (Z.pow (Q.den q) n)

include Q
