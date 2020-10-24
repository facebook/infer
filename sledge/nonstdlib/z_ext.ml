(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0

let pp = Z.pp_print
let hash = [%hash: Z.t]
let hash_fold_t s z = Int.hash_fold_t s (hash z)
let sexp_of_t z = Sexp.Atom (Z.to_string z)
let t_of_sexp = function Sexp.Atom s -> Z.of_string s | _ -> assert false

(* the signed 1-bit integers are -1 and 0 *)
let true_ = Z.minus_one
let false_ = Z.zero
let of_bool = function true -> true_ | false -> false_
let is_true = Z.equal true_
let is_false = Z.equal false_

include Z
