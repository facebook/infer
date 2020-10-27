(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Formulas *)

module Prop = Propositional.Make (Trm)
module Set = Prop.Fmls

type set = Set.t

include Prop.Fml

let tt = mk_Tt ()
let ff = _Not tt
let bool b = if b then tt else ff

let _Eq0 x =
  match (x : Trm.t) with
  | Z z -> bool (Z.equal Z.zero z)
  | Q q -> bool (Q.equal Q.zero q)
  | x -> _Eq0 x

let _Pos x =
  match (x : Trm.t) with
  | Z z -> bool (Z.gt z Z.zero)
  | Q q -> bool (Q.gt q Q.zero)
  | x -> _Pos x

let _Eq x y =
  if x == Trm.zero then _Eq0 y
  else if y == Trm.zero then _Eq0 x
  else
    let sort_eq x y =
      match Sign.of_int (Trm.compare x y) with
      | Neg -> _Eq x y
      | Zero -> tt
      | Pos -> _Eq y x
    in
    match (x, y) with
    (* x = y ==> 0 = x - y when x = y is an arithmetic equality *)
    | (Z _ | Q _ | Arith _), _ | _, (Z _ | Q _ | Arith _) ->
        _Eq0 (Trm.sub x y)
    (* α^β^δ = α^γ^δ ==> β = γ *)
    | Concat a, Concat b ->
        let m = Array.length a in
        let n = Array.length b in
        let l = min m n in
        let length_common_prefix =
          let rec find_lcp i =
            if i < l && Trm.equal a.(i) b.(i) then find_lcp (i + 1) else i
          in
          find_lcp 0
        in
        if length_common_prefix = l then tt
        else
          let length_common_suffix =
            let rec find_lcs i =
              if Trm.equal a.(m - 1 - i) b.(n - 1 - i) then find_lcs (i + 1)
              else i
            in
            find_lcs 0
          in
          let length_common = length_common_prefix + length_common_suffix in
          if length_common = 0 then sort_eq x y
          else
            let pos = length_common_prefix in
            let a = Array.sub ~pos ~len:(m - length_common) a in
            let b = Array.sub ~pos ~len:(n - length_common) b in
            _Eq (Trm._Concat a) (Trm._Concat b)
    | (Sized _ | Extract _ | Concat _), (Sized _ | Extract _ | Concat _) ->
        sort_eq x y
    (* x = α ==> ⟨x,|α|⟩ = α *)
    | x, ((Sized _ | Extract _ | Concat _) as a)
     |((Sized _ | Extract _ | Concat _) as a), x ->
        _Eq (Trm._Sized x (Trm.seq_size_exn a)) a
    | _ -> sort_eq x y

let vars p = Iter.flat_map ~f:Trm.vars (trms p)
