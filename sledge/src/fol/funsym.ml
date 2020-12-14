(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function Symbols *)

type t =
  | Rem
  | BitAnd
  | BitOr
  | BitXor
  | BitShl
  | BitLshr
  | BitAshr
  | Signed of int
  | Unsigned of int
  | Uninterp of string
[@@deriving compare, equal, sexp]

let pp ppf f =
  let pf fmt = Format.fprintf ppf fmt in
  match f with
  | Rem -> pf "%%"
  | BitAnd -> pf "&&"
  | BitOr -> pf "||"
  | BitXor -> pf "xor"
  | BitShl -> pf "shl"
  | BitLshr -> pf "lshr"
  | BitAshr -> pf "ashr"
  | Signed n -> pf "(s%i)" n
  | Unsigned n -> pf "(u%i)" n
  | Uninterp sym -> Trace.pp_styled `Bold "%s" ppf sym

let uninterp s = Uninterp s

let eval ~equal ~get_z ~ret_z ~get_q ~ret_q:_ f xs =
  match (f, xs) with
  | Rem, [|x; y|] -> (
    match get_z y with
    (* x % 1 ==> 0 *)
    | Some j when Z.equal Z.one j -> Some (ret_z Z.zero)
    | Some j when not (Z.equal Z.zero j) -> (
      match get_z x with
      (* i % j *)
      | Some i -> Some (ret_z (Z.rem i j))
      | None -> (
        match get_q x with
        (* (n/d) % i ==> (n / d) % i *)
        | Some {Q.num; den} -> Some (ret_z (Z.rem (Z.div num den) j))
        | None -> None ) )
    | _ -> None )
  | BitAnd, [|x; y|] -> (
    match (get_z x, get_z y) with
    (* i && j *)
    | Some i, Some j -> Some (ret_z (Z.logand i j))
    (* x && true ==> x *)
    | _, Some z when Z.is_true z -> Some x
    | Some z, _ when Z.is_true z -> Some y
    (* x && false ==> false *)
    | _, Some z when Z.is_false z -> Some y
    | Some z, _ when Z.is_false z -> Some x
    (* x && x ==> x *)
    | _ when equal x y -> Some x
    | _ -> None )
  | BitOr, [|x; y|] -> (
    match (get_z x, get_z y) with
    (* i || j *)
    | Some i, Some j -> Some (ret_z (Z.logor i j))
    (* x || true ==> true *)
    | _, Some z when Z.is_true z -> Some y
    | Some z, _ when Z.is_true z -> Some x
    (* x || false ==> x *)
    | _, Some z when Z.is_false z -> Some x
    | Some z, _ when Z.is_false z -> Some y
    (* x || x ==> x *)
    | _ when equal x y -> Some x
    | _ -> None )
  | BitShl, [|x; y|] -> (
    match get_z y with
    (* x shl 0 ==> x *)
    | Some z when Z.equal Z.zero z -> Some x
    | get_z_y -> (
      match (get_z x, get_z_y) with
      (* i shl j *)
      | Some i, Some j when Z.sign j >= 0 -> (
        match Z.to_int j with
        | n -> Some (ret_z (Z.shift_left i n))
        | exception Z.Overflow -> None )
      | _ -> None ) )
  | BitLshr, [|x; y|] -> (
    match get_z y with
    (* x lshr 0 ==> x *)
    | Some z when Z.equal Z.zero z -> Some x
    | get_z_y -> (
      match (get_z x, get_z_y) with
      (* i lshr j *)
      | Some i, Some j when Z.sign j >= 0 -> (
        match Z.to_int j with
        | n -> Some (ret_z (Z.shift_right_trunc i n))
        | exception Z.Overflow -> None )
      | _ -> None ) )
  | BitAshr, [|x; y|] -> (
    match get_z y with
    (* x ashr 0 ==> x *)
    | Some z when Z.equal Z.zero z -> Some x
    | get_z_y -> (
      match (get_z x, get_z_y) with
      (* i ashr j *)
      | Some i, Some j when Z.sign j >= 0 -> (
        match Z.to_int j with
        | n -> Some (ret_z (Z.shift_right i n))
        | exception Z.Overflow -> None )
      | _ -> None ) )
  | Signed n, [|x|] -> (
    match get_z x with
    (* (sN)i *)
    | Some i -> Some (ret_z (Z.signed_extract i 0 n))
    | _ -> None )
  | Unsigned n, [|x|] -> (
    match get_z x with
    (* (uN)i *)
    | Some i -> Some (ret_z (Z.extract i 0 n))
    | _ -> None )
  | _ -> None
