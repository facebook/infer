(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol
module T = Term
module F = Formula

let lookup_func lookup term =
  match Term.get_trm term with
  | Some (Apply (Uninterp name, [||])) -> lookup name
  | _ -> None

let uconst name = T.apply (Funsym.uninterp name) [||]
let global g = uconst (Llair.Global.name g)
let reg r = Var.identified ~name:(Llair.Reg.name r) ~id:(Llair.Reg.id r)
let regs rs = Var.Set.of_iter (Iter.map ~f:reg (Llair.Reg.Set.to_iter rs))
let uap0 f = T.apply f [||]
let uap1 f a = T.apply f [|a|]
let uap2 f a b = T.apply f [|a; b|]
let lit2 p a b = F.lit p [|a; b|]
let nlit2 p a b = F.not_ (lit2 p a b)

let rec ap_ttt : 'a. (T.t -> T.t -> 'a) -> _ -> _ -> 'a =
 fun f a b -> f (term a) (term b)
and ap_ttf (f : T.t -> T.t -> F.t) a b = F.inject (ap_ttt f a b)

and ap_fff (f : F.t -> F.t -> F.t) a b =
  F.inject (f (formula a) (formula b))

and ap_uut : 'a. (T.t -> T.t -> 'a) -> _ -> _ -> _ -> 'a =
 fun f typ a b ->
  let bits = Llair.Typ.bit_size_of typ in
  let unsigned x = uap1 (Unsigned bits) x in
  f (unsigned (term a)) (unsigned (term b))

and ap_uuf (f : T.t -> T.t -> F.t) typ a b = F.inject (ap_uut f typ a b)

and term : Llair.Exp.t -> T.t =
 fun e ->
  let imp p q = F.or_ (F.not_ p) q in
  let nimp p q = F.and_ p (F.not_ q) in
  let if_ p q = F.or_ p (F.not_ q) in
  let nif p q = F.and_ (F.not_ p) q in
  match e with
  (* formulas *)
  | Ap2 (Eq, Integer {bits= 1; _}, p, q) -> ap_fff F.iff p q
  | Ap2 (Dq, Integer {bits= 1; _}, p, q) -> ap_fff F.xor p q
  | Ap2 ((Gt | Ugt), Integer {bits= 1; _}, p, q) -> ap_fff nimp p q
  | Ap2 ((Lt | Ult), Integer {bits= 1; _}, p, q) -> ap_fff nif p q
  | Ap2 ((Ge | Uge), Integer {bits= 1; _}, p, q) -> ap_fff if_ p q
  | Ap2 ((Le | Ule), Integer {bits= 1; _}, p, q) -> ap_fff imp p q
  | Ap2 (Add, Integer {bits= 1; _}, p, q) -> ap_fff F.xor p q
  | Ap2 (Sub, Integer {bits= 1; _}, p, q) -> ap_fff F.xor p q
  | Ap2 (Mul, Integer {bits= 1; _}, p, q) -> ap_fff F.and_ p q
  (* div and rem are not formulas even if bits=1 due to division by 0 *)
  | Ap2 (And, Integer {bits= 1; _}, p, q) -> ap_fff F.and_ p q
  | Ap2 (Or, Integer {bits= 1; _}, p, q) -> ap_fff F.or_ p q
  | Ap2 (Xor, Integer {bits= 1; _}, p, q) -> ap_fff F.xor p q
  | Ap2 ((Shl | Lshr), Integer {bits= 1; _}, p, q) -> ap_fff nimp p q
  | Ap2 (Ashr, Integer {bits= 1; _}, p, q) -> ap_fff F.or_ p q
  | Ap3 (Conditional, Integer {bits= 1; _}, cnd, pos, neg) ->
      F.inject
        (F.cond ~cnd:(formula cnd) ~pos:(formula pos) ~neg:(formula neg))
  (* terms *)
  | Reg {name; id; typ= _} -> T.var (Var.identified ~name ~id)
  | Global {name; typ= _} | Function {name; typ= _} -> uconst name
  | Label {parent; name} ->
      uap0 (Funsym.uninterp ("label_" ^ parent ^ "_" ^ name))
  | Integer {typ= _; data} -> T.integer data
  | Float {data; typ= _} -> (
    match Q.of_float (Float.of_string_exn data) with
    | q when Q.is_real q -> T.rational q
    | _ | (exception Invalid_argument _) ->
        uap0 (Funsym.uninterp ("float_" ^ data)) )
  | Ap1 (Signed {bits}, _, e) ->
      let a = term e in
      if bits = 1 then
        match F.project a with
        | Some fml -> F.inject fml
        | _ -> uap1 (Signed bits) a
      else uap1 (Signed bits) a
  | Ap1 (Unsigned {bits}, _, e) ->
      let a = term e in
      if bits = 1 then
        match F.project a with
        | Some fml -> F.inject fml
        | _ -> uap1 (Unsigned bits) a
      else uap1 (Unsigned bits) a
  | Ap1 (Convert {src= Pointer _}, Pointer _, e) -> term e
  | Ap1 (Convert {src= Float _}, Float _, e) -> term e
  | Ap1 (Convert {src}, dst, e) ->
      let s =
        Format.asprintf "convert_%a_of_%a" Llair.Typ.pp dst Llair.Typ.pp src
      in
      uap1 (Funsym.uninterp s) (term e)
  | Ap2 (Eq, _, d, e) -> ap_ttf F.eq d e
  | Ap2 (Dq, _, d, e) -> ap_ttf F.dq d e
  | Ap2 (Gt, _, d, e) -> ap_ttf F.gt d e
  | Ap2 (Lt, _, d, e) -> ap_ttf F.lt d e
  | Ap2 (Ge, _, d, e) -> ap_ttf F.ge d e
  | Ap2 (Le, _, d, e) -> ap_ttf F.le d e
  | Ap2 (Ugt, typ, d, e) -> ap_uuf F.gt typ d e
  | Ap2 (Ult, typ, d, e) -> ap_uuf F.lt typ d e
  | Ap2 (Uge, typ, d, e) -> ap_uuf F.ge typ d e
  | Ap2 (Ule, typ, d, e) -> ap_uuf F.le typ d e
  | Ap2 (Ord, _, d, e) -> ap_ttf (lit2 (Predsym.uninterp "ord")) d e
  | Ap2 (Uno, _, d, e) -> ap_ttf (nlit2 (Predsym.uninterp "ord")) d e
  | Ap2 (Add, _, d, e) -> ap_ttt T.add d e
  | Ap2 (Sub, _, d, e) -> ap_ttt T.sub d e
  | Ap2 (Mul, _, d, e) -> ap_ttt T.mul d e
  | Ap2 (Div, _, d, e) -> ap_ttt T.div d e
  | Ap2 (Rem, _, d, e) -> ap_ttt (uap2 Rem) d e
  | Ap2 (Udiv, typ, d, e) -> ap_uut T.div typ d e
  | Ap2 (Urem, typ, d, e) -> ap_uut (uap2 Rem) typ d e
  | Ap2 (And, _, d, e) -> ap_ttt (uap2 BitAnd) d e
  | Ap2 (Or, _, d, e) -> ap_ttt (uap2 BitOr) d e
  | Ap2 (Xor, _, d, e) -> ap_ttt (uap2 BitXor) d e
  | Ap2 (Shl, _, d, e) -> ap_ttt (uap2 BitShl) d e
  | Ap2 (Lshr, _, d, e) -> ap_ttt (uap2 BitLshr) d e
  | Ap2 (Ashr, _, d, e) -> ap_ttt (uap2 BitAshr) d e
  | Ap3 (Conditional, _, cnd, thn, els) ->
      T.ite ~cnd:(formula cnd) ~thn:(term thn) ~els:(term els)
  | Ap1 (Select idx, typ, rcd) ->
      let off, len = Llair.Typ.offset_length_of_elt typ idx in
      let off = T.integer (Z.of_int off) in
      let len = T.integer (Z.of_int len) in
      T.extract ~seq:(term rcd) ~off ~len
  | Ap2 (Update idx, typ, rcd, elt) ->
      let oI, lI = Llair.Typ.offset_length_of_elt typ idx in
      let oJ = oI + lI in
      let off0 = T.zero in
      let len0 = T.integer (Z.of_int oI) in
      let len1 = T.integer (Z.of_int lI) in
      let off2 = T.integer (Z.of_int oJ) in
      let len2 = T.integer (Z.of_int (Llair.Typ.size_of typ - oI - lI)) in
      let seq = term rcd in
      T.concat
        [| T.extract ~seq ~off:off0 ~len:len0
         ; T.sized ~seq:(term elt) ~siz:len1
         ; T.extract ~seq ~off:off2 ~len:len2 |]
  | ApN (Record, typ, elts) ->
      let elt_siz i =
        T.integer (Z.of_int (snd (Llair.Typ.offset_length_of_elt typ i)))
      in
      T.concat
        (Array.mapi (IArray.to_array elts) ~f:(fun i elt ->
             T.sized ~seq:(term elt) ~siz:(elt_siz i) ))
  | Ap1 (Splat, _, byt) -> T.splat (term byt)

and formula e = F.dq0 (term e)
